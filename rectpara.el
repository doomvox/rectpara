;; rectpara-mode.el         Fri  December 20, 2002  22:10
;;                    Rev:  Wed  August   10, 2016  15:46

;; A variant of picture-mode to work on blocks of text layed out
;; roughly as rectangles, which I call "rectparas" for "rectangular paragraphs".
;; (see the example below).

;; This was originally called doomfile.el, because it's
;; original purpose was to work on "The Doomfiles"
;; (currently at http://obsidianrook.com/doomfiles).

;; Some utility commands are also included here (for now)
;; though they might not strictly be part of the mode.

;; Example:

;;   For some time
;;   I've been inclined
;;   to use emacs
;;   picture-mode           Using small,
;;   to write things        floating        Which I
;;   like this...           rectangular     tend to call
;;                          paragraphs.     rectparas.
;;     I've always
;;     wanted better
;;     tools to do
;;     this... it was
;;     an obvious idea       It only took me
;;     to write a            about 10 years
;;     mode for it.          to get to it.
;;
;;      This is a major         (And another 10
;;      mode, derived            to publish it.)
;;      from picture-mode


(provide 'rectpara-mode)
(eval-when-compile
  (require 'cl))

(require 'picture)

(define-derived-mode rectpara-mode
  picture-mode "Rectpara"
  "Major mode for editing rectparas ('rectangular paragraphs').
The editing commands are the same as in Picture mode, with
some additional commands to select or edit a rectpara:
\(\\[rectpara-mode-select-rectpara], \\[rectpara-mode-edit-rectpara]\).
\\{rectpara-mode-map}"
  (setq case-fold-search nil)
  (if (auto-fill-mode)
    (auto-fill-mode)))

;;; Grabbing Alt o as the doOomfile-mode prefix
(define-key rectpara-mode-map "\M-oe" 'rectpara-mode-edit-rectpara)
(define-key rectpara-mode-map "\M-or" 'rectpara-mode-select-rectpara)

(define-key rectpara-mode-map "\M-o\C-c" 'rectpara-mode-exit)

;; The following is cloned from picture-mode-exit.
;; (Can't just use the original, because it checks
;; the mode to make sure it's in picture).

(defun rectpara-mode-exit (&optional nostrip)
  "Undo rectpara-mode and return to previous major mode.
With no argument strips whitespace from end of every line in Picture buffer
  otherwise just return to previous mode."
  (interactive "P")
  (if (not (eq major-mode 'rectpara-mode))
      (error "You aren't in rectpara-mode.")
    (if (not nostrip) (picture-clean))
    (setq mode-name picture-mode-old-mode-name)
    (use-local-map picture-mode-old-local-map)
    (setq major-mode picture-mode-old-major-mode)
    (kill-local-variable 'tab-stop-list)
    (setq truncate-lines picture-mode-old-truncate-lines)
    (force-mode-line-update)))


;;; EDITING RECTPARAS

;;; Commands are provided to:

;;;   o automatically select the rectpara the cursor is on

;;;   o grab the current rectpara and move it to an edit buffer
;;;     to work on in isolation

;;;   o returning from the edit buffer, if the rectpara has become
;;;     bigger, it automatically tries to open up space for it.

;; These routines work with rows and columns numbering from 1
;; Note: picture-current-line also numbers from 1, though
;; the current-column uses an origin of zero.

;
;;                                 x-axis
;;   (1,1)                          =
;;     o------------------------>  columns
;;     |
;;     |           top
;;     |       +---------------+
;;     |       |               |
;;     |  left |               | right
;;     |       |               |
;;     |       +---------------+
;;     |           bottom
;;     |
;;     V
;
;;   y-axis
;;    =
;;   rows


;; The general approach in this code is to "crawl around" in
;; the plane, moving the cursor until you find what you're
;; looking for.

;; (An alternate approach might be to first read in the
;; coordinate data for all existing rectparas in a buffer,
;; and then then crunch this numeric model from then on.)


;; The boundaries of a rectpara are determined as follows:
;
;;   Presume cursor is inside the rectpara of interest.
;;   Crawl leftward, looking for two adjacent cols of space, three spaces high
;;   Crawl down the left edge, until a row with three spaces is found.
;;   Crawl up the left edge, until a row with three spaces is found.
;;   Find the longest line where, line ends are defined by this regexp:
;;      "[^.?!:]  \\|[.?!:]   " ; match 2 spaces or 3 after hard-stop

;;; Documenting some of the nitty gritty of how the code works:

;; picture-mode (and hence rectpara-mode) uses the "quarter-plane"
;; model, which is to say it fakes an infinite field of empty space
;; and quietly fills in actual spaces as you go moving around in it.
;; But this is just an illusion, and really the whitespace
;; can can be made up of tabs, newlines or actual spaces.

;; For the present I don't worry about tabs, since I've got my .emacs
;; tricked out like so:

;;  ; force emacs to always use spaces for tabbing
;;  (setq-default indent-tabs-mode nil)

;; But there's still the spaces vs. newline distinction to worry about
;; and I've had trouble getting general white space detection to work
;; reliably either via my own regexps, or using (thing-at-point 'whitespace)

;; My complaint about using thing-at-point: It doesn't work just to the
;; right of point.  If you're at the first character of a word,
;; thing-at-point calls that "whitespace", because there's a space off
;; to the left.

;; So, for the moment there's a kludge that I use repeatedly
;; in the following code: I twidlle the cursor location
;; forward and back with picture-* commmands, so if need be
;; they'll will fill in real spaces in the quarter-plane of virtual spaces.
;; then I can do simple matches for spaces
;;   (looking-at "   ")

;; Note that picture.el itself often uses character classes like this:
;;    (if (looking-at "[ \t]*$")
;; which is presumably the kind of thing I should be doing.


;;; Data structures:

;;  I specify a rectangle using four coordinates (x1, y1, x2, y2).
;;  The emacs style of region "start and end" is less robust
;;  (adding whitespace can change the values without a change to
;;  visible layout).

;;  A "rectpara" is just a "rectangle" which is a list of strings.

;;  A list of rectparas is a list of lists, with both the rectpara
;;  and the coords.

;; A pecularity of specifying rectangles (and hence rectparas):
;; You specify two points, the upper-left and lower-right,
;; where the top, bottom and left edges are included in the
;; rectangle, *but the right edge is excluded*.

;; There's no clean way of thinking about this...  you
;; don't put a box around the area, you put the right
;; edge just outside the boundary of text you're
;; interested in, but the other three edges are just
;; inside.

;; The rectpara metadata is stored in the *name* of the edit buffer,
;; including the rectpara coordinates and name of the buffer it's from.

;; An example edit buffer name:
;;   *rectpara edit: 8-61-30-231 PRETENTIOUS_RAMBLINGS*

;; This makes the edit buffer names for each rectpara
;; unique (though the main reason I did this is I didn't
;; understand buffer local variables when I came up with
;; this trick).

;;;
;;; some general functions:

(defun tlofp ()
  "Return t if on the top line of the file, nil otherwise"
  (save-excursion
    (beginning-of-line)
    (not (char-before))))

(defun last-line-p ()
  "Return t if on the bottom line of the file, nil otherwise"
  (save-excursion
    (end-of-line)
    (not (char-after))))

(defun rectpara-mode-report-if-tlofp ()  ; for debugging
  "Say something if you're on the top line of the file"
  (interactive)
  (if (tlofp) (message "stictly top line")))

(defun rectpara-mode-report_eolp ()      ; for debugging
  "If at the end of line, say so"
    (interactive)
    (if (eolp)
        (message "end o' the line, jack")
      ))

;; vertical-whitespace-p
;
;; Basic function to answer the question
;; does this column look blank in this region?
;; i.e. are there three consecutive spaces lined up
;; on top of each other?  STATUS: WORKS

(defun vertical-whitespace-p ()
  "Check near cursor to see if there's a 3 character vertical stretch of whitespace"
  (save-excursion
    (let (lower_char upper_char current_char tern)
      (setq current_char (following-char))
      (picture-move-down 1)
      (setq lower_char (following-char))
      (picture-move-up 2)
      (setq upper_char (following-char))

      (setq tern (concat (char-to-string upper_char)
                         (char-to-string current_char)
                         (char-to-string lower_char)))

      (string-match "^[ \t\n\000]+$" tern) )))

;;; TODO: possible efficiency hack: check (thing-at-point 'whitespace) first,
;;; return  nil if it fails don't have to bother with the rest.
;;; Except thing-at-point never seems to do what I want...

(defun report-vertical-whitespace-p ()  ; for debugging
  "If there's a vertical strip of whitespace, say so"
  (interactive)
  (if (vertical-whitespace-p)
      (message "yup, clear up and down")
    (message "nope, there's stuff here")))

(defun rectpara-mode-move-column (col)
  "Move to a given column, numbering from 1"
  (interactive)
  (move-to-column (- col 1) 't))

(defun rectpara-mode-move-row (row)
  "Move to a given row, numbering from 1"
  (interactive)
  (let ((col (current-column)))
    (goto-line row)
    (move-to-column col)))

(defun rectpara-mode-move-to-x-y-location (col row)
  "Move to a given location, given the column and row (numbering from 1)"
  (interactive)
  (goto-line row)
  (move-to-column (- col 1) 't))

(defun rectpara-mode-move-to-x-y-location-alt (col row)
  "Move to a given location, given the column and row (numbering from 1)"
  (interactive)
  (move-to-column (- col 1) 't)
  (goto-line row)
)


(defun rectpara-mode-convert-coords-to-start-end (coords)
  "Given a list of the rectpara coordinates x1 y1 x2 y2 return
emacs-style linear count start and end values.  Presumes that
you're in the buffer with the rectpara."
  (save-excursion
    (let (start end left top right bot)

    (setq left  (nth 0 coords))
    (setq top   (nth 1 coords))
    (setq right (nth 2 coords))
    (setq bot   (nth 3 coords))

    ; go to lower-right hand corner, the end
    (rectpara-mode-move-to-x-y-location right bot)
    (setq end (point))

    ; move point to upper-left hand corner, the start
    (rectpara-mode-move-to-x-y-location left top)
    (setq start (point))

    (list start end)
    )))


;; rectpara-mode-find-left-boundary --

;; Presumes that the cursor is inside of a rectpara at start
;; (just above or below works, too).  Returns the column
;; number of the left edge of the rectpara.

;; General method: crawls leftward, looking for a vertical
;; stretch of whitespace using the vertical-whitespace-p
;; function, or alternately the left edge of the screen.

;; This looks for *two* columns of whitespace to avoid being
;; confused (e.g. by a rectpara where word boundaries just happen
;; to line up over each other).

;; This complicates the logic, because there are
;; three different forms of success:
;; (1) there are two columns of whitespace to the left
;; (2) we're on the left margin of the buffer already
;; (3) there's only one space between us and left margin of buffer.

;; TODO finds a "boundary" even if it doesn't start inside a rectpara.
;; *not* currently inside a rectpara.
;; Logically, should report 'nil in that case.
;; Need a "rectpara_p" function?

(defun rectpara-mode-find-left-boundary ()
  "Find left side boundary column of a rectpara"

  (let ((loc 'nil))
    (save-excursion
      (catch 'FOUND
        ;; while loc is not yet defined, step to the left
        (while (progn (if (bolp)
                          (throw 'FOUND (setq loc 1))
                        ((lambda ()
                           (picture-backward-column 1)
                           (if (vertical-whitespace-p)
                               ;; peek ahead, look for more vertical whitespace,
                               ;; *or* the left edge of the screen
                               ((lambda ()
                                  (if (bolp)
                                      (throw 'FOUND (setq loc 2))
                                    (save-excursion
                                      (let ((lastcol (current-column)))
                                        (picture-backward-column 1)
                                        (if (vertical-whitespace-p)
                                            (throw 'FOUND
                                                   (setq loc (+ lastcol 2)))))))))))))
                      (not loc)))))
    loc))


(defun rectpara-mode-report-left-boundary ()
  "Find left side boundary of a rectpara"
  (interactive)
  (let ((loc 'nil)
        (starting_from (1+ (current-column))))
    (setq loc (rectpara-mode-find-left-boundary))
    (message (concat "Yow! The col found was " loc " starting from " starting_from))))


(defun rectpara-mode-move-left-boundary ()
  "Move to the left boundary of a rectpara"
  (interactive)
  (move-to-column (- (rectpara-mode-find-left-boundary) 1) 't))


;;;

;; rectpara-mode-find-lower-boundary -

;; This is functional enough, though it should be renamed:
;; It isn't reliable unless working on the left edge of a rectpara.
;; (because typically, the last line of a rectpara has spaces
;; at the end, that fool it into thinking it's at the bottom one row ahead
;; of schedule).

;; Crawls downward, if it finds whitespace, it does a regep
;; match for three spaces.  Jumps forward and back with
;; picture commands, to get it to explicitly fill in spaces
;; per the quarter-plane illusion.

;; (Had problems getting regexps to work, so used this kludge to simplify them.)

(defun rectpara-mode-find-lower-boundary ()
  "Find lower boundary of a rectpara. First row is 1.
Expects to be started on the left boundary, and crawl
down it to the lower left corner."
  (interactive)
  (let ((loc 'nil))
    (save-excursion
      (while
          (progn
            ((lambda ()
               (picture-move-down 1)
               (save-excursion
                 (picture-forward-column 3)
                 (picture-backward-column 3)
                 (if (looking-at "   ") ; three spaces
                     (setq loc (- (picture-current-line) 1) )))
               (not loc))))))
    loc))


(defun rectpara-mode-report-lower-boundary ()
  "Report lower boundary of a rectpara"

  (interactive)
  (let ((loc 'nil)
        (starting_from (picture-current-line)))
    (setq loc (rectpara-mode-find-lower-boundary))
    (message (concat "The lower boundary is at " loc " starting from " starting_from))))

(defun rectpara-mode-move-lower-boundary ()
  "Move to the lower boundary of a rectpara"

  (interactive)
  (let ((col (current-column)))
    (goto-line (rectpara-mode-find-lower-boundary))
    (move-to-column col)))


;;; rectpara-mode-find-upper-boundary --
;;;
;;; (Note, this one is a hybrid of the find-lower-boundary,
;;; and the find-left-boundary routine. )


;;; Like find-left-boundary, it's a more reliable to
;;; run this on the right edge of rectpara.  ((Huh? Left edge, right?))


(defun rectpara-mode-find-upper-boundary ()
  "Find upper boundary of a rectpara
   Expects to be started on the left boundary, and crawl up
   it to the upper left corner."
  (let ((loc 'nil))
    (save-excursion
      (catch 'gotcha
        (while
            (progn
              ((lambda ()

                 (if (tlofp)
                     (throw 'gotcha (setq loc 1)))

                 (picture-move-up 1)

                 (save-excursion
                   (picture-forward-column 3)
                   (picture-backward-column 3)
                   (if (looking-at "   ") ; three spaces
                       (setq loc (+ (picture-current-line) 1) )))

                 (not loc)))))))
    loc))


(defun rectpara-mode-report-upper-boundary ()
  "Report upper boundary of a rectpara"
  (interactive)
  (let ((loc 'nil)
        (starting_from (picture-current-line)))
    (setq loc (rectpara-mode-find-upper-boundary))
    (message (concat "The upper boundary is at " loc " starting from " starting_from))))


(defun rectpara-mode-move-upper-boundary ()
  "Move to the upper boundary of a rectpara"
  (interactive)
  (let ((col (current-column)))
    (goto-line (rectpara-mode-find-upper-boundary))
    (move-to-column col)))



;; rectpara-mode-find-rectpara-boundaries

;; Find left bound, move to left bound, find top and bottom bounds,
;; then iterate through the lines, checking the line lengths.
;; Select the maximum as the rectpara right bound.

(defun rectpara-mode-find-rectpara-boundaries ()
  "Find boundaries of rectpara cursor is inside of.
Returns list of coords: x1 y1 x2 y2"
  (let (left bot top right)
    (save-excursion
      (setq left (rectpara-mode-find-left-boundary))
      (rectpara-mode-move-column left) ;; Better than (rectpara-mode-move-left-boundary) -- redundant
      (setq bot (rectpara-mode-find-lower-boundary))
      (setq top (rectpara-mode-find-upper-boundary))
      (setq right (rectpara-mode-find-right-boundary left top bot))
      (list left top right bot))))      ; x1 y1, x2 y2


(defun rectpara-mode-report-boundaries ()  ; largely for debugging
  "Report boundaries of the current rectpara"
  (interactive)
  (let (coords)
    (setq coords (rectpara-mode-find-rectpara-boundaries))
  (message (mapconcat 'number-to-string coords " "))))


;; rectpara-mode-find-right-boundary -

;; Using pattern that allows two-space right hand
;; boundaries, unless there's a hard-stop punctuation
;; there, then require three-space.

(defun rectpara-mode-find-right-boundary (left top bot)
  "Find the right boundary of the current rectpara,
   given the other three edge boundaries."

  (let ((line top)
        (col 0)
        (rp_line_end 'nil))
    (save-excursion
    (rectpara-mode-move-to-x-y-location left top)
    (while (<= line bot)
      (while (progn ; crawl to right, look for end of this line
               (if (looking-at "[^.?!:]  \\|[.?!:]   ") ; match 2 spaces or 3 after hard-stop
                   (setq rp_line_end (current-column)))
               (picture-forward-column  5)
               (picture-backward-column 4)
               (not rp_line_end)))
      ; looking for longest line
      (if (> rp_line_end col)
          (setq col rp_line_end))
      ; prepare for next iteration
      (picture-move-down 1)
      (setq line (1+ line))
      (setq rp_line_end 'nil)
      (rectpara-mode-move-column left))
    (setq col (+ col 2))))) ;; empirically determined need for +2
                            ;; one is due to (current-column) numbering from 0
                            ;; the other because eol regexp works from one-char back (?)


;; This routine remains *slightly* flaky: sometimes you need to
;; run the command twice to leave the rectpara selected.
;; Amazingly enough, this behavior does not seem to be amenable
;; to any logical (or otherwise) attempt at solution, e.g.  it
;; works *better* if you bang out on an error condition
;; e.g. (error "").
;; If you value your sanity, do *not* work on this bug.
(defun rectpara-mode-select-rectpara ()
  "Select the current rectpara.
i.e. place mark and point so that a rectangle surrounds the
current rectpara with the region active."
  (interactive)
  (let (
        (coords (rectpara-mode-find-rectpara-boundaries)) ;; (list left top right bot)
        left top right bot
        )
    (setq left  (car coords))
    (setq top   (nth 1 coords))
    (setq right (nth 2 coords))
    (setq bot   (nth 3 coords))

    ;; go to lower-right hand corner (which becomes the mark)
    (rectpara-mode-move-row top)
    (rectpara-mode-move-column right)
    (rectpara-mode-move-row bot)

    ;; NOTE: All of the above works.  We know the boundaries of
    ;; the rectpara now and just need to set mark and point
    ;; and leave the region active

    (let ( (future_mark (point)) )

      ;; move point to upper-left hand corner
      (rectpara-mode-move-row top)
      (rectpara-mode-move-column left)

      ;; makes region active (most of the time... but do NOT think about that).
      (push-mark future_mark t t))

    (message "left: %d top: %d right: %d bottom: %d" left top right bot)
    ))


(defun rectpara-mode-find-rectpara-boundaries ()
  "Find boundaries of rectpara cursor is inside of.
Returns list of coords: x1 y1 x2 y2"
  (let (left bot top right)
    (save-excursion
      (setq left (rectpara-mode-find-left-boundary))
      (rectpara-mode-move-column left) ;; Better than (rectpara-mode-move-left-boundary) -- redundant
      (setq bot (rectpara-mode-find-lower-boundary))
      (setq top (rectpara-mode-find-upper-boundary))
      (setq right (rectpara-mode-find-right-boundary left top bot))
      (list left top right bot))))      ; x1 y1, x2 y2


;; An alternate version that assumes you're in the edit mode
;; buffer. This is oriented toward grabbing everything in the
;; buffer, even if it's multiple rectparas.
(defun rectpara-mode-edit-find-rectangle-boundaries ()
  "Find boundaries of rectpara cursor is inside of.
Returns list of coords: x1 y1 x2 y2"
  (let (left bot top right)
    (save-excursion
      (setq left 1)  ;; 1 based indexing, right?
      (setq top  1)
      (goto-char (point-min))  ;; so: skip the "mystery" kludge then?
      (let ( (i     0)
             (max   0)
             (candi 0)
             (width 0) )
        (while (<= i (point-max))
          (move-end-of-line 1) ;; really need that 1?  huh?
          (setq candi (current-column))

          (if (> candi max) (setq max candi))

          (forward-line) ;; or whatever move down one
          (setq i (1+ i))
          )
        (setq right (+ 1 max))

        (setq bot
              (count-lines (point-min) (point-max)))
       (list left top right bot)))))      ; x1 y1, x2 y2

;; was: rectpara-mode-extract-rectpara
(defun rectpara-mode-edit-extract-rectpara ()
  "Extract the current rectangle"
  (let (coords start-end start end rectpara)
    (setq coords (rectpara-mode-edit-find-rectangle-boundaries))
    (setq start-end (rectpara-mode-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))

    (setq rectpara (extract-rectangle start end))
   ))

(defun rectpara-mode-get-width-rectpara (rectpara)
"Get the width of the given rectpara"
     (let ( (l (- (length rectpara) 1))
            (i 0)
            (max 0)
            (candi 0)
            (width 0) )
       (while (<= i l)
         (setq candi (length (nth i rectpara)))
         (if (> candi max) (setq max candi))
         (setq i (1+ i))
         )
       (setq width max)))


;; Nickname: "edit"
(defun rectpara-mode-edit-rectpara ()
  "Extract the current rectpara to another buffer for easy editing"

  (interactive)
  (let ( rectpara edit_buffer_name buffy coords start-end start
         end left top right bot pair-pos col row)

;;    (setq rectpara-with-coords (rectpara-mode-extract-rectpara-with-coords))
    (setq rectpara-with-coords
          (rectpara-mode-extract-rectpara-with-coords-and-rel-pos))

    (setq rectpara (car rectpara-with-coords))
    (setq coords   (car (cdr rectpara-with-coords)))
    (setq pair-pos (nth 2 rectpara-with-coords))

    (setq left  (nth 0 coords))
    (setq top   (nth 1 coords))
    (setq right (nth 2 coords))
    (setq bot   (nth 3 coords))

    (setq col  (nth 0 pair-pos))
    (setq row  (nth 1 pair-pos))

    ;; force two window display when editing
    (one-window-p 't)

    (setq edit_buffer_name
          (format "*rectpara edit: %d-%d-%d-%d %s*" left top right bot (buffer-name)))

    (setq buffy (generate-new-buffer edit_buffer_name))

    (switch-to-buffer-other-window buffy)
    (insert-rectangle rectpara)

    ;; move to correct relative position in the new buffer
    (rectpara-mode-move-to-x-y-location (1+ (- col left)) (1+ (- row top)))


    ;; TODO AUGUST around here, go into a new text mode variant: rectpara-edit-mode

    (turn-on-auto-fill)
    (set-fill-column
     (rectpara-mode-get-width-rectpara rectpara))
         ;; TODO AUGUST on return adjust fill-col first to match what's in the buffer


    ;;; Suspect that this would work by itself, rather than the above line:
    ;;;       (set-fill-column (current-column))

     (local-set-key "\C-x#"    'rectpara-mode-edit-return)
     (local-set-key "\C-c\C-c" 'rectpara-mode-edit-return)

     (message "Use either C-x # or C-c c-c to replace the original rectpara with your edits")
     ))



;;; This is how I'm trying to handle:
;;;  "rectpara size change collision problems":

;;; Handle case of height first... Go to the "end", look to
;;; the left and right, if there's anything there, snag
;;; rectangles move out of the way, insert enough blank
;;; lines to handle the problem, then replace the snagged
;;; rectangles.

;;; (But... no need for this whitespace insert unless new
;;; rectangle will encroach on old, so check that first.)


;;; Width increase... look to the right, snag anything in the way
;;; do the rectpara insert, then put the rectangles back in
;;; locations shifted over enough that there'll be no collision.

;;; got to watch out for secondary effects, move rectparas
;;; in the way of other rectparas... recursive method?



;; rectpara-mode-edit-return - Handles the
;; most common case well:  If edited rectpara
;; has gotten taller (had lines added) will add that much
;; whitespace so that it can't over-write anything below
;; it, without messing with rectparas adajcent to it.

;; STATUS:  working, but not exactly elegant.

;;; BUG: horizontal expansion of edited rectpara
;;; can still overwrite material on the right.
;;; should automatically move things aout of the way.


;; TODO AUGUST (quoting edit):
;;     (turn-on-auto-fill)
;;     (set-fill-column
;;      (rectpara-mode-get-width-rectpara rectpara))
;;          ;; TODO AUGUST on return adjust fill-col first to match what's in the buffer

(defun rectpara-mode-edit-return ()
  "Return edited rectpara to the original buffer inserting space as needed."
  (interactive)
  (let* (
        rectpara
        left top right bot
        edit_buffer_window

        (buffer_md
         (rectpara-mode-metadata-from-this-edit-buffer-name))
        (target_buffer (nth 0 buffer_md))
        (coords        (nth 1 buffer_md))
        )

    ;; Note: we're in the edit buffer window now
    (setq rectpara (rectpara-mode-edit-extract-rectpara))

    (rectpara-mode-zap-this-edit-window target_buffer)

    (let* (
           (start-end (rectpara-mode-convert-coords-to-start-end coords))
           (start (car start-end))
           (end   (nth 1 start-end))
           )
      (clear-rectangle start end)
      )

    ;; rectpara size change collision avoidance:
    ;;   if the rectpara has grown during editing, must
    ;;   open up whitespace in the target buffer
    (let* (
           (left  (nth 0 coords))
           (top   (nth 1 coords))
           (right (nth 2 coords))
           (bot   (nth 3 coords))

           (new_width (rectpara-mode-get-width-rectpara rectpara))
           (old_width (- right left))

           (new_height (length rectpara))
           (old_height (1+ (- bot top)))
          )

      ;; TODO SOMEDAY: cover case of horizontal expansion, opening
      ;;               blank columns needed to the right.
      (if (> new_width old_width)
          (message "WARNING: gotten fatter: changed to %d from %d" new_width old_width)
        )

      (if (> new_height old_height)
          (rectpara-mode-deal-with-vertical-expansion new_height old_height coords)
        )

      (rectpara-mode-move-to-x-y-location left top)

      (picture-insert-rectangle rectpara)
      (exchange-point-and-mark)
    )))

(defun rectpara-mode-deal-with-vertical-expansion (new_height old_height coords)
  "Juggle things out of the way so expanded rectpara will fit."
  ;; if the new rectangle has gotten taller we temporarily
  ;; hide all other rectparas adjacent to the old bottom
  ;; edge, insert blanks lines, then restore the adjacent
  ;; rectparas where they were.
  (let* (
         (left  (nth 0 coords))
         ;; (top   (nth 1 coords))
         (right (nth 2 coords))
         (bot   (nth 3 coords))

         (vertical_delta (- new_height old_height))

         toe_room
         blank_lines_needed
         temp_hidden_rectparas
         empirical_correction_shift
         )

;;       (message "rectpara gotten taller: changed to %d from %d"
;;                new_height old_height) ;; DEBUG

      (rectpara-mode-move-row bot)
      (setq toe_room
            (rectpara-mode-open-how-far-down vertical_delta left right))

      ;; TODO hack:
      ;; when vertical expansion is 1, 2 works here, 3 works otherwise.
      (if (> vertical_delta 1)
          (setq empirical_correction_shift 3) ;
        (setq empirical_correction_shift 2))

      ;; difference in toe_room and expansion is how much we need to open
      (setq blank_lines_needed
            (+ (- vertical_delta toe_room) empirical_correction_shift) )

      (setq temp_hidden_rectparas
            (rectpara-mode-extract-rectpars-with-coords-on-line))
      (picture-open-line  blank_lines_needed) ;; insert blank lines
      (rectpara-mode-restore-rectparas-from-list temp_hidden_rectparas)
    ))


(defun rectpara-mode-metadata-from-this-edit-buffer-name ()
  "Interprets the current edit_buffer name, extracting target
buffer and x/y coordinates.  Returns a list of the two (where
the coordinates are themselves a list of 4).
Note: the edit buffer is expected to have a name like:
  *rectpara edit: 8-61-30-231 PRETENTIOUS_RAMBLINGS*
"
;; we need to pick out the original buffer name (there at the end) and
;; the coordinate values (x1 y1 x2 y2) defining the original rectangle
;; (those are in the middle, a hyphen separated list).
  (let ((edit_buffer (buffer-name))
        templist
        )

    (setq templist (split-string edit_buffer " "))

    (if (not (string= (car templist) "*rectpara"))
        (error "This is not a rectpara edit buffer: %s" edit_buffer))

    (setq coords (mapcar 'string-to-number
                         (split-string (nth 2 templist) "-")))


    ;; To get the actual buffer name to return to, we need to truncate
    ;; at the asterix. Also chops any emacs versioning like "<2>"
    (setq target_buffer (car (split-string (nth 3 templist) "*")))

    (list target_buffer coords)
   ))

(defun rectpara-mode-zap-this-edit-window (rectpara-buffer)
  "Close the currently active edit window and return to the source RECTPARA-BUFFER."
  ;; This is neater behavior than just doing a (switch-to-buffer rectpara-buffer)
  ;; TODO it just closes the window, and preserves the edit buffer.
  ;; With greater confidence, you'd delete the edit buffer
  ;; (could save to disk for paranoia's sake...)
  (let* ( (edit-buffer-window (selected-window))
          )
    (delete-window edit-buffer-window)
    (set-buffer rectpara-buffer)
    ))

(defun rectpara-mode-copy-rectpara-with-coords ()
  "Returns current rectpara along with a list of x-y coordinates"
  (let (coords start-end start end rectpara)
    (setq coords (rectpara-mode-find-rectpara-boundaries))

    (setq start-end (rectpara-mode-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))
    (setq rectpara (extract-rectangle start end)) ; note "extract" doesn't remove
    (list rectpara coords)))


(defun rectpara-mode-extract-rectpara-with-coords ()
  "Returns current rectpara along with a list of x-y coordinates, clears the original rectangle"
  (let (saveloc coords start-end start end rectpara)
    (setq coords (rectpara-mode-find-rectpara-boundaries))

    (setq start-end (rectpara-mode-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))
    (setq rectpara (extract-rectangle start end))
    (setq rectpara (extract-rectangle start end))
    (clear-rectangle start end)  ;;; extract implies removal
    (list rectpara coords)))

(defun rectpara-mode-extract-rectpara-with-coords-and-rel-pos ()
  "Returns current rectpara along with a list of x-y coordinates, clears the original rectangle"
  (let (saveloc coords start-end start end rectpara pos-pair)
    (setq saveloc (point)) ;; TODO use this to get relative position inside rectpara

    (setq pos-pair (rectpara-mode-x-and-y-from-pos saveloc))

    (setq coords (rectpara-mode-find-rectpara-boundaries))

    (setq start-end (rectpara-mode-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))
    (setq rectpara (extract-rectangle start end))
    (setq rectpara (extract-rectangle start end))
    (clear-rectangle start end)  ;;; extract implies removal
    (list rectpara coords pos-pair)))

;;;

(defun rectpara-mode-x-and-y-from-pos (&optional pos)
  "Given position POS, return column and row pair.
The argument pos defaults to the current point.
The column and row numbers are 1 indexed."
  (interactive) ;; TODO which I abuse horribly during development
  (let ( (saveloc (point) )
         column row
         )
    (unless pos
      (setq pos saveloc))

    (goto-char pos)
    (setq column (1+ (current-column)))
    (setq row (rectpara-mode-current-row))

    (goto-char saveloc)
    (message "r: %d c: %d" row column);; DEBUG
    (list column row);; TODO do I have a convention on this?  row/column or x/y?
  ))

(defun rectpara-mode-current-row ()
  "Get the row of the current position.
Probably should be 1 indexed, no?"
  (interactive) ;; TODO which I abuse etc.
  (let ( (pos (point) )
         row
         )
    (setq row
          (count-lines (point-min) (1+ pos)))
    (message "%d" row);; DEBUG
    row))


(defun rectpara-mode-coords-of-rectparas-on-line ()
   "Looks for all rectparas intersecting the given line and returns their coordinates"
   (save-excursion
   (let (coords_list coords)
     (picture-beginning-of-line)

     (while (not (eolp))
       (if (looking-at "[^ ]")
           ((lambda ()
              (setq coords (rectpara-mode-find-rectpara-boundaries))
              (rectpara-mode-move-column (1+ (nth 2 coords)))

;;              (setq coords_list (cons coords coords_list)) ; pushing onto the front
              (setq coords_list (append coords_list (list coords))) ; shifting on to the end

              ))
         (picture-forward-column 1)))

     coords_list)))


(defun rectpara-mode-report-coords-of-rectparas-on-line ()
  "Find the coordinates of all rectparas on the current line, and spit out messages about them."
   (interactive)
   (let ((coords_list (rectpara-mode-coords-of-rectparas-on-line))
         coords)
     (while coords_list
       (setq coords (car coords_list))
       (message "x1: %d y1: %d x2: %d y2: %d"
                (nth 0 coords)
                (nth 1 coords)
                (nth 2 coords)
                (nth 3 coords)
       (setq coords_list (cdr coords_list))))))


;; rectpara-mode-extract-rectpars-with-coords-on-line --
;
;; scans horizontally across the row one below the bottom edge of original
;; rectpara.  when non-blank, runs
;;   rectpara-mode-extract-rectpara-with-coords
;; (a way of moving rectparas out of the way temporarily)

;;; Add optional parameter to specify the line to look at?

(defun rectpara-mode-extract-rectpars-with-coords-on-line ()
   "Looks for all rectparas intersecting the current line and
removes them, returns them with their coordinates"
   (let (result_list coords rectpara-with-coords next_col)
     (save-excursion
       (picture-beginning-of-line)
       (while (not (eolp))
         (if (looking-at "[^ ]")
             ((lambda ()
                (setq rectpara-with-coords (rectpara-mode-extract-rectpara-with-coords))
                (setq coords (car (cdr rectpara-with-coords)))
                (setq next_col (1+ (nth 2 coords)))
                (rectpara-mode-move-column next_col)

                (setq result_list (append result_list (list rectpara-with-coords)))
                                        ; shifting on to the end of the list
                ))
           (picture-forward-column 1)))

       result_list)))

;; Returns a list of elements which are in turn each a list
;; of two items, which are in turn also lists: one them
;; is a rectpara/rectangle (i.e. a list of strings), and the
;; other is a list of 4 items (the coords of the
;; rectpara)....


(defun rectpara-mode-report-of-rectparas-and-coords-on-line ()
  "Report the extracted rectparas with coordinates which were all found on the current line"
   (interactive)
   (let ((big_list (rectpara-mode-extract-rectpars-with-coords-on-line))
         coords rectpara)
     (while big_list
       (setq coords (car (cdr (car big_list))))

       (message "x1: %d y1: %d x2: %d y2: %d"
                (nth 0 coords)
                (nth 1 coords)
                (nth 2 coords)
                (nth 3 coords))

       (setq rectpara (car (car big_list)))

       (message "First line of rectpara: %s" (nth 0 rectpara))

       (setq big_list (cdr big_list)))))


(defun rectpara-mode-remove-rectparas-in-list (big_list)
  "Actually remove the rectparas in the given list"
   (let (coords rectpara start-end start end)
     (while big_list
       (setq coords (car (cdr (car big_list))))

       (setq start-end (rectpara-mode-convert-coords-to-start-end coords))
       (setq start (car start-end))
       (setq end (nth 1 start-end))

       (clear-rectangle start end)

       (setq big_list (cdr big_list)))))

(defun rectpara-mode-restore-rectparas-from-list (big_list)
  "Restore the rectparas in the given list"
   (interactive)
   (let (coords rectpara x1 y1)
     (save-excursion
       (while big_list
         (setq coords (car (cdr (car big_list))))

         (setq x1 (nth 0 coords))
         (setq y1 (nth 1 coords))
;;        (setq x2 (nth 2 coords))
;;        (setq y2 (nth 3 coords))

         (rectpara-mode-move-to-x-y-location x1 y1)
         (setq rectpara (car (car big_list)))
         (picture-insert-rectangle rectpara)
         (setq big_list (cdr big_list))))))


(defun rectpara-mode-get-line-segment-as-string (left right &optional line-number)
  "Gets the requested piece of a line as a string.  Default: current line.
Returns the substring between LEFT and RIGHT.  When the string is
too short will pad with spaces (this assists in faking an infinite quarter-plane."
  (interactive)

  (let ( (saveloc (point) )
        line-start line-end line segment )

    (if line-number
        (goto-line line-number))

    ;; snag the entire line from the buffer
    (picture-beginning-of-line)
    (setq line-start (point))
    (picture-end-of-line)
    (setq line-end (point))

    (setq line
          (buffer-substring line-start line-end))

    ;; pad the line out to column indicated by 'right'
    (setq needed_padding
          (- right (length line) ))
    (if (> needed_padding 0)
        (setq line
              (concat line
                      (make-string needed_padding ?\ )
                      )) )

    ;; get the requested segment
    (setq segment
          (substring line left right))

    (goto-char saveloc)
    ;; (message "123  ret: >>%s<<" segment) ;; DEBUG
    segment))

;; Formerly: rectpara-mode-look-down-how-far-to-end-of-whitespace
(defun rectpara-mode-open-how-far-down (checkdistance left right)
  "Looks downward to see how far until the end of open space.
From the current row, scans down through the given CHECKDISTANCE,
verifying that there is whitespace between the LEFT and RIGHT
boundaries, returning the end location.  Note: the maximum return
value is the CHECKDISTANCE.  As a side-effect, creates lines at
the bottom of the buffer if the region we're checking extends
down past it."
  (let ( (saveloc (point))
         (i 0)
         (limit checkdistance)
        )
    (catch 'UP
      (while (< i checkdistance)
        (let ( (line
                (rectpara-mode-get-line-segment-as-string left right))
               )

          (if (not (string-match "^[ ]*$" line))
              (throw 'UP (setq limit i))
            )

          ;; advance to next line, creating a blank line if at end-of-buffer
          (picture-move-down 1)
          (setq i (1+ i))
          )))
    (setq limit (1+ i))  ;;; Why plus one?  Try without? TODO
    (goto-char saveloc)
    limit))

;;; ======
;;; TODO

;;; o  litters emacs with open rectpara edit buffers.
;;;    When more confident in this code, could delete old buffer
;;;    upon return-from.

;;; o  Anyway: the edit buffers should be saved to file locations
;;;    in /tmp so it doesn't keep beeping at you when you do C-x C-s
;;;    out of habit.


;;; o  implement a "skip to next rectpara" command
;;;    look for nearest rectpara downward and to the right
;;;    when there's more than one, go to the "first"
;;;    one (leftmost?).  Provide a different command to
;;;    cycle through the possible choices from the last "next".

;;; o  compose-rectpara-here (like edit, but without initial text).

;;; o  during an edit, should preserve the relative cursor location

;;; o  the edit buffer should have it's own mode (rectpara-edit-mode)
;;;    derived from text-mode.


;;; BUGS

;;; o  After edit, rightward expansion can overwrite things.


;;; o  editing rectparas one char wide eats a column of
;;;    spaces to it's right upon a return-from-edit.  This is
;;;    repeatable.  (Think it stems from picture-mode bug).


;;; o  Occasionally, rectpara-mode-select-rectpara does not leave
;;;    rectpara selected correctly (but do NOT think about this).


;;; TODO some ideas for better ways of determining the right boundary
;;;      (as in rectpara-mode-find-right-boundary)

;;; Possibly: do some sort of discard of "bad" lengths?
;;; (if a line looks much shorter than others, look past the
;;; "two-space" gap you thought was the end).

;;; Maybe, just peek up or down at adjacent lines
;;; on ambiguous cases? (like two-space after full-stop)
;;; (Same thing as clever lengths processing, but maybe
;;; easier to envision as a crawling around process).
