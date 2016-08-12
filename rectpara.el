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
;; model, which is to say it fakes an infinte field of empty space
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

;;  Where ever possible, I specify a rectangle using four
;;  coordinates (x1, y1, x2, y2) in part because the emacs style
;;  "start and end" strikes me as less robust (without changing
;;  the visible layout, you could add whitespace at the end of
;;  lines that throw off the start and end values).

;;  Typically then, there will be a coords variable, containing
;;  a list of these four numbers.  When I want the coords and
;;  the contents of a rectpara together, I use a list of two lists.
;;  (Remember, a "rectpara" is just a "rectangle" which is
;;  a list of strings).  The next larger structure I use is
;;  the list of rectparas (a list of lists of two list elements,
;;  the rectpara and the coords).

;; A pecularity of specifying rectangles (and hence rectparas):
;; You specify two points, the upper-left and lower-right,
;; but while the left column is included the right column
;; is not.  Both the top and bottom rows are included.
;; There's no clean way of thinking about this...
;; you don't put a box around the area, nor do you put
;; the box just inside the area.  You put the box inside the
;; rectangle except for the right edge, which is outside.

;; E.g. to select this rectpara, you need mark and
;; point on both the X's:

;;    Xanadu did
;;    decree a
;;    stately
;;    pleasure
;;    bone to be
;;    doomed.   X

;; Internally this rectpara becomes a structure like:

;; (  ("Xanadu did"
;;    "decree a  "
;;    "stately   "
;;    "pleasure  "
;;    "bone to be"
;;    "doomed.   " )
;;  (6 228 16 233) )

;;;

;; I use a clever (albiet not necessarily good) method of passing
;; rectpara data to and from the edit buffer:  the original
;; rectpara coordinates (including the buffer name it was in)
;; becomes encoded into the name of the edit buffer.
;; and extracted again later by "rectpara-mode-return-from-edit-rectpara"

;; An example edit buffer name:
;;   *rectpara edit: 8-61-30-231 PRETENTIOUS_RAMBLINGS*

;; (I didn't understand buffer local variables when I came
;; up with this trick.)

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


(defun rectpara-mode-convert-coords-to-start-end (coords)
  "Given a list of the rectpara coordinates x1 y1 x2 y2 return
emacs-style linear count start and end values.  Presumes that
you're in the buffer with the rectpara."
  (save-excursion
    (let (start end left top right bot)

    (setq left (nth 0 coords))
    (setq top (nth 1 coords))
    (setq right (nth 2 coords))
    (setq bot (nth 3 coords))

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

;; This version is farily robust, because it looks for *two*
;; columns of whitespace to avoid being confused by a
;; rectpara where word boundaries just happen to line up over
;; each other.

;; This complicates the logic slightly, because there are
;; three slightly different forms of success:
;; (1) we're on the left margin of the buffer already (we're in column "1")
;; (2) there's only one space between us and left margin (column is "2")
;; (3) there are two adjacent columns of whitespace.

;; Nitty gritty: overall while loop continues while
;; "loc" is not yet defined.

;; Loop moves the cursor backward, continually looking for
;; vertial whitespace.  When it's found, it peeks ahead,
;; looking for either the left edge of the
;; screen or more whitespace.  If not found, it continues
;; crawling, looking for the next vertical whitespace.

;;; Slight peculiarity: finds a "boundary" even if it's
;;; *not* currently inside a rectpara.  Logically,
;;; should report 'nil or something in that case, right?
;;; Need a "rectpara_p" function?

(defun rectpara-mode-find-left-boundary ()
  "Find left side boundary column of a rectpara"

  (let ((loc 'nil))
    (save-excursion
      (catch 'gotcha
        (while (progn (if (bolp)
                          (throw 'gotcha (setq loc 1))
                        ((lambda ()
                           (picture-backward-column 1)
                           (if (vertical-whitespace-p)
                               ((lambda ()
                                  (if (bolp)
                                      (throw 'gotcha (setq loc 2))
                                    (save-excursion
                                      (let ((lastcol (current-column)))
                                        (picture-backward-column 1)
                                        (if (vertical-whitespace-p)
                                            (throw 'gotcha (setq loc (+ lastcol 2)))))))))))))
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
;; picture commands, to get it to explicityly fill in spaces
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
    (setq col (+ col 2))))) ; empirically determined need for +2
                            ; one is due to (current-column) numbering from 0
                            ; the other possibly because eol regexp works from one-char back

;;; Working on better ways of determining the right boundary could be fun:

;;; Possibly: do some sort of discard of "bad" lengths?
;;; (if a line looks much shorter than others, look past the
;;; "two-space" gap you thought was the end).

;;; Maybe, just peek up or down at adjacent lines
;;; on ambiguous cases? (like two-space after full-stop)
;;; (Same thing as clever lengths processing, but maybe
;;; easier to envision as a crawling around process).


;;;


(defun rectpara-mode-select-rectpara ()
  "Place mark and point so that a rectangle surrounds the current rectpara"
  (interactive)
  (let (
        (coords (rectpara-mode-find-rectpara-boundaries)) ;     (list left top right bot)
        left top right bot
        )
    (setq left (car coords))
    (setq top (nth 1 coords))
    (setq right (nth 2 coords))
    (setq bot (nth 3 coords))

    ; lower-right hand corner, to become mark
    (rectpara-mode-move-row bot)
    (rectpara-mode-move-column right)
    (let ((future_mark (point)))

;;;     ; possible fix for mysterious not-always-left-selected bug
;;;      (picture-move-up (- bot top))

      ; move point to upper-left hand corner
      (rectpara-mode-move-row top)
      (rectpara-mode-move-column left)

      (push-mark future_mark 't 't) ; supresses "Mark-set" message, and makes region active

      )
    (message "left: %d top: %d right: %d bottom: %d" left top right bot)
    ))

;;; TODO
;;; The above is very close, but once in awhile it
;;; fails to leave the region active...

;;; I've tried adding things like the following to the end
;;; of the command without any luck:

    ; kludge to make the region active
;;    (exchange-point-and-mark)
;;    (exchange-point-and-mark)

    ; different kludges
;;    (push-mark (mark) 't 't) ; make region active.
;;    (set-mark (mark)) ; make region active.

;;    ;; Trying again to find a reliable way to make the region active.
;;    (setq mark-active t)

;;; Note that tricks like this don't work either:

;; (defun rectpara-mode-select-rectpara-harder ()
;;   "Kludge to fix a problem with region not left selected."
;;   (interactive)
;;   (rectpara-mode-select-rectpara)
;;   (rectpara-mode-select-rectpara))

;; Though manually doing the command twice very well might
;; work.  There's something peculiar going on with picture-modes
;; central functionality, the way it backfills empty regions with
;; spaces, but only after it touches that region for some reason.



;;;

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




;; A re-write figuring on being in the edit mode buffer, oriented
;; toward grabbing everything in the buffer, even if it's multiple
;; rectparas.
;; (It could be it's silly to go after getting rectangle boundaries,
;; but it fits existing code.)

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
        (setq right (+ 1 max)) ;; TODO add one yes? current-column uses 0-based indexing.

        (setq bot
              (count-lines (point-min) (point-max))) ;; TODO need to add one maybe
       (list left top right bot)))))      ; x1 y1, x2 y2

;; was: rectpara-mode-extract-rectpara
(defun rectpara-mode-edit-extract-rectpara ()
  "Extract the current rectangle"
  (let (coords start-end start end rectpara)
;;     (setq coords (rectpara-mode-find-rectpara-boundaries))
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

;;; The above should work, but shouldn't all lines of a
;;; rectpara be the same width?  So you could do something
;;; like:
;;;
;;;    (setq width (length (car rectpara)))
;;;
;;; The more elaborate method might cover some odd
;;; cases though.  Not sure.

(defun rectpara-mode-edit-rectpara ()
  "Extract the current rectpara to another buffer for easy editing"

  (interactive)
  (let (rectpara edit_buffer_name buffy coords start-end start end left top right bot)

    (setq rectpara-with-coords (rectpara-mode-extract-rectpara-with-coords))
    (setq rectpara (car rectpara-with-coords))
    (setq coords (car (cdr rectpara-with-coords)))   ; (car(cdr is right, right?

    (setq left (nth 0 coords))
    (setq top (nth 1 coords))
    (setq right (nth 2 coords))
    (setq bot (nth 3 coords))

;;; New additon: force two window display when editing
    (one-window-p 't)

    (setq edit_buffer_name
          (format "*rectpara edit: %d-%d-%d-%d %s*" left top right bot (buffer-name)))

    (setq buffy (generate-new-buffer edit_buffer_name))

    (switch-to-buffer-other-window buffy)
    (insert-rectangle rectpara)
    (turn-on-auto-fill)
    (set-fill-column (rectpara-mode-get-width-rectpara rectpara))
    ;;; Suspect that this would work by itself, rather than the above line:
    ;;;       (set-fill-column (current-column))

    ;; TODO AUGUST should go into a rectpara-edit-mode here to protect keymaps of Fundamental or text-mode...

     (local-set-key "\C-x#"    'rectpara-mode-return-from-edit-rectpara)
     (local-set-key "\C-c\C-c" 'rectpara-mode-return-from-edit-rectpara)
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

;;; TODO mystery ws tweak kludge
;;; Odd bug, sometimes messes with whitespace to right
;;; of inserted rectpara, sometimes adding or deleting one space
;;; (adding is more common).

;;; This seems to help (elimates the space adding, but not
;;; the rarer deletion case?):

;;;     (goto-char (point-min)) ; kludge trying to workaround a mystery problem



;; rectpara-mode-return-from-edit-rectpara - Handles the
;; most common case well:  If edited rectpara
;; has gotten taller (had lines added) will add that much
;; whitespace so that it can't over-write anything below
;; it, without messing with rectparas adajcent to it.

;; STATUS:  working, but not exactly elegant.

;;; BUG: horizontal expansion of edited rectpara
;;; can still overwrite material on the right.
;;; should automatically move things aout of the way.

(defun rectpara-mode-return-from-edit-rectpara ()
  "Replace edited rectpara into the original file inserting space as needed."
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
;;    (goto-char (point-min)) ; kludge trying to cover mystery problem (see above)
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
  (let (coords start-end start end rectpara)
    (setq coords (rectpara-mode-find-rectpara-boundaries))

    (setq start-end (rectpara-mode-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))
    (setq rectpara (extract-rectangle start end))
    (setq rectpara (extract-rectangle start end))
    (clear-rectangle start end)  ;;; Do this here if you really want this to extract, i.e. remove
    (list rectpara coords)))

;;;

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


;;; TODO

;;; clean-up naming:  line -- is that a string or a line number?
;;; edit_buffer => edit_buffer_name
;;; big_list, templist, etc.... ?  Be more verbose.


;;; Currently, litters emacs with open rectpara edit buffers.
;;; When more confident in this code, could delete old buffer
;;; upon return-from.

;;; Probably, these should be saved to file locations in /tmp
;;; so it doesn't keep beeping at you when you do C-x C-s
;;; out of habit.

;;; How about a "skip to next rectpara" command?

;;; Ideally would like to be able to follow chains in doomfiles...
;;; identify the next rectpara in sequence by looking at proximity.
;;; e.g. what is the next nearest rectpara, preference given to
;;; downward and rightward; followed by leftward; followed by upward.

;;; An internal routine could have memory, and exclude previously
;;; encountered rectparas as candidates for the next.  (Loops could
;;; be hard to handle).

;;; Think about how this maps to lisp data structures.

;;; In absence of correct word wrapping, a "compose-rectpara" command might be
;;; useful.  Less cursoring around while originally writing something.
;;; would be like rectpara-edit, but original start and end points would be identical,
;;; the original cursor location when compose is run.

;;; OPEN BUG:

;;; editing rectparas one char wide eats a column of
;;; spaces to it's right upon a return-from-edit.  This is
;;; repeatable.  (Think it stems from picture-mode bug).
