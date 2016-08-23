;; rectpara.el         Fri  December 20, 2002  22:10
;;               Rev:  Sun  August   14, 2016  19:17

;; A variant of picture-mode to work on blocks of text layed-out roughly
;; as rectangles. I call these "rectparas" for "rectangular paragraphs".
;; (see the example below).

;; This was originally called doomfile.el, because it's
;; original purpose was to work on "The Doomfiles"
;; (currently at http://obsidianrook.com/doomfiles).

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


;; Installation:
;;  Put this file into a location in your load-path, and add the following to your ~/.emacs:
;;  (require 'rectpara-mode)

;; Example use:
;;  In a file containing text like the above rectpara example:
;;  ESC x rectpara-mode
;;  cursor to a rectpara, e.g. "I've always..."
;;  M-o e
;;    You should see that rectpara extracted to a new window
;;    where it can be edited as standalone text. When done:
;;  C-C C-c
;;  That should move the edited rectpara back to the original position.
;;

(provide 'rectpara-mode)
(eval-when-compile
  (require 'cl))

(require 'picture)


;; Having a global makes it a little easier to write recursive routines that accumulate rectparas
(defvar rectpara-stash-plist ()
  "Global to stash a unique list of rectparas.")
(make-variable-buffer-local 'rectpara-stash-plist)


(define-derived-mode rectpara-mode
  picture-mode "Rectpara"
  "Major mode for editing rectparas ('rectangular paragraphs').
The editing commands are the same as in Picture mode, with
some additional commands to select or edit a rectpara:
\(\\[rectpara-select-rectpara], \\[rectpara-edit-rectpara]\).
\\{rectpara-mode-map}"
  (setq case-fold-search nil)
  (if (auto-fill-mode)
    (auto-fill-mode)))

;;; Grabbing Alt o as the doOomfile-mode prefix
(define-key rectpara-mode-map "\M-oe" 'rectpara-edit-rectpara)
(define-key rectpara-mode-map "\M-oc" 'rectpara-compose-rectpara)

(define-key rectpara-mode-map "\M-or" 'rectpara-select-rectpara)

(define-key rectpara-mode-map "\M-o\C-c" 'rectpara-exit)


(define-derived-mode rectpara-edit-mode
  text-mode "Rectpara Edit"
  "Major mode for editing an individual rectparas.
Similar to text-mode, but with an additional command to
finish and return the rectpara to the original buffer.
\\[rectpara-edit-mode-done].
\\{rectpara-edit-mode-map}"
  (turn-on-auto-fill))

(define-key rectpara-edit-mode-map "\C-x#"    'rectpara-edit-mode-done)
(define-key rectpara-edit-mode-map "\C-c\C-c" 'rectpara-edit-mode-done)


;; The following is cloned from picture-mode-exit.
;; (Can't just use the original, because it checks
;; the mode to make sure it's in picture).

(defun rectpara-exit (&optional nostrip)
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
;; current-column uses an origin of zero.

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
;; can be made up of tabs, newlines or actual spaces.

;; For the present I don't worry about tabs, since I've got my .emacs
;; tricked out like so:

;;  ; force emacs to always use spaces for tabbing
;;  (setq-default indent-tabs-mode nil)

;; But there's still the spaces vs. newline distinction to worry about
;; and I've had trouble getting general white space detection to work
;; reliably either via my own regexps, or using (thing-at-point 'whitespace)

;; My complaint about using thing-at-point: It doesn't look solely to the
;; right of point.  If you're at the first character of a word,
;; thing-at-point calls that "whitespace", because there's a space off
;; to the left.

;; So, for the moment there's a kludge that I use repeatedly
;; in the following code: I twiddle the cursor location
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

(defun vertical-whitespace-p ()
  "Check near cursor to see if there's a 3 character vertical stretch of whitespace.
Essentially, answers the question does this column look blank in this region?"
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

(defun report-vertical-whitespace-p ()  ;; for debugging
  "If there's a vertical strip of whitespace, say so"
  (interactive)
  (if (vertical-whitespace-p)
      (message "yup, clear up and down")
    (message "nope, there's stuff here")))

(defun rectpara-move-column (target-col)
  "Move to a given column, numbering from 1.
This moves across the current row.
Autovivifying spaces just on that row.
(so, to infill a rectangular area, go to column first,
then use: \\[rectpara-move-row])."
  (interactive "Ncol:")
  (let* ( (init (rectpara-current-column))
          (delta (- target-col init))
          (i 0) )

    (let* ( (direction (signum delta) )
            (loop 1) )
      (while (<= loop (abs delta))
        (picture-forward-column direction)
        (setq loop (1+ loop))
        )) ))

(defun rectpara-move-row ( target-row )
  "Move to a given TARGET-ROW, numbering from 1.
This moves down through the current column, autovivfying spaces
in any blank regions to the left of the column."
  (interactive "Nrow:")
  (let* ( (init (rectpara-current-row))
          (delta (- target-row init))
          (i 0) )

    ;; delta can be positive or negative, so need to check whether to
    ;; step up or down (really?  yeah, I think so).
    (let* ( (direction (signum delta) )
            (loop 1) )
      (while (<= loop (abs delta))
        (picture-move-down direction)
        (setq loop (1+ loop))
        )) ))


(defun rectpara-move-to-x-y-location-lite (col row)
  "Move to a given location, given the column and row (numbering from 1).
This 'lite' version makes no effort to autovivify spaces in the effected region."
  (interactive)
  (goto-line row)
  (move-to-column (- col 1) 't))

(defun rectpara-move-to-x-y-location (col row)
  "Move to a given location, given the column and row (numbering from 1).
This version autovivify spaces in the whitespace of the effected region,
between the current position and the one moved to."
  (interactive)
  (rectpara-move-column col)
  (rectpara-move-row    row)
  )



(defun rectpara-convert-coords-to-start-end (coords)
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
    (rectpara-move-to-x-y-location-lite right bot)
    (setq end (point))

    ; move point to upper-left hand corner, the start
    (rectpara-move-to-x-y-location-lite left top)
    (setq start (point))

    (list start end)
    )))


;; rectpara-find-left-boundary --

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
;; Logically, should report 'nil in that case (( but really, this doesn't matter. ))
;; Need a "rectpara_p" function? (( More precisely: "inside_rectpara_p" ))

(defun rectpara-find-left-boundary ()
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


(defun rectpara-report-left-boundary ()
  "Find left side boundary of a rectpara"
  (interactive)
  (let ((loc 'nil)
        (starting_from (1+ (current-column))))
    (setq loc (rectpara-find-left-boundary))
    (message (concat "Yow! The col found was " loc " starting from " starting_from))))


(defun rectpara-move-left-boundary ()
  "Move to the left boundary of a rectpara"
  (interactive)
  (move-to-column (- (rectpara-find-left-boundary) 1) 't))

;;;
;; TODO might be better if this did not *require* working on the left
;; boundary, but scattered spaces (and the ragged right edge) can
;; complicate doing this elsewhere.
(defun rectpara-find-lower-boundary ()
  "Find lower boundary of a rectpara. First row is 1.
Expects to be started on the left boundary, and crawl down it to
the lower left corner.

This crawls downward, continually jumping forward and back with
picture-mode commands, to fill-in spaces per the quarter-plane illusion."
  (interactive)
  (let ( (loc nil) )
    (save-excursion
;;   (rectpara-move-left-boundary)  ;; do this here to be safe?
      (while
          (progn
            ((lambda ()
               (picture-move-down 1)
               (save-excursion
                 ;; twiddle spaces into existance (simplifies match)
                 (picture-forward-column 3)
                 (picture-backward-column 3)
                 (if (looking-at "   ") ; three spaces
                     (setq loc (- (picture-current-line) 1) )))
               (not loc))))))
    loc))


(defun rectpara-report-lower-boundary ()
  "Report lower boundary of a rectpara"

  (interactive)
  (let ((loc 'nil)
        (starting_from (picture-current-line)))
    (setq loc (rectpara-find-lower-boundary))
    (message (concat "The lower boundary is at " loc " starting from " starting_from))))

(defun rectpara-move-lower-boundary ()
  "Move to the lower boundary of a rectpara"

  (interactive)
  (let ((col (current-column)))
    (goto-line (rectpara-find-lower-boundary))
    (move-to-column col)))


;;; rectpara-find-upper-boundary --
;;;
;;; (Note, this one is a hybrid of the find-lower-boundary,
;;; and the find-left-boundary routine. )


;;; Like find-left-boundary, it's a more reliable to
;;; run this on the right edge of rectpara.  ((Huh? Left edge, right?))


(defun rectpara-find-upper-boundary ()
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


(defun rectpara-report-upper-boundary ()
  "Report upper boundary of a rectpara"
  (interactive)
  (let ((loc 'nil)
        (starting_from (picture-current-line)))
    (setq loc (rectpara-find-upper-boundary))
    (message (concat "The upper boundary is at " loc " starting from " starting_from))))


(defun rectpara-move-upper-boundary ()
  "Move to the upper boundary of a rectpara"
  (interactive)
  (let ((col (current-column)))
    (goto-line (rectpara-find-upper-boundary))
    (move-to-column col)))



;; rectpara-find-rectpara-boundaries

;; Find left bound, move to left bound, find top and bottom bounds,
;; then iterate through the lines, checking the line lengths.
;; Select the maximum as the rectpara right bound.

(defun rectpara-find-rectpara-boundaries ()
  "Find boundaries of rectpara cursor is inside of.
Returns list of coords: x1 y1 x2 y2"
  (let (left bot top right)
    (save-excursion
      (setq left (rectpara-find-left-boundary))
      (rectpara-move-column left)
      (setq bot (rectpara-find-lower-boundary))
      (setq top (rectpara-find-upper-boundary))
      (setq right (rectpara-find-right-boundary left top bot))
      (list left top right bot)))) ;; x1 y1, x2 y2


(defun rectpara-report-boundaries ()  ; largely for debugging
  "Report boundaries of the current rectpara"
  (interactive)
  (let (coords)
    (setq coords (rectpara-find-rectpara-boundaries))
  (message (mapconcat 'number-to-string coords " "))))


;; rectpara-find-right-boundary -

;; Using pattern that allows two-space right hand
;; boundaries, unless there's a hard-stop punctuation
;; there, then require three-space.

(defun rectpara-find-right-boundary (left top bot)
  "Find the right boundary of the current rectpara,
   given the other three edge boundaries."

  (let ((line top)
        (col 0)
        (rp_line_end 'nil))
    (save-excursion
    (rectpara-move-to-x-y-location-lite left top)
    (while (<= line bot)
      (while (progn ; crawl to right, look for end of this line
               (if (looking-at "[^.?!:]  \\|[.?!:]   ") ; match 2 spaces or 3 after hard-stop
                   (setq rp_line_end (1+ (current-column))))
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
      (rectpara-move-column left))
    (setq col (+ col 1))
    col)))

;; TODO there's an intermittant select of an extra line.  Debug or just ignore?
;; nickname: "select"
(defun rectpara-select-rectpara ()
  "Select the current rectpara.
i.e. place mark and point so that a rectangle surrounds the
current rectpara with the region active."
  (interactive)
  (let ( (coords (rectpara-find-rectpara-boundaries)) ;; (list left top right bot)
         left top right bot
         )
    (setq left  (car coords))
    (setq top   (nth 1 coords))
    (setq right (nth 2 coords))
    (setq bot   (nth 3 coords))

    ;; go to lower-right hand corner (which becomes the mark)
    (rectpara-move-row top)
    (rectpara-move-column right)
    (rectpara-move-row bot)

    ;; now have the boundaries of rectpara...
    ;; set mark and point and leave the region active
    (let ( (future_mark (point)) )

      ;; move point to upper-left hand corner
      (rectpara-move-row top)
      (rectpara-move-column left)

      ;; makes region active (works *most* of the time... but do NOT think about that).
      (push-mark future_mark t t)
    )
    (message "left: %d top: %d right: %d bottom: %d" left top right bot)
  ))

(defun rectpara-find-rectpara-boundaries ()
  "Find boundaries of rectpara cursor is inside of.
Returns list of coords: x1 y1 x2 y2"
  (let (left bot top right)
    (save-excursion
      (setq left (rectpara-find-left-boundary))
      (rectpara-move-column left)
      (setq bot (rectpara-find-lower-boundary))
      (setq top (rectpara-find-upper-boundary))
      (setq right (rectpara-find-right-boundary left top bot))
      (list left top right bot))))      ;; x1 y1, x2 y2


;; An alternate version that assumes you're in the edit mode
;; buffer. This is oriented toward grabbing everything in the
;; buffer, even if it's multiple rectparas.
(defun rectpara-edit-find-rectangle-boundaries ()
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

;; was: rectpara-extract-rectpara
(defun rectpara-edit-extract-rectpara ()
  "Extract the current rectangle"
  (let (coords start-end start end rectpara)
    (setq coords (rectpara-edit-find-rectangle-boundaries))
    (setq start-end (rectpara-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))

    (setq rectpara (extract-rectangle start end))
   ))

(defun rectpara-get-width-rectpara (rectpara)
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
(defun rectpara-edit-rectpara ( &optional edit_fill_col )
  "Extract the current rectpara to another buffer for easy editing"
  (interactive)
  (let ( rectpara edit_buffer_name edit_buffer coords start-end start
         end left top right bot pair-pos col row)

    (setq rectpara-with-metadata
          (rectpara-extract-rectpara-with-coords-and-rel-pos))

    (setq rectpara (car      rectpara-with-metadata))
    (setq coords   (car (cdr rectpara-with-metadata)))
    (setq pair-pos (nth 2    rectpara-with-metadata))

    (setq left  (nth 0 coords))
    (setq top   (nth 1 coords))
    (setq right (nth 2 coords))
    (setq bot   (nth 3 coords))

    (setq col  (nth 0 pair-pos))
    (setq row  (nth 1 pair-pos))

    (unless edit_fill_col
      (setq edit_fill_col (rectpara-get-width-rectpara rectpara))
      )

    ;; force two window display when editing
    (one-window-p 't)

    (setq edit_buffer_name
          (format "*rectpara edit: %d-%d-%d-%d %s*"
                  left top right bot (buffer-name)))
    (setq edit_buffer (generate-new-buffer edit_buffer_name))

    (switch-to-buffer-other-window edit_buffer)
    (insert-rectangle rectpara)

    ;; move to correct relative position in the new buffer
    (rectpara-move-to-x-y-location-lite (1+ (- col left)) (1+ (- row top)))

    (rectpara-edit-mode)

    (set-fill-column edit_fill_col)

    (message "Use either C-x # or C-c c-c to finish editing")
  ))


(defun rectpara-compose-rectpara ()
  "Compose a new rectpara in an edit buffer window.
Uses an initial fill-column setting of 30."
  (interactive)
  (rectpara-spaceout)
  (rectpara-edit-rectpara 30 ) )

(defun rectpara-spaceout (&optional count)
  "Twiddle cursor location with picture-mode commands to create spaces.
This no-op turns out to be an effective way of covering certain bugs
that pop up, e.g. if you happen to be near the end of the buffer
or line, and empty space isn't getting treated the same as whitespace
you can get mysterious args out of range errors and the like."
  (unless count
    (setq count 3))
  (picture-forward-column count)
  (picture-backward-column count)
  )


;;; This is how I'm trying to handle:
;;;  "rectpara size change collision problems":

;;; Handling the case of vertical collisions:

;;; Go to the bottom and look to the left and right, if there's
;;; anything there, move all the rectangles out of the way temporarly;
;;; insert enough blank lines to handle the problem, then restore the
;;; rectangles.

;;; (But there's no need for this whitespace insertion unless
;;; the new rectangle will encroach on old, so we check that first.)


;;; Handling the case of width increase (TODO):
;;; look to the right, snag anything in the way
;;; do the rectpara insert, then put the rectangles back in
;;; locations shifted over enough that there'll be no collision.

;;; Watch out for secondaries: need to move rectparas
;;; in the way of other rectparas: use a recursive method?


;; ========
;; rectpara-edit-mode-done - Handles the
;; most common case well:  If edited rectpara
;; has gotten taller (had lines added) will add that much
;; whitespace so that it can't over-write anything below
;; it, without messing with rectparas adajcent to it.

;; STATUS:  working, but not exactly elegant.

;;; BUG: horizontal expansion of edited rectpara
;;; can still overwrite material on the right.
;;; should automatically move things aout of the way.

;; nickname: "done"
(defun rectpara-edit-mode-done ()
  "Return edited rectpara to the original buffer.
If increased in size, opens up white space as needed."
  (interactive)
  (let* (
        left top right bot
        edit_buffer_window

        (buffer_md
         (rectpara-edit-mode-buffer-metadata-from-name))
        (target_buffer (nth 0 buffer_md))
        (coords        (nth 1 buffer_md))

        ;; Note: we're still in the edit buffer window
        (new-rectpara (rectpara-edit-extract-rectpara))
        )

    ;; leaving the edit buffer window
    (rectpara-zap-this-edit-window target_buffer)

    ;; rectpara size change collision avoidance:
    ;;   if the rectpara has grown during editing, must
    ;;   open up whitespace in the target buffer
    (let* (
           (left  (nth 0 coords))
           (top   (nth 1 coords))
           (right (nth 2 coords))
           (bot   (nth 3 coords))

           (new_width (rectpara-get-width-rectpara new-rectpara))
           (old_width (1+ (- right left)))

           (new_height (length new-rectpara))
           (old_height (1+ (- bot top)))
          )

      (if (> new_width old_width)
          (rectpara-deal-with-horizontal-expansion new_width old_width coords) )

      (if (> new_height old_height)
          (rectpara-deal-with-vertical-expansion new_height old_height coords) )

      (rectpara-clear-rectangle coords)

      ;; TODO do insert conditionally?  if not adviseable, open up edit window again and warn.
      (rectpara-move-to-x-y-location-lite left top)
      (picture-insert-rectangle new-rectpara)

      ;; TODO replace this with a jump to lower right hand corner?
      ;;      (but then: if you wanna re-edit, it could be an annoyance...)
      (exchange-point-and-mark)
    )))

;; nickname: "vertical"
(defun rectpara-deal-with-vertical-expansion (new_height old_height coords)
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

      (rectpara-move-row bot)
      (setq toe_room
            (rectpara-open-how-far-down vertical_delta left right))

      ;; TODO hack:
      ;; when vertical expansion is 1, 2 works here, 3 works otherwise.
      (if (> vertical_delta 1)
          (setq empirical_correction_shift 3) ;
        (setq empirical_correction_shift 2))

      ;; difference in toe_room and expansion is how much we need to open
      (setq blank_lines_needed
            (+ (- vertical_delta toe_room) empirical_correction_shift) )

      (setq temp_hidden_rectparas
            (rectpara-extract-rectpars-with-coords-on-line))
      (picture-open-line  blank_lines_needed) ;; insert blank lines
      (rectpara-restore-rectparas-from-list temp_hidden_rectparas)
    ))

;; nickname: "horizontal"
(defun rectpara-deal-with-horizontal-expansion (new-width old-width coords)
  "Juggle things out of the way horizontally so expanded rectpara will fit."
  (let* (
          (left  (nth 0 coords))
          (top   (nth 1 coords))
          (right (nth 2 coords))
          (bot   (nth 3 coords))

          (delta (- new-width old-width ))
          (new-right (+ right delta))

          (padding 3)
          (horizon (+ delta padding))

            open-field overlap shift
            temp_hidden_rectparas           )

    ;; clear the global stash
    (setq rectpara-stash-plist () )

    ;; (rectpara-move-column top) ;; just being neat.
    (rectpara-move-column right)

    (setq open-field
          (rectpara-open-how-far-over right horizon top bot))

    ;; difference in open-field and expansion is how much we need to open
    (setq overlap (- new-right (+ open-field right)))
    (setq shift (+ overlap padding))

    (cond ( (> shift 0 )  ;; don't try to do anything unless something needs to be done

           ;; Stores extracted rectparas in global var: rectpara-stash-plist
           (rectpara-extract-rectpars-to-right coords horizon)

           ;; Pulls rectparas back in from the global rectpara-stash-plist
           (rectpara-restore-rectparas-shifted-over  shift )
           )
          )))

(defun rectpara-open-how-far-over (col horizon top bot)
  "Find amount of open space next to column COL, between rows TOP and BOT.
Looks no further than the HORIZON (a limited number of columns).
Note: the maximum return value is the horizon." ;; horizon includes padding
  (save-excursion
    (let* (
            (i 0)   ;; loop indicie: a horizontal count from the column COL
            (absolute-horizon (+ col horizon) )

            ;; looking for the "open-field": the end of the open columns
            (open-field absolute-horizon) ;; initialized to it's max value
            (row-candidate 0)             ;; candidate from each row

            (farther 1)   ;; need to look up and down a little farther to keep from encroaching on stuff
            (top (- top farther))
            (bot (+ bot farther))
           )

      ;; loop vertically from top to bottom, looking to the right of curcol
      (rectpara-move-row top)

      (while (<= (rectpara-current-row) bot )
        (setq i 0)

        ;; move (back) to original column to check from
        (rectpara-move-column col)

        ;; on each line, crawl forward looking for non-space
        (catch 'OUT
          (while (< (rectpara-current-column) absolute-horizon )
            (cond ( (not (looking-at " "))  ;; maybe better: "[ \t]"
                    (throw 'OUT nil) ) )

            ;; two steps forward and one back (to autovivify spaces in empty space)
            (picture-forward-column  2)
            (picture-backward-column 1)
            (setq i (1+ i))
            ))
        (setq row-candidate i)

        ;; if candidate is less than known limit, bring down the limit to further
        (if (< row-candidate open-field)
            (setq open-field row-candidate))

        (picture-move-down 1)
        )
      ;; (message "open-field: %d" open-field) ;; DEBUG
    open-field)))

(defun rectpara-extract-rectpars-to-right ( coords  horizon )
  "Look to the right of the area given by COORDS, extract rectparas, return as list.
Does a cascading check for what is to the right, looking as far as HORIZON.
Works by calling this routine recursively on each rectpara.
A rectpara is represented as coords, i.e. a list of four x/y parameters,
and a rectangle, i.e. a list of lines of text.
TODO CHECK."
  (save-excursion
    (let* (
           (saveloc (point) )
           (left  (nth 0 coords))
           (top   (nth 1 coords))
           (right (nth 2 coords))
           (bot   (nth 3 coords))

           (absolute-horizon (+ right horizon ))

           (farther 1) ;; need to peek up and down a little farther to get everything.

           rectpara-contents  found-coords
           )
      ;; loop vertically from top to bottom, looking to the right of the rectpara
      (rectpara-move-row (- top farther))
      (while (<= (rectpara-current-row) (+ bot farther) )

          (setq found-coords () )

          ;; move to the column just outside the given rectpara coords
          (rectpara-move-column (1+ right))

          ;; on each line, crawl forward looking for non-space
          (catch 'OUT
            (while (<= (rectpara-current-column) absolute-horizon )
              (cond ( (not (looking-at " "))  ;; maybe better: "[ \t]"
                      (setq found-coords (rectpara-find-rectpara-boundaries))
                      (throw 'OUT nil) ) )
              ;; two steps forward and one back (to autovivify spaces)
              (picture-forward-column  2)
              (picture-backward-column 1)
              ))

          ;; using global plist stash to accumulate a unique list of rectparas
          (cond ( found-coords
                  (let* ( (coords-str (mapconcat 'number-to-string found-coords "-") ) )
                    (setq rectpara-contents (rectpara-extract-rectpara-at-coords found-coords) )

                    (setq rectpara-stash-plist
                          (plist-put rectpara-stash-plist (intern coords-str) rectpara-contents))

                    ;; recursive call to keep looking out rightwards of the found rectpara
                    (rectpara-extract-rectpars-to-right found-coords horizon)
                    )))
          (picture-move-down 1)
          ))
    rectpara-stash-plist ))

(defun rectpara-restore-rectparas-shifted-over ( &optional rightshift plist )
  "Inserts rectparas from PLIST into current buffer, moved over RIGHTSHIFT chars.
PLIST defaults to rectpara-stash-plist. RIGHTSHIFT defaults to 0."
  (interactive)

  (catch 'OUT  ;; skip everything
    (unless rightshift (setq rightshift 0))
    (unless plist
      ;;    (setq plist rectpara-stash-plist)

      (setq plist
            (cond (rectpara-stash-plist
                   rectpara-stash-plist
                   )
                  (t ;; if there's no plist, can't do anything
                   (throw 'OUT nil)
                   ) ) )

      (let* ( ( keysters (rectpara-plist-keys plist) )
              ( effected (rectpara-get-effected-range-from-plist plist) )
              ;; (min-x  (nth 0 effected))
              ;; (min-y  (nth 1 effected))
              (max-x  (+ (nth 2 effected) rightshift))
              (max-y  (nth 3 effected))
              )

        ;; move through effected x y range, ensuring whitespace is filled with spaces
        ;; (starting from origin, just to be sure.)
        (rectpara-move-to-x-y-location 1 1)
        (rectpara-move-to-x-y-location max-x max-y)

        (dolist (key keysters )
          (let* (
                 (coords (mapcar 'string-to-number (split-string key "-")))
                 (left  (nth 0 coords))
                 (top   (nth 1 coords))
                 (right (nth 2 coords))
                 (bot   (nth 3 coords))

                 (newleft (+ left rightshift))
                 )
            (rectpara-move-to-x-y-location-lite newleft top)
            (let ( ( contents (rectpara-stash-lookup key plist) )
                   )
              (picture-insert-rectangle contents)
          )))))))


(defun rectpara-restore-stash ( &optional plist )
  "Inserts rectparas from stash (default rectpara-stash-plist) into current buffer."
  (interactive)
  (unless plist (setq plist rectpara-stash-plist))
    (let* ( ( keysters (rectpara-plist-keys plist) )
            ( effected (rectpara-get-effected-range-from-plist plist) )
            ;; (min-x  (nth 0 effected))
            ;; (min-y  (nth 1 effected))
            (max-x  (nth 2 effected))
            (max-y  (nth 3 effected))
          )

      ;; move through x y range, filling with spaces
      ;; (starting from origin, just to be sure.)
      (rectpara-move-to-x-y-location 1 1)
      (rectpara-move-to-x-y-location max-x max-y)

      (dolist (key keysters )
        (message ">>>%s<<<" (pp key))
        (let* (
                (coords (mapcar 'string-to-number (split-string key "-")))
                (left  (nth 0 coords))
                (top   (nth 1 coords))
                (right (nth 2 coords))
                (bot   (nth 3 coords))
                )
          (rectpara-move-to-x-y-location-lite left top)
          (let ( ( contents (rectpara-stash-lookup key plist) )
                 )
            (picture-insert-rectangle contents)
          )))))

(defun rectpara-restore-stash ( &optional plist )
  "Inserts rectparas from stash (default rectpara-stash-plist) into current buffer."
  (interactive)
  (unless plist (setq plist rectpara-stash-plist))
    (let* ( ( keysters (rectpara-plist-keys plist) )
            ( effected (rectpara-get-effected-range-from-plist plist) )
            ;; (min-x  (nth 0 effected))
            ;; (min-y  (nth 1 effected))
            (max-x  (nth 2 effected))
            (max-y  (nth 3 effected))
          )

      ;; move through x y range, filling with spaces
      ;; (starting from origin, just to be sure.)
      (rectpara-move-to-x-y-location 1 1)
      (rectpara-move-to-x-y-location max-x max-y)

      (dolist (key keysters )
        (message ">>>%s<<<" (pp key))
        (let* (
                (coords (mapcar 'string-to-number (split-string key "-")))
                (left  (nth 0 coords))
                (top   (nth 1 coords))
                (right (nth 2 coords))
                (bot   (nth 3 coords))
                )
          (rectpara-move-to-x-y-location-lite left top)
          (let ( ( contents (rectpara-stash-lookup key plist) )
                 )
            (picture-insert-rectangle contents)
          )))))


(defun rectpara-stash-lookup ( keystr &optional plist )
  "Look-up string KEYSTR in plist stash.
Defaults to rectpara-stash-plist."
  (unless plist (setq plist rectpara-stash-plist))
  (let ( (value (lax-plist-get plist (intern keystr))) )
    value))

(defun rectpara-stash-put ( keystr value &optional plist )
  "Put pair of KEYSTR and VALUE in the plist stash.
Defaults to rectpara-stash-plist."
  (unless plist (setq plist rectpara-stash-plist))
  (setq plist
        (plist-put plist (intern keystr) value)))

(defun rectpara-get-effected-range-from-plist ( &optional plist )
  "Gets max x and y values out of a plist stash of rectparas.
Defaults to global rectpara-stash-plist."
  ;; (interactive)
  (unless plist (setq plist rectpara-stash-plist))
  (let* ( (keysters (rectpara-plist-keys plist) )
          ;; use any one of the rectparas to initialize values
          (init-rect (pop keysters))
          (init-coords (mapcar 'string-to-number (split-string init-rect "-")))
          (min-x  (nth 0 init-coords))
          (min-y  (nth 1 init-coords))
          (max-x  (nth 2 init-coords))
          (max-y  (nth 3 init-coords))
                     )
      (dolist ( key keysters )
        (let* (
                (coords (mapcar 'string-to-number (split-string key "-")))
                (left  (nth 0 coords))
                (top   (nth 1 coords))
                (right (nth 2 coords))
                (bot   (nth 3 coords))
                )
          (if (< left min-x)  (setq min-x left))
          (if (< top min-y)   (setq min-y top))
          (if (> right max-x) (setq max-x right))
          (if (> bot max-y)   (setq max-y bot))
          ))
      (list min-x min-y max-x max-y)
      ))

;; Did I really need to write this?  Why am I using a language that's
;; making me do things like this?
(defun rectpara-plist-keys ( plist )
  "Return all keys of the given plist as a list of strings.
This just steps through a list and skips every other value."
  (let ( (flip t)
         (accumulator) )
    (dolist (item plist)
        (cond ( flip
                (let ( (key (symbol-name item)) ) ;; symbols-to-strings
                  (push key accumulator) )
                (setq flip nil)
                )
              (t ;; not flip
               (setq flip t))
              ))
    accumulator))

(defun rectpara-plist-values ( plist )
  "Return all values of the given plist as a list.
This just steps through a list and skips every other value."
  (let ( (flip nil)
         (accumulator ()) )
    (dolist (item plist accumulator)
      (if flip
          (push item accumulator) )
      (if flip
          (setq flip nil)
        (setq flip t))
      )))

(defun rectpara-edit-mode-buffer-metadata-from-name ()
  "Interprets the current edit_buffer name, extracting target
buffer and x/y coordinates.  Returns a list of the two (where
the coordinates are themselves a list of 4).
Note: the edit buffer is expected to have a name like:
  *rectpara edit: 8-61-30-231 PRETENTIOUS_RAMBLINGS*
"
;; we need to pick out the original buffer name (there at the end) and
;; the coordinate values (x1 y1 x2 y2) defining the original rectangle
;; (those are in the middle, a hyphen separated list).
  (let* ((edit_buffer (buffer-name))
         (templist (split-string edit_buffer " "))
        )
    (if (not (string= (car templist) "*rectpara"))
        (error "This is not a rectpara edit buffer: %s" edit_buffer))

    (setq coords (mapcar 'string-to-number
                         (split-string (nth 2 templist) "-")))

    ;; To get the actual buffer name to return to, we need to truncate
    ;; at the asterix. Also chops any emacs versioning like "<2>"
    (setq target_buffer (car (split-string (nth 3 templist) "*")))
    (list target_buffer coords)
   ))

(defun rectpara-zap-this-edit-window (rectpara-buffer)
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

(defun rectpara-clear-rectangle (coords)
  "Clear the rectangle with the given COORDS."
    (let* ( (start-end (rectpara-convert-coords-to-start-end coords))
            (start (car start-end))
            (end   (nth 1 start-end)) )
      (clear-rectangle start end)
      ))

(defun rectpara-extract-rectpara-at-coords (coords)
  "Given COORDS returns a copy of rectpara as list of strings."
    (setq start-end (rectpara-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))
    (setq rectpara (extract-rectangle start end))  ;; though extract implies removal this doesn't...
    (clear-rectangle start end)                    ;; we have to do the removal ourselves.
    rectpara )

(defun rectpara-copy-rectpara-at-coords (coords)
  "Given COORDS returns a copy of rectpara as rectangle, i.e. list of strings."
    (setq start-end (rectpara-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))
    (setq rectpara (extract-rectangle start end))
    ;; (clear-rectangle start end)  ;;; the sole difference between copy and extract
    rectpara )


;; unused
(defun rectpara-copy-rectpara-with-coords ()
  "Returns current rectpara along with a list of x-y coordinates"
  (let (coords start-end start end rectpara)
    (setq coords (rectpara-find-rectpara-boundaries))
    (setq rectpara
          (rectpara-copy-rectpara-at-coords coords))
    (list rectpara coords)))

(defun rectpara-extract-rectpara-with-coords ()
  "Returns current rectpara along with a list of x-y coordinates, clears the original rectangle"
  (let (saveloc coords start-end start end rectpara)
    (setq coords (rectpara-find-rectpara-boundaries))

;;     (setq start-end (rectpara-convert-coords-to-start-end coords))
;;     (setq start (car start-end))
;;     (setq end (nth 1 start-end))
;;     (setq rectpara (extract-rectangle start end))
;;     (setq rectpara (extract-rectangle start end))
;;     (clear-rectangle start end)  ;;; extract implies removal

    (setq rectpara
          (rectpara-extract-rectpara-at-coords coords))
    (list rectpara coords)))

(defun rectpara-extract-rectpara-with-coords-and-rel-pos ()
  "Returns current rectpara along with a list of x-y coordinates, clears the original rectangle"
  (let (saveloc coords start-end start end rectpara pos-pair)
    (setq saveloc (point)) ;; TODO use this to get relative position inside rectpara

    (setq pos-pair (rectpara-x-and-y-from-pos saveloc))

    (setq coords (rectpara-find-rectpara-boundaries))

;;     (setq start-end (rectpara-convert-coords-to-start-end coords))
;;     (setq start (car start-end))
;;     (setq end (nth 1 start-end))
;;     (setq rectpara (extract-rectangle start end))
;;     (setq rectpara (extract-rectangle start end))
;;     (clear-rectangle start end)  ;;; extract implies removal

    (setq rectpara
          (rectpara-extract-rectpara-at-coords coords))

    (list rectpara coords pos-pair)))

;;;

(defun rectpara-x-and-y-from-pos (&optional pos)
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
    (setq row (rectpara-current-row))

    (goto-char saveloc)
    ;; (message "r: %d c: %d" row column);; DEBUG
    (list column row);; TODO do I have a convention on this?  row/column or x/y?
  ))

(defun rectpara-current-row ()
  "Get the row of the current position.
The first line is row 1.  Note, could also use: \\[picture-current-line]."
  (interactive) ;; TODO which I abuse etc.
  (let ( (pos (point) )
         row )
    ;; if we're at the eob, play it safe and add a space
    (if (= pos (point-max))
          (rectpara-spaceout 1) )
    (setq row
           (count-lines (point-min) (1+ pos)))
    row))

(defun rectpara-current-column ()
  "Returns the column number, where the first column is 1."
    (1+ (current-column)))

(defun rectpara-report-current-x-y () ;; for DEBUG
  "Report current row and column."
  (interactive)
  (let* ( (r (rectpara-current-row) )
          (c (rectpara-current-column) )
          (p (picture-current-line) )
          )
    (message "row: %d col: %d prow: %d" r c p )
    ))

(defun rectpara-coords-of-rectparas-on-line ()
   "Looks for all rectparas intersecting the given line and returns their coordinates"
   (save-excursion
   (let (coords_list coords)
     (picture-beginning-of-line)
     (while (not (eolp))
       (if (looking-at "[^ ]")
           ((lambda ()
              (setq coords (rectpara-find-rectpara-boundaries))
              (rectpara-move-column (1+ (nth 2 coords)))
;;              (setq coords_list (cons coords coords_list)) ; pushing onto the front
              (setq coords_list (append coords_list (list coords))) ; shifting on to the end
              ))
         (picture-forward-column 1)))
     coords_list)))

(defun rectpara-report-coords-of-rectparas-on-line ()
  "Find the coordinates of all rectparas on the current line, and spit out messages about them."
   (interactive)
   (let ((coords_list (rectpara-coords-of-rectparas-on-line))
         coords)
     (while coords_list
       (setq coords (car coords_list))
       (message "x1: %d y1: %d x2: %d y2: %d"
                (nth 0 coords)
                (nth 1 coords)
                (nth 2 coords)
                (nth 3 coords)
       (setq coords_list (cdr coords_list))))))


;; Just used by:
;;   rectpara-extract-rectpars-with-coords-on-line
;;   rectpara-report-of-rectparas-and-coords-on-line
(defun rectpara-extract-rectpars-with-coords-on-line ()
   "Looks for all rectparas intersecting the current line and
removes them, returns them with their coordinates"
;; scans horizontally across the row one below the bottom edge of original
;; rectpara.  when non-blank, runs
;;   rectpara-extract-rectpara-with-coords
;; (a way of moving rectparas out of the way temporarily)
   (let (result_list coords rectpara-with-coords next_col)
     (save-excursion
       (picture-beginning-of-line)
       (while (not (eolp))
         (if (looking-at "[^ ]")
             ((lambda ()
                (setq rectpara-with-coords (rectpara-extract-rectpara-with-coords))
                (setq coords (car (cdr rectpara-with-coords)))
                (setq next_col (1+ (nth 2 coords)))
                (rectpara-move-column next_col)

                (setq result_list (append result_list (list rectpara-with-coords)))
                                      ;; shifting on to the end of the list
                ))
           (picture-forward-column 1)))

       result_list)))

(defun rectpara-report-of-rectparas-and-coords-on-line ()
  "Report the extracted rectparas with coordinates which were all found on the current line"
   (interactive)
   (let ((big_list (rectpara-extract-rectpars-with-coords-on-line))
         coords rectpara)
     (while big_list
       (setq coords (car (cdr (car big_list))))
       (message "x1: %d y1: %d x2: %d y2: %d"
                (nth 0 coords)
                (nth 1 coords)
                (nth 2 coords)
                (nth 3 coords))
       (setq rectpara (car (car big_list)))
       ;; (message "First line of rectpara: %s" (nth 0 rectpara))
       (setq big_list (cdr big_list)))))


(defun rectpara-remove-rectparas-in-list (big_list)
  "Actually remove the rectparas in the given list"
   (let (coords rectpara start-end start end)
     (while big_list
       (setq coords (car (cdr (car big_list))))

       (setq start-end (rectpara-convert-coords-to-start-end coords))
       (setq start (car start-end))
       (setq end (nth 1 start-end))

       (clear-rectangle start end)

       (setq big_list (cdr big_list)))))

(defun rectpara-restore-rectparas-from-list (big_list)
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

         (rectpara-move-to-x-y-location-lite x1 y1)
         (setq rectpara (car (car big_list)))
         (picture-insert-rectangle rectpara)
         (setq big_list (cdr big_list))))))


;; TODO an oddly big routine for a very small purpose.
;;      is all this stuff really needed?
(defun rectpara-get-line-segment-as-string (left right &optional line-number)
  "Gets the requested piece of a line as a string.  Default: current line.
Returns the substring between LEFT and RIGHT.  When the string is
too short will pad with spaces-- this assists in faking an infinite quarter-plane."
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
    segment))

(defun rectpara-open-how-far-down (checkdistance left right)
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
                (rectpara-get-line-segment-as-string left right))
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
;;;     o  obviously, make this a defvar setting.

;;; o  Anyway: the edit buffers should be saved to file locations
;;;    in /tmp so it doesn't keep beeping at you when you do C-x C-s
;;;    out of habit.

;;; o  Feature: would like a "skip to next rectpara" command.
;;;    Look for nearest rectpara downward and to the right
;;;    when there's more than one... do something reasonable.
;;;     o  Just go to the first one (leftmost).
;;;     o  Provide a second command to cycle through choices


;;=======

;; Copyright 2016 Joseph Brenner
;; License: GPL 2.0 (see boilerplate below)
;;
;; Author: doom@kzsu.stanford.edu
;; Version: 1.0
;; X-URL: http://obsidianrook.com/rectpara/


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
