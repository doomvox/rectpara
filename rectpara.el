; doomfile.el             Fri Dec 20 22:09:42 2002

; Define some utility commands to assist in
; writing doomfiles.  (Someday I may change 
; terminology to "doomfileraw", or some such 
; and use "doomfile" just for the finished 
; HTML file...)

;;; See ~/Cave/DoomfilesMode/notes-DoomfilesMode

;   For some time
;   I've been inclined
;   to use emacs
;   picture-mode           Using small,
;   to write things        floating        Which I
;   like this...           rectangular     tend to call
;                          paragraphs.     rectparas.
;     I've always
;     wanted better
;     tools to do
;     this... it was
;     an obvious idea       It only took me
;     to write a            about 10 years
;     "doomfile-mode".      to get to it.
;
;      This is a major
;      mode, derived
;      from picture-mode


(provide 'doomfile)
(eval-when-compile
  (require 'cl))

(require 'picture)

(define-derived-mode doomfile-mode
  picture-mode "Doomfile"
  "Major mode for editing the contents of a doomfile.
The editing commands are the same as in Picture mode, with 
some additional commands to select or edit a rectpara 
\(\\[doomfile-select-rectpara], \\[doomfile-edit-rectpara]\), 
as well as a \\[doomfile-doomify-this-buffer] to add a 
standard header to the buffer.
\\{doomfile-mode-map}"
  (setq case-fold-search nil)
  (if (auto-fill-mode)
    (auto-fill-mode)))


;;; Grabbing Alt o as the doOomfile-mode prefix
(define-key doomfile-mode-map "\M-oe" 'doomfile-edit-rectpara)
(define-key doomfile-mode-map "\M-or" 'doomfile-select-rectpara)

(define-key doomfile-mode-map "\M-oi" 'doomfile-doomify-this-buffer)

(define-key doomfile-mode-map "\M-o\C-c" 'doomfile-mode-exit)


; The following is cloned from picture-mode-exit.
; (Can't just use the original, because it checks
; the mode to make sure it's a picture).

(defun doomfile-mode-exit (&optional nostrip)
  "Undo doomfile-mode and return to previous major mode.
With no argument strips whitespace from end of every line in Picture buffer
  otherwise just return to previous mode."
  (interactive "P")
  (if (not (eq major-mode 'doomfile-mode))
      (error "You aren't editing a Doomfile.")
    (if (not nostrip) (picture-clean))
    (setq mode-name picture-mode-old-mode-name)
    (use-local-map picture-mode-old-local-map)
    (setq major-mode picture-mode-old-major-mode)
    (kill-local-variable 'tab-stop-list)
    (setq truncate-lines picture-mode-old-truncate-lines)
    (force-mode-line-update)))

; doomfile-doomify-this-buffer - expected to be used at the
; beginning of the process, after creating a file in ~/Thought

; (1) Gets filename (sans path), inserts at top of page.
; (2) Inserts date in a suitable format, near the far right.

; uses next-line rather than forward-line because experiment
; shows it works

(defun doomfile-doomify-this-buffer ()
   "Initialize doomfile with filename and date"

  (interactive)
  (goto-char (point-min))
  (insert (file-name-nondirectory (buffer-file-name)))
  (next-line 1)

  ; indent 45 spaces
  (let ((i 0))
  (while (< i 45 )
    (setq i (1+ i))
    (insert " ")))

;  (insert (format-time-string "%A %B %d, %Y"))  ; Saturday February 17, 2007
  (insert (format-time-string "%B %d, %Y"))      ; February 17, 2007
  (insert "")
  (next-line 1)
  (insert "")
  (next-line 1))


;;; 
;;; Towards a 
;;; doomfile-jumpify-nodename
;;;
;;; This pattern finds the end of a nodename:
;;;   [^-A-Z0-9c_]\|$
;;; (Extra-credit: first use this pattern to find the beginning 
;;; of the nearest nodename candidate:
;;;   [-_A-Z0-9][-_cA-Z0-9][-_A-Z0-9]*
;;;
;;; (end_of_nodename_pat  "[^-A-Z0-9c_]\\|$")


(defun doomfile-jumpify-next-nodename ()
  "Position the cursor at (or just before)
a node name like  YOWSA_MAMA_JUMPCAKES and convert it 
into an html jump like 
<A HREF=\"YOWSA_MAMA_JUMPCAKES.html\">YOWSA_MAMA_JUMPCAKES</A>"
  (interactive)
  (let ( (nodename-pat "[-_A-Z0-9][-_cA-Z0-9][-_A-Z0-9]*")  ; greedy match, lowercase c only allowed in second place
        ) 

;;; Better: make sure there's whitespace before and after node name
;;; And maybe: don't allow a jump in the first column...

;    (re-search-forward REGEXP nil t)
;    (replace-match TO-STRING nil nil)

))

;;; Following functions are largely designed to make
;;; it easier to edit existing rectparas.  You can
;;; use keystroke commands to:

;;;   o automatically select the rectpara the cursor is on

;;;   o grab the current rectpara and move it to an edit buffer
;;;     to work on in isolation

;;;   o returning from the edit buffer, if the rectpara has become
;;;     bigger, it automatically tries to open up space for it.

; These routines work with rows and columns numbering from 1
; Note: picture-current-line also numbers from 1, though 
; the current-column uses an origin of zero.

;
;                                 x-axis
;   (1,1)                          =
;     o------------------------>  columns
;     |
;     |           top
;     |       +---------------+
;     |       |               |
;     |  left |               | right
;     |       |               |
;     |       +---------------+
;     |           bottom
;     |
;     V
;
;   y-axis
;    =
;   rows


; The general approach in this code is to "crawl around" in
; the plane, moving the cursor until you find what you're
; looking for.

; (An alternate approach might be to first read in the
; coordinate data for all existing rectparas in a buffer,
; and then then crunch this numeric model from then on.)


; The boundaries of a rectpara are determined as follows:
;
;   Presume cursor is inside the rectpara of interest.
;   Crawl leftward, looking for two adjacent vertical strips of three spaces
;   Crawl down the left edge, until a horizontal stretch of three spaces is found.
;   Crawl up the left edge, until a horizontal stretch of three spaces is found.
;   Sweep from top to bottom, looking at the length of each line, find the longest.
;      Line ends are defined by this regexp:
;          "[^.?!:]  \\|[.?!:]   " ; match 2 spaces or 3 after hard-stop

;;; Documenting some of the nitty gritty of how the code works:

; picture-mode (and hence doomfile-mode) uses the "quarter-plane"
; model, which is to say it fakes an infinte field of empty space
; and quietly fills in actual spaces as you go moving around in it.
; But this is just an illusion, and really the whitespace
; can can be made up of tabs, newlines or actual spaces.

; For the present I don't worry about tabs, since I've got my .emacs
; tricked out like so:

;  ; force emacs to always use spaces for tabbing
;  (setq-default indent-tabs-mode nil)

; But there's still the spaces vs. newline distinction to worry about
; and I've had trouble getting general white space detection to work
; reliably either via my own regexps, or using (thing-at-point 'whitespace)

; My complaint about using thing-at-point: It doesn't work just to the
; right of point.  If you're at the first character of a word,
; thing-at-point calls that "whitespace", because there's a space off
; to the left.

; So, for the moment there's a kludge that I use repeatedly
; in the following code: I twidlle the cursor location
; forward and back with picture-* commmands, so if need be
; they'll will fill in real spaces in the quarter-plane of virtual spaces.
; then I can do simple matches for spaces
;   (looking-at "   ")

; Note that picture.el itself often uses character classes like this:
;    (if (looking-at "[ \t]*$")
; which is presumably the kind of thing I should be doing.

;;; Control structures...
;  The style of control structures here can probably be infered
;  from reading the documentation for doomfile-find-left-boundary
;  All these functions use the same idioms (e.g. while loops that
;  exit when the desired quantity is finally defined,  while loops
;  that increment a location pointer, frequently with
;  a catch and throw wrapper for early exit, and so on).

;;; Data structures:

;  Where ever possible, I specify a rectangle using four
;  coordinates (x1, y1, x2, y2) in part because the emacs style
;  "start and end" strikes me as less robust (without changing
;  the visible layout, you could add whitespace at the end of
;  lines that throw off the start and end values).

;  Typically then, there will be a coords variable, containing
;  a list of these four numbers.  When I want the coords and
;  the contents of a rectpara together, I use a list of two lists.
;  (Remember, a "rectpara" is just a "rectangle" which is
;  a list of strings).  The next larger structure I use is
;  the list of rectparas (a list of lists of two list elements,
;  the rectpara and the coords).

; A pecularity of specifying rectangles (and hence rectparas):
; You specify two points, the upper-left and lower-right, 
; but while the left column is included the right column 
; is not.  Both the top and bottom rows are included.
; There's no clean way of thinking about this...
; you don't put a box around the area, nor do you put 
; the box just inside the area.  You put the box inside the 
; rectangle except for the right edge, which is outside.

; E.g. to select this rectpara, you need mark and 
; point on both the X's:
        
;    Xanadu did    
;    decree a    
;    stately    
;    pleasure    
;    bone to be    
;    doomed.   X

; Internally this rectpara becomes a structure like:

; (  ("Xanadu did"
;    "decree a  " 
;    "stately   " 
;    "pleasure  " 
;    "bone to be"  
;    "doomed.   " ) 
;  (6 228 16 233) )

;;;

; I use a clever (albiet not necessarily good) method of passing
; rectpara data to and from the edit buffer:  the original
; rectpara coordinates (including the buffer name it was in)
; becomes encoded into the name of the edit buffer.
; and extracted again later by "doomfile-return-from-edit-rectpara"

; An example edit buffer name:
;   *doomfile edit: 8-61-30-231 PRETENTIOUS_RAMBLINGS*

; (I didn't understand buffer local variables when I came 
; up with this trick.)

;;;
;;; some general functions:

(defun tlofp ()
  "Return t if on the top line of the file, nil otherwise"
  (save-excursion
    (beginning-of-line)
    (not (char-before))))

(defun blofp ()
  "Return t if on the bottom line of the file, nil otherwise"
  (save-excursion
    (end-of-line)
    (not (char-after))))

(defun doomfile-report-if-tlofp ()  ; for debugging
  "Say something if you're on the top line of the file"
  (interactive)
  (if (tlofp) (message "stictly top line")))

(defun doomfile-report_eolp ()      ; for debugging
  "If at the end of line, say so"
    (interactive)
    (if (eolp)
        (message "end o' the line, jack")
      ))

; vertical-whitespace-p
;
; Basic function to answer the question
; does this column look blank in this region?
; i.e. are there three consecutive spaces lined up
; on top of each other?  STATUS: WORKS

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

(defun doomfile-move-column (col)
  "Move to a given column, numbering from 1"
  (interactive)
  (move-to-column (- col 1) 't))

(defun doomfile-move-row (row)
  "Move to a given row, numbering from 1"
  (interactive)
  (let ((col (current-column)))
    (goto-line row)
    (move-to-column col)))

(defun doomfile-move-to-x-y-location (col row)
  "Move to a given location, given the column and row (numbering from 1)"
  (interactive)
  (goto-line row)
  (move-to-column (- col 1) 't))


(defun doomfile-convert-coords-to-start-end (coords)
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
    (doomfile-move-to-x-y-location right bot)
    (setq end (point))

    ; move point to upper-left hand corner, the start
    (doomfile-move-to-x-y-location left top)
    (setq start (point))

    (list start end)
    )))


; doomfile-find-left-boundary --

; Presumes that the cursor is inside of a rectpara at start
; (just above or below works, too).  Returns the column
; number of the left edge of the rectpara.

; General method: crawls leftward, looking for a vertical
; stretch of whitespace using the vertical-whitespace-p
; function, or alternately the left edge of the screen.

; This version is farily robust, because it looks for *two*
; columns of whitespace to avoid being confused by a
; rectpara where word boundaries just happen to line up over
; each other.

; This complicates the logic slightly, because there are
; three slightly different forms of success:
; (1) we're on the left margin of the buffer already (we're in column "1")
; (2) there's only one space between us and left margin (column is "2")
; (3) there are two adjacent columns of whitespace.

; Nitty gritty: overall while loop continues while
; "loc" is not yet defined.

; Loop moves the cursor backward, continually looking for
; vertial whitespace.  When it's found, it peeks ahead,
; looking for either the left edge of the
; screen or more whitespace.  If not found, it continues
; crawling, looking for the next vertical whitespace.

;;; Slight peculiarity: finds a "boundary" even if it's
;;; *not* currently inside a rectpara.  Logically,
;;; should report 'nil or something in that case, right?
;;; Need a "rectpara_p" function?

(defun doomfile-find-left-boundary ()
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


(defun doomfile-report-left-boundary ()
  "Find left side boundary of a rectpara"
  (interactive)
  (let ((loc 'nil)
        (starting_from (1+ (current-column))))
    (setq loc (doomfile-find-left-boundary))
    (message (concat "Yow! The col found was " loc " starting from " starting_from))))


(defun doomfile-move-left-boundary ()
  "Move to the left boundary of a rectpara"
  (interactive)
  (move-to-column (- (doomfile-find-left-boundary) 1) 't))


;;;

; doomfile-find-lower-boundary -

; This is functional enough, though it should be renamed:
; It isn't reliable unless working on the left edge of a rectpara.
; (because typically, the last line of a rectpara has spaces
; at the end, that fool it into thinking it's at the bottom one row ahead
; of schedule).

; Crawls downward, if it finds whitespace, it does a regep
; match for three spaces.  Jumps forward and back with
; picture commands, to get it to explicityly fill in spaces
; per the quarter-plane illusion.

; (Had problems getting regexps to work, so used this kludge to simplify them.)

(defun doomfile-find-lower-boundary ()
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


(defun doomfile-report-lower-boundary ()
  "Report lower boundary of a rectpara"

  (interactive)
  (let ((loc 'nil)
        (starting_from (picture-current-line)))
    (setq loc (doomfile-find-lower-boundary))
    (message (concat "The lower boundary is at " loc " starting from " starting_from))))

(defun doomfile-move-lower-boundary ()
  "Move to the lower boundary of a rectpara"

  (interactive)
  (let ((col (current-column)))
    (goto-line (doomfile-find-lower-boundary))
    (move-to-column col)))


;;; doomfile-find-upper-boundary --
;;;
;;; (Note, this one is a hybrid of the find-lower-boundary,
;;; and the find-left-boundary routine. )


;;; Like find-left-boundary, it's a more reliable to
;;; run this on the right edge of rectpara.  ((Huh? Left edge, right?))


(defun doomfile-find-upper-boundary ()
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


(defun doomfile-report-upper-boundary ()
  "Report upper boundary of a rectpara"
  (interactive)
  (let ((loc 'nil)
        (starting_from (picture-current-line)))
    (setq loc (doomfile-find-upper-boundary))
    (message (concat "The upper boundary is at " loc " starting from " starting_from))))


(defun doomfile-move-upper-boundary ()
  "Move to the upper boundary of a rectpara"
  (interactive)
  (let ((col (current-column)))
    (goto-line (doomfile-find-upper-boundary))
    (move-to-column col)))



; doomfile-find-rectpara-boundaries -
;
; The General Algorithm is find left bound, move to left bound, find top
; and bottom bounds, then iterate through the lines, checking the
; line lengths.  Select the maximum as the rectpara right bound.

(defun doomfile-find-rectpara-boundaries ()
  "Find boundaries of rectpara cursor is inside of.  
Returns list of coords: x1 y1 x2 y2"
  (let (left bot top right)
    (save-excursion
      (setq left (doomfile-find-left-boundary))

      (doomfile-move-column left) ; Better than (doomfile-move-left-boundary) -- redundant

      (setq bot (doomfile-find-lower-boundary))

      (setq top (doomfile-find-upper-boundary))

      (setq right (doomfile-find-right-boundary left top bot))

      (list left top right bot))))      ; x1 y1, x2 y2


(defun doomfile-report-boundaries ()  ; largely for debugging
  "Report boundaries of the current rectpara"
  (interactive)
  (let (coords)
    (setq coords (doomfile-find-rectpara-boundaries))
  (message (mapconcat 'number-to-string coords " "))))


; doomfile-find-right-boundary -

; Using pattern that allows two-space right hand
; boundaries, unless there's a hard-stop punctuation
; there, then require three-space.

(defun doomfile-find-right-boundary (left top bot)
  "Find the right boundary of the current rectpara,
   given the other three edge boundaries."

  (let ((line top)
        (col 0)
        (rp_line_end 'nil))
    (save-excursion
    (doomfile-move-to-x-y-location left top)
    (while (<= line bot)
      (while (progn ; crawl to right, look for end of this line
               (if (looking-at "[^.?!:]  \\|[.?!:]   ") ; match 2 spaces or 3 after hard-stop
                   (setq rp_line_end (current-column)))
               (picture-forward-column 5)
               (picture-backward-column 4)
               (not rp_line_end)))
      ; looking for longest line
      (if (> rp_line_end col)
          (setq col rp_line_end))
      ; prepare for next iteration
      (picture-move-down 1)
      (setq line (1+ line))
      (setq rp_line_end 'nil)
      (doomfile-move-column left))
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


(defun doomfile-select-rectpara ()
  "Place mark and point so that a rectangle surrounds the current rectpara"
  (interactive)
  (let (
        (coords (doomfile-find-rectpara-boundaries)) ;     (list left top right bot)
        left top right bot
        )
    (setq left (car coords))
    (setq top (nth 1 coords))
    (setq right (nth 2 coords))
    (setq bot (nth 3 coords))

    ; lower-right hand corner, to become mark
    (doomfile-move-row bot)
    (doomfile-move-column right)
    (let ((future_mark (point)))

;;;     ; possible fix for mysterious not-always-left-selected bug
;;;      (picture-move-up (- bot top))

      ; move point to upper-left hand corner
      (doomfile-move-row top)
      (doomfile-move-column left)

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
;    (exchange-point-and-mark)
;    (exchange-point-and-mark)

    ; different kludges
;    (push-mark (mark) 't 't) ; make region active.
;    (set-mark (mark)) ; make region active.

;    ;; Trying again to find a reliable way to make the region active.
;    (setq mark-active t)    

;;; Note that tricks like this don't work either:

;; (defun doomfile-select-rectpara-harder ()
;;   "Kludge to fix a problem with region not left selected."
;;   (interactive)
;;   (doomfile-select-rectpara)
;;   (doomfile-select-rectpara))

;; Though manually doing the command twice very well might 
;; work.  There's something peculiar going on with picture-modes 
;; central functionality, the way it backfills empty regions with 
;; spaces, but only after it touches that region for some reason. 



;;;

;;; someday generalize things like this to take optional coords
;;; parameter make current rectpara just the default

(defun doomfile-extract-rectpara ()
"Extract the current rectangle"
  (let (coords start-end start end rectpara)
    (setq coords (doomfile-find-rectpara-boundaries))
    (setq start-end (doomfile-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))
    (setq rectpara (extract-rectangle start end))))


(defun doomfile-get-width-rectpara (rectpara)
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

(defun doomfile-edit-rectpara ()
  "Extract the current rectpara to another buffer for easy editing"

  (interactive)
  (let (rectpara edit_buffer_name buffy coords start-end start end left top right bot)

    (setq rectpara-with-coords (doomfile-extract-rectpara-with-coords))
    (setq rectpara (car rectpara-with-coords))
    (setq coords (car (cdr rectpara-with-coords)))   ; (car(cdr is right, right?

    (setq left (nth 0 coords))
    (setq top (nth 1 coords))
    (setq right (nth 2 coords))
    (setq bot (nth 3 coords))

;;; New additon: force two window display when editing
    (one-window-p 't)

    (setq edit_buffer_name
          (format "*doomfile edit: %d-%d-%d-%d %s*" left top right bot (buffer-name)))

    (setq buffy (generate-new-buffer edit_buffer_name))

    (switch-to-buffer-other-window buffy)
    (insert-rectangle rectpara)
    (turn-on-auto-fill)
    (set-fill-column (doomfile-get-width-rectpara rectpara))
    ;;; Suspect that this would work by itself, rather than the above line:
    ;;;       (set-fill-column (current-column))

     (local-set-key "\C-x#" 'doomfile-return-from-edit-rectpara)
     (local-set-key "\C-c\C-c" 'doomfile-return-from-edit-rectpara)
     (message "Use either C-x # or C-c c-c to replace the original rectpara with your edits")
     ))


;;;

;;; Odd bug, sometimes messes with whitespace to right
;;; of inserted rectpara, sometimes adding or deleting one space
;;; (adding is more common).

;;; This seems to help (elimates the space adding, but not
;;; the rarer deletion case?):

;;;     (goto-char (point-min)) ; kludge trying to workaround a mystery problem


;;;

; doomfile-return-from-edit-rectpara - Handles the
; most common case tolerably well:  If edited rectpara
; has gotten taller (had lines added) will add that much
; whitespace so that it can't over-write anything below
; it, without messing with rectparas adajcent to it.

; STATUS:  Not too buggy, but not exactly elegant.

;;; Still could use handling of horizontal expansion 
;;; (moving things on the right further right).

(defun doomfile-return-from-edit-rectpara ()
  "Replace the edited rectpara into the original file inserting space as needed."
  (interactive)

  ; The current buffer is expected to have a name like:
  ;   *doomfile edit: 8-61-30-231 PRETENTIOUS_RAMBLINGS*

  ; need to pick out the original buffer name (at the end) and the
  ; coordinate values (x1 y1 x2 y2) defining the original rectangle
  ; (these are in the third field, hyphen separated).

  (let ((edit_buffer (buffer-name))
        templist start end rectpara start-end coords
        left top right bot
        edit_buffer_window)

    (setq templist (split-string edit_buffer " "))

    (if (not (string= (car templist) "*doomfile"))
        (error "This is not a doomfile edit buffer: %s" edit_buffer))

    (setq coords (mapcar 'string-to-number
                         (split-string (nth 2 templist) "-")))

    (setq target_buffer (car (split-string (nth 3 templist) "*")))
       ; To get the actuall buffer name to return to, we
       ; need to truncate at the asterix. Also
       ; chops any emacs versioning like "<2>"

    (goto-char (point-min)) ; kludge trying to workaround a mystery problem
    (setq rectpara (doomfile-extract-rectpara))

;;; INSTEAD OF:
;    (switch-to-buffer target_buffer)
;;; TRYING:
    (setq edit_buffer_window (selected-window))
    (delete-window edit_buffer_window)
    (set-buffer target_buffer)
;;; OKAY?

    (setq start-end (doomfile-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))

    (clear-rectangle start end)

    ; rectpara size change collision avoidance:

    ;  if the rectpara has grown during editing, must
    ;  insert whitespace in the target buffer in an approprate way

    (setq left  (nth 0 coords))
    (setq top   (nth 1 coords))
    (setq right (nth 2 coords))
    (setq bot   (nth 3 coords))

    (let (new_width new_height old_width old_height
          vertical_expansion horizontal_expansion
          vertical_shift horizontal_shift
          temp_hidden_rectparas 
          empirical_correction_shift)

      (setq new_width (doomfile-get-width-rectpara rectpara))
      (setq old_width (- right left))
      (if (> new_width old_width)
          (message "gotten fatter: changed to %d from %d" new_width old_width))

      (setq new_height (length rectpara))
      (setq old_height (1+ (- bot top)))

      ; if the new rectangle has gotten taller
      ; we will temporarily hide all other 
      ; rectparas adjacent to the old bottom edge, 
      ; insert blanks lines, then restore the adjacent
      ; rectparas where they were.
      (if (> new_height old_height)
          ((lambda ()
            (message "gotten taller: changed to %d from %d" new_height old_height)
            (setq vertical_expansion (- new_height old_height))

            (doomfile-move-row bot)
            (setq toe_room
                  (doomfile-look-down-how-far-to-end-of-whitespace vertical_expansion left right))

            ; stupid hack:
            (if (> vertical_expansion 1)
                (setq empirical_correction_shift 3) ; i.e. fudge
              (setq empirical_correction_shift 2))
            ; fudge of 2 works for an expansion of 1, 3 works otherwise.  Not resolved.

            (setq vertical_shift (+ (- vertical_expansion toe_room) empirical_correction_shift) )

            (setq temp_hidden_rectparas (doomfile-extract-rectpars-with-coords-on-line))
            (picture-open-line vertical_shift) ; opening horizontal space
            (doomfile-restore-rectparas-from-list temp_hidden_rectparas))))

    (doomfile-move-to-x-y-location left top)

    (picture-insert-rectangle rectpara)
    (exchange-point-and-mark)
    )))


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

;;;

(defun doomfile-copy-rectpara-with-coords ()
  "Returns current rectpara along with a list of x-y coordinates"
  (let (coords start-end start end rectpara)
    (setq coords (doomfile-find-rectpara-boundaries))

    (setq start-end (doomfile-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))
    (setq rectpara (extract-rectangle start end)) ; note "extract" doesn't remove
    (list rectpara coords)))


(defun doomfile-extract-rectpara-with-coords ()
  "Returns current rectpara along with a list of x-y coordinates, clears the original rectangle"
  (let (coords start-end start end rectpara)
    (setq coords (doomfile-find-rectpara-boundaries))

    (setq start-end (doomfile-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))
    (setq rectpara (extract-rectangle start end))
    (setq rectpara (extract-rectangle start end))
    (clear-rectangle start end)  ;;; Do this here if you really want this to extract, i.e. remove
    (list rectpara coords)))

;;;

(defun doomfile-coords-of-rectparas-on-line ()
   "Looks for all rectparas intersecting the given line and returns their coordinates"
   (save-excursion
   (let (coords_list coords)
     (picture-beginning-of-line)

     (while (not (eolp))
       (if (looking-at "[^ ]")
           ((lambda ()
              (setq coords (doomfile-find-rectpara-boundaries))
              (doomfile-move-column (1+ (nth 2 coords)))

;              (setq coords_list (cons coords coords_list)) ; pushing onto the front
              (setq coords_list (append coords_list (list coords))) ; shifting on to the end

              ))
         (picture-forward-column 1)))

     coords_list)))


(defun doomfile-report-coords-of-rectparas-on-line ()
  "Find the coordinates of all rectparas on the current line, and spit out messages about them."
   (interactive)
   (let ((coords_list (doomfile-coords-of-rectparas-on-line))
         coords)
     (while coords_list
       (setq coords (car coords_list))
       (message "x1: %d y1: %d x2: %d y2: %d"
                (nth 0 coords)
                (nth 1 coords)
                (nth 2 coords)
                (nth 3 coords)
       (setq coords_list (cdr coords_list))))))


; doomfile-extract-rectpars-with-coords-on-line --
;
; scans horizontally across the row one below the bottom edge of original
; rectpara.  when non-blank, runs
;   doomfile-extract-rectpara-with-coords
; (a way of moving rectparas out of the way temporarily)

;;; Add optional parameter to specify the line to look at?

(defun doomfile-extract-rectpars-with-coords-on-line ()
   "Looks for all rectparas intersecting the current line and
removes them, returns them with their coordinates"
   (let (result_list coords rectpara-with-coords next_col)
     (save-excursion
       (picture-beginning-of-line)
       (while (not (eolp))
         (if (looking-at "[^ ]")
             ((lambda ()
                (setq rectpara-with-coords (doomfile-extract-rectpara-with-coords))
                (setq coords (car (cdr rectpara-with-coords)))
                (setq next_col (1+ (nth 2 coords)))
                (doomfile-move-column next_col)

                (setq result_list (append result_list (list rectpara-with-coords)))
                                        ; shifting on to the end of the list
                ))
           (picture-forward-column 1)))

       result_list)))

; Returns a list of elements which are in turn each a list
; of two items, which are in turn also lists: one them
; is a rectpara/rectangle (i.e. a list of strings), and the
; other is a list of 4 items (the coords of the
; rectpara)....


(defun doomfile-report-of-rectparas-and-coords-on-line ()
  "Report the extracted rectparas with coordinates which were all found on the current line"
   (interactive)
   (let ((big_list (doomfile-extract-rectpars-with-coords-on-line))
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


(defun doomfile-remove-rectparas-in-list (big_list)
  "Actually remove the rectparas in the given list"
   (let (coords rectpara start-end start end)
     (while big_list
       (setq coords (car (cdr (car big_list))))

       (setq start-end (doomfile-convert-coords-to-start-end coords))
       (setq start (car start-end))
       (setq end (nth 1 start-end))

       (clear-rectangle start end)

       (setq big_list (cdr big_list)))))

(defun doomfile-restore-rectparas-from-list (big_list)
  "Restore the rectparas in the given list"
   (interactive)
   (let (coords rectpara x1 y1)
     (save-excursion
       (while big_list
         (setq coords (car (cdr (car big_list))))

         (setq x1 (nth 0 coords))
         (setq y1 (nth 1 coords))
;        (setq x2 (nth 2 coords))
;        (setq y2 (nth 3 coords))

         (doomfile-move-to-x-y-location x1 y1)
         (setq rectpara (car (car big_list)))
         (picture-insert-rectangle rectpara)
         (setq big_list (cdr big_list))))))


(defun doomfile-look-down-how-far-to-end-of-whitespace (check_distance left right)
  "From the current row, scans down through the given check_distance,
to see how many rows there are with whitespace between the left
and right boundaries.  Note: the maximum return value is the
check_distance itself."
 (save-excursion
   (let ((i 0)
         checkrect limit coords start-end start end)

     (setq coords (list
                   left
                   (picture-current-line)
                   right
                   (+ (picture-current-line) check_distance)))

     (setq start-end (doomfile-convert-coords-to-start-end coords))
     (setq start (car start-end))
     (setq end (nth 1 start-end))

     (setq  checkrect (extract-rectangle start end))

     (catch 'stuff
       (while (< i check_distance)
         (let ((line (nth i checkrect)))
           (if (not (string-match "^[ ]*$" line))
               (throw 'stuff (setq limit i))
             (setq i (1+ i))
             ))))
     (setq limit (1+ i))  ;;; Why plus one?  Try without?
     )))

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

