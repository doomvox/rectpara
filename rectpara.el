;; rectpara.el            Fri  December  20, 2002  22:10
;;                  Rev:  Mon  September 19, 2016  17:40

;; rectpara-mode is a variant of picture-mode to work on blocks of text layed-out
;; roughly as rectangles. I call these "rectparas" for "rectangular paragraphs".

;; Example:

;;   For some time
;;   I've been inclined
;;   to use emacs
;;   picture-mode         Using small,
;;   to write things      floating        Which I
;;   like this...         rectangular     tend to call
;;                        paragraphs.     rectparas.
;;     I've always
;;     wanted better
;;     tools to do
;;     this... it was
;;     an obvious idea       It only took me
;;     to write a            about 10 years
;;     mode for it.          to get to it.
;;
;;                           (And another 10
;;                           to publish it.)
;;

;; The rectpara-edit-at-point command extracts one of those rectparas
;; and temporarily moves it to a separate window in rectpara-edit-mode
;; (which is derived from text-mode).

;; The central motivation is to do work on "The Doomfiles",
;; currently at
;;    http://obsidianrook.com/doomfiles.

;; Installation and set-up:
;;  Put the rectpara.el file into a location in your load-path,
;;  and add something like the following to your ~/.emacs
;;  (this uses the single character prefix I prefer, Alt-o):
;;    (require 'rectpara)
;;    (global-unset-key "\M-o")   ;; I want this, facemenu.el can't have it
;;    (rectpara-standard-setup  "\M-o")

;; With no argument the key prefix will be "C-c ;" (control-C semicolon):
;;   (rectpara-standard-setup)

;; rectpara-standard-setup also associates files with a standard
;; extension of *.rect with rectpara-mode.

;; Example use:
;;  In a file containing text like the above rectpara examples:
;;     ESC x rectpara-mode
;;  Then cursor to a rectpara, and do an "edit":
;;    M-o e
;;  You should see that rectpara extracted to a new window
;;  where it can be edited as stand-alone text. When done:
;;    C-C C-c
;;  That should move the edited rectpara back to the original position,
;;  moving things over and/or down if necessary to make room for it.

(provide 'rectpara)
(eval-when-compile
  (require 'cl))

(require 'picture)

;;====
;; variable and mode definitions

;;-------
;; user settings

(defvar rectpara-clean-up-edit-buffers nil
  "When set to t, edit buffers will be removed when edit is done.")
;; (setq rectpara-clean-up-edit-buffers t)

(defvar rectpara-archive-edits t
  "When set to t, preserve edited rectparas on disk.
It's recommended to leave this set to t, if only so that when you
compulsively do save commands they won't error out.")

(defvar rectpara-archive-location "$HOME/.rectpara"
  "The place where copies of any edited rectparas will be saved.
Will be created if does not already exist.
Default: a sub-directory named \".rectpara\" in the user's
home directory.")

;; Having a global (really, buffer local) for this makes it a
;; little easier to write recursive routines that accumulate
;; rectparas without passing the intermediate results around.
(defvar rectpara-stash-plist ()
  "Global stash of a unique list of rectparas.")
(make-variable-buffer-local 'rectpara-stash-plist)

;;--------
;; project meta-data

(defvar rectpara-version "1.00"
 "Version number of the rectpara.el elisp file.")

;;-------
;; define (and set-up) rectpara modes

(define-derived-mode rectpara-mode
  picture-mode "Rectpara"
  "Major mode for editing rectparas ('rectangular paragraphs').
The editing commands are the same as in Picture mode, with
some additional commands to select or edit a rectpara:
\(\\[rectpara-select-rectpara], \\[rectpara-edit-at-point]\).
\\{rectpara-mode-map}"
  (use-local-map rectpara-mode-map)
  (turn-off-auto-fill)
  (setq-default indent-tabs-mode nil)
  (setq case-fold-search nil)  )


(define-derived-mode rectpara-edit-mode
  text-mode "Rectpara Edit"
  "Major mode for editing individual rectparas.
Similar to text-mode, but with an additional command to
finish and return the rectpara to the original buffer.
\\[rectpara-edit-mode-done].
\\{rectpara-edit-mode-map}"
  (turn-on-auto-fill)
  (setq-default indent-tabs-mode nil) )

(defun rectpara-standard-setup ( &optional key-prefix )
  "Perform the standard set-up operations.
Calling this is intended to be a single step to get useful
keybindings and so on.  KEY-PREFIX defaults to 'control-C
semicolon' though I recommend something more convienient."
  (interactive) ;; DEBUG
  (unless key-prefix (setq key-prefix "\C-c;"))

  ;; This uses *.rect as the standard rectpara file extension
  (add-to-list
   'auto-mode-alist
   '("\\.\\(rect\\)\\'" . rectpara-mode))

  ;; nickname: "edit"    "\M-oe"
  (define-key rectpara-mode-map
    (format "%se" key-prefix) 'rectpara-edit-at-point)

  ;; nickname: "compose"  "\M-oc"
  (define-key rectpara-mode-map
    (format "%sc" key-prefix) 'rectpara-compose-rectpara)

  ;; nickname: "select"   "\M-or"
  (define-key rectpara-mode-map
    (format "%sr" key-prefix) 'rectpara-select-rectpara)

  ;; nickname: "exit"    "\M-o\C-c"
  (define-key rectpara-mode-map
    (format "%s\C-c" key-prefix)  'rectpara-exit)

  ;; the edit mode map:
  ;; nickname: "done"
  (define-key rectpara-edit-mode-map "\C-x#"    'rectpara-edit-mode-done)
  (define-key rectpara-edit-mode-map "\C-c\C-c" 'rectpara-edit-mode-done)

  (setq rectpara-archive-location
        (rectpara-fixdir rectpara-archive-location))
  )

;;; ========
;;; EDITING RECTPARAS

;;; Commands are provided to:

;;;   o automatically select the rectpara the cursor is on

;;;   o grab the current rectpara and move it to an edit buffer
;;;     to work on in isolation

;;;   o return from the edit buffer.  If the modified rectpara
;;;     has become bigger, automatically opens up space for it.

;;--------
;; coordinate system

;; The routines here work with rows and columns numbering from 1
;; Note: picture-current-line also numbers from 1, though
;; current-column uses an origin of zero.

;;
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

;; The boundaries of a rectpara are determined roughly by
;; a line of spaces above and below and two columns of
;; spaces to the left and right.

;; A common approach in this code is to "crawl around" in the
;; plane, moving the cursor until you find what you're looking for.
;; So, more precisely, the boundaries are found like so:

;;   Presume cursor is inside the rectpara of interest.
;;   Crawl leftward, looking for two adjacent cols of spaces, three rows high
;;   Crawl down the left edge, until a row with three spaces is found.
;;   Crawl up the left edge, until a row with three spaces is found.
;;   Find the longest line where, line ends are defined by this regexp:
;;      "[^.?!:]  \\|[.?!:]   " ; match 2 spaces or 3 after hard-stop


;;--------
;; autovivifying spaces

;; picture-mode (and hence rectpara-mode) uses the "quarter-plane" model,
;; which is to say it fakes an infinite field of empty space and quietly
;; fills in actual spaces as you go moving around in it.

;; For rectpara-mode and rectpara-edit-mode, I ensure that spaces are used
;; for whitespace by setting indent-tabs-mode to nil.

;; There are often issues with getting the end-of-line and end-of-buffer to
;; behave like an indefinite expanse of spaces: in this code I frequently
;; resort to a hack where I twiddle the cursor location forward and back
;; with picture-* commmands, so if need be the open space will be filled in
;; real spaces.  That makes it easier to work with, with simple matches
;; for spaces such as (looking-at " ")

;; And there's a class of rectpara-move-* commands here that typically
;; laboriously step around one space or column at a time so that the
;; underlying picture-mode commands can autovivify spaces.

;;--------
;; Data structures:

;;  I specify a rectpara location using four coordinates (x1, y1,
;;  x2, y2) which locate the upper-left and lower-right corners
;;  using a pair of column and row numbers.  The standard emacs
;;  style of specifying a rectangular region using two byte-count
;;  values (i.e. the point and mark or start and end) is less
;;  robust, because the byte-counts can change without any change
;;  in the visible layout, e.g. if spaces are added at the end of
;;  lines.

;;  The contents of a "rectpara" is just a "rectangle" which is a
;;  list of strings.

;;  A list of rectparas is a list of lists, including both the rectpara
;;  and the four coordinates (usually abbreviated "coords").

;;  A "rectpara stash plist" is used in places to maintain a
;;  unique list of rectparas that will need to be moved.

;; A pecularity of specifying rectangles (and hence rectparas): You
;; specify two points, the upper-left and lower-right, where the top,
;; bottom and left edges are included in the rectangle, *but the right
;; edge is excluded*.  There's no clean way of thinking about this...
;; you don't put a box *around* the area and neither do you put the box
;; just *inside* the area, you put the right edge outside the area, and
;; the other three edges just inside.

;; In the edit buffers, the rectpara metadata is stored in the *name* of the buffer,
;; including the rectpara coordinates and name of the buffer it's from.
;;
;; An example edit buffer name:
;;
;;   *rectpara edit: 8-61-30-231 PRETENTIOUS_RAMBLINGS*
;;
;; This makes the edit buffer names for each rectpara unique
;; (though the main reason I did this is I didn't understand
;; buffer local variables when I came up with this trick).

;;========
;;; primitive functions

;;--------
;; turn open space to actual spaces
;; (note: also see "rectpara-move-* commands")

(defun rectpara-spaceout (&optional count)
  "Twiddle cursor location with picture-mode to insert spaces in open area.
This no-op turns out to be an effective way of covering certain bugs
that come up, e.g. if you happen to be near the end of the buffer
or line, and empty space isn't getting treated the same as whitespace
you can get mysterious args out of range errors and so on."
  (unless count
    (setq count 3))
  (picture-forward-column count)
  (picture-backward-column count)
  )

;;---------
;; where am i and what's here

(defun rectpara-current-row ()
  "Get the row of the current position.
The first line is row 1.  Note, could also use: \\[picture-current-line]."
  (let ( (pos (point) )
         row )
    ;; if we're at the eob, add a few spaces
    (if (= pos (point-max))
          (rectpara-spaceout 2) ) ;; experimentally, 2 works, 1 doesn't: args out of range
    (setq row
           (count-lines (point-min) (1+ pos)))
    row))

(defun rectpara-current-column ()
  "Returns the column number, where the first column is 1."
    (1+ (current-column)))

(defun rectpara-tlofp ()
  "Return t if on the top line of the file, nil otherwise"
  (save-excursion
    (beginning-of-line)
    (not (char-before))))

;; unused
(defun rectpara-last-line-p ()
  "Return t if on the bottom line of the file, nil otherwise"
  (save-excursion
    (end-of-line)
    (not (char-after))))

(defun rectpara-vertical-whitespace-p ()
  "Check near cursor to see if there's a 3 character vertical stretch of whitespace.
Essentially, answers the question does this column look blank in this region?"
  (save-excursion
    (let (lower-char upper-char current-char tern)
      (setq current-char (following-char))
      (picture-move-down 1)
      (setq lower-char (following-char))
      (picture-move-up 2)
      (setq upper-char (following-char))
      (setq tern (concat (char-to-string upper-char)
                         (char-to-string current-char)
                         (char-to-string lower-char)))
      (string-match "^[ \t\n\000]+$" tern) )))

(defun rectpara-last-row ()
  "Get the row number for the end of the buffer."
  (save-excursion
    (goto-char (point-max))
    (let ( ( row (rectpara-current-row) ) )
          (message "last row: %d" row)
          row)))


;;--------
;; rectangle utilities

;; used by "edit" and "done"
(defun rectpara-width-rectangle (rectpara)
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
       (setq width (1+ max)) ;; plus 1 confirmed experimentally
       ))

;;--------
;; rectpara-move-* commands
(defun rectpara-move-column (target-col)
  "Move to a given column, numbering from 1.
This moves across the current row, autovivifying spaces just on
that row.  To convert a rectangular area to physical spaces, use
this first, then use: rectpara-move-row."
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

;; Used by: "select", "vertical", "horizontal"...
;;  rectpara-open-how-far-over, rectpara-move-to-x-y-location, rectpara-extract-rectpars-to-right
(defun rectpara-move-row (target-row)
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

(defun rectpara-move-to-x-y-location (column row)
  "Move to a given location, given the COLUMN and ROW (numbering from 1).
This version creates spaces in the open space of the effected region--
between the current position and the one moved to."
  (rectpara-move-column column)
  (rectpara-move-row    row) )

(defun rectpara-move-to-x-y-location-lite (column row)
  "Move to a given location, given the column and row (numbering from 1).
This 'lite' version makes no effort to autovivify spaces in the effected region."
  (goto-line row)
  (move-to-column (- column 1) 't))

;;---------
;; coordinate conversion

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

;;-------
;; find rectpara boundaries

;; Used by "select" and (indirectly) "edit", "horizontal"....
(defun rectpara-find-rectpara-boundaries ()
  "Find boundaries of rectpara where point resides.
Returns list of coords: x1 y1 x2 y2"
  (let (left bot top right)
    (save-excursion
      (setq left (rectpara-find-left-boundary))
      (rectpara-move-column left)
      (setq bot (rectpara-find-lower-boundary))
      (setq top (rectpara-find-upper-boundary))
      (setq right (rectpara-find-right-boundary left top bot))
      (list left top right bot))))   ;; x1 y1, x2 y2


;; used by: rectpara-find-rectpara-boundaries
(defun rectpara-find-left-boundary ()
  "Find the left side boundary column of a rectpara.
If not started inside a rectpara, finds the left side of the buffer."
;; General method: crawls leftward, looking for a vertical
;; stretch of whitespace using the vertical-whitespace-p
;; function, or alternately the left edge of the screen.
;; This looks for *two* columns of whitespace to avoid being
;; confused (e.g. by a rectpara where word boundaries just happen
;; to line up over each other).
;; This complicates the logic, because there are
;; three different forms of success:
;; (1) there are two columns of whitespace to the left
;; (2) we're up against the left edge of the buffer already
;; (3) there's only one space between us and the left edge.
  (let ((loc 'nil))
    (save-excursion
      (catch 'FOUND
        ;; while loc is not yet defined, step to the left
        (while (progn (if (bolp)
                          (throw 'FOUND (setq loc 1))
                        ((lambda ()
                           (picture-backward-column 1)
                           (if (rectpara-vertical-whitespace-p)
                               ;; peek ahead, look for more vertical whitespace,
                               ;; *or* the left edge of the screen
                               ((lambda ()
                                  (if (bolp)
                                      (throw 'FOUND (setq loc 2))
                                    (save-excursion
                                      (let ((lastcol (current-column)))
                                        (picture-backward-column 1)
                                        (if (rectpara-vertical-whitespace-p)
                                            (throw 'FOUND
                                                   (setq loc (+ lastcol 2)))))))))))))
                      (not loc)))))
    loc))

;; used indirectly by "select", via rectpara-find-rectpara-boundaries
(defun rectpara-find-lower-boundary ()
  "Find lower boundary of a rectpara. First row is 1.
Expects to be started on the left boundary, and crawl down it to
the lower left corner, so run rectpara-move-left-boundary first.
As this crawls downward it continually jumps forward and back with
picture-mode commands, to fill-in spaces per the quarter-plane illusion."
  ;; (interactive)
  (let ( (loc nil) )
    (save-excursion
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

;; Used indirectly by "select" and "horizontal", via rectpara-find-rectpara-boundaries
(defun rectpara-find-upper-boundary ()
  "Find upper boundary of a rectpara
   Expects to be started on the left boundary, and crawl up
   it to the upper left corner."
;; Note: this is a hybrid of find-lower-boundary and find-left-boundary.
  (let ((loc 'nil))
    (save-excursion
      (catch 'gotcha
        (while
            (progn
              ((lambda ()
                 (if (rectpara-tlofp)
                     (throw 'gotcha (setq loc 1)))
                 (picture-move-up 1)
                 (save-excursion
                   (picture-forward-column 3)
                   (picture-backward-column 3)
                   (if (looking-at "   ") ;; three spaces
                       (setq loc (+ (picture-current-line) 1) )))
                 (not loc)))))))
    loc))

;; Used by "select", "horizontal", "edit"... via rectpara-find-rectpara-boundaries
(defun rectpara-find-right-boundary (left top bot)
  "Find the right boundary of the current rectpara,
   given the other three edge boundaries."
;; Uses a pattern that allows two-space right hand boundaries,
;; unless there's a hard-stop punctuation there, then require three-space.
;; end-of-line also counts as a boundary
  (let ((line top)
        (col 0)
        (rp-line-end 'nil))
    (save-excursion
    (rectpara-move-to-x-y-location-lite left top)
    (while (<= line bot)
      (while (progn ; crawl to right, look for end of this line
;;               (if (looking-at "[^.?!:]  \\|[.?!:]   ") ; match 2 spaces or 3 after hard-stop
               (if (looking-at "[^.?!:]  \\|[.?!:]   \|$") ; match 2 spaces or 3 after hard-stop
                   (setq rp-line-end (1+ (current-column))))
               (picture-forward-column  5)
               (picture-backward-column 4)
               (not rp-line-end)))
      ; looking for longest line
      (if (> rp-line-end col)
          (setq col rp-line-end))
      ; prepare for next iteration
      (picture-move-down 1)
      (setq line (1+ line))
      (setq rp-line-end 'nil)
      (rectpara-move-column left))
    (setq col (+ col 1))
    col)))

;;--------
;; plist utilities

(defun rectpara-stash-lookup ( keystr &optional plist )
  "Look-up string KEYSTR in plist stash.
Defaults to rectpara-stash-plist."
  (unless plist (setq plist rectpara-stash-plist))
  (let ( (value (lax-plist-get plist (intern keystr))) )
    value))

;; unused (for parallelism)
(defun rectpara-stash-put ( keystr value &optional plist )
  "Put pair of KEYSTR and VALUE in the plist stash.
Defaults to rectpara-stash-plist."
  (unless plist (setq plist rectpara-stash-plist))
  (setq plist
        (plist-put plist (intern keystr) value)))

;; It's hard to believe I needed to write this.
(defun rectpara-plist-keys ( plist )
  "Return all keys of the given plist as a list of strings."
;; Step through a list and skipping the even values
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
  "Return all values of the given plist as a list."
;; Step through a list and skipping the odd values
  (let ( (flip nil)
         (accumulator ()) )
    (dolist (item plist accumulator)
      (if flip
          (push item accumulator) )
      (if flip
          (setq flip nil)
        (setq flip t))
      )))


;;-------
;; working with the rectpara stash plist

(defun rectpara-get-effected-range-from-plist ( &optional plist )
  "Gets max x and y values out of a plist stash of rectparas.
Defaults to global rectpara-stash-plist."
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


;;=========
;; rectpara-mode interactive routines (and satellites):
;; "select", "edit", "compose", "exit"

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
    (let ( (future-mark (point)) )

      ;; move point to upper-left hand corner
      (rectpara-move-row top)
      (rectpara-move-column left)

      ;; makes region active (works *most* of the time... but do NOT think about that).
      (push-mark future-mark t t)
    )
    (message "left: %d top: %d right: %d bottom: %d" left top right bot)
  ))

;; Nickname: "edit"
(defun rectpara-edit-at-point ( &optional edit-fill-col )
  "Extract the current rectpara to another buffer for easy editing"
  (interactive)
  (let ( rectpara edit-buffer-name edit-buffer coords start-end start
         end left top right bot pair-pos col row display-lines)

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

    (unless edit-fill-col
      (setq edit-fill-col (rectpara-width-rectangle rectpara)) )

    (setq display-lines (+ (- bot top) 7) )        ;; height of rectpara plus some
    (split-window-vertically (* -1 display-lines)) ;; negative means lower window
    (other-window 1)                               ;; move to lower window

    (setq edit-buffer-name
          (format "*rectpara edit: %d-%d-%d-%d %s*"
                  left top right bot
                   (buffer-name)) )

    (setq edit-buffer (generate-new-buffer edit-buffer-name))

    (switch-to-buffer edit-buffer)
    (insert-rectangle rectpara)

    (if rectpara-archive-edits
        (rectpara-edit-archive-rectpara))

    ;; move to correct relative position in the new buffer
    (rectpara-move-to-x-y-location-lite (1+ (- col left)) (1+ (- row top)))
    (rectpara-edit-mode)
    (set-fill-column edit-fill-col)
    (message "Use either C-x # or C-c c-c to finish editing")
  ))

(defun rectpara-edit-archive-rectpara ()
  "Save buffer to the archive location.
Create appropriate directory in archive if needed.
See: rectpara-archive-location
Preserves the existing buffer name."
  (let* ( (buffname (buffer-name) )
          (buffer-md
           (rectpara-edit-mode-buffer-metadata-from-name))
          (target-buffer (nth 0 buffer-md))
          (coords        (nth 1 buffer-md))
          (left  (nth 0 coords))
          (top   (nth 1 coords))
          (right (nth 2 coords))
          (bot   (nth 3 coords))
          (arcname (format "%d-%d-%d-%d.rect" left top right bot))

          (buffy (rectpara-clean-buffer-name target-buffer))
          (loc (rectpara-fixdir
                (concat rectpara-archive-location buffy)))
          (arcfile (concat loc arcname))
        )
    (write-file arcfile)
    (rename-buffer buffname)
    ))

(defun rectpara-clean-buffer-name (name)
  "Remove uniqueifying crap from end of given buffer NAME.
   e.g.  'BANKO<2>'      => 'BANKO'
         'BUNKO|Thought' => 'BUNKO'
   "
  (let* ( (trunc-pat "[<|]")
          (scrape-pat (concat "^\\(.*?\\)" trunc-pat)) )
    (cond ((string-match scrape-pat name)
           (setq name (match-string 1 name))
           ))
    name))


;; nickname: "compose"
(defun rectpara-compose-rectpara ()
  "Compose a new rectpara in an edit buffer window.
Uses an initial fill-column setting of 30."
  (interactive)
  (rectpara-spaceout)
  (rectpara-edit-at-point 30 ) )


;; nickname: "exit"
(defun rectpara-exit (&optional nostrip)
  "Undo rectpara-mode and return to previous major mode.
With no argument strips whitespace from end of every line in Picture buffer
  otherwise just return to previous mode."
;; This is copied from picture-mode-exit, which can't be used by a derived mode,
;; because it checks the mode to make sure the buffer is in picture-mode.
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

;;=========
;; rectpara-edit-mode interactive routines
;; i.e. "done" (and it's *many* satellites):

;; nickname: "done"
(defun rectpara-edit-mode-done ()
  "Return edited rectpara to the original buffer.
If increased in size, opens up white space as needed."
  (interactive)
  ;; we begin in the edit buffer window
  (let* (
        (buffer-md
         (rectpara-edit-mode-buffer-metadata-from-name))
        (target-buffer (nth 0 buffer-md))
        (coords        (nth 1 buffer-md))
        (new-rectpara (rectpara-edit-extract-rectpara))
        )

    ;; leaving the edit buffer window
    (save-buffer)
    (rectpara-zap-this-edit-window target-buffer)

    ;; rectpara size change collision avoidance:
    ;;   if the rectpara has grown during editing, must
    ;;   open up whitespace in the target buffer
    (let* (
           ;; the original coordinates, before edit
           (left  (nth 0 coords))
           (top   (nth 1 coords))
           (right (nth 2 coords))
           (bot   (nth 3 coords))

           (new-width (rectpara-width-rectangle new-rectpara))
           (old-width (1+ (- right left)))

           (new-height (length new-rectpara))
           (old-height (1+ (- bot top)))

           (delta-height (- new-height old-height ))
           (delta-width  (- new-width old-width ))

           (new-bot   (+ bot delta-height))
           (new-right (+ right delta-width))
          )

      (if (> (1+ delta-width) 0) ;; adding 1 confirmed experimentally
          (rectpara-deal-with-horizontal-expansion delta-width coords) )

      (if (> delta-height 0) ;; no need for 1+
          (rectpara-deal-with-vertical-expansion delta-height delta-width coords) )

      (rectpara-clear-rectangle coords)

      ;; autovivify spaces through-out area of new rectpara (e.g. append newlines)
      (rectpara-move-to-x-y-location new-right new-bot)

      ;; TODO might be better to do this conditionally:
      ;;      if there's a problem warn and re-open the edit window again
      (rectpara-move-to-x-y-location-lite left top)
      (picture-insert-rectangle new-rectpara)

      (exchange-point-and-mark)
    )))

;; used by "done"
(defun rectpara-edit-mode-buffer-metadata-from-name ()
  "Interprets the current edit-buffer name, extracting target
buffer and x/y coordinates.  Returns a list of the two (where
the coordinates are themselves a list of 4).
Note: the edit buffer is expected to have a name like:
  *rectpara edit: 8-61-30-231 PRETENTIOUS_RAMBLINGS*
"
;; we need to pick out the original buffer name and the
;; coordinate values (x1 y1 x2 y2) that defined the original rectangle
  (let* ((edit-buffer (buffer-name))
         (templist (split-string edit-buffer " "))
        )
    (if (not (string= (car templist) "*rectpara"))
        (error "This is not a rectpara edit buffer: %s" edit-buffer))

    (setq coords (mapcar 'string-to-number
                         (split-string (nth 2 templist) "-")))

    ;; To get the actual buffer name to return to, we need to truncate
    ;; at the asterix. Also chops any emacs versioning like "<2>"
    (setq target-buffer (car (split-string (nth 3 templist) "*")))
    (list target-buffer coords)
   ))


;; used by "done" (indirectly, via rectpara-edit-extract-rectpara)
(defun rectpara-edit-find-rectangle-boundaries ()
  "In rectpara 'edit', find boundaries of a rectangle around all text.
Returns list of coords: x1 y1 x2 y2."
;; This code handles the case of a rectpara subdivided during editing.
;; One rectpara can become more than one after editing.
  (let (left bot top right)
    (save-excursion
      (setq left 1)  ;; 1 based indexing
      (setq top  1)
      (goto-char (point-min))
      (let ( (i     0)
             (max   0)
             (candi 0)
             (width 0) )
        (while (<= i (point-max))
          (move-end-of-line 1) ;; nil won't work: need that 1 (contradicts docs?)
          (setq candi (current-column))

          (if (> candi max) (setq max candi))

          (forward-line) ;; or whatever move down one
          (setq i (1+ i))
          )
        (setq right (+ 1 max))

        (setq bot
              (count-lines (point-min) (point-max)))
       (list left top right bot)))))      ; x1 y1, x2 y2

;; used by "done"
(defun rectpara-edit-extract-rectpara ()
  "Extract the current rectangle"
  (let (coords start-end start end rectpara)
    (setq coords (rectpara-edit-find-rectangle-boundaries))
    (setq start-end (rectpara-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))

    (setq rectpara (extract-rectangle start end))
   ))

;; used by "done"
(defun rectpara-zap-this-edit-window (rectpara-buffer)
  "Close the currently active edit window and return to the source RECTPARA-BUFFER."
  ;; This is neater behavior than just doing a (switch-to-buffer rectpara-buffer)
  ;; TODO it just closes the window, and preserves the edit buffer.
  ;; With greater confidence, you'd delete the edit buffer
  ;; (could save to disk for paranoia's sake...)
  (let* ( (edit-buffer-window (selected-window))
          (buffy (buffer-name) )
          )
    (delete-window edit-buffer-window)
    (if rectpara-clean-up-edit-buffers
        (kill-buffer buffy))

    (set-buffer rectpara-buffer)
    ))

;; used by "done"
(defun rectpara-clear-rectangle (coords)
  "Clear the rectangle with the given COORDS."
    (let* ( (start-end (rectpara-convert-coords-to-start-end coords))
            (start (car start-end))
            (end   (nth 1 start-end)) )
      (clear-rectangle start end)
      ))


;;;--------
;;; rectpara size change collision handling:

;;; The "done" routine has to do some work to deal with the case
;;; of a rectpara getting larger during editing: rather than
;;; over-write nearby text, we try to identify effected rectparas
;;; and move them (either rightward or downward) enough to get
;;; them out of the way.

;;;--------
;;; horizontal collisions

;; used by "done"
;; nickname: "horizontal"
(defun rectpara-deal-with-horizontal-expansion (delta-width coords)
  "Juggle things out of the way horizontally so expanded rectpara will fit."
  ;; Looks to the right and snags anything in the way of the rectpara
  ;; insert, then puts the rectparas back in locations shifted over
  ;; enough that there'll be no collision.  Uses a recursive check
  ;; on each rectpara to find other rectparas that need to be moved.
  (let* (
          (left  (nth 0 coords))
          (top   (nth 1 coords))
          (right (nth 2 coords))
          (bot   (nth 3 coords))

          (new-right (+ right delta-width))

          (padding 3)
          (horizon (+ delta-width padding))

            open-field shiftover
            ;; temp-hidden-rectparas
            )
    (setq rectpara-stash-plist () ) ;; clear the global stash
    (rectpara-move-column right)
    (setq open-field
          (rectpara-open-how-far-over right horizon top bot))
    (setq shiftover (- (+ delta-width padding) open-field))
    (cond ( (> shiftover 0 )
           ;; Stores extracted rectparas in global var: rectpara-stash-plist
           (rectpara-extract-rectpars-to-right coords horizon)

           ;; Pulls rectparas back in from the global rectpara-stash-plist
           (rectpara-restore-rectparas-shifted-over  shiftover )
           )
          )))

;; used by "horizontal"
(defun rectpara-open-how-far-over (col horizon top bot)
  "Find amount of open space next to column COL, between rows TOP and BOT.
Looks no further than the HORIZON (a limited number of columns).
Note: the maximum return value is the horizon."
  (save-excursion
    (let* (
            (i 0)   ;; loop: a horizontal count from the column COL
            (absolute-horizon (+ col horizon) )

            ;; looking for the "open-field": the end of the open columns
            (open-field absolute-horizon) ;; initialized to it's max value
            (candidate 0)                 ;; candidate col from each row

            (farther 1)   ;; look farther to keep from encroaching on stuff
            (top (- top farther))
            (bot (+ bot farther))
           )
      ;; loop vertically from top to bottom, looking to the right of current col
      (rectpara-move-row top)
      (while (<= (rectpara-current-row) bot )
        (setq i 0)
        (rectpara-move-column col) ;; move (back) to the original column

        ;; on each line, crawl forward looking for non-space
        (catch 'OUT
          (while (< (rectpara-current-column) absolute-horizon )
            (cond ( (not (looking-at " "))
                    (throw 'OUT nil) ) )

            ;; two steps forward and one back (to autovivify spaces in empty space)
            (picture-forward-column  2)
            (picture-backward-column 1)
            (setq i (1+ i))
            ))
        (setq candidate i)
        (if (< candidate open-field)
            (setq open-field candidate))
        (picture-move-down 1))
    open-field)))

;; used by "horizontal"
(defun rectpara-extract-rectpars-to-right ( coords  horizon )
  "Look to the right of the area given by COORDS, extract rectparas, return as list.
Does a cascading check for what is to the right, looking as far as HORIZON.
Works by calling this routine recursively on each rectpara.
A rectpara is represented as coords, i.e. a list of four x/y parameters,
and a rectangle, i.e. a list of lines of text."
  (save-excursion
    (let* (
           (saveloc (point) )
           (left  (nth 0 coords))
           (top   (nth 1 coords))
           (right (nth 2 coords))
           (bot   (nth 3 coords))

           (absolute-horizon (+ right horizon ))

           (farther 1) ;; peek up and down a bit farther to get everything.

           rectpara-contents  found-coords )
      ;; loop vertically from top to bottom, looking to the right of the rectpara
      (rectpara-move-row (- top farther))
      (while (<= (rectpara-current-row) (+ bot farther) )
          (setq found-coords () )
          ;; move to the column just outside the given rectpara coords
          (rectpara-move-column (1+ right))

          (setq found-coords
                (rectpara-find-rectpara-to-right absolute-horizon))

          ;; using global plist stash to accumulate a unique list of rectparas
          (cond ( found-coords
                  (let* ( (coords-str
                           (mapconcat 'number-to-string found-coords "-") ) )
                    (setq rectpara-contents
                          (rectpara-extract-rectpara-at-coords found-coords) )

                    (setq rectpara-stash-plist
                          (plist-put rectpara-stash-plist
                                     (intern coords-str)
                                     rectpara-contents))

                    ;; recursive call to keep looking out rightwards of the found rectpara
                    (rectpara-extract-rectpars-to-right found-coords horizon)
                    )))
          (picture-move-down 1)
          ))
    rectpara-stash-plist ))

(defun rectpara-find-rectpara-to-right (absolute-horizon)
  "Look for a rectpara to the right, somewhere before column ABSOLUTE-HORIZON.
If found, returns the list of coords of the rectpara."
  (let ( found-coords )
    ;; on each line, crawl forward looking for non-space
    (catch 'OUT
      (while (<= (rectpara-current-column) absolute-horizon )
        (cond
         ( (eolp)
           (throw 'OUT nil) )
         ( (not (looking-at " "))
           (setq found-coords (rectpara-find-rectpara-boundaries))
           (throw 'OUT found-coords) ) )
        ;; two steps forward and one back (to autovivify spaces)
        (picture-forward-column  2)
        (picture-backward-column 1)
        ))
    found-coords))

;; used by rectpara-deal-with-horizontal-expansion
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

;;;--------
;;; vertical collisions

;; used by "done"
;; nickname: "vertical" (newstyle)
(defun rectpara-deal-with-vertical-expansion (delta-height delta-width coords )
  "Juggle things out of the way vertically so expanded rectpara will fit."
  ;; uses a recursive check similar to "horizontal"
  (let* ( (left  (nth 0 coords))
          (top   (nth 1 coords))
          (right (nth 2 coords))
          (bot   (nth 3 coords))

          (new-bot   (+ bot   delta-height))
          (new-right (+ right delta-width ))

          (padding 2)
          (horizon (+ delta-height padding))

          open-field  shiftdown )
    ;; clear the global stash
    (setq rectpara-stash-plist () )
    (rectpara-move-row bot)
    (setq open-field
          (rectpara-open-how-far-down horizon left new-right))

    ;; difference in open-expansion and field is how much we need to open
    ;; (note: delta-height and open-field are both relative)
    (setq shiftdown (- (+ delta-height padding) open-field))
    (cond ( (> shiftdown 0 )
           ;; Stores extracted rectparas in global var: rectpara-stash-plist
           (rectpara-extract-rectpars-down-below coords horizon)

           ;; Pulls rectparas back in from the global rectpara-stash-plist
           (rectpara-restore-rectparas-shifted-down  shiftdown )
           )
          )))

;; used by "vertical" (both the newstyle and the old)
(defun rectpara-open-how-far-down (checkdistance left right &optional staythere)
  "Looks downward to see how far until the end of open space.
From the current row, scans down through the given CHECKDISTANCE,
verifying that there is whitespace between the LEFT and RIGHT
boundaries, returning the end location.  Note: the maximum return
value is the CHECKDISTANCE.  As a side-effect, creates lines at
the bottom of the buffer if the region we're checking extends
down past it.  If option STAYTHERE is t, will not restore point."
  (let ( (saveloc (point))
         (i 0)
         (limit checkdistance) )
    (catch 'UP
      (while (<= i checkdistance)
        (let ( (line (rectpara-get-line-segment-as-string left right)) )
          (if (not (string-match "^[ ]*$" line))
              (throw 'UP (setq limit i)) )
          ;; advance to next line, creating a blank line if at end-of-buffer
          (picture-move-down 2)
          (picture-move-up 1)
          (setq i (1+ i))
          )))
    (unless staythere
      (goto-char saveloc))
    limit))

;; used by rectpara-open-how-far-down
;;    and in turn by: rectpara-deal-with-vertical-expansion
(defun rectpara-get-line-segment-as-string (left right &optional line-number)
  "Gets the requested piece of a line as a string.  Default: current line.
Returns the substring between LEFT and RIGHT.  When the string is
too short will pad with spaces-- this assists in faking an infinite quarter-plane."
  ;; This routine may seem larger than it should be: one complication is
  ;; it works with the column numbers, not the absolute character count,
  ;; the other is the space infill code.
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
    (setq needed-padding
          (- right (length line) ))
    (if (> needed-padding 0)
        (setq line
              (concat line
                      (make-string needed-padding ?\ )
                      )) )

    ;; get the requested segment
    (setq segment
          (substring line left right))

    (goto-char saveloc)
    segment))


;; used by "vertical"
(defun rectpara-extract-rectpars-down-below ( coords  horizon )
  "Look to the downward below the area given by COORDS, extract rectparas, return as list.
Does a cascading check for what is down below, looking as far as HORIZON.
Works by calling this routine recursively on each rectpara.
A rectpara is represented as coords, i.e. a list of four x/y parameters,
and a rectangle, i.e. a list of lines of text."
  (save-excursion
    (let* (
           (saveloc (point) )
           (left  (nth 0 coords))
           (top   (nth 1 coords))
           (right (nth 2 coords))
           (bot   (nth 3 coords))
           (absolute-horizon (+ bot horizon ))
           (farther 1) ;; need to peek left and right a little farther to get everything.
           ;; (rp-count 0) ;; DEBUG
           rectpara-contents  found-coords
           )
      ;; work across from left to right, looking for rectparas down in each column
      (rectpara-move-column (- left farther))
      (while (<= (rectpara-current-column) (+ right farther) )
          ;; move to the row just below the given rectpara coords
          (rectpara-move-row (1+ bot))
          (setq found-coords
                (rectpara-find-next-rectpara-down-this-column absolute-horizon))
          ;; using global plist stash to accumulate a unique list of rectparas
          (cond ( found-coords
                  (let* ( (coords-str (mapconcat 'number-to-string found-coords "-") ) )
                    (setq rectpara-contents (rectpara-extract-rectpara-at-coords found-coords) )
                    (setq rectpara-stash-plist
                          (plist-put rectpara-stash-plist (intern coords-str) rectpara-contents))

                    ;; (setq rp-count (1+ rp-count))
                    ;; (if (> rp-count 1000)
                    ;;    (error "More than 1000 rps in stash: WTF?")) ;; DEBUG

                    ;; recursive call to keep looking below the found rectpara
                    (rectpara-extract-rectpars-down-below found-coords horizon)
                    )))
          (picture-forward-column  1)
          ))
    rectpara-stash-plist ))

(defun rectpara-find-next-rectpara-down-this-column (absolute-horizon)
  "Crawls down current column looking for non-space.
Returns the coords of the rectpara found at that point.  Required
argument ABSOLUTE-HORIZON specifies the row at which search
should stop."
  ;; in each column, crawl downward looking for non-space
  (let (found-coords)
    (catch 'OUT
      (while (<= (rectpara-current-row) absolute-horizon )
        (cond ( (not (looking-at " "))
                (setq found-coords (rectpara-find-rectpara-boundaries))
                (throw 'OUT nil) ) )
        ;; Step down, then step back up (to autovivify spaces)
        (rectpara-spaceout)
        (picture-move-down 1)
        (rectpara-spaceout)
        (picture-move-down 1)
        (rectpara-spaceout)
        (picture-move-up   1)
        ))
  found-coords))

;; used by rectpara-deal-with-vertical-expansion
(defun rectpara-restore-rectparas-shifted-down ( &optional downshift plist )
  "Inserts rectparas from PLIST into current buffer, moved down DOWNSHIFT chars.
PLIST defaults to rectpara-stash-plist. DOWNSHIFT defaults to 0."

  (catch 'OUT  ;; skip everything
    (unless downshift (setq downshift 0))
    (unless plist

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
              (max-x  (nth 2 effected))
              (max-y  (+ (nth 3 effected) downshift))
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

                 (newtop (+ top downshift))
                 )
            (rectpara-move-to-x-y-location-lite left newtop)
            (let ( ( contents (rectpara-stash-lookup key plist) )
                   )
              (picture-insert-rectangle contents)
          )))))))

;;--------
;; rectpara extraction, restore, removal...

;; used by "vertical" indirectly, via rectpara-extract-rectpars-with-coords-on-line
(defun rectpara-extract-rectpara-with-coords ()
  "Returns current rectpara along with a list of x-y coordinates, clears the original rectangle"
  (let (saveloc coords start-end start end rectpara)
    (setq coords (rectpara-find-rectpara-boundaries))
    (setq rectpara
          (rectpara-extract-rectpara-at-coords coords))
    (list rectpara coords)))

;; used by "vertical" indirectly, via rectpara-extract-rectpara-with-coords (see above)
;; and by "horizontal", via rectpara-extract-rectpars-to-right
(defun rectpara-extract-rectpara-at-coords (coords)
  "Given COORDS returns a copy of rectpara as list of strings."
    (setq start-end (rectpara-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))
    (setq rectpara (extract-rectangle start end))  ;; though extract implies removal this doesn't...
    (clear-rectangle start end)                    ;; we have to do the removal ourselves.
    rectpara )

;; used by "edit"
(defun rectpara-extract-rectpara-with-coords-and-rel-pos ()
  "Returns current rectpara along with a list of x-y coordinates, clears the original rectangle"
  (let (curloc coords start-end start end rectpara pos-pair)
    (setq curloc (point))
    (setq pos-pair (rectpara-x-and-y-from-pos curloc))
    (setq coords (rectpara-find-rectpara-boundaries))
    (setq rectpara
          (rectpara-extract-rectpara-at-coords coords))
    (list rectpara coords pos-pair)))

;; Used by "edit" via rectpara-extract-rectpara-with-coords-and-rel-pos,
(defun rectpara-x-and-y-from-pos (&optional pos)
  "Given position POS, return column and row pair.
The argument pos defaults to the current point.
The column and row numbers are 1 indexed."
  (let ( (saveloc (point) )
         column row
         )
    (unless pos
      (setq pos saveloc))

    (goto-char pos)
    (setq column (1+ (current-column)))
    (setq row (rectpara-current-row))

    (goto-char saveloc)
    (list column row) ;; col/row seems backwards, but convention is x/y
  ))


;; unused: was used by oldstyle "vertical"
(defun rectpara-restore-rectparas-from-list (big-list)
  "Restore the rectparas in the given list"
   (interactive)
   (let (coords rectpara x1 y1)
     (save-excursion
       (while big-list
         (setq coords (car (cdr (car big-list))))

         (setq x1 (nth 0 coords))
         (setq y1 (nth 1 coords))
;;        (setq x2 (nth 2 coords))
;;        (setq y2 (nth 3 coords))

         (rectpara-move-to-x-y-location-lite x1 y1)
         (setq rectpara (car (car big-list)))
         (picture-insert-rectangle rectpara)
         (setq big-list (cdr big-list))))))


;; unused: was used by oldstyle of "vertical"
(defun rectpara-extract-rectpars-with-coords-on-line ()
   "Looks for all rectparas intersecting the current line and
removes them, returns them with their coordinates"
;; scans horizontally across the row one below the bottom edge of original
;; rectpara.  when non-blank, runs
;;   rectpara-extract-rectpara-with-coords
;; (a way of moving rectparas out of the way temporarily)
   (let (result-list coords rectpara-with-coords next-col)
     (save-excursion
       (picture-beginning-of-line)
       (while (not (eolp))
         (if (looking-at "[^ ]")
             ((lambda ()
                (setq rectpara-with-coords (rectpara-extract-rectpara-with-coords))
                (setq coords (car (cdr rectpara-with-coords)))
                (setq next-col (1+ (nth 2 coords)))
                (rectpara-move-column next-col)

                (setq result-list (append result-list (list rectpara-with-coords)))
                                       ;; shifting on to the end of the list
                ))
           (picture-forward-column 1)))
       result-list)))


;;---------
;; rectpara copy utilities
;; (unused, at present)

(defun rectpara-copy-rectpara-at-coords (coords)
  "Given COORDS returns a copy of rectpara as rectangle, i.e. list of strings."
    (setq start-end (rectpara-convert-coords-to-start-end coords))
    (setq start (car start-end))
    (setq end (nth 1 start-end))
    (setq rectpara (extract-rectangle start end))
    ;; (clear-rectangle start end)  ;;; the sole difference between copy and extract
    rectpara )

(defun rectpara-copy-rectpara-with-coords ()
  "Returns current rectpara along with a list of x-y coordinates"
  (let (coords start-end start end rectpara)
    (setq coords (rectpara-find-rectpara-boundaries))
    (setq rectpara
          (rectpara-copy-rectpara-at-coords coords))
    (list rectpara coords)))


;;--------
;; file system utilities

(defun rectpara-fixdir ( location )
  "Fixes up the file directory LOCATION.
Conditions directory paths for portability and robustness.
If the directory does not yet exist, it will be created.\n
Relative paths are converted to absolute, using the current
`default-directory' setting."
  (let ((location
         (substitute-in-file-name
          (convert-standard-filename
           (file-name-as-directory
            (expand-file-name location))))))
    (unless (file-directory-p location)
      (make-directory location t))
    location))


;;========
;; TODO

;; o  would be useful to shut off expansion handling, (defcustom)
;;    and simply refuse to complete a return from edit
;;    until issue is resolved manually.

;; o  new class of features to trace chains of rectparas:
;;    o  would like a "skip to next rectpara" command.
;;       Look for nearest rectpara downward and to the right
;;       when there's more than one... do something reasonable.
;;        o  Just go to the first one (leftmost).
;;        o  Provide a second command to cycle through choices


;;=======
;; legal

;; Copyright 2016 Joseph Brenner
;; License: GPL 3.0 (see boilerplate below)
;;
;; Author: doom@kzsu.stanford.edu
;; Version: 1.0
;; X-URL: http://obsidianrook.com/rectpara/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
