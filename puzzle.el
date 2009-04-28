;;; puzzle.el --- slide puzzle for Emacs

;;; Copyright (C) 2004, 2005 Matthew P. Hodges

;; Author: Matthew P. Hodges <MPHodges@member.fsf.org>
;; Version: $Id: puzzle.el,v 1.27 2006/09/18 08:40:56 mphodges-guest Exp $

;; puzzle.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; puzzle.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:
;;
;; Slide puzzle for Emacs.  Start with M-x puzzle RET. Press ? for
;; help.

;;; Code:

(defconst puzzle-version "2.0.0"
  "Version number of this package.")

(defgroup puzzle nil
  "Puzzle game for Emacs."
  :group 'games
  :link '(url-link "http://mph-emacs-pkgs.alioth.debian.org/PuzzleEl.html"))

;; Customizable variables

(defcustom puzzle-rows 4
  "Number of puzzle rows."
  :group 'puzzle
  :type 'integer)

(defcustom puzzle-columns 4
  "Number of puzzle columns."
  :group 'puzzle
  :type 'integer)

(defcustom puzzle-max-rows-columns 10
  "Maximum number of puzzle rows and columns."
  :group 'puzzle
  :type 'integer)

(defcustom puzzle-shuffle-factor 5
  "Affects the degree to which the puzzle is shuffled.
Multiplies `puzzle-rows' and `puzzle-columns'."
  :group 'puzzle
  :type 'integer)

(defcustom puzzle-header-line-string "Emacs Puzzle"
  "String used in `header-line-format' in Puzzle buffer."
  :group 'puzzle
  :type 'string)

(defcustom puzzle-random-interval 0.1
  "Delay used between random moves in `puzzle-random-moves'."
  :group 'puzzle
  :type 'number)

;; Other variables

(defvar puzzle-image-file (concat data-directory "splash.xpm")
  "File used for current puzzle (see also `puzzle-image-buffer') .")

(defvar puzzle-image-buffer nil
  "Buffer used for current puzzle (see also `puzzle-image-file').")

(defvar puzzle-image-string nil
  "Propertized string holding puzzle image.")

(defvar puzzle-moves-made 0
  "Number of puzzle moves made since last restart.")

(defvar puzzle-image-char-width nil
  "Width of puzzle image in characters.")

(defvar puzzle-showing-solution nil
  "Non-nil if we are showing the puzzle solution.")

;; Constants

(defconst puzzle-image-slice-string "slice "
  "Arbitrary string used by `insert-sliced-image'.")

;; Faces

(defface puzzle-blank-face
'((((class color))
     (:inherit default :inverse-video t)))
  "Face used for the blank puzzle piece.
Useful if the `puzzle-image-file' image is transparent."
  :group 'puzzle)

;; Entry point

;;;###autoload
(defun puzzle ()
  "Puzzle game for Emacs."
  (interactive)
  (puzzle-check-features)
  (unless (buffer-live-p (get-buffer "*puzzle*"))
    (get-buffer-create "*puzzle*"))
  (set-buffer "*puzzle*")
  (puzzle-mode)
  (select-window (display-buffer "*puzzle*"))
  (puzzle-restart))

;; Commands

(defun puzzle-restart ()
  "Initialize puzzle buffer."
  (interactive)
  (unless (equal major-mode 'puzzle-mode)
    (error "Not in Puzzle buffer"))
  (let ((inhibit-read-only t))
    (puzzle-check-status)
    (erase-buffer)
    (puzzle-insert-image)
    (puzzle-randomize)
    (setq puzzle-moves-made 0)
    (puzzle-recenter)))

(defun puzzle-recenter ()
  "Recenter the puzzle image in each window displaying it.
This is achieved by setting the window margins."
  (interactive)
  (let ((buffer (current-buffer))
        margin)
    ;; Set window margins
    (walk-windows
     (lambda (w)
       (with-selected-window w
         (when (eq (window-buffer w) buffer)
           (set-window-margins nil 0 0)
           (setq margin
                 (max (/ (- (window-width) puzzle-image-char-width) 2)
                      0))
           (set-window-margins nil margin margin))))
     nil t)))

(defun puzzle-move-piece ()
  "Move piece at point."
  (interactive)
  (unless (equal major-mode 'puzzle-mode)
    (error "Not in Puzzle buffer"))
  (when (puzzle-move-valid-p)
    (puzzle-move-piece-internal)))

(defun puzzle-move-piece-down ()
  "Move piece down."
  (interactive)
  (unless (equal major-mode 'puzzle-mode)
    (error "Not in Puzzle buffer"))
  (let* ((coords (puzzle-point-to-coords (puzzle-blank-position)))
         (row (car coords))
         (column (cdr coords))
         (posn (puzzle-coords-to-point (1- row) column)))
    (when posn
      (goto-char posn)
      (puzzle-move-piece))))

(defun puzzle-move-piece-left ()
  "Move piece left."
  (interactive)
  (unless (equal major-mode 'puzzle-mode)
    (error "Not in Puzzle buffer"))
  (let* ((coords (puzzle-point-to-coords (puzzle-blank-position)))
         (row (car coords))
         (column (cdr coords))
         (posn (puzzle-coords-to-point row (1+ column))))
    (when posn
      (goto-char posn)
      (puzzle-move-piece))))

(defun puzzle-move-piece-right ()
  "Move piece right."
  (interactive)
  (unless (equal major-mode 'puzzle-mode)
    (error "Not in Puzzle buffer"))
  (let* ((coords (puzzle-point-to-coords (puzzle-blank-position)))
         (row (car coords))
         (column (cdr coords))
         (posn (puzzle-coords-to-point row (1- column))))
    (when posn
      (goto-char posn)
      (puzzle-move-piece))))

(defun puzzle-move-piece-up ()
  "Move piece up."
  (interactive)
  (unless (equal major-mode 'puzzle-mode)
    (error "Not in Puzzle buffer"))
  (let* ((coords (puzzle-point-to-coords (puzzle-blank-position)))
         (row (car coords))
         (column (cdr coords))
         (posn (puzzle-coords-to-point (1+ row) column)))
    (when posn
      (goto-char posn)
      (puzzle-move-piece))))

(defun puzzle-mouse-move-piece (event)
  "Move piece selected by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (puzzle-move-piece))

(defun puzzle-random-move ()
  "Do random move of puzzle piece."
  (interactive)
  (unless (equal major-mode 'puzzle-mode)
    (error "Not in Puzzle buffer"))
  (let* ((coords (puzzle-point-to-coords (puzzle-blank-position)))
         (row (car coords))
         (column (cdr coords))
         (valid-posns
          (delete nil
                  (mapcar (lambda (row-col)
                            (setq row (car row-col)
                                  column (cdr row-col))
                            (puzzle-coords-to-point row column))
                          `((,row . ,(1- column)) ;; Left
                            (,row . ,(1+ column)) ;; Right
                            (,(1- row) . ,column) ;; Above
                            (,(1+ row) . ,column) ;; Below
                            )))))
    (goto-char (elt valid-posns (random (length valid-posns))))
    (puzzle-move-piece-internal)))

(defun puzzle-random-moves ()
  "Perform continuous random moves until the puzzle is solved.
This can take a very large number of moves even for a 3x3
puzzle."
  (interactive)
  (unless (equal major-mode 'puzzle-mode)
    (error "Not in Puzzle buffer"))
  (while (and (not (puzzle-solved-p))
              (not (input-pending-p)))
    (puzzle-random-move)
    (sit-for puzzle-random-interval))
  (discard-input))

(defun puzzle-quit (&optional arg)
  "Quit the Puzzle buffer.
With prefix argument ARG, kill the buffer."
  (interactive "P")
  (when (equal major-mode 'puzzle-mode)
    (quit-window arg)))

(defun puzzle-show-solution ()
  "Temporarily show puzzle solution."
  (interactive)
  (unless (puzzle-solved-p)
    (save-restriction
      (narrow-to-region (point-min) (point-min))
      (let ((puzzle-showing-solution t))
        (momentary-string-display puzzle-image-string (point-min)
                                  nil
                                  "Press key to continue.")))
    (discard-input)))

(defun puzzle-more-pieces (arg)
  "Increase `puzzle-rows' and `puzzle-columns' by ARG.
If no argument, increase by one."
  (interactive "p")
  (setq puzzle-rows (+ puzzle-rows (or arg 1))
        puzzle-columns (+ puzzle-columns (or arg 1)))
  (puzzle-restart))

(defun puzzle-fewer-pieces (arg)
  "Decrease `puzzle-rows' and `puzzle-columns' by ARG.
If no argument, increase by one."
  (interactive "p")
  (puzzle-more-pieces (- arg)))

(defun puzzle-change-image-from-file (filename)
  "Set FILENAME as new `puzzle-image-file'."
  (interactive "fNew image file: ")
  (setq puzzle-image-file filename
        puzzle-image-buffer nil)
  (puzzle-restart))

(defun puzzle-change-image-from-buffer (buffer)
  "Set BUFFER as new `puzzle-image-buffer'."
  (interactive "bNew image buffer: ")
  (setq puzzle-image-buffer buffer
        puzzle-image-file nil)
  (puzzle-restart))

(defun puzzle-delete-other-windows ()
  "Delete all other windows in the frame."
  (interactive)
  (delete-other-windows)
  (puzzle-recenter))

;; Functions

(defun puzzle-check-features ()
  "Check features required to run `puzzle'."
  (cond
   ((featurep 'xemacs)
    (error "XEmacs not supported"))
   ((not (featurep 'image))
    (error "Images not supported"))
   ((not (fboundp 'insert-sliced-image))
    (error "Sliced images not supported"))
   ((not (display-images-p))
    (error "Cannot display images"))))

(defun puzzle-check-status ()
  "Check some puzzle variables."
  (cond
   (puzzle-image-file
    (unless (image-type-available-p
             (image-type-from-file-header puzzle-image-file))
      (error "Unsupported image type for %s" puzzle-image-file)))
   (puzzle-image-buffer
    (unless (with-current-buffer puzzle-image-buffer
              (and (eq major-mode 'image-mode)
                   (member 'image (get-text-property (point-min) 'display))))
      (error "Image not found in buffer %s" (buffer-name buffer))))
   (t
    (error "No valid puzzle image file or buffer")))
  (setq puzzle-rows (max puzzle-rows 2))
  (setq puzzle-rows (min puzzle-rows puzzle-max-rows-columns))
  (setq puzzle-columns (max puzzle-columns 2))
  (setq puzzle-columns (min puzzle-columns puzzle-max-rows-columns)))

(defun puzzle-insert-image ()
  "Insert image from `puzzle-image-file'."
  (let ((inhibit-read-only t)
        (counter 0)
        (length (length puzzle-image-slice-string))
	image)
    (setq image (if puzzle-image-file
                    (create-image puzzle-image-file nil nil)
                  (with-current-buffer puzzle-image-buffer
                    (member 'image (get-text-property (point-min) 'display)))))
    (insert-sliced-image image
                         puzzle-image-slice-string nil
                         puzzle-rows puzzle-columns)
    (setq puzzle-image-char-width (floor (car (image-size image))))
    (goto-char (point-min))
    (puzzle-insert-blank-image (/ (car (image-size image t)) puzzle-columns)
			       (/ (cdr (image-size image t)) puzzle-rows))
    (setq puzzle-image-string (buffer-string))
    (save-excursion
      (while (< counter (* puzzle-rows puzzle-columns))
        (add-text-properties (point) (1+ (point))
                             `(puzzle-index ,counter))
        (forward-char length)
        (and (eolp) (forward-char 1))
        (setq counter (1+ counter))))))

(defun puzzle-insert-blank-image (width height)
  "Put blank image slice (WIDTH by HEIGHT pixels) at point."
  (let ((length (length puzzle-image-slice-string))
        (string (concat "\"" (make-string width ?\ ) "\",\n"))
        (counter 0)
        data)
    (with-temp-buffer
      (insert "/* XPM */\nstatic char * blank[] = {\n")
      (insert (format "\"%d %d 1 1\",\n" width height))
      (insert "\" 	c None\",\n")
      (while (< counter height)
        (insert string)
        (setq counter (1+ counter)))
      (backward-delete-char 2)
      (insert "};\n")
      (setq data (buffer-string)))
    (save-excursion
      (insert-image (create-image data 'xpm t) puzzle-image-slice-string)
      (delete-char length)))
  (add-text-properties (point) (1+ (point))
		       '(puzzle-blank t face puzzle-blank-face)))

(defun puzzle-blank-position ()
  "Get the position of the blank puzzle piece."
  (text-property-any (point-min) (point-max) 'puzzle-blank t))

(defun puzzle-coords-to-point (row column)
  "Return the position in the Puzzle buffer for ROW and COLUMN.
If ROW and COLUMN and not valid return nil."
  (let ((length (length puzzle-image-slice-string)))
    (if (or (< row 0)
	    (> row (1- puzzle-rows))
	    (< column 0)
	    (> column (1- puzzle-columns)))
	nil
      (+ (* row (1+ (* puzzle-columns length)))
	 (* column length)
	 1))))

(defun puzzle-point-to-coords (posn)
  "Return the puzzle row and column at POSN.
Return nil if not on a puzzle piece."
  (let ((length (length puzzle-image-slice-string))
	row column)
    (save-excursion
      (goto-char posn)
      (if (not (get-text-property (point) 'puzzle-index))
          nil
        (setq row (count-lines (point-min) (line-beginning-position))
              column (/ (- posn (line-beginning-position)) length)))
      (cons row column))))

(defun puzzle-randomize ()
  "Randomize puzzle."
  (let ((steps (* puzzle-shuffle-factor
		  puzzle-rows puzzle-columns))
	(count 0))
    (while (< count steps)
      (puzzle-random-move)
      (setq count (1+ count)))))

(defun puzzle-move-piece-internal ()
  "Move piece at point."
  (let ((length (length puzzle-image-slice-string))
	(index (get-text-property (point) 'puzzle-index))
        (blankp (get-text-property (point) 'puzzle-blank))
	(blank-posn (puzzle-blank-position))
        (inhibit-read-only t))
    (when (and index
               (not blankp))
      (transpose-regions (point) (+ (point) length)
			 blank-posn (+ blank-posn length))
      (set-window-start (selected-window) (point-min))))
  (set-buffer-modified-p nil)
  (setq puzzle-moves-made (1+ puzzle-moves-made)))

(defun puzzle-move-valid-p ()
  "Return t if we can move puzzle piece at point."
  (let* ((coords (puzzle-point-to-coords (point)))
	 (row (car coords))
	 (column (cdr coords))
	 posn)
    (cond
     ((get-text-property (point) 'puzzle-blank)
      nil)
     ((not (get-text-property (point) 'puzzle-index))
      nil)
     ;; Left
     ((and (setq posn (puzzle-coords-to-point row (1- column)))
	   (get-text-property posn 'puzzle-blank)))
     ;; Right
     ((and (setq posn (puzzle-coords-to-point row (1+ column)))
	   (get-text-property posn 'puzzle-blank)))
     ;; Up
     ((and (setq posn (puzzle-coords-to-point (1- row) column))
	   (get-text-property posn 'puzzle-blank)))
     ;; Down
     ((and (setq posn (puzzle-coords-to-point (1+ row) column))
	   (get-text-property posn 'puzzle-blank))))))

(defun puzzle-solved-p ()
  "Return t if the puzzle has been solved."
  (let ((solved t)
        (counter 0)
        posn)
    (save-excursion
      (goto-char (point-min))
      (catch 'not-solved
        (while (< counter (* puzzle-rows puzzle-columns))
          (setq posn (text-property-any (point) (point-max)
                                        'puzzle-index counter))
          (if posn
              (goto-char posn)
            (throw 'not-solved (setq solved nil)))
          (setq counter (1+ counter)))))
    solved))

(defun puzzle-header-line-format ()
  "Construct variable of `header-line-format' for Puzzle buffer."
  (let* ((string
          (concat puzzle-header-line-string
                  (if puzzle-showing-solution
                      " (solution)"
                    (format " (moves: %d%s)"
                            puzzle-moves-made
                            (if (puzzle-solved-p) " -- solved" "")))))
         (length (/ (- (min puzzle-image-char-width (window-width))
                       (length string)) 2)))
    (unless (wholenump length)
      (setq length 0))
    (concat (propertize " " 'display `(space :align-to ,length)) string)))

;; Mode settings

(defvar puzzle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "1")   'puzzle-delete-other-windows)
    (define-key map (kbd "b")   'puzzle-change-image-from-buffer)
    (define-key map (kbd "f")   'puzzle-change-image-from-file)
    (define-key map (kbd "q")   'puzzle-quit)
    (define-key map (kbd "r")   'puzzle-restart)
    (define-key map (kbd "m")   'puzzle-random-move)
    (define-key map (kbd "M")   'puzzle-random-moves)
    (define-key map (kbd "R")   'puzzle-recenter)
    (define-key map (kbd "s")   'puzzle-show-solution)
    (define-key map (kbd "+")   'puzzle-more-pieces)
    (define-key map (kbd "-")   'puzzle-fewer-pieces)
    (define-key map (kbd "?")   'describe-mode)
    (define-key map (kbd "RET") 'puzzle-move-piece)
    (define-key map (kbd "<down>")  'puzzle-move-piece-down)
    (define-key map (kbd "<left>")  'puzzle-move-piece-left)
    (define-key map (kbd "<right>") 'puzzle-move-piece-right)
    (define-key map (kbd "<up>")    'puzzle-move-piece-up)
    (define-key map (kbd "<mouse-1>") 'puzzle-mouse-move-piece)
    map)
  "Keymap for puzzle mode.")

;; Menus

(defvar puzzle-menu nil
  "Menu to use for `puzzle-mode'.")

(easy-menu-define puzzle-menu puzzle-mode-map "Puzzle Menu"
  '("Puzzle"
    ["Move piece"    puzzle-move-piece t]
    ["Random move"   puzzle-random-move t]
    ["Random moves"  puzzle-random-moves t]
    ["Show solution" puzzle-show-solution t]
    ["Recenter"      puzzle-recenter t]
    "---"
    ["Restart"       puzzle-restart t]
    ["Change image from file"  puzzle-change-image-from-file t]
    ["Change image from buffer"  puzzle-change-image-from-buffer t]
    ["More pieces"   puzzle-more-pieces t]
    ["Fewer pieces"  puzzle-fewer-pieces t]
    "---"
    ["Quit"          puzzle-quit t]))

(defun puzzle-mode ()
  "Major mode for controlling Puzzle game.

\\{puzzle-mode-map}"
  (kill-all-local-variables)
  (use-local-map puzzle-mode-map)
  (setq major-mode 'puzzle-mode)
  (setq mode-name "Puzzle")
  (setq buffer-read-only t
        buffer-undo-list t
        truncate-lines t
        fringes-outside-margins t
        header-line-format
        '((:eval (puzzle-header-line-format))))
  (run-hooks 'puzzle-mode-hook))

(provide 'puzzle)

;;; puzzle.el ends here
