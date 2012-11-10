;;; simple-call-tree-ext.el --- Extensions to simple-call-tree

;; Filename: simple-call-tree-ext.el
;; Description: extensions to simple-call-tree
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Created: 2012-11-01 21:28:07
;; Version: 0.1
;; Last-Updated: 2012-11-01 21:28:07
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/simple-call-tree-ext.el
;; Keywords: programming
;; Compatibility: GNU Emacs 24.2.1
;;
;; Features that might be required by this library:
;;
;; simple-call-tree.el
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;; 
;; Extensions to simple-call-tree.el for elisp libraries.
;; 
;; 

;;; Installation:
;;
;; Put simple-call-tree-ext.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'simple-call-tree-ext)

;;; Customize:
;;
;; To automatically insert descriptions of customizable variables defined in this buffer
;; place point at the beginning of the next line and do: M-x insert-customizable-variable-descriptions

;;
;; All of the above can customized by:
;;      M-x customize-group RET simple-call-tree-ext RET
;;

;;; Change log:
;;	
;; 2012/11/01
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; Rewrite simple-call-tree-view-function and simple-call-tree-visit-function to use
;; the markers in simple-call-tree-locations-alist.
;; Follow mode, using fm.el
;;

;;; Require
(require 'thingatpt)
(require 'outline-magic)
;;; Code:

(defgroup simple-call-tree nil
  "Simple call tree - display a simple call tree for functions in a buffer."
  :group 'tools
  :link '(url-link "http://www.emacswiki.org/SimpleCallTree"))

(defcustom simple-call-tree-default-recenter 'middle
  "How to recenter the window after moving to another function in the \"*Simple Call Tree*\" buffer.
Can be one of the following symbols: 'top 'middle 'bottom.
This variable is used by the `simple-call-tree-jump-to-function' function when no prefix arg is given."
  :group 'simple-call-tree
  :type '(choice (const :tag "Top" top)
                 (const :tag "Middle" middle)
                 (const :tag "Bottom" bottom)))

;; Saves a little typing
(defmacro whilelast (&rest forms)
  `(while (progn ,@forms)))

(defmacro whilenotlast (&rest forms)
  `(while (not (progn ,@forms))))

(define-derived-mode simple-call-tree-mode outline-mode "Simple Call Tree"
  "The major-mode for the one-key menu buffer."
  :group 'simple-call-tree
  (setq simple-call-tree-mode-map (make-keymap)
        buffer-read-only nil)
  ;; Set keymap
  (define-key simple-call-tree-mode-map (kbd "q") 'simple-call-tree-quit)
  (define-key simple-call-tree-mode-map (kbd "<tab>") 'outline-cycle)
  (define-key simple-call-tree-mode-map (kbd "SPC") 'simple-call-tree-view-function)
  (define-key simple-call-tree-mode-map (kbd "C-o") 'simple-call-tree-view-function)
  (define-key simple-call-tree-mode-map (kbd "<return>") 'simple-call-tree-visit-function)
  (define-key simple-call-tree-mode-map (kbd "o") 'simple-call-tree-visit-function)
  (define-key simple-call-tree-mode-map (kbd "j") 'simple-call-tree-jump-to-function)
  (define-key simple-call-tree-mode-map (kbd "u") 'simple-call-tree-move-up)
  (define-key simple-call-tree-mode-map (kbd "n") 'simple-call-tree-move-next)
  (define-key simple-call-tree-mode-map (kbd "p") 'simple-call-tree-move-prev)
  (define-key simple-call-tree-mode-map (kbd "f") 'simple-call-tree-move-next-samelevel)
  (define-key simple-call-tree-mode-map (kbd "b") 'simple-call-tree-move-prev-samelevel)
  (define-key simple-call-tree-mode-map (kbd "i") 'simple-call-tree-invert-buffer)
  (define-key simple-call-tree-mode-map (kbd "d") 'simple-call-tree-change-maxdepth)
  (define-key simple-call-tree-mode-map (kbd "/") 'simple-call-tree-toggle-narrowing)
  (define-key simple-call-tree-mode-map (kbd "w") 'widen)
  (use-local-map simple-call-tree-mode-map)
  (setq mode-line-format
        (append
         (subseq mode-line-format 0
                 (1+ (position 'mode-line-buffer-identification
                               mode-line-format)))
         (list '(:eval (format (if simple-call-tree-inverted-bufferp
                                   " Inverted, maxdepth=%d "
                                 " maxdepth=%d ")
                               simple-call-tree-current-maxdepth)))
         (subseq mode-line-format
                 (+ 2 (position 'mode-line-buffer-identification
                                mode-line-format)))))
  (outline-minor-mode 1)
  (setq outline-regexp "|\\([-<>]*\\) "
        outline-level 'simple-call-tree-outline-level))

(defvar simple-call-tree-alist nil
  "Alist of functions and the functions they call.")

(defvar simple-call-tree-locations-alist nil
  "Alist of functions and their locations within their respective buffers.
The car of each element is a function name, and the cdr is a marker indicating the position
of the functions definition.")

(defvar simple-call-tree-inverted-bufferp nil
  "Indicates if the *Simple Call Tree* buffer is currently inverted or not.
If non-nil then children correspond to callers of parents in the outline tree.
Otherwise it's the other way around.")

(defvar simple-call-tree-current-maxdepth nil
  "The current maximum depth of the tree in the *Simple Call Tree* buffer.
The minimum value is 0 which means show top level functions only.")

;;; Functions from simple-call-tree.el (some are rewritten)

(defun simple-call-tree-add (start end alist)
  "Add tokes between START and END to ALIST.
ALIST is a list with a string identifying the function in its car,
and the list of functions it calls in the cdr."
  (dolist (entry simple-call-tree-alist)
    (goto-char start)
    (catch 'done
      (while (search-forward (car entry) end t)
	(let ((faces (get-text-property (point) 'face)))
	  (unless (listp faces)
	    (setq faces (list faces)))
	  (unless (or (memq 'font-lock-comment-face faces)
		      (memq 'font-lock-string-face faces))
	    (setcdr alist (cons (car entry)
				(cdr alist)))
	    (throw 'done t)))))))

(defun* simple-call-tree-analyze (&optional test (buffers (list (current-buffer))))
  "Analyze the current buffer, or the buffers in list BUFFERS.
The result is stored in `simple-call-tree-alist'.
If optional function TEST is given, it must return non-nil when
called with one parameter, the starting position of the function
name.
Optional arg BUFFERS is a list of buffers to analyze together.
By default it is set to a list containing the current buffer."
  (interactive)
  (setq simple-call-tree-alist nil
        simple-call-tree-locations-alist nil)
  ;; First add all the functions defined in the buffers to simple-call-tree-alist.
  (let ((pos (point-min))
        oldpos count nextfunc max item olditem)
    (dolist (buf buffers)
      (with-current-buffer buf
        (font-lock-default-fontify-buffer)
        (setq pos (point-min)
              count 0)
        (save-excursion
          (while (setq nextfunc (simple-call-tree-next-func 'pos test))
            (goto-char pos)
            (setq simple-call-tree-alist (cons (list nextfunc) simple-call-tree-alist)
                  simple-call-tree-locations-alist (cons (cons nextfunc (point-marker)) simple-call-tree-locations-alist)
                  count (1+ count))
            (message "Identifying functions...%d" count)))))
    ;; Set variables in preparation for next part.
    (dolist (buf buffers)
      (with-current-buffer buf
        (setq pos (point-min)
              max count
              count 0
              oldpos pos
              olditem '("*Start*") ;; dummy value required for 1st iteration of following loop
              item nextfunc)
        (save-excursion
          ;; Loop through functions, adding called functions to associated items in simple-call-tree-alist.
          (while (setq nextfunc (simple-call-tree-next-func 'pos test))
            (setq item (assoc nextfunc simple-call-tree-alist)
                  count (1+ count))
            (message "Identifying functions called...%d/%d" count max)
            (simple-call-tree-add oldpos (- pos (length nextfunc)) olditem)
            (setq oldpos pos olditem item))
          ;; Final function needs to be dealt with separately using a different method for finding its end.
          (goto-char oldpos)
          (end-of-defun)
          (simple-call-tree-add oldpos (point) olditem)))))
  (message "simple-call-tree done"))

(defun simple-call-tree-analyze-perl ()
  "Call `simple-call-tree-analyze-perl' for CPerl code."
  (interactive)
  (simple-call-tree-analyze (lambda (pos)
		       (goto-char pos)
		       (beginning-of-line)
		       (looking-at "sub"))))

(defun simple-call-tree-invert (alist)
  "Invert ALIST and return the result."
  (let (result)
    (dolist (item simple-call-tree-alist)
      (let ((caller (car item))
            (callees (cdr item)))
        (dolist (callee callees)
          (let ((elem (assoc callee result)))
            (if elem
                (setcdr elem (cons caller (cdr elem)))
              (setq result (cons (list callee caller) result)))))))
    result))

;;; New functions (not in simple-call-tree.el)

(defun* simple-call-tree-get-function-at-point (&optional (buf "*Simple Call Tree*"))
  "Return the name of the function nearest point in the *Simple Call Tree* buffer.
If optional arg BUF is supplied then use BUF instead of the *Simple Call Tree* buffer."
  (with-current-buffer buf
    (if (and (equal buf "*Simple Call Tree*")
             (looking-at "[|-<>]* [^|-<> ]"))
        (goto-char (next-single-property-change (point) 'face)))
    (let* ((symb (if (functionp 'symbol-nearest-point)
                     (symbol-nearest-point)
                   (symbol-at-point)))
           (fn (or (and (fboundp symb) symb)
                   (function-called-at-point))))
      (symbol-name fn))))

(defun simple-call-tree-next-func (posvar &optional test)
  "Find the next function in the current buffer after position POSVAR, and return its name.
POSVAR should be a symbol which evaluates to a position in the current buffer. If a function is found
its value will be changed to the position in the current buffer just after the function name.
If optional function TEST is given, it must return non-nil when called with one parameter, the starting
position of the function name."
  (let ((start (eval posvar)) end)
    (while (and (not (and (eq (get-text-property start 'face)
                              'font-lock-function-name-face)
                          (or (not (functionp test))
                              (funcall test start))))
                (setq start (next-single-property-change start 'face))))
    (unless (not start)
      (setq end (next-single-property-change start 'face))
      (set posvar end)
      (unless (not end)
        (buffer-substring-no-properties start end)))))

(defun* simple-call-tree-display-buffer (&optional depth files)
  "Display call tree for current buffer.
If optional args DEPTH and FILES are supplied they specify the depth of the tree,
and the files to search for functions to display in the tree.
When called interactively DEPTH will be set to the prefix arg if supplied, or be
prompted for, and only functions in the current buffer will be used."
  (interactive "P")
  (let ((maxdepth (if current-prefix-arg (prefix-numeric-value depth)
                    (or depth
                        (floor (abs (read-number "Maximum depth to display: " 2))))))
        buffers)
    (or current-prefix-arg files
        (if (y-or-n-p "Include other files?")
            (whilenotlast (setq file (read-file-name "File: " nil "SENTINEL"))
                          (add-to-list 'files file)
                          (string= file "SENTINEL"))))
    (save-excursion
      (setq buffers (if (or current-prefix-arg (not files)) (list (current-buffer))
                      (cons (current-buffer)
                            (loop for file in files
                                  unless (string= file "SENTINEL")
                                  collect (find-file file))))))
    (simple-call-tree-analyze nil buffers)
    (simple-call-tree-list-callers-and-functions maxdepth)))

(defun* simple-call-tree-current-function (func &optional wide)
  "Display call tree for function FUNC.
If called interactively FUNC will be set to the symbol nearest point,
unless a prefix arg is used in which case the function returned by `which-function'
will be used.
Note: `which-function' may give incorrect results if `imenu' has not been used in
the current buffer.
If a call tree containing FUNC has not already been created then the user is prompted
for which files to build the tree from.

If optional arg WIDE is non-nil then the *Simple Call Tree* buffer will be widened,
otherwise it will be narrowed around FUNC."
  (interactive (list (if current-prefix-arg
                         (which-function)
                       (simple-call-tree-get-function-at-point (current-buffer)))))
  (if (assoc func simple-call-tree-alist)
      (if (get-buffer "*Simple Call Tree*")
          (switch-to-buffer "*Simple Call Tree*")
        (simple-call-tree-list-callers-and-functions))
    (simple-call-tree-display-buffer))
  (simple-call-tree-jump-to-function func)
  (if wide (simple-call-tree-toggle-narrowing 1)
    (simple-call-tree-toggle-narrowing -1)))

(defun* simple-call-tree-list-callers-and-functions (&optional (maxdepth 2)
                                                               (funclist simple-call-tree-alist))
  "List callers and functions in FUNCLIST to depth MAXDEPTH.
By default FUNCLIST is set to `simple-call-tree-alist'."
  (switch-to-buffer (get-buffer-create "*Simple Call Tree*"))
  (if (not (equal major-mode 'simple-call-tree-mode)) (simple-call-tree-mode))
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((inverted (not (equal funclist simple-call-tree-alist)))
        (maxdepth (max maxdepth 1)))
    (dolist (item funclist)
      (simple-call-tree-list-callees-recursively (car item) maxdepth 1 funclist inverted))
    (setq simple-call-tree-current-maxdepth (max maxdepth 1)
          simple-call-tree-inverted-bufferp inverted
          buffer-read-only t)))

(defun* simple-call-tree-list-callees-recursively (fname &optional (maxdepth 2)
                                                         (curdepth 1)
                                                         (funclist simple-call-tree-alist)
                                                         (inverted (not (equal funclist simple-call-tree-alist))))
  "Insert a call tree for the function named FNAME, to depth MAXDEPTH.
FNAME must be the car of one of the elements of FUNCLIST which is set to `simple-call-tree-alist' by default.
The optional arguments MAXDEPTH and CURDEPTH specify the maximum and current depth of the tree respectively.
This is a recursive function, and you should not need to set CURDEPTH."
  (let* ((callees (cdr (assoc fname funclist)))
         (arrowtail (make-string (1- curdepth) 45))
         (arrow (if inverted (concat (if (> curdepth 1) "<") arrowtail " ")
                  (concat arrowtail (if (> curdepth 1) "> " " "))))
         (face (intern-soft (format "outline-%d" (mod curdepth 8)))))
    (insert "|" arrow (propertize fname
                                  'font-lock-face (list :inherit face :underline t)
                                  'mouse-face 'highlight) "\n")
    (if (< curdepth maxdepth)
        (dolist (callee callees)
          (simple-call-tree-list-callees-recursively callee maxdepth (1+ curdepth) funclist inverted)))))

(defun simple-call-tree-outline-level nil
  "Return the outline level of the function at point."
  (with-current-buffer "*Simple Call Tree*"
    (save-excursion
      (move-beginning-of-line 1)
      (re-search-forward outline-regexp)
      (let ((len (length (match-string 1))))
        (if (= len 0) 1 len)))))

;;; Major-mode commands bound to keys

(defun simple-call-tree-quit nil
  "Quit the *Simple Call Tree* buffer."
  (interactive)
  (let ((win (get-buffer-window "*Simple Call Tree*")))
    (if win (with-selected-window win
              (if fm-working (fm-toggle))
              (if (> (length (window-list)) 1)
                  (delete-window)
                (bury-buffer))))))

(defun simple-call-tree-invert-buffer (&optional maxdepth)
  "Invert the tree in *Simple Call Tree* buffer."
  (interactive "P")
  (let ((funclist (if simple-call-tree-inverted-bufferp
                      simple-call-tree-alist
                    (simple-call-tree-invert simple-call-tree-alist)))
        (depth (if current-prefix-arg (prefix-numeric-value maxdepth)
                 simple-call-tree-current-maxdepth))
        (thisfunc (simple-call-tree-get-function-at-point))
        (narrowedp (simple-call-tree-buffer-narrowed-p)))
    (simple-call-tree-list-callers-and-functions depth funclist)
    (simple-call-tree-jump-to-function thisfunc)
    (if narrowedp (simple-call-tree-toggle-narrowing))
    (setq simple-call-tree-current-maxdepth (max depth 1))))

(defun simple-call-tree-change-maxdepth (maxdepth)
  "Alter the maximum tree depth in the *Simple Call Tree* buffer."
  (interactive "P")
  (let* ((depth (if current-prefix-arg (prefix-numeric-value current-prefix-arg)
                  (floor (abs (read-number "Maximum depth to display: " 2)))))
         (funclist (if simple-call-tree-inverted-bufferp
                       (simple-call-tree-invert simple-call-tree-alist)
                     simple-call-tree-alist))
         (narrowedp (simple-call-tree-buffer-narrowed-p)))
;         (outline-level))
    (simple-call-tree-list-callers-and-functions depth funclist)
    (setq simple-call-tree-current-maxdepth (max depth 1))))

(defun simple-call-tree-view-function (fnstr)
  "Display the source code for function with name FNSTR.
When called interactively FNSTR will be set to the function name under point,
or if called with a prefix arg it will be prompted for."
  (interactive (list (if current-prefix-arg
                         (ido-completing-read "Display function: "
                                              (mapcar 'car simple-call-tree-alist))
                       (simple-call-tree-get-function-at-point))))
  (let* ((funmark (cdr (assoc fnstr simple-call-tree-locations-alist)))
         (buf (marker-buffer funmark))
         (pos (marker-position funmark)))
    (display-buffer buf)
    (with-selected-window (get-buffer-window buf)
      (goto-char pos)
      (recenter 1))))

(defun* simple-call-tree-visit-function (&optional (fnstr (simple-call-tree-get-function-at-point)))
  "Display the source code for function with name FNSTR.
When called interactively FNSTR will be set to the function name under point,
or if called with a prefix arg it will be prompted for."
  (interactive (list (if current-prefix-arg
                         (ido-completing-read "Jump to function: "
                                              (mapcar 'car simple-call-tree-alist))
                       (simple-call-tree-get-function-at-point))))
  (let* ((funmark (cdr (assoc fnstr simple-call-tree-locations-alist)))
         (buf (marker-buffer funmark))
         (pos (marker-position funmark)))
    (pop-to-buffer buf)
    (goto-char pos)
    (recenter 1)))

(defun simple-call-tree-jump-to-function (fnstr)
  "Move cursor to the line corresponding to the function with name FNSTR.
When called interactively FNSTR will be set to the function name under point,
or if called with a prefix arg it will be prompted for."  
  (interactive (list (if current-prefix-arg
                         (ido-completing-read "Jump to function: "
                                              (mapcar 'car simple-call-tree-alist))
                       (simple-call-tree-get-function-at-point))))
  (let* ((narrowedp (simple-call-tree-buffer-narrowed-p)))
    (widen)
    (with-current-buffer "*Simple Call Tree*"
      (goto-char (point-min))
      (re-search-forward (concat "^" (regexp-opt (list (concat "| " fnstr)))))
      (if narrowedp (simple-call-tree-toggle-narrowing)
        (case simple-call-tree-default-recenter
          (top (recenter 0))
          (middle (recenter))
          (bottom (recenter -1))
          (t (recenter arg)))))))

(defun simple-call-tree-move-up nil
  "Move cursor to the parent of this function."
  (interactive)
  (outline-up-heading 1)
  (goto-char (next-single-property-change (point) 'face)))

(defun simple-call-tree-move-next nil
  "Move cursor to the next function."
  (interactive)
  (outline-next-visible-heading 1)
  (goto-char (next-single-property-change (point) 'face)))  

(defun simple-call-tree-move-prev nil
  "Move cursor to the previous function."
  (interactive)
  (outline-previous-visible-heading 1)
  (goto-char (next-single-property-change (point) 'face)))

(defun simple-call-tree-move-next-samelevel nil
  "Move cursor to the next function at the same level as the current one."
  (interactive)
  (outline-forward-same-level 1)
  (goto-char (next-single-property-change (point) 'face)))

(defun simple-call-tree-move-prev-samelevel nil
  "Move cursor to the previous function at the same level as the current one."  
  (interactive)
  (outline-backward-same-level 1)
  (goto-char (next-single-property-change (point) 'face)))

(defun simple-call-tree-buffer-narrowed-p nil
  "Return non-nil if *Simple Call Tree* buffer is narrowed."
  (with-current-buffer "*Simple Call Tree*"
    (or (/= (point-min) 1)
        (/= (point-max) (1+ (buffer-size))))))

;; Change this so that it takes an optional arg to set the state, and toggles by default.
;; Also change name.
(defun simple-call-tree-toggle-narrowing (&optional state)
  "Toggle narrowing of *Simple Call Tree* buffer.
If optional arg STATE is > 0, or if called interactively with a positive prefix arg,
then widen the buffer. If STATE is < 0 or if called interactively with a negative
prefix arg then narrow the buffer.
Otherwise toggle the buffer between narrow and wide state.
When narrowed, the buffer will be narrowed to the subtree at point."
  (interactive "P")
  (with-current-buffer "*Simple Call Tree*"
    (if (or (and state (> (prefix-numeric-value state) 0))
            (and (not state) (simple-call-tree-buffer-narrowed-p)))
        (widen)
      (if (looking-at "^| ")
          (re-search-forward "^| ")
        (re-search-backward "^| "))
      (outline-mark-subtree)
      (narrow-to-region (region-beginning) (region-end))
      (let (select-active-regions) (deactivate-mark)))))


(if (featurep 'fm)
    (add-to-list 'fm-modes '(simple-call-tree-mode simple-call-tree-visit-function)))
                 
(add-hook 'simple-call-tree-mode-hook 'fm-start)

(provide 'simple-call-tree-ext)

;;; simple-call-tree-ext.el ends here
