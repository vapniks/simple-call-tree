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
;; Stuff
;;

;;; Require
(require 'simple-call-tree)
(require 'outline-magic)
;;; Code:

(defgroup simple-call-tree nil
  "Simple call tree - display a simple call tree for functions in a buffer."
  :group 'tools
  :link '(url-link "http://www.emacswiki.org/SimpleCallTree"))

(defcustom simple-call-tree-default-recenter 'middle
  "How to recenter the window after moving to another function in the \"*Simple Call Tree*\" buffer.
Can be one of the following symbols: 'top 'middle 'bottom.
This variable is used by the `simple-call-tree-jump-to-function-at-point' function when no prefix arg is given."
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
  (define-key simple-call-tree-mode-map (kbd "<tab>") 'outline-cycle)
  (define-key simple-call-tree-mode-map (kbd "<return>") 'simple-call-tree-display-function-at-point)
  (define-key simple-call-tree-mode-map (kbd "j") 'simple-call-tree-jump-to-function-at-point)
  (define-key simple-call-tree-mode-map (kbd "n") 'simple-call-tree-move-next)
  (define-key simple-call-tree-mode-map (kbd "p") 'simple-call-tree-move-prev)
  (define-key simple-call-tree-mode-map (kbd "i") 'simple-call-tree-invert-buffer)
  (define-key simple-call-tree-mode-map (kbd "d") 'simple-call-tree-change-maxdepth)
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
  (outline-minor-mode 1))

(defvar simple-call-tree-alist nil
  "Alist of functions and the functions they call.")

(defvar simple-call-tree-locations nil
  "Alist of functions and their locations within their respective buffers.
The car of each element is a function name, and the cdr is a cons cell in the form (BUF . LINENUM)
where BUF is a buffer name and LINENUM is the line number of the function.")

(defvar simple-call-tree-inverted-bufferp nil
  "Indicates if the *Simple Call Tree* buffer is currently inverted or not.
If non-nil then children correspond to callers of parents in the outline tree.
Otherwise it's the other way around.")

(defvar simple-call-tree-current-maxdepth nil
  "The current maximum depth of the tree in the *Simple Call Tree* buffer.")

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
  (setq simple-call-tree-alist nil)
  ;; First add all the functions defined in the buffers to simple-call-tree-alist.
  (let ((pos (point-min))
        oldpos count nextfunc max item olditem)
    (dolist (buf buffers)
      (with-current-buffer buf
        (font-lock-default-fontify-buffer)
        (setq pos (point-min)
              count 0)
        (while (setq nextfunc (simple-call-tree-next-func 'pos test))
          (setq count (1+ count))
          (message "Identifying functions...%d" count)
          (setq simple-call-tree-alist (cons (list nextfunc) simple-call-tree-alist)))))
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
  "Display call tree for current buffer."
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

;; (defun* simple-call-tree-current-function (&optional (func (which-function)))
;;   "Display call tree for function FUNC in current buffer"
;;   (interactive)
;;   (let ((func2 (if current-prefix-arg
;;                    (completing-read "Function: " (remove-if-not 'functionp obarray))
;;                  func))
;;         (file (symbol-file func 'defun)))
;;     (if file
;;   )

(defun* simple-call-tree-list-callers-and-functions (&optional (maxdepth 2)
                                                               (funclist simple-call-tree-alist))
  "List callers and functions in FUNCLIST to depth MAXDEPTH.
By default FUNCLIST is set to `simple-call-tree-alist'."
  (switch-to-buffer (get-buffer-create "*Simple Call Tree*"))
  (if (not (equal major-mode 'simple-call-tree-mode)) (simple-call-tree-mode))
  (setq buffer-read-only nil)
  (erase-buffer)
  (dolist (item funclist)
    (simple-call-tree-list-callees-recursively (car item) maxdepth 1 funclist))
  (setq simple-call-tree-current-maxdepth maxdepth
        simple-call-tree-inverted-bufferp (not (equal funclist simple-call-tree-alist))
        buffer-read-only t))

(defun* simple-call-tree-list-callees-recursively (fname &optional (maxdepth 3)
                                                         (curdepth 1)
                                                         (funclist simple-call-tree-alist))
  "Insert a call tree for the function named FNAME, to depth MAXDEPTH.
FNAME must be the car of one of the elements of FUNCLIST which is set to `simple-call-tree-alist' by default.
The optional arguments MAXDEPTH and CURDEPTH specify the maximum and current depth of the tree respectively.
This is a recursive function, and you should not need to set CURDEPTH."
  (let* ((callees (cdr (assoc fname funclist)))
         (stars (make-string curdepth 42))
         (face (intern-soft (format "outline-%d" (1+ (mod (1- curdepth) 8))))))
    (insert stars " " (propertize fname
                                  'font-lock-face (list :inherit face :underline t)
                                  'mouse-face 'highlight) "\n")
    (if (< curdepth maxdepth)
        (dolist (callee callees)
          (simple-call-tree-list-callees-recursively callee maxdepth (1+ curdepth) funclist)))))

;;; Major-mode commands bound to keys

(defun simple-call-tree-invert-buffer (&optional maxdepth)
  "Invert the tree in *Simple Call Tree* buffer."
  (interactive "P")
  (let ((funclist (if simple-call-tree-inverted-bufferp
                      simple-call-tree-alist
                    (simple-call-tree-invert simple-call-tree-alist)))
        (depth (if current-prefix-arg (prefix-numeric-value maxdepth)
                 simple-call-tree-current-maxdepth)))
    (simple-call-tree-list-callers-and-functions depth funclist)
    (setq simple-call-tree-current-maxdepth depth)))

(defun simple-call-tree-change-maxdepth (maxdepth)
  "Alter the maximum tree depth in the *Simple Call Tree* buffer."
  (interactive "P")
  (let ((depth (if current-prefix-arg (prefix-numeric-value current-prefix-arg)
                 (floor (abs (read-number "Maximum depth to display: " 2)))))
        (funclist (if simple-call-tree-inverted-bufferp
                      (simple-call-tree-invert simple-call-tree-alist)
                    simple-call-tree-alist)))
    (simple-call-tree-list-callers-and-functions depth funclist)
    (setq simple-call-tree-current-maxdepth depth)))

(defun simple-call-tree-display-function-at-point nil
  "Show the function at point."
  (interactive)
  (let* ((symb (symbol-nearest-point))
         (fn (or (and (fboundp symb) symb) (function-called-at-point)))
         (find-function-recenter-line 1))
    (delete-other-windows)
    (find-function-do-it fn nil 'display-buffer)
    (set-mark-command 1)))

(defun simple-call-tree-jump-to-function-at-point (arg)
  "Move cursor to the line corresponding to the function at point"
  (interactive "P")
  (let* ((symb (symbol-nearest-point))
         (fn (or (and (fboundp symb) symb) (function-called-at-point)))
         (fnstr (symbol-name fn)))
    (with-current-buffer "*Simple Call Tree*"
      (goto-char (point-min))
      (re-search-forward (concat "^" (regexp-opt (list (concat "* " fnstr)))))
      (case (or arg simple-call-tree-default-recenter)
        (top (recenter 0))
        (middle (recenter))
        (bottom (recenter -1))
        (t (recenter arg))))))

(defun simple-call-tree-move-next nil
  "Move cursor to the next function."
  (interactive)
  (re-search-forward "^\\*+ " nil t))

(defun simple-call-tree-move-prev nil
  "Move cursor to the next function."
  (interactive)
  (re-search-backward "^\\*+" nil t)
  (previous-line 1)
  (re-search-forward "\\*+ "))


(provide 'simple-call-tree-ext)

;;; simple-call-tree-ext.el ends here
