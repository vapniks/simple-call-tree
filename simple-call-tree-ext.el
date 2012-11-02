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

(define-derived-mode simple-call-tree-mode outline-mode "Simple Call Tree"
  "The major-mode for the one-key menu buffer."
  :group 'simple-call-tree
  (setq simple-call-tree-mode-map (make-keymap)
        buffer-read-only nil)
  ;; Set keymap
  (define-key simple-call-tree-mode-map (kbd "<tab>") 'outline-cycle)
  (define-key simple-call-tree-mode-map (kbd "<return>") 'simple-call-tree-find-function-at-point)
  (define-key simple-call-tree-mode-map (kbd "j") 'simple-call-tree-jump-to-function-at-point)
  (define-key simple-call-tree-mode-map (kbd "n") 'simple-call-tree-move-next)
  (define-key simple-call-tree-mode-map (kbd "p") 'simple-call-tree-move-prev)
  (use-local-map simple-call-tree-mode-map)
  (outline-minor-mode 1))

(defun simple-call-tree-display-buffer nil
  "Display call tree for current buffer."
  (interactive)
  (simple-call-tree-analyze)
  (simple-call-tree-list-callers-and-functions))

(defun simple-call-tree-list-callers-and-functions nil
  "List callers and functions in `simple-call-tree-alist'."
  (interactive)
  (let ((list simple-call-tree-alist))
    (switch-to-buffer (get-buffer-create "*Simple Call Tree*"))
    (if (not (equal major-mode 'simple-call-tree-mode)) (simple-call-tree-mode))
    (erase-buffer)
    (dolist (entry list)
      (let ((callees (mapcar (lambda (func)
                               (propertize func
                                           'font-lock-face (list :inherit 'outline-2
                                                                 :underline t)
                                           'mouse-face 'highlight))
                             (cdr entry))))
        (insert "* " (propertize (car entry)
                                 'font-lock-face (list :inherit 'outline-1
                                                       :underline t)
                                 'mouse-face 'highlight) "\n")
        (unless (not callees)
          (dolist (callee callees)
            (insert "** " callee "\n"))))))
  (setq buffer-read-only t))

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
