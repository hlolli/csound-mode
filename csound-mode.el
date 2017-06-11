;;; csound-mode.el
;; Copyright (C) 2017  Hlöðver Sigurðsson

;; Author: Hlöðver Sigurðsson <hlolli@gmail.com>
;; Version: 1.0.0-alpha

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary
;;; See README.md (https://github.com/hlolli/csound-mode/blob/master/README.md)

;;; Code:

(when (version< emacs-version "25.1")
  (error "csound-mode requires at least GNU Emacs 25.1"))

(setq csound-shared-library-loaded-p
      (or (ignore-errors (module-load "emacscsnd.so"))
	  (ignore-errors (module-load "./csoundAPI_emacsLisp/emacscsnd.so"))
	  (ignore-errors (module-load "./emacscsnd.so"))))

(require 'font-lock)
(require 'csound-opcodes)
(require 'csound-eldoc)
(require 'csound-font-lock)
(when csound-shared-library-loaded-p
  (require 'csound-repl))
(require 'csound-indentation)
(require 'csound-score)
(require 'csound-skeleton)
(require 'csound-util)
(require 'shut-up)


(defvar csound-mode-hook nil)

(defgroup csound-mode nil
  "Tiny functionality enhancements for evaluating sexps."
  :prefix "csound-mode-"
  :group 'csound-mode)


(defun csound-play ()
  "Play the csound file."
  (interactive)
  (compile (format "csound -odac %s" (buffer-file-name))))

(defun csound-opcode-completion-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            csdoc-opcode-database
            :exclusive 'no
            :company-docsig (lambda (cand)
			      (chomp (replace-regexp-in-string
				      "\n\\|\s+" "\s"
				      (nth 3 (gethash cand csdoc-opcode-database)))))
            :company-doc-buffer (lambda (cand)
				  (prin1-to-string (nth 11 (gethash cand csdoc-opcode-database))))
	    ;;:company-location (lambda (cand) (nth 11 (gethash cand csdoc-opcode-database)))
	    ))))

(defun csound-api-compile ()
  (interactive)
  (case system-type
    ((or gnu/linux darwin cygwin)
     (progn (compile "make -C ./csoundAPI_emacsLisp")
	    (setq csound-compilation-success-p
		  (or (ignore-errors (module-load "emacscsnd.so"))
		      (ignore-errors (module-load "./csoundAPI_emacsLisp/emacscsnd.so"))
		      (ignore-errors (module-load "./emacscsnd.so"))))
	    (if csound-compilation-success-p
		(prog2 (require 'csound-repl)
		    (message (concat "csound-api module was successfully compiled, "
				     "you can now start the csound REPL with keybinding c-c c-z, "
				     "or M-x csound-start-repl")))
	      (error (concat "csound-api module compilation failed, "
			     "please go to "
			     "https://github.com/hlolli/csoundAPI_emacsLisp "
			     "and create a ticket, or if you know what you're doing, "
			     "compile the csoundAPI yourself.")))))
    ;; Else
    (t (error (concat "Your operating system is not supported for compiling "
		      "the csoundAPI, please go to "
		      "https://github.com/hlolli/csoundAPI_emacsLisp "
		      "and create a ticket, or if you know what you're doing, "
		      "compile the csoundAPI yourself.")))))

(defun csound-repl-start ()
  "Start the csound-repl."
  (interactive)
  (if (fboundp 'csound-mode--repl-buffer-create)
      (csound-mode--repl-buffer-create)
    (when (y-or-n-p (concat
		     "csound-api module for emacs not found, "
		     "do you want emacs to compile it for you?"))
      (csound-api-compile)))
  (csound-mode--repl-buffer-create))

(defun csound-mode-keybindings ()
  (local-set-key (kbd "C-c C-z") #'csound-repl-start)
  (local-set-key (kbd "C-c d") #'csound-thing-at-point-doc)
  (local-set-key (kbd "C-c C-s") #'csound-score-align-block)
  (local-set-key (kbd "C-M-x") #'csound-evaluate-region)
  (local-set-key (kbd "C-x C-e") #'csound-evaluate-line))

;;;###autoload
(defun csound-mode ()
  "csound-mode major mode."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table csound-mode-syntax-table)
  
  (setq ad-redefinition-action 'accept)
  (setq major-mode 'csound-mode)
  (setq mode-name "Csound")

  (setq-local eldoc-documentation-function 'csound-eldoc-function)
  (setq-local indent-line-function 'csound-indent-line)
  (setq-local comment-start ";; ")
  (setq-local comment-end "")
  
  (add-hook 'csound-mode-hook #'eldoc-mode)
  (set (make-local-variable 'eldoc-documentation-function) 'csound-eldoc-function)
  (add-hook 'csound-mode-hook #'csound-mode-keybindings)
  (add-hook 'completion-at-point-functions 'csound-opcode-completion-at-point nil 'local)

  (add-hook 'csound-mode-hook (lambda ()
  				(font-lock-add-keywords nil csound-font-lock-list)
  				(when csound-rainbow-score-parameters?
  				  (setq-local font-lock-fontify-region-function 'csound-fontify-region)
  				  (setq-local jit-lock-contextually t))
  				(shut-up
				  (with-silent-modifications
				    (csound-font-lock-param--flush-buffer)))
  				(when csound-rainbow-score-parameters?
  				  (shut-up
				    (with-silent-modifications
				      (csound-font-lock-param--flush-score)
				      (csound-font-lock--flush-block-comments))))))
  (run-hooks 'csound-mode-hook))

(eval-after-load 'csound-mode
  '(progn
     (define-auto-insert "\\.csd\\'" 'csound-new-csd)
     (add-to-list 'auto-mode-alist '("\\.csd\\'\\|\\.orc\\'\\|\\.sco\\'\\|\\.udo\\'" . csound-mode))))

(provide 'csound-mode)

;;; csound-mode.el ends here
