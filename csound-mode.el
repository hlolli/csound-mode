;;; csound-mode.el --- A major mode for interacting and coding Csound
;;  Copyright (C) 2017  Hlöðver Sigurðsson

;; Author: Hlöðver Sigurðsson <hlolli@gmail.com>
;; Version: 0.1.2
;; Package-Requires: ((emacs "25") (shut-up "0.3.2") (multi "2.0.1"))

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

;;; Commentary:

;;  This file stores mode-specific bindings to `csound-mode`,
;;  "offline" csound-interactions and major-mode definition,
;;  syntax table.

;;; Code:


(when (version< emacs-version "25.1")
  (error "csound-mode requires at least GNU Emacs 25.1"))

(defvar csound-shared-library-loaded-p nil)

(defvar csound-mode--dir-path
  (first (reduce (lambda (i p)
		   (if (string-match "csound\\-mode" p)
		       (cons p i)
		     i))
		 load-path)))

(defun csound-mode--load-module ()
  (setq csound-shared-library-loaded-p
	(or (ignore-errors (module-load "emacscsnd.so"))
	    (ignore-errors (module-load (concat csound-mode--dir-path "emacscsnd.so")))
	    (ignore-errors (module-load (concat csound-mode--dir-path "csoundAPI_emacsLisp/emacscsnd.so"))))))

(csound-mode--load-module)

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


(defgroup csound-mode nil
  "Tiny functionality enhancements for evaluating sexps."
  :prefix "csound-mode-"
  :group 'csound-mode)

(defvar csound-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    ;; (modify-syntax-entry ?+ "w" st)
    ;; (modify-syntax-entry ?- "w" st)
    (modify-syntax-entry ?. "w" st)
    (modify-syntax-entry ?! "w" st)
    (modify-syntax-entry ?% "-" st)    
    (modify-syntax-entry ?\" "\"\"" st)
    ;; (modify-syntax-entry ?| "\"" st) 
    (modify-syntax-entry ?\\ "\\" st)
    ;; Comment syntax
    ;; good read: https://www.lunaryorn.com/posts/syntactic-fontification-in-emacs.html
    (modify-syntax-entry ?\/ ". 124b" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\; "<" st
    			 )
    (modify-syntax-entry ?\n ">" st
    			 )
    st)
  "Syntax table for csound-mode")

;; https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close
(defun csound-bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without error"
  (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward-regexp "error[^s]" nil t))))
    (bury-buffer buffer)))

(defun csound-play ()
  "Play the csound file in current buffer."
  (interactive)
  (compile (format "csound -odac %s" (buffer-file-name))))

(defun csound-render (bit filename)
  "Render csound to file."
  (interactive
   (list
    (read-string "File bit-value(16/24/32), defaults to 16: ")
    (read-string (format "Filename, defaults to %s.wav: " (file-name-base)))))
  ;;(compile (format "csound -o %s" (buffer-file-name)))
  ;; (message "var1: %s var2: %s" var1 var2)
  (let ((filename (if (string= "" filename)
		      (concat (file-name-base) ".wav")
		    filename)))
    (compile (format "csound %s -o %s --format=%s %s"
		     (buffer-file-name)
		     filename 
		     (-> (split-string filename "\\.")
			 rest first)
		     (case bit
		       ("32" "-f")
		       ("24" "-3")
		       (t "-s"))))))

(defun csound-opcode-completion-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            csdoc-opcode-database
            :exclusive 'no
            :company-docsig (lambda (cand)
			      (csound-util-chomp (replace-regexp-in-string
						  "\n\\|\s+" "\s"
						  (nth 3 (gethash cand csdoc-opcode-database)))))
            :company-doc-buffer (lambda (cand)
				  (prin1-to-string (nth 11 (gethash cand csdoc-opcode-database))))
	    ;;:company-location (lambda (cand) (nth 11 (gethash cand csdoc-opcode-database)))
	    ))))


(defun csound-api-compile ()
  (case system-type
    ((or gnu/linux darwin cygwin)
     (progn (add-hook 'compilation-finish-functions (lambda (_ _)
						      (csound-mode--load-module)
						      (sleep-for 1)
						      (if csound-shared-library-loaded-p
							  (prog2 (require 'csound-repl)
							      (message (concat "csound-api module was successfully compiled, "
									       "you can now start the csound REPL with keybinding c-c c-z, "
									       "or M-x csound-repl-start")))
							(error (concat "csound-api module compilation failed, "
								       "please go to "
								       "https://github.com/hlolli/csoundAPI_emacsLisp "
								       "and create a ticket, or if you know what you're doing, "
								       "compile the csoundAPI yourself."))) 
						      (remove-hook 'compilation-finish-functions
								   (first compilation-finish-functions))))
	    (compile (format "make -C %s" (if (member "csoundAPI_emacsLisp" (directory-files csound-mode--dir-path nil))
					      (concat csound-mode--dir-path "csoundAPI_emacsLisp/")
					    csound-mode--dir-path)))))
    ;; Else
    (t (error (concat "Your operating system is not supported for compiling "
		      "the csoundAPI, please go to "
		      "https://github.com/hlolli/csoundAPI_emacsLisp "
		      "and create a ticket, or if you know what you're doing, "
		      "compile the csoundAPI yourself.")))))

(defun csound-repl-start ()
  "Start the csound-repl."
  (interactive)
  (if (fboundp 'csound-repl--buffer-create)
      (ignore-errors
	(csound-repl--buffer-create))
    (when (y-or-n-p (concat
		     "csound-api module for emacs not found, "
		     "do you want emacs to compile it for you?"))
      (csound-api-compile))))

(defvar csound-mode-map nil)

(setq csound-mode-map
      (let ((map (make-sparse-keymap)))
	;; Offline keybindings
	(define-key map (kbd "C-c C-p") 'csound-play)
	(define-key map (kbd "C-c C-r") 'csound-render) 
	;; REPL Keybindings
	(define-key map (kbd "C-c C-z") 'csound-repl-start)
	(define-key map (kbd "C-M-x")   'csound-repl-evaluate-region)
	(define-key map (kbd "C-x C-e") 'csound-repl-evaluate-line)
	;; Utilities
	(define-key map (kbd "C-c C-s") 'csound-score-align-block)
	(define-key map (kbd "M-.")     'csound-score-find-instr-def)
	(define-key map (kbd "C-c C-f") 'csound-repl-plot-ftgen)
	map))

;;;###autoload
(define-derived-mode csound-mode
  prog-mode "Csound Mode"
  "A major mode for interacting and coding Csound"
  :syntax-table csound-mode-syntax-table
  (setq-local eldoc-documentation-function 'csound-eldoc-function)
  (setq-local comment-start ";; ")
  ;; (setq-local comment-end "")
  (setq-local indent-line-function 'csound-indentation-line)
  
  (setq-local compilation-scroll-output t)
  (setq-local ad-redefinition-action 'accept)
  
  (add-hook 'completion-at-point-functions #'csound-opcode-completion-at-point nil t)
  (add-hook 'compilation-finish-functions #'csound-bury-compile-buffer-if-successful nil t)
  ;; (add-hook 'skeleton-end-hook #'csound-font-lock-flush-buffer nil t) 
  (font-lock-add-keywords nil csound-font-lock-list)
  (setq-local font-lock-fontify-region-function 'csound-font-lock-fontify-region)
  (setq-local jit-lock-mode t)
  (setq-local jit-lock-contextually t)
  (shut-up
    (with-silent-modifications
      (csound-font-lock-flush-buffer))))


(add-to-list 'auto-mode-alist `(,(concat "\\." (regexp-opt '("csd" "orc" "sco" "udo")) "\\'") . csound-mode))

(provide 'csound-mode)

;;; csound-mode.el ends here
