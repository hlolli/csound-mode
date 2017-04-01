;;; csound-mode.el

;; Copyright (C) 2017  Hlöðver Sigurðsson

;; Author: Hlöðver Sigurðsson <hlolli@gmail.com>

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


(require 'font-lock)
(require 'csound-opcodes)
(require 'csound-eldoc)
(require 'csound-font-lock)
(require 'csound-score)
(require 'csound-skeleton)
(require 'csound-live-interaction)


(defvar csound-mode-hook nil)

(defgroup csound-mode nil
  "Tiny functionality enhancements for evaluating sexps."
  :prefix "csound-mode-"
  :group 'csound-mode)

(defcustom csound-indentation-spaces 2
  "Set how many spaces are in indentation"
  :type 'integer
  :group 'csound-mode)


(defun csound-play ()
  (interactive)
  (compile (format "csound -odac %s" (buffer-file-name))))

(defun csound-indent-begin-of-expr? ()
  (save-excursion
    (beginning-of-line 1)
    (search-forward-regexp "\\b\\(instr\\)\\b\\|\\b\\(opcode\\)\\b" (line-end-position 1) t)))

(defun csound-indent-end-of-expr? ()
  (save-excursion
    (beginning-of-line 1)
    (search-forward-regexp "\\b\\(endin\\)\\b\\|\\b\\(endop\\)\\b" (line-end-position 1) t)))

(defun csound-indent-inside-instr? ()
  (let* ((last-instr (save-excursion (search-backward-regexp "\\(instr\\)\\b" nil t)))
	 (last-endin (save-excursion (search-backward-regexp "\\(endin\\)\\b" nil t))))
    (cond ((eq 'nil last-instr) nil)
	  ((and (numberp last-instr) (eq 'nil last-endin)) t)
	  ((< last-endin last-instr) t)
	  (t nil))))

(defun csound-indent-inside-opcode? ()
  (let* ((last-opcode (save-excursion (search-backward-regexp "\\(opcode\\)\\b" nil t)))
	 (last-endop (save-excursion (search-backward-regexp "\\(endop\\)\\b" nil t))))
    (cond ((eq 'nil last-opcode) nil)
	  ((and (numberp last-opcode) (eq 'nil last-endop)) t)
	  ((< last-endop last-opcode) t)
	  (t nil))))

(defun recursive-count (regex string start)
  (if (string-match regex string start)
      (+ 1 (recursive-count regex string (match-end 0)))
    0))


(defun csound-indent-beginning-of-bool? ()
  (save-excursion
    (beginning-of-line 1)
    (if (and (search-forward-regexp
	      "\\b\\(if\\)\\b\\|\\b\\(while\\)\\b\\|\\b\\(else\\)\\b\\|\\b\\(elseif\\)\\b"
	      (line-end-position 1) t)
	     ;; if in mix with gotos
	     ;; dont have endif therefore
	     ;; dont create logical blocks
	     (prog2
		 (beginning-of-line 1)
		 (not (search-forward-regexp "goto" (line-end-position 1) t))))
	1 0)))

(defun csound-indent-end-of-bool? ()
  (save-excursion
    (beginning-of-line 1)
    (if (search-forward-regexp
	 "\\b\\(endif\\)\\b\\|\\b\\(od\\)\\b\\|\\b\\(else\\)\\b\\|\\b\\(elseif\\)\\b"
	 (line-end-position 1))
	1 0)))

(defun csound-indent-count-goto-if-mix (end-of-expr cnt)
  (if (<= end-of-expr (point))
      cnt
    (prog2
	(beginning-of-line 2)
	(csound-indent-count-goto-if-mix end-of-expr (if (and (search-forward-regexp "\\b\\(if\\)\\b" (line-end-position 1) t 1)
							      (search-forward-regexp "goto" (line-end-position 1) t 1))
							 (1+ cnt) cnt)))))

(defun csound-indent-inside-expression-calc (expr-type)
  (let* ((beginning-of-expr (if (eq 'instr expr-type)
				(save-excursion
				  (search-backward-regexp "\\(instr\\)\\b" nil t))
			      (save-excursion
				(search-backward-regexp "\\(opcode\\)\\b" nil t))))
	 (ending-of-current-line (line-end-position))
	 (expression-to-point (buffer-substring beginning-of-expr (line-end-position 1)))
	 (count-if-statements (recursive-count  "\\b\\(if\\)\\b" expression-to-point 0))
	 (goto-if-mix (save-excursion
			(prog2
			    (goto-char beginning-of-expr)
			    (csound-indent-count-goto-if-mix ending-of-current-line 0))))
	 ;; (count-elseif-statements (recursive-count "\\b\\(elseif\\)\\b" (buffer-substring beginning-of-expr (line-end-position 1)) 0))
	 (count-endif-statements (recursive-count "\\b\\(endif\\)\\b" expression-to-point 0))
	 (count-while-statements (recursive-count "\\b\\(while\\)\\b" expression-to-point 0))
	 (count-od-statements (recursive-count "\\b\\(od\\)\\b" expression-to-point 0))
	 (after-goto-statement (if (string-match-p "\\<\\w*:\\B" expression-to-point) 1 0))
	 (line-at-goto-statement (if (save-excursion
				       (beginning-of-line)
				       (search-forward-regexp "\\<\\w*:" (line-end-position 1) t 1)) 
				     1 0))
	 ;; (end-of-bool? (csound-indent-end-of-bool?))
	 (begin-of-bool? (csound-indent-beginning-of-bool?))
	 (tab-count (max 1 (1+ (- (+ count-if-statements
				     after-goto-statement
				     ;; count-elseif-statements
				     count-while-statements) 
				  count-endif-statements 
				  count-od-statements 
				  begin-of-bool?
				  line-at-goto-statement
				  goto-if-mix
				  ;;end-of-bool?
				  )))))
    ;; (message "gotos: %d, bool-begin: %d, line-at-goto: %d, count-if: %d, mix %d" after-goto-statement begin-of-bool? line-at-goto-statement count-if-statements goto-if-mix)
    ;; (message "tab: %d begin-bool: %d " tab-count begin-of-bool?)
    ;; (when (and (eq 't end-of-bool?) (not (eq 't begin-of-bool?))) (indent-line-to (* csound-indentation-spaces (1- tab-count))))
    (indent-line-to (* csound-indentation-spaces tab-count))))


(defun csound-indent-line ()
  "Indent current line."
  ;;(interactive)
  (cond ;;((csound-indent-xml-line?) (indent-to 0))
   ;; ((csound-indent-within-score?) (csound-score-indentation))
   ((csound-indent-begin-of-expr?) (indent-line-to 0))
   ((csound-indent-end-of-expr?) (indent-line-to 0))
   ((csound-indent-inside-instr?) (csound-indent-inside-expression-calc 'instr))   
   ((csound-indent-inside-opcode?) (csound-indent-inside-expression-calc 'opcode)) 
   (t (indent-line-to 0))))


(defun opcode-completion-at-point ()
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

(defun csound-mode-keybindings ()
  (local-set-key (kbd "C-c d") #'csound-thing-at-point-doc)
  (local-set-key (kbd "C-j") #'newline-and-indent)
  (local-set-key (kbd "C-c C-s") #'csound-score-align-block)
  (local-set-key (kbd "C-M-x") #'csound-evaluate)
  (local-set-key (kbd "C-x C-e") #'csound-evaluate-line))


;;;###autoload
(defun csound-mode ()
  (interactive) 
  (kill-all-local-variables)
  (auto-insert-mode)
  (set-syntax-table csound-mode-syntax-table)
  
  (setq ad-redefinition-action 'accept)
  (setq major-mode 'csound-mode)
  (setq mode-name "Csound") 
  ;; (set (make-local-variable 'eldoc-documentation-function) 'csound-eldoc-function)
  ;; (setq-local font-lock-multiline t)
  (setq-local eldoc-documentation-function 'csound-eldoc-function)
  (setq-local indent-line-function 'csound-indent-line)
  ;; (font-lock-add-keywords 'csound-mode csound-font-lock-list)
  
  (add-hook 'csound-mode-hook #'eldoc-mode)
  (set (make-local-variable 'eldoc-documentation-function) 'csound-eldoc-function)
  (add-hook 'csound-mode-hook #'csound-mode-keybindings) 
  (add-hook 'completion-at-point-functions 'opcode-completion-at-point nil 'local)
  (add-hook 'csound-mode-hook (lambda ()
				(when csound-shared-library-loaded?
				  (csound-mode--message-buffer-create))
  				(font-lock-add-keywords nil csound-font-lock-list)
  				(when csound-rainbow-score-parameters?
  				  (setq-local font-lock-fontify-region-function 'csound-fontify-region)
  				  (setq-local jit-lock-contextually t)
				  )
  				(csound-font-lock-param--flush-buffer)
  				(when csound-rainbow-score-parameters?
  				  (csound-font-lock-param--flush-score)
  				  (csound-font-lock--flush-block-comments))))
  ;; From http://stackoverflow.com/questions/25400328/how-can-i-define-comment-syntax-for-a-major-mode
  (add-hook 'csound-mode-hook (lambda ()
				(set (make-local-variable 'comment-start) ";")
				(set (make-local-variable 'comment-end) "")))
  (run-hooks 'csound-mode-hook))

(eval-after-load 'csound-mode
  '(progn     
     (define-auto-insert "\\.csd\\'" 'csound-new-csd)
     (add-to-list 'auto-mode-alist '("\\.csd\\'\\|\\.orc\\'\\|\\.sco\\'\\|\\.udo\\'" . csound-mode))))

(provide 'csound-mode)

;;; csound-mode.el ends here
