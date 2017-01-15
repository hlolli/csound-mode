(require 'font-lock)
(require 'dash)
(require 'csound-opcodes)
(require 'csound-eldoc)
(require 'csound-font-lock)
(require 'csound-skeleton)


(defvar csound-mode-hook nil)

(defgroup csound-mode nil
  "Tiny functionality enhancements for evaluating sexps."
  :prefix "csound-mode-"
  :group 'csound-mode)

(defvar csound-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent) map)
  "Keymap for csound-mode")

(defcustom csound-indentation-spaces 2
  "Set how many spaces are in indentation"
  :type 'integer
  :group 'csound-mode)

;; (defun csound-indent-xml-line? ()
;;   (save-excursion
;;     (beginning-of-line 0)
;;     (search-forward-regexp "\\<.+\\>" (line-end-position 2) t)))

(defun csound-indent-begin-of-expr? ()
  (save-excursion
    (beginning-of-line 1)
    (search-forward-regexp "\\b\\(instr\\)\\b\\|\\b\\(opcode\\)\\b" (line-end-position 1) t)))

(defun csound-indent-end-of-expr? ()
  (save-excursion
    (beginning-of-line 1)
    (search-forward-regexp "\\b\\(endin\\)\\b\\|\\b\\(endop\\)\\b" (line-end-position 1) t)))

(defun csound-indent-inside-instr? ()
  ;; (interactive)
  (let* ((last-instr (save-excursion (search-backward-regexp "\\(instr\\)\\b" nil t)))
	 (last-endin (save-excursion (search-backward-regexp "\\(endin\\)\\b" nil t))))
    (cond ((eq 'nil last-instr) nil)
	  ((and (numberp last-instr) (eq 'nil last-endin)) t)
	  ((< last-endin last-instr) t)
	  (t nil))))

(defun csound-indent-inside-opcode? ()
  ;; (interactive)
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

(defun csound-indent-end-of-bool? ()
  (save-excursion
    (beginning-of-line 1)
    (if (search-forward-regexp
	 "\\b\\(endif\\)\\b\\|\\b\\(od\\)\\b\\|\\b\\(else\\)\\b\\|\\b\\(elseif\\)\\b"
	 (line-end-position 2) t)
	t nil)))

(defun csound-indent-inside-expression-calc (expr-type)
  (let* ((beginning-of-expr (if (eq 'instr expr-type)
				(save-excursion
				  (search-backward-regexp "\\(instr\\)\\b" nil t))
			      (save-excursion
				(search-backward-regexp "\\(opcode\\)\\b" nil t))))
	 (count-if-statements (recursive-count "\\b\\(if\\)\\b" (buffer-substring beginning-of-expr (line-end-position 1)) 0))
	 (count-elseif-statements (recursive-count "\\b\\(elseif\\)\\b" (buffer-substring beginning-of-expr (line-end-position 1)) 0))
	 (count-endif-statements (recursive-count "\\b\\(endif\\)\\b" (buffer-substring beginning-of-expr (line-end-position 1)) 0))
	 (count-while-statements (recursive-count "\\b\\(while\\)\\b" (buffer-substring beginning-of-expr (line-end-position 1)) 0))
	 (count-od-statements (recursive-count "\\b\\(od\\)\\b" (buffer-substring beginning-of-expr (line-end-position 1)) 0))
	 (end-of-bool? (csound-indent-end-of-bool?))
	 (tab-count (max 1 (1+ (- (+ count-if-statements
				     count-elseif-statements
				     count-while-statements) 
				  count-endif-statements 
				  count-od-statements))))) 
    ;; (message "%d" tab-count)
    (when (eq 't end-of-bool?)
      ;; (message "end of bool!")
      (indent-line-to (* csound-indentation-spaces (1- tab-count))))
    (save-excursion 
      (indent-line-to (* csound-indentation-spaces tab-count))
      ;; (beginning-of-line 2)
      (indent-to (* csound-indentation-spaces tab-count)))))

(defun csound-indent-line ()
  "Indent current line."
  ;;(interactive)
  (cond ;;((csound-indent-xml-line?) (indent-to 0))
   ((csound-indent-begin-of-expr?) (save-excursion
				     (indent-line-to 0)
				     (beginning-of-line 2)
				     (indent-line-to csound-indentation-spaces)))
   ((csound-indent-end-of-expr?) (indent-line-to 0))
   ((csound-indent-inside-instr?) (csound-indent-inside-expression-calc 'instr))
   ((csound-indent-inside-opcode?) (csound-indent-inside-expression-calc 'opcode)) 
   
   (t (indent-to 0))))


(defun opcode-completion-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            csdoc-opcode-database
            :exclusive 'no
            :company-docsig (lambda (cand)
			      (replace-regexp-in-string
			       "\n\s" "\n"
			       (nth 11 (gethash cand csdoc-opcode-database))))
            :company-doc-buffer (lambda (cand)
				  (nth 11 (gethash cand csdoc-opcode-database)))
	    :company-location (lambda (cand)
				(nth 11 (gethash cand csdoc-opcode-database)))))))


(defun csound-mode-keybindings ()
  (local-set-key (kbd "C-c d") #'csound-thing-at-point-doc))

;; (gethash "delay" csdoc-opcode-database)
;; "\\(,+\s*\\)+\\|\\(\s+,*\\)+"
;; (length (split-string (nth 11 (gethash "linseg" csdoc-opcode-database)) "\n"))
;; (replace-regexp-in-string "\n\s" "\n" (nth 9 (gethash "oscil" csdoc-opcode-database)))
;; (maphash (lambda (key val) ) csdoc-opcode-database)

;;;###autoload
(defun csound-mode ()
  (interactive)
  (kill-all-local-variables)
  (auto-insert-mode)
  (set (make-local-variable 'font-lock-defaults) '(csound-font-lock-keywords))
  (setq ad-redefinition-action 'accept)
  (setq major-mode 'csound-mode)
  (setq mode-name "Csound") 
  ;; (set (make-local-variable 'eldoc-documentation-function) 'csound-eldoc-function)
  (setq-local eldoc-documentation-function 'csound-eldoc-function)
  (setq-local indent-line-function 'csound-indent-line)
  (add-hook 'csound-mode-hook #'eldoc-mode)
  (add-hook 'csound-mode-hook #'csound-mode-keybindings)
  (add-hook 'completion-at-point-functions 'opcode-completion-at-point nil 'local)
  (add-hook 'csound-mode-hook (lambda ()
				(set (make-local-variable 'comment-start) ";;")
				(set (make-local-variable 'comment-end) ""))) 
  (run-hooks 'csound-mode-hook))

(eval-after-load 'csound-mode 
  '(progn
     (define-auto-insert "\\.csd\\'\\|\\.orc\\'\\|\\.sco\\'" 'csound-new-csd)
     (add-to-list 'auto-mode-alist '("\\.csd\\'\\|\\.orc\\'\\|\\.sco\\'" . csound-mode))))

(provide 'csound-mode)
