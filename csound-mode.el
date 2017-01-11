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

(defun csound-indent-xml-line? ()
  (save-excursion
    (beginning-of-line 0)
    (search-forward-regexp "\\<.+\\>" (line-end-position 2) t)))

(defun csound-indent-definstr-line? ()
  (save-excursion
    (beginning-of-line 0)
    (search-forward-regexp "\\(instr\s\\)+" (line-end-position 2) t)))

(defun csound-indent-inside-instr? ()
  (interactive)
  (let* ((last-instr (save-excursion (search-backward "\\(instr\s\\)+" (beginning-of-buffer) t 1)))
	 (lest-endin (save-excursion (search-backward "endin" (beginning-of-buffer) t 1))))
    (if (not (numberp last-instr))
	;; nil
	(message "nei1")
      (if (not (numberp last-endin))
	  ;; t
	  (message "já1")
	(let ((delta (- last-instr last-endin)))
	  (if (< 0 delta)
	      (message "já2") (message "nei2")
	      ;; t nil
	      ))))))

(defun henda ()
  (interactive)
  (beginning-of-line 0)
  ;;(search-backward "CsScore" (line-beginning-position))
  )


(defun csound-indent-line () (csound-indent-definstr-line?))


(defun csound-indent-line ()
  "Indent current line."
  (interactive)
  (cond ((csound-indent-xml-line?) (indent-to 0)
	 (csound-indent-definstr-line?) (indent-to 2))))


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
