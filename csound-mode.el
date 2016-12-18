(require 'font-lock)
(require 'dash)
(require 'csound-opcodes)
(require 'csound-eldoc)
(require 'csound-font-lock)


(defvar csound-mode-hook nil)

(defgroup csound-mode nil
  "Tiny functionality enhancements for evaluating sexps."
  :prefix "csound-mode-"
  :group 'csound-mode)


(defvar csound-mode-map
  (-let [map (make-keymap)]
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for csound-mode")

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




;; (gethash "delay" csdoc-opcode-database)
;; "\\(,+\s*\\)+\\|\\(\s+,*\\)+"
;; (length (split-string (nth 11 (gethash "linseg" csdoc-opcode-database)) "\n"))
;; (replace-regexp-in-string "\n\s" "\n" (nth 9 (gethash "oscil" csdoc-opcode-database)))
;; (maphash (lambda (key val) ) csdoc-opcode-database)

;;;###autoload
(defun csound-mode ()
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults) '(csound-font-lock-keywords))
  (setq major-mode 'csound-mode)
  (setq mode-name "Csound") 
  ;; (set (make-local-variable 'eldoc-documentation-function) 'csound-eldoc-function)
  (setq-local eldoc-documentation-function 'csound-eldoc-function)
  (add-hook 'csound-mode-hook #'eldoc-mode)
  (add-hook 'completion-at-point-functions 'opcode-completion-at-point nil 'local)
  (add-hook 'csound-mode-hook (lambda ()
				(set (make-local-variable 'comment-start) ";;")
				(set (make-local-variable 'comment-end) ""))) 
  (run-hooks 'csound-mode-hook))

(eval-after-load 'csound-mode 
  '(progn 
     (add-to-list 'auto-mode-alist '("\\.csd\\'\\|\\.orc\\'\\|\\.sco\\'" . csound-mode))))

(provide 'csound-mode)
