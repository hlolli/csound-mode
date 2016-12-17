(require 'font-lock)
(require 'dash)
(require 'csound-opcodes)


(defvar csound-mode-hook nil)

(defvar csound-mode-map
  (-let [map (make-keymap)]
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for csound-mode")

(defun hash-table-keys (hash-table)
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys))

(defconst csound-font-lock-keywords
  (font-lock-add-keywords
   'csound-mode
   (hash-table-keys csdoc-opcode-database)))

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


(defun csound-eldoc-function ()
  "Returns a doc string appropriate for the current context, or nil."
  (ignore-errors
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (let* ((beg
              (save-excursion
                (+ 1 (or
                      (re-search-backward "\\(;\\|{\\)" nil t)
                      (- (point-min) 1)))))
             (end
              (save-excursion
                (goto-char beg)
                (forward-symbol 1)
                (point)))
             (property (buffer-substring-no-properties beg end))
	     (look (gethash property csdoc-opcode-database)))
	(message property)
	(when look
	  (let* ((templ (nth 9 look))
		 (fix-space (replace-regexp-in-string "\n\s" "\n" templ)))
	    (split-string fix-space "\n")))))))

;; (gethash "linseg" csdoc-opcode-database)
;; "\\(,+\s*\\)+\\|\\(\s+,*\\)+"
;; (length (split-string (nth 11 (gethash "linseg" csdoc-opcode-database)) "\n"))
;; (replace-regexp-in-string "\n\s" "\n" (nth 9 (gethash "oscil" csdoc-opcode-database)))
;; (maphash (lambda (key val) ) csdoc-opcode-database)


(defun csound-mode ()
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults)
       '(csound-font-lock-keywords))
  (setq major-mode 'csound-mode)
  (setq mode-name "Csound")
  (set (make-local-variable 'eldoc-documentation-function)
       'csound-eldoc-function)
  (add-hook 'completion-at-point-functions 'opcode-completion-at-point nil 'local) 
  (run-hooks 'csound-mode-hook))

(provide 'csound-mode)


