(defvar csound-font-lock-list '())

(defconst csound-faces-comments
  (push '(";+.*" . font-lock-comment-face)  csound-font-lock-list))

;; (prog2    (push '("^\\(/\\*\\)?\\(\\s \\|\\*\\)*\\(@[a-z]+\\)" . font-lock-comment-face)  csound-font-lock-list))

(defconst custom-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "///" limit gtkdoc-font-lock-doc-comments)))))

;; (setq-default c-doc-comment-style (quote (gtkdoc javadoc autodoc custom)))

;; (defconst csound-faces-operators
;;   (let ((keys ()))
;;     (maphash (lambda (k v)
;; 	       (when (not (eq :opcode (nth 0 v)))
;; 		 (push
;; 		  `(,(concat "[^a-z]" (if (or (string= "+" k)
;; 					      (string= "*" k)
;; 					      (string= "%" k)
;; 					      (string= "/" k))
;; 					  (concat "\\" k) k)
;; 			     "[^a-z]") . font-lock-builtin-face)
;; 		  csound-font-lock-list)))
;; 	     csdoc-opcode-database)
;;     (sort keys (lambda (a b) (> (length a) (length b))))))

(defconst csound-faces-opcodes
  (maphash (lambda (k v)
	     (when (eq :opcode (nth 0 v))
	       (push
		`(,(concat "[\\s\\n\\t,\\(]" k "[\\s\\n\\t,\\(]")
		  ;;(concat "[^a-z]" k "[^a-z]")
		  . 'font-lock-function-name-face)
		csound-font-lock-list)))
	   csdoc-opcode-database)
  (sort keys (lambda (a b) (> (length a) (length b)))))


(defconst csound-font-lock-keywords
  (font-lock-add-keywords
   'csound-mode
   csound-font-lock-list))

(provide 'csound-font-lock)
