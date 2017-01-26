(defvar csound-font-lock-list '())

(defface csound-p-face
  '((((class color)) (:foreground "#F9E79F" :bold t))
    (t :inverse-video t))
  "Face for csound p3, p4 ..."
  :group 'csound-mode)

(defface csound-i-score-face
  '((((class color)) (:inherit font-lock-builtin-face :bold t))
    (t :inverse-video t))
  "Face for csound p3, p4 ..."
  :group 'csound-mode)


(defface csound-f-rate-global-face
  '((((class color)) (:inherit font-lock-negation-char-face :bold t))
    (t :inverse-video t))
  "Face for csound p3, p4 ..."
  :group 'csound-mode)

(defface csound-a-rate-global-face
  '((((class color)) (:inherit font-lock-constant-face :bold t))
    (t :inverse-video t))
  "Face for csound p3, p4 ..."
  :group 'csound-mode)

(defface csound-k-rate-global-face
  '((((class color)) (:inherit font-lock-function-name-face :bold t))
    (t :inverse-video t))
  "Face for csound p3, p4 ..."
  :group 'csound-mode)

(defface csound-i-rate-global-face
  '((((class color)) (:inherit font-lock-variable-name-face :bold t))
    (t :inverse-video t))
  "Face for csound p3, p4 ..."
  :group 'csound-mode)

;; (prog2    (push '("^\\(/\\*\\)?\\(\\s \\|\\*\\)*\\(@[a-z]+\\)" . font-lock-comment-face)  csound-font-lock-list))

;; (defconst custom-font-lock-keywords
;;   `((,(lambda (limit)
;;         (c-font-lock-doc-comments "///" limit gtkdoc-font-lock-doc-comments)))))

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



;; (concat "[^a-z]" k "[^a-z]")

;; (sort keys (lambda (a b) (> (length a) (length b))))

;; (push
;;  ;;`(,(concat "[^a-z]" k "[^a-z]") . 'font-lock-function-name-face)
;;  csound-font-lock-list )
;; (stringp  "")

;; (substring (car (first csound-faces-opcodes)) 0 5)

;; font-lock-warning-face
;; String faces always automatic??
;; (defconst csound-faces-strings
;;   (push '("\\\".*\n? \\\"" . font-lock-string-face) csound-font-lock-list))



(defconst csound-faces-p-parameter-variable
  (push '("p[[:digit:]]+" . 'csound-p-face)
	csound-font-lock-list))

(defconst csound-faces-end-statement
  (push '("\\<[e]\\>" . font-lock-warning-face) csound-font-lock-list))

(defconst csound-faces-f-rate-global-variable
  (push '("\\<gf+\\w*" . 'csound-f-rate-global-face) csound-font-lock-list))

(defconst csound-faces-f-rate-variable
  (push '("\\<f+\\w*" . font-lock-negation-char-face) csound-font-lock-list))

(defconst csound-faces-i-rate-global-variable
  (push '("\\<gi+\\w*" . 'csound-i-rate-global-face) csound-font-lock-list))

(defconst csound-faces-i-rate-variable
  (push '("\\<i+\\w*" . font-lock-variable-name-face) csound-font-lock-list))

(defconst csound-i-score-statement
  (push '("\\<i\\>" . 'csound-i-score-face) csound-font-lock-list))

(defconst csound-faces-k-rate-global-variable
  (push '("\\<gk+\\w*" . 'csound-k-rate-global-face) csound-font-lock-list))

(defconst csound-faces-k-rate-variable
  (push '("\\<k+\\w*" . font-lock-function-name-face) csound-font-lock-list))

(defconst csound-faces-a-rate-global-variable
  (push '("\\<ga+\\w*" . 'csound-a-rate-global-face) csound-font-lock-list))

(defconst csound-faces-a-rate-variable
  (push '("\\<a+\\w*" . font-lock-constant-face) csound-font-lock-list))

(setq-local missing-faces
	    (apply 'concat (mapcar (lambda (s) (concat "\\|\\<" s "\\>"))
				   '("then" "do" "od" "else" "elseif" "endif"))))

(defconst csound-faces-opcodes
  (prog2
      (setq mutz "")
      (let* ((or-regex-opcodes (maphash (lambda (k v)
					  (when (stringp k)
					    (setq mutz (concat mutz  "\\|\\<" k "\\>"))))
					csdoc-opcode-database))
	     (mutz (concat "" (substring mutz 3 (length mutz)) missing-faces)))
	(push `(,mutz . font-lock-builtin-face) csound-font-lock-list))))

(defconst csound-faces-macros
  (push '("#[[:alpha:]]*\\|\\$[[:alpha:]]*" . font-lock-preprocessor-face)
	csound-font-lock-list))

(defconst csound-faces-s-rate-variables
  (push '("\\<[S]+\\w*" . font-lock-variable-name-face) csound-font-lock-list))

(defconst csound-faces-xml-tags
  (push '("</?CsoundSynthesizer>\\|</?CsOptions>\\|</?CsInstruments>\\|</?CsScore>" . font-lock-keyword-face) csound-font-lock-list))

(defconst csound-faces-comments
  (push '(";+.*" . font-lock-comment-face)  csound-font-lock-list))

(defconst csound-font-lock-keywords
  (font-lock-add-keywords
   'csound-mode
   csound-font-lock-list))

(provide 'csound-font-lock)

