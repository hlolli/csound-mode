(require 'csound-opcodes)

(defun csound-eldoc-line-escape-count ()
  (save-excursion
    (progn (setq linenums 1)
	   (while (search-backward-regexp "\\\\.*\n" (line-end-position -1) t)
	     (setq linenums (1- linenums)))
	   linenums)))

(defun csound-eldoc-statement ()
  (save-excursion
    (let ((countback (csound-eldoc-line-escape-count)))
      (buffer-substring
       (line-beginning-position countback)
       (line-end-position)))))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)

(defun csound-eldoc-statement-list (string-statement)
  (split-string
   (chomp string-statement)
   "\\(,+\s*\\)+\\|\\(\s+,*\\)+"))


(defun csound-eldoc-template-lookup (statement-list)
  (progn (setq result nil
	       opdoce nil)
	 (dolist (statement statement-list)
	   (-when-let (res (gethash statement csdoc-opcode-database))
	     (setq result (nth 9 res)
		   opcode statement)))
	 (when result
	   (let ((rate-list (split-string (replace-regexp-in-string "\n\s" "\n" result) "\n")))
	     (if (= (length rate-list) 1)
		 (list opcode (first rate-list))
	       (let ((rate-candidate (substring (first statement-list) 0 1)))
		 (setq rate-match nil)
		 (dolist (xrate rate-list)
		   (when (string= rate-candidate (substring xrate 0 1))
		     (setq rate-match xrate)))
		 (if rate-match
		     (list opcode rate-match)
		   (list opcode (first rate-list)))))))))

(defun csound-eldoc-argument-index (opcode-match)
  (save-excursion
    (let* ((statement (buffer-substring
		       (line-beginning-position (csound-eldoc-line-escape-count))
		       (point)))
	   (komma-format-list (split-string (replace-regexp-in-string opcode "," statement) ",")))
      (length komma-format-list))))

;;;###autoload
(defun csound-eldoc-function ()
  "Returns a doc string appropriate for the current context, or nil."
  (let* ((csound-statement (csound-eldoc-statement))
	 (statement-list (csound-eldoc-statement-list csound-statement))
	 (template-lookup (csound-eldoc-template-lookup statement-list)))
    (when template-lookup
      (let* ((opcode-match (first template-lookup))
	     (csound-template (replace-regexp-in-string
			       "\\[, " "["
			       (nth 1 template-lookup)))
	     (template-list (csound-eldoc-statement-list csound-template))
	     (template-list-length (1- (length template-list)))
	     (argument-index (csound-eldoc-argument-index opcode-match))
	     (infinite-args? (string= "[...]" (car (last template-list)))))
	(setq indx 0 eldocstr "" inf-arg nil)
	(dolist (arg template-list)
	  (setq indx (if (string= arg opcode-match) indx (1+ indx))
		inf-arg (if (and infinite-args?
				 (< template-list-length argument-index))
			    t nil)
		eldocstr (concat eldocstr
				 ;;(prog2 (put-text-property 0 (length arg) 'face 'error arg) arg)
				 ;;(string= opcode-match (thing-at-point 'symbol))
				 (if (string= arg opcode-match)
				     (prog2 (put-text-property 0 (length arg) 'face
							       (list :foreground "#C70039" :weight (if (string= opcode-match (thing-at-point 'symbol))
												       'bold 'normal))
							       arg)
					 arg)
				   (if (or (and (= indx argument-index)
						(not (string= opcode-match (thing-at-point 'symbol))))
					   (and inf-arg (string= "[...]" arg)))
				       (prog2 (put-text-property 0 (length arg) 'face '(:foreground "#A4FF00" :weight bold) arg)
					   arg)
				     arg)) " ")))
	eldocstr
	;; argument-index
	;; template-list
	;; (list opcode-match statement-list template-list)
	;; csound-statement
	;; (replace-regexp-in-string "\\[, " "[" csound-template)
	;; (car (last template-list))
	)) 
    ))

;;;###autoload
(defun csound-turn-on-eldoc () 
  (set (make-local-variable 'eldoc-documentation-function) 'csound-eldoc-function)
  (eldoc-mode))



;; (defun henda ()
;;   (interactive)
;;   (message
;;    (csound-eldoc-function)))

(provide 'csound-eldoc)

;; asig oscil a1, a2


