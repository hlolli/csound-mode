;;; csound-indentation.el --- A major mode for interacting and coding Csound

;; Copyright (C) 2017 - 2023  Hlöðver Sigurðsson

;; Author: Hlöðver Sigurðsson <hlolli@gmail.com>
;; Version: 0.2.9
;; Package-Requires: ((emacs "25") (shut-up "0.3.2") (multi "2.0.1") (dash "2.16.0") (highlight "0"))
;; URL: https://github.com/hlolli/csound-mode

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

;;  Indentation rules for csound-mode

;;; Code:


(require 'csound-util)
(require 'csound-score)

;;; define some customizable variables
(defcustom csound-indentation-spaces 2
  "Set how many spaces are in indentation."
  :type 'integer
  :group 'csound-mode)

(defcustom csound-indentation-aggressive-score nil
  "If true, then align blocks will be called for every indent
   calls in a score file or within CsScore tags. Works well
   when used in combination with aggressive-indent mode.
   (defaults to nil(false))"
  :type 'boolean
  :group 'csound-mode)

(defcustom csound-indentation-indent-goto nil
  "If true, then anything that comes after goto symbol
   will be indented."
  :type 'boolean
  :group 'csound-mode)

;;; define some predicate functions for checking regexp
(defun csound-indentation--current-line-breaks-p ()
  "returns t if a \ is found not in the middle of a word in the current line, otherwise nil"
  (if (save-excursion 
        (beginning-of-line 0) 
        (search-forward-regexp
	 "\\B\\\\\\B" (csound-util-line-boundry) t 1))
      t nil))

(defun csound-indentation--previous-line-breaks-p ()
  "returns t if a \ not in the middle of word is found in the previous
line, otherwise nil"
  (if (save-excursion
        (beginning-of-line 0)
        (search-forward-regexp "\\B\\\\\\B" (csound-util-line-boundry) t 1))
      t nil))

(defun csound-indentation--current-line-empty-p ()
  "returns t if the current line is empty, otherwise nil"
  (and
   (save-excursion
     (beginning-of-line)
     ;; (looking-at-p "[[:blank:]]*$") ; is this a mistake? 
     (search-forward-regexp "[[:blank:]]*$" (csound-util-line-boundry)
			    t 1))   ; did you mean this instead?
   ;; don't account for line continuation token being empty
   (not (and (csound-indentation--previous-line-breaks-p)
             (csound-indentation--current-line-breaks-p)))))

(defun csound-indentation--pointer-inside-paren-p ()
  "returns t when pointer is inside a parenthesis, otherwise nil"
  (let* ((case-fold-search nil)
	 (last-open (save-excursion (search-backward-regexp "(" nil t)))
	 (last-closed (save-excursion (search-backward-regexp ")" nil t))))
    (cond ((eq 'nil last-open) nil)
	  ((and (numberp last-open) (eq 'nil last-closed)) t)
	  ((< last-closed last-open) t)
	  (t nil))))

(defun csound-indentation--cursor-behind-indentation-point-p ()
  "returns t if the cursor is behind an indentation point, which is
whitespace in the beginning of a line, otherwise nil"
  (> (or
      (save-excursion
        (beginning-of-line)
        ;; (search-forward-regexp "^\\s-*" (csound-util-line-boundry) t
	;; 		       1) ; is this a mistake?
	(search-forward-regexp "^\\s-+" (csound-util-line-boundry) t
  1)) ; did you mean this instead? 
      (point)) 
     (point)))

(defun csound-indentation-begin-of-expr-p ()
  "returns t when in the next line a instr or opcode block starts, otherwise nil"
  (save-excursion
    (beginning-of-line 1) 
    (search-forward-regexp "\\b\\(instr\\|opcode\\)\\b" (line-end-position 1) t)))

(defun csound-indentation-end-of-expr-p ()
  "returns t when in the next line a instr or opcode block is closed, otherwise nil"
  (save-excursion
    (beginning-of-line 1)
    (search-forward-regexp "\\b\\(endin\\|endop\\)\\b" (line-end-position 1) t)))

(defun csound-indentation-inside-instr-p ()
  "returns t when point is inside a instr block, otherwise nil"
  (let* ((case-fold-search nil)
	 (last-instr (save-excursion (search-backward-regexp "^\\s-*\\(instr\\)\\b" nil t)))
	 (last-endin (save-excursion (search-backward-regexp "^\\s-*\\(endin\\)\\b" nil t))))
    (cond ((eq 'nil last-instr) nil)
	  ((and (numberp last-instr) (eq 'nil last-endin)) t)
	  ((< last-endin last-instr) t)
	  (t nil))))

(defun csound-indentation-inside-opcode-p ()
  "returns t when point is inside a opcode block, otherwise nil"
  (let* ((case-fold-search nil)
	 (last-opcode (save-excursion (search-backward-regexp "^\\s-*\\(opcode\\)\\b" nil t)))
	 (last-endop (save-excursion (search-backward-regexp "^\\s-*\\(endop\\)\\b" nil t))))
    (cond ((eq 'nil last-opcode) nil)
	  ((and (numberp last-opcode) (eq 'nil last-endop)) t)
	  ((< last-endop last-opcode) t)
	  (t nil))))

(defun csound-indentation-beginning-of-bool-p ()
  "returns 1 when the next line a boolean expressions starts but without a goto,
otherwise 0"
  (save-excursion
    (beginning-of-line 1)
    (if (and (search-forward-regexp
	      "\\<\\(if\\|while\\|else\\|elseif\\|until\\)\\>"
	      (csound-util-line-boundry) t 1)
 	     ;; if in mix with gotos
	     ;; dont have endif therefore
	     ;; dont create logical blocks
	      (prog2
	      	  (beginning-of-line)
		  (not (search-forward-regexp "\\<\\(kgoto\\|igoto\\|goto\\)\\>" (csound-util-line-boundry) t 1))))
	1 0)))

(defun csound-indentation-end-of-bool-p ()
  "returns 1 when in the next line a boolean expresion is closed,
otherwise 0"
  (save-excursion
    (beginning-of-line 1)
    (if (search-forward-regexp
	 "\\<\\(endif\\|od\\|else\\|elseif\\)\\>"
	 (csound-util-line-boundry) t 1)
	1 0)))

;;; not needed
;; (defun csound-indentation-count-goto-if-mix
;;     (end-of-expr cnt current-depth)
;;   "if"
  
;;   (if (or (> current-depth 50) (<= end-of-expr (point)))
;;       ;; if curren-depth is > 50 or pointer is behind the end of the
;;       ;; current instr or opcode block return cnt
;;       cnt
;;       ;; else do this
;;     (prog2
;; 	(beginning-of-line 1) ;; go down one line
;; 	(if ;; if a if statement and a goto statement is found count
;; 	     ;; upwards, if not return current cnt
;; 	     (and (search-forward-regexp "\\<\\(if\\)\\>" (csound-util-line-boundry) t 1)
;; 		  ;; (search-forward-regexp "\\<\\(goto\\)\\>"
;; 		  ;; 			 (csound-util-line-boundry) t
;; 		  ;; 			 1)
;; 		  ;; (search-forward-regexp "\\<\\(igoto\\|kgoto\\|goto\\)\\>"
;; 		  ;; 			 (csound-util-line-boundry) t
;; 		  ;; 			 1)
;; 		  (search-forward-regexp "\\<\\(kgoto\\|igoto\\|goto\\)\\>"
;; 					 (csound-util-line-boundry) t
;; 					 1))
;; 	    (csound-indentation-count-goto-if-mix end-of-expr (1+ cnt)
;; 						  (1+ current-depth))
;; 	  (csound-indentation-count-goto-if-mix end-of-expr cnt (1+ current-depth))))))

;;; this function is never used
;; (defun csound-indentation-expression-first-arg (expr-string non-comma-cnt initial-recur-p)
;;   (let ((trimmed-str (csound-util-chomp expr-string)))
;;     (let* ((beginning-of-delimination (string-match "[\s\t,]" trimmed-str 0))
;;            (end-of-delimination (when beginning-of-delimination
;;                                   (string-match "[^\s^\t^,]" trimmed-str beginning-of-delimination)))
;;            (comma-inbetween  (when end-of-delimination
;;                                (string-match-p ","
;;                                                (substring-no-properties trimmed-str beginning-of-delimination
;;                                                                         end-of-delimination))))
;;            (next-form (when end-of-delimination
;;                         (substring-no-properties trimmed-str end-of-delimination (length trimmed-str))))
;;            (next-beginning (string-match "[\s\t,]" next-form 0))
;;            (next-end (when next-beginning
;;                        (string-match "[^\s^\t^,]" next-form next-beginning)))
;;            (next-comma (when next-end
;;                          (string-match-p ","
;;                                          (substring-no-properties next-form next-beginning next-end)))))
;;       (if (or (not (= 0 non-comma-cnt))
;;               (and initial-recur-p (not comma-inbetween) next-comma))
;;           (substring-no-properties trimmed-str end-of-delimination
;;                                    (or (string-match "[\s\|\t]" trimmed-str end-of-delimination)
;;                                        (length trimmed-str)))
;;         (when next-form
;;           (csound-indentation-expression-first-arg
;;            next-form
;;            (if comma-inbetween non-comma-cnt (1+ non-comma-cnt)) nil))))))


;;; this functions is never used
;; (defun csound-indentation-line-break-indent ()
;;   ;; If it's the second occourance in a row
;;   (if (save-excursion
;;         (beginning-of-line 0)
;;         (csound-indentation-line-break-escape-p) ; this function
;; 					; doesn't exists
;; 	)
;;       (indent-line-to (save-excursion
;;                         (beginning-of-line 0)
;;                         (search-forward-regexp "[^\s^\t]" (line-end-position 1) t 1)
;;                         (1- (current-column))))
;;     (let* ((assignment-operator (save-excursion
;;                                   (beginning-of-line 0)
;;                                   (search-forward-regexp "=" (csound-util-line-boundry) t 1)))
;;            (paren-open (when assignment-operator
;;                          (save-excursion
;;                            (goto-char assignment-operator)
;;                            (search-forward-regexp "(" (csound-util-line-boundry) t 1)))))
;;       (cond
;;        ;; ivar = poscil( |ival, ival2)
;;        ((and assignment-operator paren-open)
;;         (indent-line-to (save-excursion
;;                           (goto-char paren-open)
;;                           (search-forward-regexp "[^\s^\t^(]" (csound-util-line-boundry) t 1)
;;                           (1- (current-column)))))
;;        ;; ivar = |"coulbeanything"
;;        ((and assignment-operator (not paren-open))
;;         (indent-line-to (save-excursion
;;                           (goto-char assignment-operator)
;;                           (search-forward-regexp "[^\s^\t]" (csound-util-line-boundry) t 1)
;;                           (current-column))))
;;        ;; First element that has comma after an element without comma
;;        (t (let* ((first-arg
;;                   (csound-indentation-expression-first-arg
;;                    (csound-util-remove-comment-in-string
;;                     (buffer-substring-no-properties
;;                      (line-beginning-position 0)
;;                      (line-end-position 0)))
;;                    0 t))
;;                  (first-arg-pos
;;                   (save-excursion
;;                     (beginning-of-line 0)
;;                     (search-forward first-arg (line-end-position 1) t 1)
;;                     (current-column))))
;;             (if (and first-arg first-arg-pos)
;;                 (indent-line-to (- first-arg-pos (length first-arg)))
;;               ;; Fallback, ident +2 from first non-whitespace char above
;;               (indent-line-to
;;                (save-excursion
;;                  (beginning-of-line 0)
;;                  (search-forward-regexp "[^\s^\t]" (csound-util-line-boundry) t 1)
;;                  (current-column))))))))))

(defun csound-indentation-inside-expression-calc (expr-type)
  "calculates the indentation level for current line an indent it"
  (let* ((beginning-of-expr (if (eq 'instr expr-type)
                                (save-excursion
				  (search-backward-regexp "\\s-*\\<\\(instr\\)\\b" nil t))
			      (save-excursion
				(search-backward-regexp "\\s-*\\<\\(opcode\\)\\b" nil t))))
	 (end-of-expr (or (if (eq 'instr expr-type)
			      (save-excursion
				(search-forward-regexp "\\s-*\\<\\(endin\\)\\b" nil t))
			    (save-excursion
			      (search-forward-regexp "\\s-*\\<\\(endop\\)\\b" nil t)))
			  (point-max)))
	 (ending-of-current-line (line-end-position))
	 (expression-to-point (replace-regexp-in-string
                               "\".*\"\\|;;.*\\|//.*" ""
                               (buffer-substring beginning-of-expr (line-end-position 1)) ))
	 (expression-to-line-above (buffer-substring beginning-of-expr (line-end-position 0)))
	 (count-if-statements (save-excursion
				(csound-util-recursive-count
				 "\\<\\(if\\)\\((\\|\\>\\)"
				 expression-to-point 0)))
	 (goto-if-mix (save-excursion
				(csound-util-recursive-count
				 "\\<if\\>.*\\<\\(goto\\|igoto\\|kgoto\\)\\>" 
				 expression-to-point 0)))			    
	 ;; (count-elseif-statements (recursive-count
	 ;; "\\b\\(elseif\\)\\b" (buffer-substring beginning-of-expr
	 ;; (line-end-position 1)) 0))
	 (count-endif-statements (csound-util-recursive-count "\\s-?\\(endif\\)\\s-?" expression-to-point 0))
	 (count-while-statements (csound-util-recursive-count "\\s-?\\(while\\)\\s-?" expression-to-point 0))
	 (count-od-statements (csound-util-recursive-count "\\<\\(od\\)\\>" expression-to-point 0))
         ;;(count-switch-statements (csound-util-recursive-count "\\s-?\\(switch\\)\\s-?" expression-to-point 0))
         ;;(count-endw-statements (csound-util-recursive-count "\\s-?\\(endsw\\)\\s-?" expression-to-point 0))
         (count-multiline-string-open (csound-util-recursive-count "{{" expression-to-line-above 0))
	 (count-multiline-string-close (csound-util-recursive-count "}}" expression-to-point 0))
	 (after-goto-statement
	  (if csound-indentation-indent-goto
	      (if (and (string-match-p "\\<\\w*:\\b" expression-to-point)
		       (= 0 (- count-multiline-string-open count-multiline-string-close)))
		  1 0)
	    0))
	 (line-at-goto-statement
	  (if (and csound-indentation-indent-goto
                   (save-excursion
		     (beginning-of-line)
		     (search-forward-regexp "\\<\\w*\\:\\s-*$" (line-end-position 1) t 1)))
	      1 0))
	 (begin-of-bool-p (csound-indentation-beginning-of-bool-p))
         (previous-line-break-adjust
	  (if (csound-indentation--previous-line-breaks-p)
	      1 0))
	 (unbalanced-parens
	  (if (csound-indentation--pointer-inside-paren-p)
	      1 0))
	 (unbalanced-parens-or-line-break
	  (if (or (eq 1 unbalanced-parens) (eq 1 previous-line-break-adjust))
					      1 0))
	 (tab-count (max 1 (1+ (- (+ count-if-statements
                                     after-goto-statement
                                     count-multiline-string-open
				     ;; count-elseif-statements
				     count-while-statements
                                     ;;count-switch-statements
                                     ;;previous-line-break-adjust
				     unbalanced-parens-or-line-break)
				  count-endif-statements
				  count-od-statements
                                  ;;count-endw-statements
				  begin-of-bool-p
				  (if (> after-goto-statement 0)  line-at-goto-statement 0)
				  goto-if-mix
                                  count-multiline-string-close
				  ;;end-of-bool-p
				  )))))
;;     (message "topoint: %s" expression-to-point)
;;     (message "bool-begin: %d, ods: %d, line-at-goto: %d, aft-goto: %d,
;; count-if: %d, count-endif: %d mix: %d mls: %d.%d unbalanced-parens: %d
;; tab-count: %d"
;;              begin-of-bool-p
;;              count-od-statements
;;              line-at-goto-statement
;;              after-goto-statement
;;              count-if-statements
;;              count-endif-statements
;;              goto-if-mix
;;              count-multiline-string-open
;;              count-multiline-string-close
;; 	     unbalanced-parens
;;              tab-count)
;;     (message "RES %d"  (* csound-indentation-spaces tab-count))
;;     ;;(message "str-open: %d str-close: %d " count-string-open count-string-close)
;;     (message "multistr-open: %d multistr-close: %d " count-multiline-string-open count-multiline-string-close)
    ;; (when (and (eq 't end-of-bool-p) (not (eq 't begin-of-bool-p))) (indent-line-to (* csound-indentation-spaces (1- tab-count))))
    (indent-line-to (* csound-indentation-spaces tab-count))))

(defun csound-indentation--for-each-line (start end fn)
  (while (< (point) end)
    (funcall fn)
    (forward-line)))

(defun csound-indentation--do-indent ()
  (let ((score-p (or (save-excursion (search-backward "<CsScore" nil t 1))
		     (string-match-p ".sco$" (buffer-name (current-buffer))))))
    (cond
     (score-p (if csound-indentation-aggressive-score
		  (csound-score-align-block)
		(indent-line-to 0)))
     ((csound-indentation-begin-of-expr-p) (indent-line-to 0))
     ((csound-indentation-end-of-expr-p) (indent-line-to 0))
     ;; ((csound-indentation-line-break-escape-p) (csound-indentation-line-break-indent))
     ((csound-indentation-inside-instr-p) (csound-indentation-inside-expression-calc 'instr))
     ((csound-indentation-inside-opcode-p) (csound-indentation-inside-expression-calc 'opcode))
     (t (indent-line-to 0)))))

(defun csound-indentation-line ()
  "Indent current line."
  (if (or (csound-indentation--current-line-empty-p)
          (csound-indentation--cursor-behind-indentation-point-p))
      (csound-indentation--do-indent)
    (save-excursion (csound-indentation--do-indent))))

(provide 'csound-indentation)

;;; csound-indentation.el ends here
