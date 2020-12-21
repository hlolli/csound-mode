;;; csound-indentation.el --- A major mode for interacting and coding Csound

;; Copyright (C) 2017  Hlöðver Sigurðsson

;; Author: Hlöðver Sigurðsson <hlolli@gmail.com>
;; Version: 0.2.2
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


(defun csound-indentation-begin-of-expr-p ()
  (save-excursion
    (beginning-of-line 1)
    (search-forward-regexp "\\b\\(instr\\)\\b\\|\\b\\(opcode\\)\\b" (line-end-position 1) t)))

(defun csound-indentation-end-of-expr-p ()
  (save-excursion
    (beginning-of-line 1)
    (search-forward-regexp "\\b\\(endin\\)\\b\\|\\b\\(endop\\)\\b" (line-end-position 1) t)))

(defun csound-indentation-inside-instr-p ()
  (let* ((case-fold-search nil)
	 (last-instr (save-excursion (search-backward-regexp "\\(instr\\)\\b" nil t)))
	 (last-endin (save-excursion (search-backward-regexp "\\(endin\\)\\b" nil t))))
    (cond ((eq 'nil last-instr) nil)
	  ((and (numberp last-instr) (eq 'nil last-endin)) t)
	  ((< last-endin last-instr) t)
	  (t nil))))

(defun csound-indentation-inside-opcode-p ()
  (let* ((case-fold-search nil)
	 (last-opcode (save-excursion (search-backward-regexp "\\(opcode\\)\\b" nil t)))
	 (last-endop (save-excursion (search-backward-regexp "\\(endop\\)\\b" nil t))))
    (cond ((eq 'nil last-opcode) nil)
	  ((and (numberp last-opcode) (eq 'nil last-endop)) t)
	  ((< last-endop last-opcode) t)
	  (t nil))))

(defun csound-indentation-beginning-of-bool-p ()
  (save-excursion
    (beginning-of-line 1)
    (if (and (search-forward-regexp
	      "\\b\\(if\\)\\b\\|\\b\\(while\\)\\b\\|\\b\\(else\\)\\b\\|\\b\\(elseif\\)\\b\\|\\b\\(until\\)\\b"
	      (csound-util-line-boundry) t 1)
	     ;; if in mix with gotos
	     ;; dont have endif therefore
	     ;; dont create logical blocks
	     (prog2
		 (beginning-of-line 1)
		 (not (search-forward-regexp "[a\\|k\\|i]?goto" (csound-util-line-boundry) t))))
	1 0)))

(defun csound-indentation-end-of-bool-p ()
  (save-excursion
    (beginning-of-line 1)
    (if (search-forward-regexp
	 "\\b\\(endif\\)\\b\\|\\b\\(od\\)\\b\\|\\b\\(else\\)\\b\\|\\b\\(elseif\\)\\b"
	 (csound-util-line-boundry) t 1)
	1 0)))

(defun csound-indentation-count-goto-if-mix (end-of-expr cnt)
  (if (<= end-of-expr (point))
      cnt
    (prog2
	(beginning-of-line 2)
	(csound-indentation-count-goto-if-mix
	 end-of-expr (if (and (search-forward-regexp "\\b\\(if\\)\\b" (csound-util-line-boundry) t 1)
			      (search-forward-regexp "goto" (csound-util-line-boundry) t 1))
			 (1+ cnt) cnt)))))


(defun csound-indentation-line-break-escape-p ()
  (save-excursion
    (beginning-of-line 0)
    (let ((line-as-string (buffer-substring (point) (csound-util-line-boundry))))
      (string-match-p "\\\\[[:space:]]*$" line-as-string))))


(defun csound-indentation-expression-first-arg (expr-string non-comma-cnt initial-recur-p)
  (let ((trimmed-str (csound-util-chomp expr-string)))
    (let* ((beginning-of-delimination (string-match "[\s\t,]" trimmed-str 0))
           (end-of-delimination (when beginning-of-delimination
                                  (string-match "[^\s^\t^,]" trimmed-str beginning-of-delimination)))
           (comma-inbetween  (when end-of-delimination
                               (string-match-p ","
                                               (substring-no-properties trimmed-str beginning-of-delimination
                                                                        end-of-delimination))))
           (next-form (when end-of-delimination
                        (substring-no-properties trimmed-str end-of-delimination (length trimmed-str))))
           (next-beginning (string-match "[\s\t,]" next-form 0))
           (next-end (when next-beginning
                       (string-match "[^\s^\t^,]" next-form next-beginning)))
           (next-comma (when next-end
                         (string-match-p ","
                                         (substring-no-properties next-form next-beginning next-end)))))
      (if (or (not (= 0 non-comma-cnt))
              (and initial-recur-p (not comma-inbetween) next-comma))
          (substring-no-properties trimmed-str end-of-delimination
                                   (or (string-match "[\s\|\t]" trimmed-str end-of-delimination)
                                       (length trimmed-str)))
        (when next-form
          (csound-indentation-expression-first-arg
           next-form
           (if comma-inbetween non-comma-cnt (1+ non-comma-cnt)) nil))))))

(defun csound-indentation-line-break-indent ()
  ;; If it's the second occourance in a row
  (if (save-excursion
        (beginning-of-line 0)
        (csound-indentation-line-break-escape-p))
      (indent-line-to (save-excursion
                        (beginning-of-line 0)
                        (search-forward-regexp "[^\s^\t]" (line-end-position 1) t 1)
                        (1- (current-column))))
    (let* ((assignment-operator (save-excursion
                                  (beginning-of-line 0)
                                  (search-forward-regexp "=" (csound-util-line-boundry) t 1)))
           (paren-open (when assignment-operator
                         (save-excursion
                           (goto-char assignment-operator)
                           (search-forward-regexp "(" (csound-util-line-boundry) t 1)))))
      (cond
       ;; ivar = poscil( |ival, ival2)
       ((and assignment-operator paren-open)
        (indent-line-to (save-excursion
                          (goto-char paren-open)
                          (search-forward-regexp "[^\s^\t^(]" (csound-util-line-boundry) t 1)
                          (1- (current-column)))))
       ;; ivar = |"coulbeanything"
       ((and assignment-operator (not paren-open))
        (indent-line-to (save-excursion
                          (goto-char assignment-operator)
                          (search-forward-regexp "[^\s^\t]" (csound-util-line-boundry) t 1)
                          (current-column))))
       ;; First element that has comma after an element without comma
       (t (let* ((first-arg
                  (csound-indentation-expression-first-arg
                   (csound-util-remove-comment-in-string
                    (buffer-substring-no-properties
                     (line-beginning-position 0)
                     (line-end-position 0)))
                   0 t))
                 (first-arg-pos
                  (save-excursion
                    (beginning-of-line 0)
                    (search-forward first-arg (line-end-position 1) t 1)
                    (current-column))))
            (if (and first-arg first-arg-pos)
                (indent-line-to (- first-arg-pos (length first-arg)))
              ;; Fallback, ident +2 from first non-whitespace char above
              (indent-line-to
               (save-excursion
                 (beginning-of-line 0)
                 (search-forward-regexp "[^\s^\t]" (csound-util-line-boundry) t 1)
                 (current-column))))))))))

(defun csound-indentation-inside-expression-calc (expr-type)
  (let* ((beginning-of-expr (if (eq 'instr expr-type)
				(save-excursion
				  (search-backward-regexp "\\(instr\\)\\b" nil t))
			      (save-excursion
				(search-backward-regexp "\\(opcode\\)\\b" nil t))))
	 (end-of-expr (or (if (eq 'instr expr-type)
			      (save-excursion
				(search-forward-regexp "\\(endin\\)\\b" nil t))
			    (save-excursion
			      (search-forward-regexp "\\(endop\\)\\b" nil t)))
			  (point-max)))
	 (ending-of-current-line (line-end-position))
	 (expression-to-point (buffer-substring beginning-of-expr (line-end-position 1)))
         (expression-to-line-above (buffer-substring beginning-of-expr (line-end-position 0)))
	 (count-if-statements (csound-util-recursive-count  "\\b\\(if\\)\\b" expression-to-point 0))
	 (goto-if-mix (save-excursion
			(prog2
			    (goto-char beginning-of-expr)
			    (csound-indentation-count-goto-if-mix end-of-expr 0))))
	 ;; (count-elseif-statements (recursive-count "\\b\\(elseif\\)\\b" (buffer-substring beginning-of-expr (line-end-position 1)) 0))
	 (count-endif-statements (csound-util-recursive-count "\\b\\(endif\\)\\b" expression-to-point 0))
	 (count-while-statements (csound-util-recursive-count "\\b\\(while\\)\\b" expression-to-point 0))
	 (count-od-statements (csound-util-recursive-count "\\b\\(od\\)\\b" expression-to-point 0))
         (count-multiline-string-open (csound-util-recursive-count "{{" expression-to-line-above 0))
	 (count-multiline-string-close (csound-util-recursive-count "}}" expression-to-point 0))
	 (after-goto-statement (if (and (string-match-p "\\<\\w*:\\B" expression-to-point)
                                        (= 0 (- count-multiline-string-open count-multiline-string-close)))
                                   1 0))
	 (line-at-goto-statement (if (save-excursion
				       (beginning-of-line)
				       (search-forward-regexp "\\<\\w*:" (line-end-position 1) t 1))
				     1 0))
	 ;; (end-of-bool-p (csound-indentation-end-of-bool-p))
	 (begin-of-bool-p (csound-indentation-beginning-of-bool-p))
	 (tab-count (max 1 (1+ (- (+ count-if-statements
				     after-goto-statement
                                     count-multiline-string-open
				     ;; count-elseif-statements
				     count-while-statements)
				  count-endif-statements
				  count-od-statements
				  begin-of-bool-p
				  line-at-goto-statement
				  goto-if-mix
                                  count-multiline-string-close
				  ;;end-of-bool-p
				  )))))
    ;; (message "gotos: %d, bool-begin: %d, line-at-goto: %d, count-if: %d, mix %d" after-goto-statement begin-of-bool-p line-at-goto-statement count-if-statements goto-if-mix)
    ;; (message "multistr-open: %d multistr-close: %d " count-multiline-string-open count-multiline-string-close)
    ;; (when (and (eq 't end-of-bool-p) (not (eq 't begin-of-bool-p))) (indent-line-to (* csound-indentation-spaces (1- tab-count))))
    (indent-line-to (* csound-indentation-spaces tab-count))))

(defun csound-indentation--for-each-line (start end fn)
  (while (< (point) end)
    (funcall fn)
    (forward-line)))

(defun csound-indentation-line ()
  "Indent current line."
  (let ((score-p (or (save-excursion (search-backward "<CsScore" nil t 1))
		     (string-match-p ".sco$" (buffer-name (current-buffer))))))
    (cond
     (score-p (if csound-indentation-aggressive-score
		  (csound-score-align-block)
		(indent-line-to 0)))
     ((csound-indentation-begin-of-expr-p) (indent-line-to 0))
     ((csound-indentation-end-of-expr-p) (indent-line-to 0))
     ((csound-indentation-line-break-escape-p) (csound-indentation-line-break-indent))
     ((csound-indentation-inside-instr-p) (csound-indentation-inside-expression-calc 'instr))
     ((csound-indentation-inside-opcode-p) (csound-indentation-inside-expression-calc 'opcode))
     (t (indent-line-to 0)))))

(provide 'csound-indentation)

;;; csound-indentation.el ends here
