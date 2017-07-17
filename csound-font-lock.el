;;; csound-font-lock.el --- A major mode for interacting and coding Csound
;;  Copyright (C) 2017  Hlöðver Sigurðsson

;; Author: Hlöðver Sigurðsson <hlolli@gmail.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "25") (shut-up "0.3.2") (multi "2.0.1"))

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
;;  Font lock functionalities for csound-mode, both
;;  score and orchestra specific, manual fontifycation
;;  hacks for rainbow delimited parameter fields as well.

;;; Code:

(require 'font-lock)
(require 'shut-up)

(defcustom csound-font-lock-rainbow-score-parameters-p t
  "Color each parameter field for
   not events within CsScore/.sco"
  :type 'boolean
  :group 'csound-mode-font-lock)

(defface csound-font-lock-eval-flash
  '((((class color)) (:background "#0AD600" :foreground "white" :bold t))
    (t (:inverse-video t)))
  "Face for highlighting during evaluation."
  :group 'csound-mode-font-lock)

(defface csound-font-lock-eval-flash-error
  '((((class color)) (:foreground "#D60000" :bold t))
    (t (:inverse-video t)))
  "Face for highlighting signaled errors during evaluation."
  :group 'csound-mode-font-lock)

(defface csound-font-lock-i-rate
  '((((class color)) (:inherit font-lock-variable-name-face)))
  "Face for i-rate variables (i)"
  :group 'csound-mode-font-lock)

(defface csound-font-lock-global-i-rate
  '((((class color)) (:inherit font-lock-variable-name-face :bold t)))
  "Face for global i-rates (gi)"
  :group 'csound-mode-font-lock)

(defface csound-font-lock-k-rate
  '((((class color)) (:inherit font-lock-function-name-face)))
  "Face for control rate variables in orchestra (k)"
  :group 'csound-mode-font-lock)

(defface csound-font-lock-global-k-rate
  '((((class color)) (:inherit font-lock-function-name-face :bold t)))
  "Face for global control rates (gk)"
  :group 'csound-mode-font-lock)

(defface csound-font-lock-f-rate
  '((((class color)) (:inherit font-lock-negation-char-face)))
  "Face for f-rates (f)"
  :group 'csound-mode-font-lock)

(defface csound-font-lock-global-f-rate
  '((((class color)) (:inherit font-lock-negation-char-face :bold t)))
  "Face for global f-rates (gf)"
  :group 'csound-mode-font-lock)

(defface csound-font-lock-a-rate
  '((((class color)) (:inherit font-lock-constant-face)))
  "Face for a-rates (a)"
  :group 'csound-mode-font-lock)

(defface csound-font-lock-global-a-rate
  '((((class color)) (:inherit font-lock-constant-face :bold t)))
  "Face for global a-rates"
  :group 'csound-mode-font-lock)

(defface csound-font-lock-s-variables
  '((((class color) (background light)) (:foreground "#999601"))
    (((class color) (background dark)) (:foreground "#F7F300")))
  "Face for strings (S)"
  :group 'csound-mode-font-lock)

(defface csound-font-lock-global-s-variables
  '((((class color) (background light)) (:foreground "#999601" :bold t))
    (((class color) (background dark)) (:foreground "#F7F300" :bold t)))
  "Face for global strings (gS)"
  :group 'csound-mode-font-lock)

(defface csound-font-lock-goto
  '((((class color)) (:inherit font-lock-constant-face)))
  "Symbols that have been defined with goto ending with colon (end:)"
  :group 'csound-mode-font-lock)

(defface csound-font-lock-p
  '((((class color)) (:foreground "#F9E79F" :bold t)))
  "Face for csound parameter fields (p3, p4 etc.)"
  :group 'csound-mode-font-lock)

(defface csound-font-lock-i
  '((((class color)) (:inherit font-lock-builtin-face)))
  "Instrument statement in score."
  :group 'csound-mode-font-lock)

(defface csound-font-lock-e
  '((((class color)) (:inherit font-lock-warning-face)))
  "Face for end of score statement (e)"
  :group 'csound-mode-font-lock)

;; TODO add faces for all score statements

(defface csound-font-lock-macros
  '((((class color)) (:inherit font-lock-preprocessor-face)))
  "Face for macro definition and instanciation (#macro# $macro)"
  :group 'csound-mode-font-lock)

(defface csound-font-lock-strings
  '((((class color)) (:inherit font-lock-string-face)))
  "Face for strings themselves seperated by double quotation marks."
  :group 'csound-mode-font-lock)

(defface csound-font-lock-xml-tags
  '((((class color)) (:inherit font-lock-keyword-face)))
  "Face for the core .csd xml tags, (<CsoundSynthesizer> etc)"
  :group 'csound-mode-font-lock)


;; Add faces macros to variables
;; TODO: why doesn't defface make a symbol?
(mapc (lambda (sym) (eval `(defvar ,sym ',sym)))
      '(csound-font-lock-i-rate
	csound-font-lock-global-i-rate
	csound-font-lock-k-rate
	csound-font-lock-global-k-rate
	csound-font-lock-f-rate
	csound-font-lock-global-f-rate
	csound-font-lock-a-rate
	csound-font-lock-global-a-rate
	csound-font-lock-s-variables
	csound-font-lock-global-s-variables
	csound-font-lock-goto
	csound-font-lock-p
	csound-font-lock-i
	csound-font-lock-e
	csound-font-lock-macros
	csound-font-lock-strings
	csound-font-lock-xml-tags))

(defvar csound-font-lock-list '())

(defconst csound-font-lock-keywords
  (eval-when-compile
    ;; Regex for i-rates
    (push '("\\<i+\\w*" . csound-font-lock-i-rate) csound-font-lock-list)
    
    ;; Regex for global i-rates
    (push '("\\<\\(gi\\)+\\w*" . csound-font-lock-global-i-rate) csound-font-lock-list)

    ;; Regex for k-rates
    (push `("\\<k+\\w*" . csound-font-lock-k-rate) csound-font-lock-list)

    ;; Regex for global k-rates
    (push '("\\<\\(gk\\)+\\w*" . csound-font-lock-k-rate-global-face) csound-font-lock-list)

    ;; Regex for f-rate variables
    (push '("\\<f+\\w*" . csound-font-lock-f-rate) csound-font-lock-list)

    ;; Regex for global f-rate variables
    (push '("\\<\\(gf\\)+\\w*" . csound-font-lock-global-f-rate) csound-font-lock-list)

    ;; Regex for a-rates
    (push '("\\<a+\\w*" . csound-font-lock-a-rate) csound-font-lock-list)

    ;; Regex for global a-rates
    (push '("\\<\\(ga\\)+\\w*" . csound-font-lock-global-a-rate) csound-font-lock-list)

    ;; Regex for S variables
    (push '("\\<S+\\w*" . csound-font-lock-s-variables) csound-font-lock-list)

    ;; Regex for global S variables
    (push '("\\<\\(gS\\)+\\w*" . csound-font-lock-global-s-variables) csound-font-lock-list)

    ;; Regex for goto symbols ending with colon
    (push '("\\<\\w*:\\B" . csound-font-lock-goto) csound-font-lock-list)

    ;; Regex for p-fields
    (push '("\\bp[[:digit:]]+" . csound-font-lock-p) csound-font-lock-list)

    ;; Regex for `e` statement
    (push '("\\<[e]\\>" . csound-font-lock-e) csound-font-lock-list)

    ;; Regex for csound macros types
    (push '("\\#\\w*\\|\\$\\w*" . csound-font-lock-macros) csound-font-lock-list)

    ;; Regex for csound string types
    (push '("\\s\"\\(.*?\\)[^\\]\\s\"" . csound-font-lock-strings) csound-font-lock-list)

    ;; Regex for core csound xml tags
    (push '("</?CsoundSynthesizer>\\|</?CsOptions>\\|</?CsInstruments>\\|</?CsScore[=\\\"0-9a-zA-z]?>" . csound-font-lock-xml-tags) csound-font-lock-list)

    ;; Some opcodes got missing but dont need docstrings
    (setq-local missing-faces
		(apply 'concat (mapcar (lambda (s) (concat "\\|\\<" s "\\>"))
				       '("then" "do" "od" "else" "elseif" "endif"))))

    ;; Add opcodes to font-lock table csdoc-opdocde-database hash-table
    (let* ((mutz "")
	   (or-regex-opcodes (maphash (lambda (k v)
					(when (stringp k)
					  (setq-local mutz (concat mutz  "\\|\\<" k "\\>"))))
				      csdoc-opcode-database))
	   (mutz (concat "" (substring mutz 3 (length mutz)) missing-faces)))
      (push `(,mutz . font-lock-builtin-face) csound-font-lock-list))

    ;; Regex for `i` events in score
    (push '("\\<i\\'" . csound-font-lock-i) csound-font-lock-list)
    ;; Single line comments
    (push '(";+.*" . font-lock-comment-face)  csound-font-lock-list)))


;; Borrowed from rainbow-delimiters.el
(eval-when-compile
  (defmacro csound-font-lock-param-delimiters--define-depth-faces ()
    (let ((faces '())
          (light-colors ["#709870" "#7388d6" "#909183" "#FF0099" "#4acabb"
			 "#93eaa3" "#858580" "#80a880" "#887070"])
	  (dark-colors ["#909183" "#7388d6" "#99ff00" "#0099ff" "#FF0099"
			"#4acabb" "#93eaa3"  "#D31D21" "#f6bd1f"]))
      (dotimes (i 9)
        (push `(defface ,(intern (format "csound-score-param-delimiters-depth-%d-face" (1+ i)))
                 '((((class color) (background light)) :foreground ,(aref light-colors i))
                   (((class color) (background dark)) :foreground ,(aref dark-colors i)))
                 ,(format "Nested delimiter face, depth %d." (1+ i))
                 :group 'csound-mode)
              faces))
      `(progn ,@faces))))
(csound-font-lock-param-delimiters--define-depth-faces)

(defun csound-font-lock-param-delimiters-default-pick-face (depth)
  (intern-soft
   (concat "csound-score-param-delimiters-depth-"
	   (number-to-string
	    (if (<= depth 9)
		depth
	      (1+ (mod depth 9))))
	   "-face")))

(defun csound-font-lock--fontify-score ()
  (if (save-excursion
	(beginning-of-line)
	(search-forward-regexp "\\(^\\s-*\\|^\\t-*\\)i+\\|f+[0-9\\\".*]*\\b" (line-end-position) t 1))
      (let ((beg-word nil)
	    (end-word nil)
	    (end-line (line-end-position 1))
	    (passed-i-p nil)
	    (depth 2)
	    (comment-begin (save-excursion
			     (beginning-of-line)
			     (search-forward ";" (line-end-position) t 1)))
	    (start-of-i (save-excursion
			  (search-forward-regexp "\\bi\\|\\bf" (line-end-position) t 1)))) 
	(when csound-font-lock-rainbow-score-parameters-p
	  (save-excursion
	    (beginning-of-line 1)
	    (while (< (point) end-line) 
	      (if (and comment-begin
		       (>= (point) comment-begin))
		  (prog2 (font-lock-prepend-text-property (1- comment-begin) (line-end-position) 'face "font-lock-comment-face")
		      (goto-char end-line))
		(if (not passed-i-p)
		    (progn (if start-of-i
			       (goto-char start-of-i)
			     (search-forward-regexp "i\\|f\\|a\\|t" (line-end-position) t 1))
			   (if (or (string-equal "i" (thing-at-point 'word t))
				   (string-equal "f" (thing-at-point 'word t)))
			       (prog2 (setq passed-i-p t)
				   (font-lock-prepend-text-property (1- (point)) (point) 'face "csound-font-lock-i")))
			   (progn 
			     (setq beg-word (point)
				   end-word (search-forward-regexp "\\s-\\|$" (line-end-position))
				   passed-i-p t)
			     ;; Recolor i to overwrite i-rate behaviour
			     (font-lock-prepend-text-property (1- beg-word) beg-word 'face "csound-font-lock-i")
			     ;; Color P1 values
			     (font-lock-prepend-text-property beg-word end-word 'face
							      (funcall #'csound-font-lock-param-delimiters-default-pick-face depth))
			     (setq depth (1+ depth))))
		  ;; If passed i marker
		  (progn
		    ;; (message "line: %d" (line-number-at-pos))
		    (setq beg-word (min (1- (or (save-excursion (search-forward-regexp "[0-9a-zA-Z\\[\\.\\+\\<\\>\"]" (line-end-position) t 1)) 
						(line-end-position))))
			  end-word (save-excursion
				     (goto-char beg-word)
				     (let ((e (search-forward-regexp "\\s-\\|$" (line-end-position))))
				       (if (< e end-line)
					   e end-line))))
		    ;; (message "beg: %d end: %d" beg-word end-word)
		    (goto-char end-word)
		    ;; (add-text-properties beg-word end-word `(face ,(funcall #'csound-font-lock-param-delimiters-default-pick-face depth)))
		    (font-lock-prepend-text-property beg-word end-word 'face (funcall #'csound-font-lock-param-delimiters-default-pick-face depth))
		    (setq depth (1+ depth)))))))))))

(defun csound-font-lock-fontify-region (beg end &optional loud)
  (shut-up
    (save-excursion
      (let ((score-p (or (save-excursion (search-backward "<CsScore>" nil t 1))
			 (string-match-p ".sco$" (buffer-name (current-buffer))))))
	(if score-p
	    (csound-font-lock--fontify-score)
	  ;; All normal font-lock calls
	  (let ((last-line (save-excursion (goto-char end) (line-number-at-pos))))
	    (goto-char beg)
	    (when (not (save-excursion
			 (beginning-of-buffer)
			 (search-forward-regexp "</CsInstruments>" end t 1)))
	      (while (< (line-number-at-pos) last-line)
		(font-lock-default-fontify-region (line-beginning-position) (line-end-position) nil)		
		(beginning-of-line 2)))))))))

(defun csound-font-lock-param--flush-buffer ()
  (save-excursion
    (end-of-buffer)
    (let ((line-count (line-number-at-pos)))
      (beginning-of-buffer) 
      (while (< (line-number-at-pos) line-count)
	(font-lock-default-fontify-region (line-beginning-position) (line-end-position) nil)
	(beginning-of-line 2)))))

(defun csound-font-lock-param--flush-score ()
  (save-excursion
    (end-of-buffer)
    (let ((line-count (line-number-at-pos)))
      (beginning-of-buffer)
      (when (or (search-forward-regexp "<CsScore>" nil t 1)
		(string-match-p ".sco$" (buffer-name (current-buffer))))
	(while (< (line-number-at-pos) line-count)
	  (csound-font-lock--fontify-score)
	  (beginning-of-line 2))))))

;; (defun csound-font-lock--flush-block-comments ()
;;   (save-excursion
;;     (beginning-of-buffer)
;;     (while (search-forward "/*" (point-max) t 1)
;;       (font-lock-prepend-text-property
;;        (- (point) 2)
;;        (or (search-forward "*/" (point-max) t 1)
;; 	   (point-max))
;;        'face "font-lock-comment-face"))
;;     (beginning-of-buffer)
;;     (while (search-forward ";" (point-max) t 1)
;;       (font-lock-prepend-text-property
;;        (- (point) 1)
;;        (line-end-position 1)
;;        'face "font-lock-comment-face"))))

(defun csound-font-lock-param--bugfix ()
  (save-excursion
    (when (search-forward-regexp "</CsoundSynthesizer>" nil t 1)
      (beginning-of-line)
      (font-lock-default-fontify-region (line-beginning-position) (line-end-position) nil))))

(defun csound-font-lock-flush-buffer ()
  (progn (csound-font-lock-param--flush-buffer)
	 (csound-font-lock-param--flush-score)
	 ;; (csound-font-lock--flush-block-comments)
	 (csound-font-lock-param--bugfix)))

(provide 'csound-font-lock)

;;; csound-font-lock.el ends here
