;;; csound-font-lock.el

;; Copyright (C) 2017  Hlöðver Sigurðsson

;; Author: Hlöðver Sigurðsson <hlolli@gmail.com>

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

;;; Commentary

;; See README.md (https://github.com/hlolli/csound-mode/blob/master/README.md)

;;; Code:


(require 'shut-up)

(defcustom csound-rainbow-score-parameters? t
  "Color each parameter field for
   not events within CsScore/.sco"
  :type 'boolean
  :group 'csound-mode-font-lock)

(defvar csound-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    ;; (modify-syntax-entry ?+ "w" st)
    ;; (modify-syntax-entry ?- "w" st)
    (modify-syntax-entry ?. "w" st)
    (modify-syntax-entry ?! "w" st)
    (modify-syntax-entry ?% "-" st)
    (modify-syntax-entry ?\" "\"\"" st)
    ;; (modify-syntax-entry ?| "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    ;; Comment syntax
    (modify-syntax-entry ?\/ ". 14" st)
    (modify-syntax-entry ?* ". 23" st)
    st)
  "Syntax table for csound-mode")

(defvar csound-font-lock-list '())

(defface csound-eval-flash
  '((((class color)) (:background "blue" :foreground "white" :bold t))
    (t (:inverse-video t)))
  "Face for highlighting during evaluation."
  :group 'csound-mode-font-lock)

(defface csound-eval-flash-error
  '((((class color)) (:foreground "red" :bold t))
    (t (:inverse-video t)))
  "Face for highlighting signaled errors during evaluation."
  :group 'csound-mode-font-lock)

(defface csound-p-face
  '((((class color)) (:foreground "#F9E79F" :bold t)))
  "Face for csound p3, p4 ..."
  :group 'csound-mode-font-lock)

(defface csound-i-score-face
  '((((class color)) (:inherit font-lock-builtin-face :bold t)))
  ""
  :group 'csound-mode-font-lock)


(defface csound-f-rate-global-face
  '((((class color)) (:inherit font-lock-negation-char-face :bold t)))
  ""
  :group 'csound-mode-font-lock)

(defface csound-a-rate-global-face
  '((((class color)) (:inherit font-lock-constant-face :bold t)))
  ""
  :group 'csound-mode-font-lock)

(defface csound-k-rate-global-face
  '((((class color)) (:inherit font-lock-function-name-face :bold t)))
  ""
  :group 'csound-mode-font-lock)

(defface csound-i-rate-global-face
  '((((class color)) (:inherit font-lock-variable-name-face :bold t)))
  ""
  :group 'csound-mode-font-lock)

(defconst csound-faces-p-parameter-variable
  (push '("\\bp[[:digit:]]+" . 'csound-p-face)
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

(defconst csound-faces-goto-symbol
  (push '("\\<\\w*:\\B" . font-lock-constant-face) csound-font-lock-list))


(setq-local missing-faces
	    (apply 'concat (mapcar (lambda (s) (concat "\\|\\<" s "\\>"))
				   '("then" "do" "od" "else" "elseif" "endif"))))

(defconst csound-faces-opcodes
  (let* ((mutz "")
	 (or-regex-opcodes (maphash (lambda (k v)
				      (when (stringp k)
					(setq-local mutz (concat mutz  "\\|\\<" k "\\>"))))
				    csdoc-opcode-database))
	 (mutz (concat "" (substring mutz 3 (length mutz)) missing-faces)))
    (push `(,mutz . font-lock-builtin-face) csound-font-lock-list)))

(defconst csound-faces-macros
  (push '("\\#\\w*\\|\\$\\w*" . font-lock-preprocessor-face)
	csound-font-lock-list))

(defconst csound-faces-s-rate-variables
  (push '("\\<[S]+\\w*" . font-lock-variable-name-face) csound-font-lock-list))

(defconst csound-faces-xml-tags
  (push '("</?CsoundSynthesizer>\\|</?CsOptions>\\|</?CsInstruments>\\|</?CsScore>" . font-lock-keyword-face) csound-font-lock-list))

(defconst csound-faces-comments
  (push '(";+.*" . font-lock-comment-face)  csound-font-lock-list))

(defconst csound-faces-string
  (push '("\\s\"\\(.*?\\)[^\\]\\s\""
	  . font-lock-string-face) csound-font-lock-list))


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
	    (passed-i? nil)
	    (depth 2)
	    (comment-begin (save-excursion
			     (beginning-of-line)
			     (search-forward ";" (line-end-position) t 1)))
	    (start-of-i (save-excursion
			  (search-forward-regexp "\\bi\\|\\bf" (line-end-position) t 1)))) 
	(if (and (not start-of-i)
		 comment-begin)
	    (font-lock-prepend-text-property (1- comment-begin) (line-end-position) 'face "font-lock-comment-face")
	  (when csound-rainbow-score-parameters?
	    (save-excursion
	      (beginning-of-line 1)
	      (while (< (point) end-line) 
		(if (and comment-begin
			 (>= (point) comment-begin))
		    (prog2 (font-lock-prepend-text-property (1- comment-begin) (line-end-position) 'face "font-lock-comment-face")
			(goto-char end-line))
		  (if (not passed-i?)
		      (progn (if start-of-i
				 (goto-char start-of-i)
			       (search-forward-regexp "i\\|f\\|a\\|t" (line-end-position) t 1))
			     (if (or (string-equal "i" (thing-at-point 'word t))
				     (string-equal "f" (thing-at-point 'word t)))
				 (prog2 (setq passed-i? t)
				     (font-lock-prepend-text-property (1- (point)) (point) 'face "csound-i-score-face")))
			     (progn 
			       (setq beg-word (point)
				     end-word (search-forward-regexp "\\s-\\|$" (line-end-position))
				     passed-i? t)
			       ;; Recolor i to overwrite i-rate behaviour
			       (font-lock-prepend-text-property (1- beg-word) beg-word 'face "csound-i-score-face")
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
		      (setq depth (1+ depth))))))))))))

;; (defun csound-fontify-region (beg end &optional loud)
;;   (save-excursion
;;     (when (save-excursion (search-backward-regexp "<CsScore>" nil t 1))
;;       (csound-font-lock--fontify-score))))

(defun csound-fontify-region (beg end &optional loud)
  (shut-up
   (save-excursion
     (if (or (save-excursion (search-backward-regexp "<CsScore>" nil t 1))
	     (string-match-p ".sco$" (buffer-name (current-buffer))))
	 (csound-font-lock--fontify-score)
       ;; All normal font-lock calls
       (let ((open-comment (save-excursion (search-backward "/*" (point-min) t 1)))
	     (close-comment (save-excursion (search-backward "*/" (point-min) t 1))))
	 (if (and open-comment
		  (or (not close-comment)
		      (< close-comment open-comment)))
	     (font-lock-prepend-text-property (- open-comment 2)
					      (or (save-excursion (search-forward "*/" (point-max) t 1))
						  (point-max))
					      'face 'font-lock-comment-face)
	   (let ((last-line (save-excursion (goto-char end) (line-number-at-pos))))
	     (goto-char beg)
	     (when (not (save-excursion
			  (beginning-of-buffer)
			  (search-forward-regexp "</CsInstruments>" end t 1)))
	       (while (< (line-number-at-pos) last-line)
		 (when (not (save-excursion
			      (beginning-of-line)
			      (search-forward-regexp "\\(^\\s-*\\|^\\t-*\\)i+[0-9\\\".*]*\\b" (line-end-position) t 1)))
		   ;; (message "region line %d" (line-number-at-pos))
		   ;;(font-lock-default-fontify-region beg end nil) 
		   (font-lock-default-fontify-region (line-beginning-position) (line-end-position) nil))
		 (beginning-of-line 2))))))))))

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

(defun csound-font-lock--flush-block-comments ()
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward "/*" (point-max) t 1)
      (font-lock-prepend-text-property
       (- (point) 2)
       (or (search-forward "*/" (point-max) t 1)
	   (point-max))
       'face "font-lock-comment-face"))
    (beginning-of-buffer)
    (while (search-forward ";" (point-max) t 1)
      (font-lock-prepend-text-property
       (- (point) 1)
       (line-end-position 1)
       'face "font-lock-comment-face"))))

(defun csound-font-lock-param--bugfix ()
  (save-excursion
    (when (search-forward-regexp "</CsoundSynthesizer>" nil t 1)
      (beginning-of-line)
      (font-lock-default-fontify-region (line-beginning-position) (line-end-position) nil))))

(defun csound-font-flush ()
  (progn (csound-font-lock-param--flush-buffer)
	 (csound-font-lock-param--flush-score)
	 (csound-font-lock--flush-block-comments)
	 (csound-font-lock-param--bugfix)))

(provide 'csound-font-lock)

;;; csound-font-lock.el ends here
