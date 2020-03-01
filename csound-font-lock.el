;;; csound-font-lock.el --- A major mode for interacting and coding Csound
;;  Copyright (C) 2017  Hlöðver Sigurðsson

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
;;  Font lock functionalities for csound-mode, both
;;  score and orchestra specific, manual fontifycation
;;  hacks for rainbow delimited parameter fields as well.

;;; Code:

(require 'font-lock)
(require 'shut-up)

(defvar csound-font-lock--missing-faces '())

(defcustom csound-font-lock-rainbow-score-parameters-p nil
  "Color each parameter field for
   not events within CsScore/.sco"
  :type 'boolean
  :group 'csound-mode-font-lock)

(defface csound-font-lock-eval-flash
  '((((class color) (background light)) (:foreground "#999601" :background "#42ff42"))
    (((class color) (background dark)) (:background "#637863" :foreground "#00e4f0"))
    (t (:inverse-video t)))
  "Face for highlighting during evaluation."
  :group 'csound-mode-font-lock)

(defface csound-font-lock-eval-flash-error
  '((((class color)) (:foreground "#5e0d0d" :bold t))
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
  '((((class color) (background light)) (:foreground "#999601"))
    (((class color) (background dark)) (:foreground "#85C4B5")))
  "Face for f-rates (f)"
  :group 'csound-mode-font-lock)

(defface csound-font-lock-global-f-rate
  '((((class color)) (:inherit csound-font-lock-f-rate :bold t)))
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
  '((((class color) (background light)) (:foreground "#A48E32" :bold t))
    (((class color) (background dark)) (:foreground "#F9E79F" :bold t)))
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
  '((((class color)) (:inherit font-lock-string-face :bold nil)))
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

;;;###autoload
(defvar csound-font-lock-list '())

(defconst csound-font-lock-keywords
  (ignore-errors
    (eval-when-compile
      ;; Regex for i-rates
      (push '("\\<i+\\w*" . csound-font-lock-i-rate) csound-font-lock-list)

      ;; Regex for global i-rates
      (push '("\\<\\(gi\\)+\\w*" . csound-font-lock-global-i-rate) csound-font-lock-list)

      ;; Regex for k-rates
      (push `("\\<k+\\w*" . csound-font-lock-k-rate) csound-font-lock-list)

      ;; Regex for global k-rates
      (push '("\\<\\(gk\\)+\\w*" . csound-font-lock-global-k-rate) csound-font-lock-list)

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

      ;; Regex for csound string types  (use syntactic fontification?)
      ;; (push '("\\s\"\\(.*?\\)[^\\]\\s\"" . csound-font-lock-strings) csound-font-lock-list)

      ;; Regex for core csound xml tags
      ;; "</?CsoundSynthesizer>\\|</?CsOptions>\\|</?CsInstruments>\\|</?CsScore[=\\\"0-9a-zA-z]?>\\|</?CsLicense>"
      (push `(,(concat (regexp-opt '("<CsoundSynthesizer>" "</CsoundSynthesizer>"
				     "<CsOptions>" "</CsOptions>"
				     "<CsInstruments>" "</CsInstruments>"
				     "<CsLicense>" "</CsLicense>"))
		       ;; account for preprocessors
		       "\\|<CsScore[.\\\"]*>\\|</?CsScore>?")
	      . csound-font-lock-xml-tags)
	    csound-font-lock-list)
      ;; Some opcodes got missing but dont need docstrings
      (setq csound-font-lock--missing-faces '("then" "do" "od" "else" "elseif" "endif"))
      ;; Add opcodes to font-lock table csdoc-opdocde-database hash-table
      (let ((mutz '()))
	(maphash (lambda (k v)
		   (when (stringp k)
		     (setq mutz (cons k mutz))))
		 csdoc-opcode-database)
	(setq mutz (append mutz csound-font-lock--missing-faces))
	(setq mutz (regexp-opt mutz 'words))
	(push `(,mutz . font-lock-builtin-face) csound-font-lock-list))
      ;; Regex for `i` events in score
      (push '("\\<i\\'" . csound-font-lock-i) csound-font-lock-list)
      ;; Single line comments (use syntactic fontification?)
      (push '("//.*" . font-lock-comment-face)  csound-font-lock-list)
      )))


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

(defun csound-font-lock--fontify-score (beg end)
  (let ((beg-line-num (line-number-at-pos beg))
	(end-line-num (min (line-number-at-pos (max-char))
                           (+ 2 (line-number-at-pos end)))))
    (save-excursion
      (beginning-of-line)
      (while (< (line-number-at-pos) end-line-num)
	(let* ((beg-word nil)
	       (end-word nil)
	       (end-line (line-end-position))
	       (passed-i-p nil)
	       (depth 2)
	       (rainbow-line-p (save-excursion
				 (back-to-indentation)
				 (let ((first-word (thing-at-point 'word t)))
				   (when first-word
				     (string-match-p "^[-]?i\\|^[-]?f" first-word))))))
	  (if (not rainbow-line-p)
	      (save-excursion (font-lock-default-fontify-region (line-beginning-position) (line-end-position) nil))
	    (while (< (point) end-line)
	      (if (and rainbow-line-p (not passed-i-p))
		  (progn
		    (back-to-indentation)
		    (setq passed-i-p t)
		    (setq beg-word (point)
			  end-word (search-forward-regexp "\\s-\\|$" (line-end-position)))
		    ;; Recolor i to overwrite i-rate behaviour
		    (font-lock-prepend-text-property (1- beg-word) beg-word 'face "csound-font-lock-i")
		    ;; Color P1 values
		    (font-lock-prepend-text-property beg-word end-word 'face
						     (funcall #'csound-font-lock-param-delimiters-default-pick-face depth))
		    (setq depth (1+ depth)))
		;; If passed i marker
		(progn
		  (setq beg-word (min (1- (or (save-excursion (search-forward-regexp "[-?0-9a-zA-Z\\[\\.\\+\\<\\>\"]" (line-end-position) t 1))
					      (line-end-position))))
			end-word (save-excursion
				   (goto-char beg-word)
				   (let ((e (search-forward-regexp "\\s-\\|$" (line-end-position))))
				     (if (< e end-line)
					 e end-line))))
		  (goto-char end-word)
		  (font-lock-prepend-text-property beg-word end-word 'face (funcall #'csound-font-lock-param-delimiters-default-pick-face depth))
		  (setq depth (1+ depth)))))))
        (forward-line)))))

(defun csound-font-lock-fontify-region (beg end &optional loud)
  (save-excursion
    (let ((within-score-p (or (save-excursion (search-backward "<CsScore" nil t 1))
                              (string-match-p ".sco$" (buffer-name (current-buffer)))))
          (score-boundry (if (string-match-p ".sco$" (buffer-name (current-buffer)))
                             (max-char)
                           (or (save-excursion (goto-char (point-min))
                                               (search-forward-regexp "<CsScore" nil t 1))
                               nil)))
          (orchestra-boundry (if (or (string-match-p ".orc$" (buffer-name (current-buffer)))
                                     (string-match-p ".udo$" (buffer-name (current-buffer))))
                                 (buffer-size)
                               (or (save-excursion (goto-char (point-min))
                                                   (search-forward-regexp "</CsInstruments>" end t 1))
                                   (buffer-size)))))
      (if (and within-score-p score-boundry csound-font-lock-rainbow-score-parameters-p)
          (csound-font-lock--fontify-score (max score-boundry beg) (min end (max-char)))
        ;; All normal font-lock calls, but let's keep rainbow delimited fonts untouched
        (let ((end-line (1- (line-number-at-pos (min end (point-max)))))
              (end-line (if (and score-boundry csound-font-lock-rainbow-score-parameters-p)
                            (line-number-at-pos score-boundry)
                          end-line)))
          (goto-char beg)
          (beginning-of-line)
          (while (< (line-number-at-pos) (1+ end-line))
            (save-excursion
              (font-lock-default-fontify-region (line-beginning-position) (line-end-position) nil))
            (forward-line)))))))

(defun csound-font-lock--flush-buffer (&optional start)
  (save-excursion
    (goto-char (or start (point-max)))
    (let ((line-count (line-number-at-pos)))
      (goto-char (point-min))
      (while (< (line-number-at-pos) line-count)
	(save-excursion (font-lock-default-fontify-region (line-beginning-position) (line-end-position) nil))
        (forward-line)))))

(defun csound-font-lock--flush-score (&optional start)
  (when csound-font-lock-rainbow-score-parameters-p
    (save-excursion
      (goto-char (or start (point-min)))
      (let ((score-beg (if (string-match-p ".sco$" (buffer-name (current-buffer)))
			   0
			 (save-excursion (search-forward "<CsScore" nil t 1))))
	    (score-end (or (save-excursion (search-forward "</CsoundSynthesizer>" nil t 1)) (line-number-at-pos (buffer-size)))))
	(when (and score-beg score-end)
	  (csound-font-lock--fontify-score score-beg score-end))))))

(defun csound-font-lock-flush-buffer (&optional start)
  (progn (csound-font-lock--flush-buffer start)
	 (csound-font-lock--flush-score start)))

(provide 'csound-font-lock)

;;; csound-font-lock.el ends here
