;;; csound-score.el --- A major mode for interacting and coding Csound

;; Copyright (C) 2017  Hlöðver Sigurðsson

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

;; See README.md (https://github.com/hlolli/csound-mode/blob/master/README.md)

;;; Code:

(require 'csound-font-lock)
(require 'csound-util)

;; align-cols
;; Author: Matthias Helmling <matt@acid.rhein-neckar.de>

(defun csound-score-align-cols (start end max-cols)
  "Align text between point and mark as columns.
Columns are separated by whitespace characters.
Prefix arg means align that many columns. (default is all)"
  (interactive "r\nP")
  (save-excursion
    (let ((p start)
	  pos
	  end-of-line
	  word
	  count
	  (max-cols (if (numberp max-cols) (max 0 (1- max-cols)) nil))
	  (pos-list nil)
	  (ref-list nil))
      ;; find the positions
      (goto-char start)
      (while (< p end)
	(beginning-of-line)
	(setq count 0)
	(setq end-of-line (save-excursion (end-of-line) (point)))
	(re-search-forward "^\\s-*" end-of-line t)
	(setq pos (current-column))	;start of first word
	(if (null (car ref-list))
	    (setq pos-list (list pos))
	  (setq pos-list (list (max pos (car ref-list))))
	  (setq ref-list (cdr ref-list)))
	(while (and (if max-cols (< count max-cols) t)
		    (re-search-forward "\\s-+" end-of-line t))
	  (setq count (1+ count))
	  (setq word (- (current-column) pos))
	  ;; length of next word including following whitespaces
	  (setq pos (current-column))
	  (if (null (car ref-list))
	      (setq pos-list (cons word pos-list))
	    (setq pos-list (cons (max word (car ref-list)) pos-list))
	    (setq ref-list (cdr ref-list))))
	(while ref-list
	  (setq pos-list (cons (car ref-list) pos-list))
	  (setq ref-list (cdr ref-list)))
	(setq ref-list (nreverse pos-list))
	(forward-line)
	(setq p (point)))
      ;; aling the cols starting with last row
      (setq pos-list (copy-sequence ref-list))
      (setq start 
	    (save-excursion (goto-char start) (beginning-of-line) (point)))
      (goto-char end)
      (beginning-of-line)
      (while (>= p start)
	(beginning-of-line)
	(setq count 0)
	(setq end-of-line (save-excursion (end-of-line) (point)))
	(re-search-forward "^\\s-*" end-of-line t)
	(goto-char (match-end 0))
	(setq pos (nth count pos-list))
	(while (< (current-column) pos)
	  (insert-char ?\040 1))
	(setq end-of-line (save-excursion (end-of-line) (point)))
	(while (and (if max-cols (< count max-cols) t)
		    (re-search-forward "\\s-+" end-of-line t))
	  (setq count (1+ count))
	  (setq pos   (+  pos (nth count pos-list)))
	  (goto-char (match-end 0))
	  (while (< (current-column) pos)
	    (insert-char ?\040 1))
	  (setq end-of-line (save-excursion (end-of-line) (point))))
	(forward-line -1)
	(if (= p (point-min)) (setq p (1- p))
	  (setq p (point)))))))
;; end of align-cols

(defun csound-score-align-block ()
  "Align score block so that all
parameter are of same space width."
  (interactive)
  ;; See if point is on an score event line
  (when (save-excursion
	  (prog2
	      (beginning-of-line 1)
	      (search-forward-regexp
	       "\\(^\\s-*\\|^\\t-*\\)i+[0-9\\\".*]*\\b"
	       (line-end-position 1) t 1)))
    ;; Search for beginning of block
    (setq beginning-of-block nil
	  line-num-test 1 
	  ending-of-line-at-point (line-end-position 1)
	  beginning-of-line-at-point (line-beginning-position 1)
	  ending-of-block nil)
    (save-excursion
      (while (not (numberp beginning-of-block))
	(goto-char ending-of-line-at-point)
	(end-of-line line-num-test)
	(if (search-backward-regexp
	     "\\(^\\s-*\\|^\\t-*\\)i+[0-9\\\".*]*\\b"
	     (line-beginning-position 1) t 1)
	    (setq line-num-test (1- line-num-test))
	  (setq beginning-of-block (line-beginning-position 2)))))
    (setq line-num-test 1)
    ;; Search for ending of block
    (save-excursion
      (while (not (numberp ending-of-block))
	(goto-char beginning-of-line-at-point)
	(beginning-of-line line-num-test)
	(if (search-forward-regexp
	     "\\(^\\s-*\\|^\\t-*\\)i+[0-9\\\".*]*\\b"
	     (line-end-position 1) t 1)
	    (setq line-num-test (1+ line-num-test))
	  (setq ending-of-block (line-end-position 0)))))
    (csound-score-align-cols beginning-of-block ending-of-block 100)))



(provide 'csound-score)
;;; csound-score.el ends here
