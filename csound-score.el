;;; csound-score.el --- A major mode for interacting and coding Csound

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

;; This fine includes all helpers and handling of csound-score events
;; for interactive composition

;;; Code:

(require 'cl-lib)
(require 'csound-font-lock)
(require 'csound-util)
(require 'font-lock)
(require 'dash)

(defun csound-score--align-cols (start end)
  (save-excursion
    (let ((line-end (line-number-at-pos end))
          (max-matrix '()))
      ;; Move all lines to beginning of line
      (goto-char start)
      (beginning-of-line)
      (while (< (line-number-at-pos (point))
                line-end)
        (indent-line-to 0)
        (forward-line))
      (let ((statements (-> (buffer-substring start (line-end-position))
                            (substring-no-properties)
                            (split-string "\n"))))
        ;; Create matrix of max lengths
        (dolist (stm statements)
          ;; Remove comments and extra whitespaces
          (let* ((stm* (->> (replace-regexp-in-string ";.*" "" stm)
                            csound-util-chomp
                            (replace-regexp-in-string "\\s-+" " ")))
                 (param-list (split-string stm* " "))
                 (param-num (length param-list))
                 (max-matrix-len (length max-matrix))
                 (index 0))
            ;; (print stm*)
            (dolist (param param-list)
              (if (<= max-matrix-len index)
                  (setq max-matrix (append max-matrix (list (length param)))
                        index (1+ index))
                (prog2
                    (setf (nth index max-matrix)
                          (max (length param)
                               (nth index max-matrix)))
                    (setq index (1+ index))))))))
      (goto-char start)
      (while (< (line-number-at-pos (point))
                (1+ line-end))
        (beginning-of-line)
        (forward-word)
        (let ((index 0)
              (line-num (line-number-at-pos (point))))
          (while (= (line-number-at-pos (point)) line-num)
            ;; (print (length (thing-at-point 'symbol)))
            (let ((space-beg-point (point))
                  (param-length (length (thing-at-point 'symbol))))
              (when (re-search-forward "\\w\\|\\+" (line-end-position) t 1)
                (backward-char)
                (let ((spaces-to-add (- (1+ (nth index max-matrix))
                                        (+ param-length
                                           (- (point) space-beg-point))))
                      (possibly-after-comment
                       (save-excursion
                         (search-backward-regexp "\\;\\|\\/" (line-beginning-position) t 1))))
                  (if possibly-after-comment
                      (let* ((subvec (cl-subseq max-matrix index))
                             (before-comment-spaces (- (+ (apply #'+ subvec)
                                                          (1+ (length subvec)))
                                                       (+ (- possibly-after-comment
                                                             space-beg-point)
                                                          param-length))))
                        (goto-char possibly-after-comment)
                        ;; Align comments nicely at the end
                        (if (<= 0 before-comment-spaces)
                            (insert (make-string before-comment-spaces  ?\040))
                          (delete-char before-comment-spaces))
                        (forward-line))
                    (prog2 (goto-char space-beg-point)
                        (if (<= 0 spaces-to-add)
                            (insert
                             (make-string spaces-to-add ?\040))
                          (delete-char (abs spaces-to-add)))))))
              (let ((possible-overseen-plus
                     (save-excursion
                       (search-forward "+" (line-end-position) t 1))))
                (forward-word)
                (when possible-overseen-plus
                  (when (and (< possible-overseen-plus (point))
                             (= (line-number-at-pos (point)) line-num))
                    (goto-char possible-overseen-plus)))))
            (setq index (1+ index))))))))


(defun csound-score-align-block ()
  "Align score block so that all
parameter are of same space width."
  (interactive)
  ;; See if point is on an score event line
  (when (save-excursion
          (prog2
              (beginning-of-line 1)
              (search-forward-regexp
               "\\(^\\s-*\\|^\\t-*\\)[if]"
               (line-end-position 1) t 1)))
    ;; Search for beginning of block
    (let ((beginning-of-block nil)
          (line-num-test 1)
          (ending-of-line-at-point (line-end-position 1))
          (beginning-of-line-at-point (line-beginning-position 1))
          (ending-of-block nil))
      (save-excursion
        (while (not (numberp beginning-of-block))
          (goto-char ending-of-line-at-point)
          (end-of-line line-num-test)
          (if (search-backward-regexp
               "\\(^\\s-*\\|^\\t-*\\)[if]"
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
               "\\(^\\s-*\\|^\\t-*\\)[if]"
               (line-end-position 1) t 1)
              (setq line-num-test (1+ line-num-test))
            (setq ending-of-block (line-end-position 0)))))
      (csound-score--align-cols beginning-of-block ending-of-block))))

(defun csound-score-trim-time (score-string)
  (let ((trimmed-string (split-string (substring-no-properties
                                       score-string) "\n"))
        (min-p2 0)
        (closure-list '())
        (final-str "")
        (lex-p2-list '())
        (p2-list '())
        (last-p3 0))
    (dolist (event trimmed-string)
      (lexical-let* ((lexical-p-list (split-string
                                      (replace-regexp-in-string
                                       "\\s-+" " " (csound-util-chomp event))
                                      " "))
                     (lex-last-p3 last-p3)
                     (lex-p2-list (cons (if (< 2 (length lexical-p-list))
                                            (if (string-equal "+" (nth 2 lexical-p-list))
                                                (if (car p2-list)
                                                    (+ (car p2-list) lex-last-p3)
                                                  last-p3)
                                              (if (string-equal "." (nth 2 lexical-p-list))
                                                  (if (car p2-list)
                                                      (car p2-list)
                                                    0)
                                                (string-to-number
                                                 (nth 2 lexical-p-list))))
                                          0)
                                        p2-list)))
        (setq p2-list lex-p2-list
              closure-list (cons
                            (lambda (min-time)
                              (setf (nth 2 lexical-p-list)
                                    (number-to-string
                                     (- (car lex-p2-list)
                                        ;;(nth 2 lexical-p-list)
                                        min-time)))
                              ;; (message "%s lastp3: %s" lex-p2-list lex-last-p3)
                              (string-join lexical-p-list " "))
                            closure-list)
              last-p3 (if (string-equal "." (nth 3 lexical-p-list))
                          last-p3
                        (string-to-number
                         (nth 3 lexical-p-list))))))
    ;; (message "p2-list: %s" p2-list)
    (setq min-p2 (apply #'min p2-list)
          closure-list (reverse closure-list))
    (dolist (event-fn closure-list)
      (setq final-str (concat final-str (funcall event-fn min-p2) "\n")))
    ;; (message "%s" final-str)
    final-str))

(defvar csound-score--last-start)

(defvar csound-score--last-end)

(defun csound-score--flash ()
  (hlt-highlight-region
   csound-score--last-start
   csound-score--last-end
   'font-lock-string-face)
  (run-with-idle-timer
   0.15
   nil
   (lambda ()
     (hlt-unhighlight-region csound-score--last-start csound-score--last-end))))

(defun csound-score-find-instr-def ()
  "For a score statement,
   jump the cursor to where
   its defined in the orchestra.
   Sets a mark."
  (interactive)
  (let* ((instr-on-line (save-excursion
                          (beginning-of-line)
                          (search-forward-regexp "\\<i\\s-?\\(\\\".*\\\"\\|[0-9]+\\)"
                                                 (line-end-position) t 1)
                          (match-string-no-properties 1)))
         (instr-on-line (replace-regexp-in-string "\\\"" "" instr-on-line))
         (search-attempt (save-excursion
                           (goto-char 0)
                           (search-forward-regexp (format "\\<instr\\s-+%s" instr-on-line) nil t 1))))
    (if search-attempt
        (progn (goto-char search-attempt)
               (setq csound-score--last-start (- (point) (length (thing-at-point 'symbol)))
                     csound-score--last-end (1- (point)))
               (csound-score--flash)
               (recenter-top-bottom))
      (message "instrument: %s not found in buffer" instr-on-line))))


(provide 'csound-score)

;;; csound-score.el ends here
