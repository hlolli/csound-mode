;;; csound-eldoc.el --- A major mode for interacting and coding Csound
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

;;  Eldoc functionality for csound-mode

;;; Code:


(require 'csound-opcodes)
(require 'csound-util)
(require 'cl-lib)

(defun csound-eldoc-get-template (opcode-list)
  (let ((templ nil)
        (indx 0))
    (while (and (< indx (length opcode-list))
                (eq templ nil))
      (when (eq :template (nth indx opcode-list))
        (setq templ t))
      (setq indx (1+ indx)))
    (if templ
        (nth indx opcode-list)
      "")))

(defun csound-eldoc-line-escape-count ()
  (save-excursion
    (let ((linenums 1))
      (while (search-backward-regexp "\\\\.*\n" (line-end-position -1) t)
        (setq linenums (1- linenums)))
      linenums)))

(defun csound-eldoc-statement ()
  (save-excursion
    (let ((countback (csound-eldoc-line-escape-count)))
      (buffer-substring
       (line-beginning-position countback)
       (csound-util-line-boundry)))))

(defun csound-eldoc-statement-list (string-statement)
  (split-string
   (csound-util-untab (csound-util-chomp string-statement))
   "\\(,+\s*\\)+\\|\\(\s+,*\\)+"))

(defun csound-eldoc-template-lookup (statement-list)
  (let ((result nil)
        (opdoce nil)
        (last-open-paren (save-excursion (search-backward "(" (line-beginning-position) t 1)))
        (last-close-paren (save-excursion (search-backward ")" (line-beginning-position) t 1)))
        (cand nil)
        (opcode nil)
        (rate-match nil)
        (rate-cand nil)
        (functional-syntax-p nil))
    ;; Functional syntax lookup
    (when (and last-open-paren
               (> last-open-paren
                  (or last-close-paren
                      (line-beginning-position))))
      (save-excursion (progn (setq cand (thing-at-point 'symbol (search-backward-regexp "(" (line-beginning-position) t 1)))
                             (when (= 1 (length cand))
                               (setq rate-cand cand))
                             (while (or (and (not cand)
                                             (not (eq (point) (line-beginning-position))))
                                        (= 1 (length cand)))
                               (setq cand (thing-at-point 'symbol))
                               (backward-char))
                             (when (gethash cand csdoc-opcode-database)
                               (setq result (csound-eldoc-get-template
                                             (gethash cand csdoc-opcode-database))
                                     opcode cand
                                     functional-syntax-p t)))))
    ;; Normal statement lookup
    (when (not result)
      (dolist (statement statement-list)
        (when (gethash statement csdoc-opcode-database)
          (setq result (csound-eldoc-get-template
                        (gethash statement csdoc-opcode-database))
                opcode statement))))
    (when result
      (let ((rate-list (split-string (replace-regexp-in-string "\n\s" "\n" result) "\n")))
        (if (= (length rate-list) 1)
            (list opcode (car rate-list) functional-syntax-p)
          (let ((rate-candidate (or rate-cand (substring (car statement-list) 0 1))))
            (dolist (xrate rate-list)
              (when (string= rate-candidate (substring xrate 0 1))
                (setq rate-match xrate)))
            (if rate-match
                (list opcode rate-match functional-syntax-p)
              (list opcode (car rate-list) functional-syntax-p))))))))


(defun csound-eldoc-argument-index (opcode-match opcode-index point-on-opcode?)
  (if point-on-opcode?
      0
    (save-excursion
      (let* ((statement (buffer-substring
                         (line-beginning-position (csound-eldoc-line-escape-count))
                         (point)))
             (statement (replace-regexp-in-string "(.*)" "" statement))
             (komma-format-list (split-string
                                 (replace-regexp-in-string
                                  opcode-match
                                  (concat "," opcode-match ",")
                                  statement) ","))
             (indx 0)
             (pos nil))
        (dolist (i komma-format-list)
          (if (string= opcode-match i)
              (setq indx 0
                    pos t)
            (if pos
                (setq indx (1+ indx))
              (setq indx (1- indx)))))
        indx))))

(defun csound-eldoc-opcode-index (opcode-match template-list)
  (let ((indx 0)
        (match? nil))
    (while (and (< indx (length template-list))
                (not match?))
      (if (string= (nth indx template-list)
                   opcode-match)
          (setq match? t)
        (setq indx (1+ indx))))
    indx))


;;;###autoload
(defun csound-eldoc-function ()
  "Returns a doc string appropriate for the current context, or nil."
  (let* ((csound-statement (csound-eldoc-statement))
         (statement-list (csound-eldoc-statement-list csound-statement))
         (template-lookup (csound-eldoc-template-lookup statement-list)))
    (when template-lookup
      (let* ((opcode-match (car template-lookup))
             (point-on-opcode? (string= opcode-match (thing-at-point 'symbol)))
             (csound-template (replace-regexp-in-string
                               "[^\\[]\\.\\.\\." ""
                               (replace-regexp-in-string
                                "\\[, " "["
                                (nth 1 template-lookup))))
             (template-list (csound-eldoc-statement-list csound-template))
             (template-list-length (1- (length template-list)))
             (opcode-index (csound-eldoc-opcode-index opcode-match template-list))
             (template-list (if (nth 2 template-lookup)
                                (cl-subseq template-list opcode-index)
                              template-list))
             (argument-index (csound-eldoc-argument-index opcode-match opcode-index point-on-opcode?))
             (infinite-args? (string= "[...]" (car (last template-list))))
             (indx -1)
             (list-index 0)
             (eldocstr "")
             (inf-arg nil))
        (dolist (arg template-list)
          (setq
           inf-arg (if (and infinite-args?
                            (< template-list-length argument-index))
                       t nil)
           eldocstr (concat eldocstr
                            (when (string= arg opcode-match)
                              (put-text-property 0 (length arg) 'face
                                                 (list :foreground "#C70039"
                                                       :weight (if point-on-opcode?
                                                                   'bold 'normal))
                                                 arg))
                            (if (or (and (= indx argument-index)
                                         ;;(string= arg (car (last template-list)))
                                         (not point-on-opcode?))
                                    (and inf-arg (string= "[...]" arg)))
                                (prog2 (put-text-property 0 (length arg) 'face '(:foreground "#A4FF00" :weight bold) arg)
                                    arg)
                              arg)
                            (if (or (eq template-list-length list-index)
                                    (string= arg opcode-match)
                                    (string= opcode-match (nth (1+ list-index) template-list))
                                    (string= "=" arg))
                                " "
                              ", "))
           indx (if (string= arg opcode-match) 1
                  (if (string= "=" arg)
                      indx
                    (if (> 0 indx)
                        (1- indx)
                      (1+ indx))))
           list-index (1+ list-index)))
        eldocstr))))


(provide 'csound-eldoc)

;;; csound-eldoc.el ends here
