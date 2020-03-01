;;; csound-repl-interaction.el --- A major mode for interacting and coding Csound -*- lexical-binding: t; -*-

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

;; Here are all the functionalities that can be used
;; when typing a command to the comint (csound-repl)
;; prompt.

;;; Code:


(require 'csound-score)
(require 'csound-util)
(require 'multi)
(require 'shut-up)

;; (defun csound-repl-interaction--plot (table-num)
;;   (if (not (eq 0 (shut-up
;; 		   (shell-command "gnuplot --version"))))
;;       (error "gnuplot was not found")
;;     (let ((prev-buffer (buffer-name))
;; 	  (tmp-filename (concat "/tmp/" (csound-util--generate-random-uuid) ".png"))
;; 	  (table-str "")
;; 	  (tab-size (csoundTableLength csound-repl--csound-instance table-num))
;; 	  (index 0))
;;       (if (eq -1 tab-size)
;; 	  (error  "Table %d doesn't exist" table-num)
;; 	(progn
;; 	  (while (< index tab-size)
;; 	    (setq table-str (concat table-str
;; 				    (number-to-string index) " "
;; 				    (number-to-string
;; 				     (csoundTableGet
;; 				      csound-repl--csound-instance
;; 				      table-num index)) "\n")
;; 		  index (1+ index)))
;; 	  ;; (setq table-list (string-join table-list " "))
;; 	  ;; (print table-list)
;; 	  (shell-command
;; 	   (concat
;; 	    (format "echo '%s' |" table-str)
;; 	    (format
;; 	     (concat"gnuplot -e \"set term png size 480,320;"
;; 		    (format "set title 'tbl: %s';" table-num)
;; 		    "set tics font ', 10';"
;; 		    "set lmargin at screen 0.15;"
;; 		    "set output '%s';"
;; 		    (format "set xrange [0:%s];" tab-size)
;; 		    "plot '-' notitle with line;"
;; 		    "\"")
;; 	     tmp-filename)))
;; 	  (csound-repl--insert-message (format "\nfile://%s" tmp-filename))
;; 	  (if (string-equal prev-buffer csound-repl-buffer-name)
;; 	      (funcall 'iimage-mode)
;; 	    (progn
;; 	      (switch-to-buffer-other-window csound-repl-buffer-name)
;; 	      (with-current-buffer (buffer-name) (funcall 'iimage-mode))
;; 	      (switch-to-buffer-other-window prev-buffer))))))))

(setq csound-repl-interaction--last-callback nil)

(defun csound-repl-interaction-input-message (csound-udp input)
  (let ((callback (lambda () (process-send-string csound-udp (concat "$" input)))))
    (funcall callback)
    (setq csound-repl-interaction--last-callback callback)))

(defmulti read-csound-repl (op _ &rest _)
  op)

(defmulti-method read-csound-repl 'i (_ csound-udp input)
  (csound-repl-interaction-input-message csound-udp (csound-score-trim-time input)))

(defmulti-method read-csound-repl 'f (_ csound-udp input)
  (csound-repl-interaction-input-message csound-udp input))

(defmulti-method-fallback read-csound-repl (_ csound-udp input)
  (process-send-string csound-udp input))

;; (defmulti-method read-csound-repl 'table (_ csound-udp args)
;;   (let ((callback (lambda ()
;; 		    (csound-repl-interaction--plot
;; 		     (string-to-number (nth 1 args))))))
;;     (funcall callback)
;;     (setq csound-repl-interaction--last-callback callback)))

(defun csound-repl-interaction-evaluate-last-expression ()
  "Evaluate the last expression typed into the repl."
  (interactive)
  (if csound-repl-interaction--last-callback
      (funcall csound-repl-interaction--last-callback)
    (message "Repl history is empty")))

(provide 'csound-repl-interaction)

;;; csound-repl-interaction.el ends here
