;;; csound-interaction.el

;; Copyright (C) 2016  Hlöðver Sigurðsson

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

(require 'csound-opcodes)

(defcustom csound-mode--repl-name "*CSOUND REPL*"
  "Buffer name given to the csound-mode repl."
  :group 'csound-mode
  :type 'constant)

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)


(defun csound-repl--buffer-already-exists? ()
  (progn (setq indx 0
	       exists? nil)
	 (while (and (< indx (length (buffer-list)))
		     (not exists?))
	   (if (string-match
		csound-mode--repl-name
		(buffer-name (nth indx (buffer-list))))
	       (setq exists? t)
	     (setq indx (1+ indx))))
	 exists?))

(defun csound-repl--create-buffer ()
  (interactive)
  (when (not (csound-repl--buffer-already-exists?))
    (save-excursion
      (generate-new-buffer
       csound-mode--repl-name)
      (split-window-sensibly)
      (switch-to-buffer-other-window csound-mode--repl-name))))

(defun csound-repl--python-version ()
  (let* ((python-version (shell-command-to-string "python -V ")))
    (if (string-match "command not found" python-version)
	(message "NOT INSTALLED, changeme")
      (message (chomp python-version)))))


(defcustom csound-manual-html-directory
  (expand-file-name "~/csound/manual/html/")
  "Root directory of the csound manual,
   download here:
   https://github.com/csound/manual"
  :group 'csound-mode
  :type 'file)

(defun csound-thing-at-point-doc ()
  (interactive)
  (message
   (gethash (thing-at-point 'symbol)
	    csdoc-opcode-database)))

(provide 'csound-interaction)
;;; csound-interaction.el ends here
