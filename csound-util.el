;;; csound-util.el
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
;;; Helper functions needed by various csound-mode files.

;;; Code:


(defun csound-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)

(defun csound-untab (str)
  (while (string-match "\t" str)
    (setq str (replace-match " " t t str)))
  str)

(defun csound-recursive-count (regex string start)
  (if (string-match regex string start)
      (+ 1 (csound-recursive-count regex string (match-end 0)))
    0))

(provide 'csound-util)

;;; csound-mode.el ends here
