;;; csound-util.el --- A major mode for interacting and coding Csound
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
;; Helper functions needed by various csound-mode files.

;;; Code:

(require 'csound-opcodes)
(require 'dash)

(defun csound-util-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)

(defun csound-util-untab (str)
  (while (string-match "\t" str)
    (setq str (replace-match " " t t str)))
  str)

(defun csound-util-line-boundry ()
  (let ((comment (save-excursion
                   (search-forward ";" (line-end-position 1) t 1)))
        (multi-comment (save-excursion
                         (search-forward "/*" (line-end-position 1) t 1))))
    (cond
     (comment (1- comment))
     (multi-comment (1- (1- multi-comment)))
     (t (line-end-position 1)))))

(defun csound-util-remove-comment-in-string (string)
  (->> string
       (replace-regexp-in-string ";.*" "")
       (replace-regexp-in-string "/\\*\\(.\\|\n\\)*\\*/" "")))

(defun csound-util-recursive-count* (regex string start)
  (if (string-match regex string start)
      (+ 1 (csound-util-recursive-count* regex string (match-end 0)))
    0))

(defun csound-util-recursive-count (regex string start)
  (csound-util-recursive-count* regex (csound-util-remove-comment-in-string string) start))


(defun csound-util--generate-random-uuid ()
  "Insert a random UUID.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d
WARNING: this is a simple implementation.
The chance of generating the same UUID is much higher than a robust algorithm.."
  (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
	  (random (expt 16 4))
	  (random (expt 16 4))
	  (random (expt 16 4))
	  (random (expt 16 4))
	  (random (expt 16 4))
	  (random (expt 16 6))
	  (random (expt 16 6))))

(defun csound-util-strip-text-properties (txt)
  (set-text-properties 0 (length txt) nil txt)
  txt)


(defun csound-util-opcode-completion-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            csdoc-opcode-database
            :exclusive 'no
            :company-docsig (lambda (cand)
			      (csound-util-chomp (replace-regexp-in-string
						  "\n\\|\s+" "\s"
						  (nth 3 (gethash cand csdoc-opcode-database)))))
            :company-doc-buffer (lambda (cand)
				  (prin1-to-string (nth 11 (gethash cand csdoc-opcode-database))))
	    ;;:company-location (lambda (cand) (nth 11 (gethash cand csdoc-opcode-database)))
	    ))))


(provide 'csound-util)

;;; csound-util.el ends here
