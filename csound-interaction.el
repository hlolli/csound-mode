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
