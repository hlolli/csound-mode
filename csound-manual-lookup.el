;;; csound-eldoc.el --- A major mode for interacting and coding Csound
;;  Copyright (C) 2017 - 2023  Hlöðver Sigurðsson

;; Author: Hlöðver Sigurðsson <hlolli@gmail.com>
;; Version: 0.2.7
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

;;  This module implements Csound manual lookup functionality for 
;;; csound-mode. 

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Csound manual lookup
;;;
;;; DESCRIPTION
;;; This extension enables the interactive lookup of Csound functions in the
;;; Csound reference manual located at the Csound homepage.
;;; 
;;; TODO:
;;; - Implement a global variable referring to the Csound manual base url.
;;;   This might be handy e.g. when the manual is installed locally.
;;;
;;; AUTHOR
;;; Ruben Philipp
;;;
;;; CREATED
;;; 2023-12-26, Lütgendortmund
;;; 
;;; $$ Last modified:  00:57:30 Wed Apr 26 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'csound-opcodes)
(require 'csound-util)
(require 'cl-lib)
(require 'browse-url)
(require 'thingatpt)


(defun csound-manual-lookup ()
  (interactive)
  (let* ((lemma (thing-at-point 'word 'no-properties))
         ;;; cf. TODO
         ;;; RP  Mon Apr 17 23:58:01 2023
         (csound-manual-url "http://www.csounds.com/manual/html/")
         (lookup-lemma (if (gethash lemma
                                    csdoc-opcode-database)
                           (downcase lemma)
                         (read-string "Lookup function in Csound manual: "))))
    (browse-url (concat csound-manual-url
                        lookup-lemma
                        ".html"))))

;;; key binding

(eval-after-load 'csound-mode
  '(define-key csound-mode-map (kbd "C-c C-d h") 'csound-manual-lookup))


(provide 'csound-manual-lookup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF csound-manual-lookup.el
