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
;;; $$ Last modified:  16:40:01 Tue Oct 24 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'csound-opcodes)
(require 'csound-util)
(require 'cl-lib)
(require 'browse-url)
(require 'thingatpt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The url to the Csound manual
;;; Customizing this could be useful e.g. when the manual is
;;; installed locally.
;;; RP  Wed Sep 20 19:51:22 2023
;;; Updated by Brandon Hale April 25, 2026
(defcustom csound-manual-url
  "https://csound.com/docs/manual/"
  "The URL to the root directory of the Csound manual."
  :group 'csound-mode-manual-lookup
  :type 'string)

(defcustom csound-browse-manual-url
  "https://csound.com/docs/manual/index.html"
  "The URL to the index of the Csound manual, useful for browsing of opcodes and learning the language."
  :group 'csound-mode-manual-lookup
  :type 'string)

(defcustom csound-browse-gen-manual-url
  "https://csound.com/docs/manual/ScoreGenRef.html"
  "The URL to the GEN routines of the Csound manual, useful for browsing of routines."
  :group 'csound-mode-manual-lookup
  :type 'string)

(defun csound-manual-lookup ()
  (interactive)
  (let* ((lemma (thing-at-point 'word 'no-properties))
         (lookup-lemma (if (gethash lemma
                                    csdoc-opcode-database)
                           (downcase lemma)
                         (read-string "Lookup function in Csound manual: "))))
    (browse-url (concat csound-manual-url
                        lookup-lemma
                        ".html"))))

(defun csound-gen-manual-lookup ()
  (interactive)
  (let* ((cursor-text (thing-at-point 'number 'no-properties))
	 (user-input (if (not cursor-text)
			 (read-string "Lookup GEN in Csound manual: ")
		       (number-to-string cursor-text)))
	 (use-input (if (= (length user-input) 1)
			(concat "0" user-input)
		      user-input)))
    (browse-url (concat csound-manual-url "GEN" use-input ".html"))))

(defun csound-browse-manual ()
  (interactive)
  (browse-url csound-browse-manual-url))

(defun csound-browse-gen-manual ()
  (interactive)
  (browse-url csound-browse-gen-manual-url))

;;; key binding

(with-eval-after-load 'csound-mode
  (define-key csound-mode-map (kbd "C-c C-d h") 'csound-manual-lookup)
  (define-key csound-mode-map (kbd "C-c C-d g") 'csound-gen-manual-lookup)
  (define-key csound-mode-map (kbd "C-c g") 'csound-browse-gen-manual)
  (define-key csound-mode-map (kbd "C-c m") 'csound-browse-manual))

(provide 'csound-manual-lookup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF csound-manual-lookup.el
