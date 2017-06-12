;;; csound-skeleton.el

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
;;; Skeleton for when creating new .csd file.

;; Initialize defaults values


(defcustom csound-skeleton-default-sr 44100
  "Set the default sr value when creating new csound file."
  :type 'integer
  :group 'csound-mode)

(defcustom csound-skeleton-default-ksmps 32
  "Set the default ksmps value when creating new csound file."
  :type 'integer
  :group 'csound-mode)


(define-skeleton csound-new-csd
  "Throwaway C skeleton"
  nil
  "<CsoundSynthesizer>\n"
  "<CsOptions>\n</CsOptions>\n"
  "<CsInstruments>\n\n"
  (concat "sr = " (number-to-string csound-skeleton-default-sr) "\n")
  (concat "ksmps = " (number-to-string csound-skeleton-default-ksmps) "\n") 
  "nchnls = 2\n"
  "0dbfs = 1.0\n"
  "\n\n\n"
  "</CsInstruments>\n"
  "<CsScore>\n"
  "</CsScore>\n"
  "</CsoundSynthesizer>\n")


(provide 'csound-skeleton)

;;; csound-skeleton.el ends here
