;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Csound manual lookup
;;;
;;; an extension enabling interactive lookup of csound function
;;; definitions
;;; Extension to the Emacs Csound-mode
;;;
;;; author: Ruben Philipp
;;; created: 2022-12-26, Lütgendortmund
;;; $$ Last modified:  21:04:37 Mon Dec 26 2022 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'browse-url)
(require 'thingatpt)


(defun csound-manual-lookup ()
  (interactive)
  (let* ((lemma (thing-at-point 'word 'no-properties))
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