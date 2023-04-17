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
;;; 2022-12-26, Lütgendortmund
;;; 
;;; $$ Last modified:  23:58:26 Mon Apr 17 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
