;; Initialize defaults values
(setq csound-skeleton-default-sr "44100"
      ;; csound-skeleton-default-kr "1378"
      csound-skeleton-default-ksmps "32")


;; See if global configuarion exists
;; and overwrite the defaults
(let* ((file (with-temp-buffer
	       (insert-file-contents "~/.csound6rc")
	       (buffer-string))))
  (when file
    (let* ((sr (match-data (string-match "\\(-r[[:space:]]?\\)\\([[:digit:]]+\\)" file)))) 
      (when sr
	(setq csound-skeleton-default-sr (substring file (nth 4 sr) (nth 5 sr)))))))


(define-skeleton csound-new-csd
  "Throwaway C skeleton"
  nil
  "<CsoundSynthesizer>\n"
  "<CsOptions></CsOptions>\n"
  "<CsInstruments>\n\n"
  (concat " sr = " csound-skeleton-default-sr "\n")
  (concat " ksmps = " csound-skeleton-default-ksmps "\n") 
  " nchnls = 2\n"
  " 0dbfs = 1.0\n"
  "\n\n\n"
  "</CsInstruments>\n"
  "<CsScore>\n"
  "f1 0 8192 10 1\n\n\n\n"
  "</CsScore>\n"
  "</CsoundSynthesizer>\n")


(provide 'csound-skeleton)
