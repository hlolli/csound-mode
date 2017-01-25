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
  "<CsOptions></CsOptions>\n"
  "<CsInstruments>\n\n"
  (concat " sr = " (number-to-string csound-skeleton-default-sr) "\n")
  (concat " ksmps = " (number-to-string csound-skeleton-default-ksmps) "\n") 
  " nchnls = 2\n"
  " 0dbfs = 1.0\n"
  "\n\n\n"
  "</CsInstruments>\n"
  "<CsScore>\n"
  "f1 0 8192 10 1\n\n\n\n"
  "</CsScore>\n"
  "</CsoundSynthesizer>\n")


(provide 'csound-skeleton)
