;;; csound-mode-tests.el --- Test helper

;;; Commentary: expects tab-mode to be turned off
;;

;;; Code:

(require 'ert-x)
(require 'cl-lib)
(require 'test-simple)

(note "Initializing test information")

(defmacro test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (csound-mode)
     ,@body))

(defun assert-string-equal (expected actual &optional fail-message test-info)
  "Expectation is that ACTUAL should be equal to EXPECTED."
  (assert-op 'string-equal expected actual fail-message test-info))

(test-simple-start)

(note "Make sure the file is found and opened without error")

(assert-t (load-file "./csound-mode.el")
	  "Can't load csound-mode.el - are you in the right directory?" )

(note "Test for minor modes")

(test-with-temp-buffer ""
  (let ((active-modes))
    (declare (indent 2))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (assert-t (memq 'eldoc-mode minor-mode-list))
    (assert-t (memq 'font-lock-mode minor-mode-list))))

(defvar example-csd-1
  "<CsoundSynthesizer>
<CsOptions>
-odac
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1
instr 1
asig poscil .2, p4
out asig, asig
endin
</CsInstruments>
<CsScore>
i 1 0 2 1000
</CsScore>
</CsoundSynthesizer>")

(note "Test fontification")

(test-with-temp-buffer example-csd-1
  (goto-char (point-min))
  ;; <CsoundSynthesizer>
  (with-current-buffer (buffer-name)
    (assert-equal 'csound-font-lock-xml-tags (get-text-property (point) 'face)))
  ;; >sr = 44100
  (goto-line 6)
  (with-current-buffer (buffer-name)
    (assert-equal 'font-lock-builtin-face (get-text-property (point) 'face)))
  ;; sr = 44100<
  (goto-char (line-end-position))
  (with-current-buffer (buffer-name)
    (assert-nil (get-text-property (point) 'face))))

(defvar example-udo-1 ;; From https://github.com/kunstmusik/libsyi/blob/master/adsr140.udo
  "; Gated, Retriggerable Envelope Generator UDO (adsr140)
; Based on design of Doepfer A-140 Envelope Generator Module
; Code based on ADSR code by Nigel Redmon
; (http://www.earlevel.com/main/2013/06/03/envelope-generators-adsr-code/)
; Example by Steven Yi (2015.02.08)
opcode adsr140_calc_coef, k, kk
knum_samps, kratio xin
xout exp( -log((1.0 + kratio) / kratio) / knum_samps)
endop
/* Gated, Re-triggerable ADSR modeled after the Doepfer A-140 */
opcode adsr140, a, aakkkk
agate, aretrig, kattack, kdecay, ksustain, krelease xin
kstate init 0  ; 0 = attack, 1 = decay, 2 = sustain
klasttrig init -1
kval init 0.0
asig init 0
kindx = 0
kattack_base init 0
kdecay_base init 0
krelease_base init 0
kattack_samps init 0
kdecay_samps init 0
krelease_samps init 0
kattack_coef init 0
kdecay_coef init 0
ksustain_coef init 0
klast_attack init -1
klast_decay init -1
klast_release init -1
if (klast_attack != kattack) then
klast_attack = kattack
if(kattack > 0) then
kattack_samps = kattack * sr
kattack_coef = adsr140_calc_coef(kattack_samps, 0.3)
kattack_base = (1.0 + 0.3) * (1 - kattack_coef)
else
kattack_samps = 0
kattack_coef = 0
kattack_base = 0
endif
endif
if (klast_decay != kdecay) then
klast_decay = kdecay
kdecay_samps = kdecay * sr
kdecay_coef = adsr140_calc_coef(kdecay_samps, 0.0001)
kdecay_base = (ksustain - 0.0001) * (1.0 - kdecay_coef)
endif
if (klast_release != krelease) then
klast_release = krelease
krelease_samps = krelease * sr
krelease_coef = adsr140_calc_coef(krelease_samps, 0.0001)
krelease_base =  -0.0001 * (1.0 - krelease_coef)
endif
while (kindx < ksmps) do
if (agate[kindx] > 0) then
kretrig = aretrig[kindx]
if (kretrig > 0 && klasttrig <= 0) then
kstate = 0
endif
klasttrig = kretrig
if (kstate == 0) then
if(kattack <= 0) then
kval = 1.0
kstate = 1
else
kval = kattack_base + (kval * kattack_coef)
if(kval >= 1.0) then
kval = 1.0
kstate = 1
endif
asig[kindx] = kval
endif
elseif (kstate == 1) then
kval = kdecay_base + (kval * kdecay_coef)
if(kval <= ksustain) then
kval = ksustain
kstate = 2
endif
asig[kindx] = kval
else
asig[kindx] = ksustain
endif
else ; in a release state
kstate = 0
if (kval == 0.0) then
asig[kindx] = 0
else
; releasing
kval = krelease_base + (kval * krelease_coef)
if(kval <= 0.0) then
kval = 0.0
endif
asig[kindx] = kval
endif
endif
kindx += 1
od
xout asig
endop")

(defvar example-udo-1-expected
  "; Gated, Retriggerable Envelope Generator UDO (adsr140)
; Based on design of Doepfer A-140 Envelope Generator Module
; Code based on ADSR code by Nigel Redmon
; (http://www.earlevel.com/main/2013/06/03/envelope-generators-adsr-code/)
; Example by Steven Yi (2015.02.08)
opcode adsr140_calc_coef, k, kk
  knum_samps, kratio xin
  xout exp( -log((1.0 + kratio) / kratio) / knum_samps)
endop
/* Gated, Re-triggerable ADSR modeled after the Doepfer A-140 */
opcode adsr140, a, aakkkk
  agate, aretrig, kattack, kdecay, ksustain, krelease xin
  kstate init 0  ; 0 = attack, 1 = decay, 2 = sustain
  klasttrig init -1
  kval init 0.0
  asig init 0
  kindx = 0
  kattack_base init 0
  kdecay_base init 0
  krelease_base init 0
  kattack_samps init 0
  kdecay_samps init 0
  krelease_samps init 0
  kattack_coef init 0
  kdecay_coef init 0
  ksustain_coef init 0
  klast_attack init -1
  klast_decay init -1
  klast_release init -1
  if (klast_attack != kattack) then
    klast_attack = kattack
    if(kattack > 0) then
      kattack_samps = kattack * sr
      kattack_coef = adsr140_calc_coef(kattack_samps, 0.3)
      kattack_base = (1.0 + 0.3) * (1 - kattack_coef)
    else
      kattack_samps = 0
      kattack_coef = 0
      kattack_base = 0
    endif
  endif
  if (klast_decay != kdecay) then
    klast_decay = kdecay
    kdecay_samps = kdecay * sr
    kdecay_coef = adsr140_calc_coef(kdecay_samps, 0.0001)
    kdecay_base = (ksustain - 0.0001) * (1.0 - kdecay_coef)
  endif
  if (klast_release != krelease) then
    klast_release = krelease
    krelease_samps = krelease * sr
    krelease_coef = adsr140_calc_coef(krelease_samps, 0.0001)
    krelease_base =  -0.0001 * (1.0 - krelease_coef)
  endif
  while (kindx < ksmps) do
    if (agate[kindx] > 0) then
      kretrig = aretrig[kindx]
      if (kretrig > 0 && klasttrig <= 0) then
        kstate = 0
      endif
      klasttrig = kretrig
      if (kstate == 0) then
        if(kattack <= 0) then
          kval = 1.0
          kstate = 1
        else
          kval = kattack_base + (kval * kattack_coef)
          if(kval >= 1.0) then
            kval = 1.0
            kstate = 1
          endif
          asig[kindx] = kval
        endif
      elseif (kstate == 1) then
        kval = kdecay_base + (kval * kdecay_coef)
        if(kval <= ksustain) then
          kval = ksustain
          kstate = 2
        endif
        asig[kindx] = kval
      else
        asig[kindx] = ksustain
      endif
    else ; in a release state
      kstate = 0
      if (kval == 0.0) then
        asig[kindx] = 0
      else
        ; releasing
        kval = krelease_base + (kval * krelease_coef)
        if(kval <= 0.0) then
          kval = 0.0
        endif
        asig[kindx] = kval
      endif
    endif
    kindx += 1
  od
  xout asig
endop")

(note "Test indentation")

(test-with-temp-buffer example-udo-1
  (goto-char (point-min))
  (indent-region (point-min) (point-max))
  (assert-string-equal
   example-udo-1-expected
   (buffer-substring-no-properties (point-min) (point-max))))

(end-tests)
