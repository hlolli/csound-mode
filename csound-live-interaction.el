;;; csound-interaction.el

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


(require 'csound-opcodes)
(require 'comint)
(require 'async)
(require 'font-lock)
(require 'csound-font-lock)

(module-load "emacscsnd.so")

(defcustom csound-mode--message-buffer-name "*Csound Messages*"
  "Buffer name given to the csound-mode repl."
  :group 'csound-mode
  :type 'constant)


(defun csound-mode--message-buffer-already-exists? ()
  (progn (setq indx 0
	       exists? nil)
	 (while (and (< indx (length (buffer-list)))
		     (not exists?))
	   (if (string-match
		csound-mode--message-buffer-name
		(buffer-name (nth indx (buffer-list))))
	       (setq exists? t)
	     (setq indx (1+ indx))))
	 exists?))

(defun csound-mode--message-buffer-create ()
  (when (not (csound-mode--message-buffer-already-exists?))
    (let ((prev-buffer (buffer-name)))
      (save-excursion
	(generate-new-buffer
	 csound-mode--message-buffer-name)
	(split-window-sensibly)
	(switch-to-buffer-other-window csound-mode--message-buffer-name)
	(with-current-buffer (buffer-name) (funcall 'csound-interactive-mode))
	(switch-to-buffer-other-window prev-buffer)))))

;; (csound-mode--message-buffer-create)

(defun generate-random-uuid ()
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


(defconst csound-live-interaction-prompt
  (let ((prompt "csnd> ")) 
    (put-text-property 0 (length prompt) 'read-only t prompt)
    prompt))

(defvar csound-live-interaction--input nil)

(defvar csound-live-interaction--input-history (make-hash-table :test 'equal))

(defun csound-live-interaction--input-sender (proc input)
  (unless (eq 0 (length input)) 
    (let ((id (generate-random-uuid))
	  (buffer-read-only nil)
	  (lb (- (line-beginning-position) 5))
	  (return-val (-> input read eval)))
      (comint-output-filter proc (format "%s\n" return-val))
      (push (cons id input) csound-live-interaction--input)
      (message "%s" input)
      (puthash id (process-buffer proc) csound-live-interaction--input-history)))
  (comint-output-filter proc csound-live-interaction-prompt))



(setq csound-interactive--welcome-message
      (let ((s (concat ";; Welcome to Csound interactive message buffer.\n\n")))
	(put-text-property 0 (length s) 'face 'font-lock-comment-face s)
	s))

(define-derived-mode
  csound-interactive-mode comint-mode "CsoundInteractive"
  "Csound Interactive Message Buffer and REPL."
  :syntax-table csound-mode-syntax-table
  ;;(setq comint-prompt-regexp (concat "^" (regexp-quote elnode-ijs-prompt)))
  (setq comint-input-sender 'csound-live-interaction--input-sender)  
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (let ((csnd-proc (start-process "csnd" (current-buffer) "hexl")))
      (set-process-query-on-exit-flag csnd-proc nil)
      (setq-local font-lock-defaults '(csound-font-lock-list))
      (insert csound-interactive--welcome-message)
      (set-marker
       (process-mark csnd-proc) (point))
      (comint-output-filter csnd-proc csound-live-interaction-prompt))))


;; (async-start 
;;  (lambda ()
;;    (while (< 0 (csoundGetMessageCnt csound))
;;      (save-excursion
;;        (switch-to-buffer csound-mode--message-buffer-name)
;;        (goto-char (buffer-size))
;;        (beginning-of-line)
;;        (insert (csoundGetFirstMessage csound))
;;        (switch-to-prev-buffer)
;;        (csoundPopFirstMessage csound))))
;;  (lambda () nil))


;; (async-start
;;  (lambda ()
;;    (test-csoundAPI)
;;    (sleep-for 3)
;;    nil)
;;  (lambda (result)
;;    (message "Finish")))

;; (async-start
;;  ;; What to do in the child process
;;  (lambda ()
;;    (message "This is a test")
;;    (sleep-for 3)
;;    222)

;;  ;; What to do when it finishes
;;  (lambda (result)
;;    (message "Async process done, result should be 222: %s"
;; 	    result)))


(defun test-csoundAPI ()
  (csoundInitialize (logior CSOUNDINIT_NO_ATEXIT
			    CSOUNDINIT_NO_SIGNAL_HANDLER))

  (setq csound (csoundCreate))
  
  (csoundCreateMessageBuffer csound 0)

  (csound-mode--message-buffer-create)
  
  (csoundSetOption csound "-odac")
  
  (setq orc "
sr=44100
ksmps=32
nchnls=2
0dbfs=1
instr 1 
aout vco2 0.5, 440
outs aout, aout
endin")

  (csoundCompileOrc csound orc)

  (setq sco "i1 0 1")

  (csoundReadScore csound sco)

  (csoundStart csound)
  (while (eq 0 (csoundPerformKsmps csound)))
  (csoundStop csound)
  (csoundPerform csound)
  )

;; (defcustom csound-manual-html-directory
;;   (expand-file-name "~/csound/manual/html/")
;;   "Root directory of the csound manual,
;;    download here:
;;    https://github.com/csound/manual"
;;   :group 'csound-mode
;;   :type 'file)

;; (defun csound-thing-at-point-doc ()
;;   (interactive)
;;   (message
;;    (gethash (thing-at-point 'symbol)
;; 	    csdoc-opcode-database)))



(progn (csound-mode--message-buffer-create))

(provide 'csound-live-interaction)

;;; csound-interaction.el ends here

