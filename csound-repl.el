;;; csound-repl.el --- A major mode for interacting and coding Csound

;; Copyright (C) 2017  Hlöðver Sigurðsson

;; Author: Hlöðver Sigurðsson <hlolli@gmail.com>
;; Version: 0.1.2
;; Package-Requires: ((emacs "25") (shut-up "0.3.2") (multi "2.0.1"))


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
;; Repl functionality for csound-mode

;;; Code:

(require 'comint)
(require 'csound-font-lock)
(require 'csound-opcodes)
(require 'csound-repl-interaction)
(require 'csound-util)
(require 'font-lock)
(require 'shut-up)
(require 'gnuplot)

(defvar csound-repl--csound-instance nil)

;; For flash effects, expression variables,
;; need to live longer than the funcall
(setq-local csound-repl--expression-start 0)
(setq-local csound-repl--expression-end 0)
(setq-local csound-repl--expression-tmp-buffer-size 0)

(defvar csound-repl--process-tty-name
  ""
  "tty-name of the comnit process that
  communicates with Csound instance.")

(defcustom csound-repl-buffer-name "*Csound REPL*"
  "Buffer name given to the csound-mode repl."
  :group 'csound-mode-repl
  :type 'string)

(defcustom csound-repl-sr 44100
  "Sample rate of the csound repl"
  :group 'csound-mode-repl
  :type 'integer)

(defcustom csound-repl-ksmps 32
  "ksmps value of the csound repl"
  :group 'csound-mode-repl
  :type 'integer)

(defcustom csound-repl-nchnls 2
  "Number of out channels for the csound repl"
  :group 'csound-mode-repl
  :type 'integer)

(defcustom csound-repl-0dbfs 1
  "0dbfs value of the csound repl"
  :group 'csound-mode-repl
  :type 'integer)

(defun csound-repl-get-options ()
  (format "sr=%d\nksmps=%d\nnchnls=%d\n0dbfs=%d"
	  csound-repl-sr
	  csound-repl-ksmps
	  csound-repl-nchnls
	  csound-repl-0dbfs))

(defun csound-repl-buffer-running-p ()
  (let ((indx 0)
	(exists? nil))
    (while (and (< indx (length (buffer-list)))
		(not exists?))
      (if (string-match
	   csound-repl-buffer-name
	   (buffer-name (nth indx (buffer-list))))
	  (setq exists? t)
	(setq indx (1+ indx))))
    exists?))

(defun csound-repl--buffer-create ()
  (when (not (csound-repl-buffer-running-p))
    (let ((prev-buffer (buffer-name)))
      (save-excursion
	(generate-new-buffer
	 csound-repl-buffer-name)
	(split-window-sensibly)
	(switch-to-buffer-other-window csound-repl-buffer-name)
	(with-current-buffer (buffer-name) (funcall 'csound-interactive-mode))
	(switch-to-buffer-other-window prev-buffer)))))

(defun csound-repl-last-visited-csd ()
  "This decides which filename is given to repl buffer.
   Returns a list of (buffer-name absolute-path)"
  (shut-up
    (let ((indx--last-visited 0)
	  (match-p nil)
	  (last-file "not-found")
	  (len (length (buffer-list))))
      (while (and (< indx--last-visited len)
		  (not match-p))
	(if (string-match-p ".csd$" (buffer-name (nth indx--last-visited (buffer-list))))
	    (prog2
		(setq-local last-file (list
				       (buffer-name
					(nth indx--last-visited (buffer-list)))
				       (file-name-directory
					(buffer-file-name
					 (nth indx--last-visited (buffer-list)))))) 
		(setq-local match-p t))
	  (setq-local indx--last-visited (1+ indx--last-visited))))
      last-file)))


(defconst csound-repl-prompt
  (let ((prompt "csnd> ")) 
    (put-text-property 0 (length prompt) 'read-only t prompt)
    prompt))

(defvar csound-repl--input nil)

(defvar csound-repl--input-history (make-hash-table :test 'equal))


(defun csound-repl--input-sender (proc input)
  (unless (eq 0 (length input)) 
    (let ((id (csound-util--generate-random-uuid))
	  (buffer-read-only nil)
	  (lb (- (line-beginning-position) 5))
	  (split-input (-> input csound-util-chomp split-string)))
      (read-csound-repl (intern (first split-input)) csound-repl--csound-instance split-input)
      ;; (comint-output-filter proc (format "%s\n" return-val))
      (push (cons id input) csound-repl--input)
      ;; (message "%s" input)
      (puthash id (process-buffer proc) csound-repl--input-history)
      ;; (save-excursion (set-buffer csound-repl-buffer-name) (point-max))
      ))
  (comint-output-filter proc csound-repl-prompt))

(defun csound-repl--generate-welcome-message (cur-file)
  (let* ((csound-repl---welcome-title
	  (concat "  __   __   __             __             __   __   __ \n"
		  " /    /    /  | /  | /| ||/  |      /|/| /  ||/  | /   \n"
		  "(    (___ (   |(   |( | ||   | ___ ( / |(   ||   |(___ \n"
		  "|   )    )|   )|   )| | )|   )     |   )|   )|   )|    \n"
		  "|__/  __/ |__/ |__/ | |/ |__/      |  / |__/ |__/ |__  \n"))
	 (s (format (concat "\n"
			    ";;  csound-mode 0.1.2\n"
			    ";;  file: " cur-file "\n"
			    ";;  sr: %d\n"
			    ";;  ksmps: %d\n"
			    ";;  nchnls: %d\n"
			    ";;  0dbfs: %d\n\n\n")
		    csound-repl-sr
		    csound-repl-ksmps
		    csound-repl-nchnls
		    csound-repl-0dbfs)))
    (concat csound-repl---welcome-title s)))

(defun csound-repl--set-default-dir-options ()
  (let ((filedir (nth 1 (csound-repl-last-visited-csd))))
    (mapc (lambda (opt)
	    (csoundSetOption csound-repl--csound-instance
			     (format "--env:%s+=;%s"
				     opt filedir)))
	  '("INCDIR" "SFDIR"
	    "SSDIR" "SADIR"
	    "MFDIR"))))

(defun csound-repl--restart ()
  (when csound-repl--csound-instance
    (progn
      (setq csound-repl--csound-instance (csoundCreate))
      (sleep-for 0.1)
      (csoundInitialize (logior CSOUNDINIT_NO_ATEXIT
				CSOUNDINIT_NO_SIGNAL_HANDLER))
      (when (not (string-empty-p csound-repl--process-tty-name))
      	(csoundMessageTty csound-repl--csound-instance csound-repl--process-tty-name))
      (csound-repl--set-default-dir-options)
      (csoundSetOption csound-repl--csound-instance "-odac")
      (csoundSetOption csound-repl--csound-instance "-d")
      (csoundCompileOrc csound-repl--csound-instance (csound-repl-get-options))
      (csoundInputMessage csound-repl--csound-instance "e 0 360000")
      ;; TODO make this customizeable or automatic
      (csoundStart csound-repl--csound-instance)
      (csoundAsyncPerform csound-repl--csound-instance))))

(defun csound-repl--boot-instance ()
  (if csound-repl--csound-instance
      (csound-repl--restart)
    (progn
      (csoundInitialize (logior CSOUNDINIT_NO_ATEXIT
				CSOUNDINIT_NO_SIGNAL_HANDLER))
      (setq csound-repl--csound-instance (csoundCreate))
      (sleep-for 0.1)
      (when (not (string-blank-p csound-repl--process-tty-name))
	(csoundMessageTty csound-repl--csound-instance csound-repl--process-tty-name))
      (csound-repl--set-default-dir-options)
      (csoundSetOption csound-repl--csound-instance "-odac")
      (csoundSetOption csound-repl--csound-instance "-d")
      ;; TODO make this customizeable or automatic
      (csoundCompileOrc csound-repl--csound-instance (csound-repl-get-options))
      ;; (csoundReadScore csound-repl--csound-instance "e 0 3600")
      (csoundInputMessage csound-repl--csound-instance "e 0 360000")
      (csoundStart csound-repl--csound-instance)
      (csoundAsyncPerform csound-repl--csound-instance))))

(defun csound-repl--expression-at-point ()
  (save-excursion 
    (end-of-line)
    (let ((fallback (list (line-beginning-position) (line-end-position)))
	  (beg (search-backward-regexp "^\\s-*\\<instr\\>\\|^\\s-*\\<opcode\\>" nil t))
	  (end (search-forward-regexp "^\\s-*\\<endin\\>\\|^\\s-*\\<endop\\>" nil t)))
      (if (and beg end (<= beg (first fallback) end))
	  (list beg end)
	fallback
	;; (throw 'no-expression "No instrument or opcode expression was found.")
	))))

(defun csound-repl--newline-seperated-score-block ()
  (let ((beg-block (save-excursion
		     (end-of-line 0)
		     (while (search-backward-regexp
			     "\\(^\\s-*\\|^\\t-*\\)i+[0-9\\\".*]*\\b"
			     (line-beginning-position 1) t 1)
		       (end-of-line 0))
		     (line-beginning-position 2)))
	(end-block (save-excursion
		     (end-of-line 1)
		     (while (search-backward-regexp
			     "\\(^\\s-*\\|^\\t-*\\)i+[0-9\\\".*]*\\b"
			     (line-beginning-position 1) t 1)
		       (end-of-line 2))
		     (line-end-position 0))))
    (list beg-block end-block)))

(defun csound-repl--insert-message (msg)
  (save-current-buffer
    (set-buffer csound-repl-buffer-name)
    (goto-char (buffer-size))
    (let ((msg (replace-regexp-in-string " " "" msg)))
      (if (prog2 (beginning-of-line)
	      (search-forward csound-repl-prompt nil t 1))
	  (progn 
	    (beginning-of-line)
	    (end-of-line 0)
	    (insert (concat msg "\n")))
	(progn 
	  (goto-char (buffer-size))
	  (end-of-line 1)
	  (insert (concat msg "\n"))))
      (when (or (string-match-p  "rtjack\\: error" msg)
		(string-match-p "rtjack\\: could not connect" msg))
	(csoundDestroy csound-repl--csound-instance)
	(insert "REPL ERROR: Something went wrong, please restart the repl to continue.\n")))
    (goto-char (1+ (buffer-size)))))

(defun csound-repl--errorp (pre-eval-size)
  (save-current-buffer
    (set-buffer csound-repl-buffer-name)
    (goto-char pre-eval-size)
    (beginning-of-line 0)
    (if (or (search-forward-regexp "error: " nil t 1)
	    (search-forward-regexp "Can't open" nil t 1)
	    (search-forward-regexp "Can't find" nil t 1))
	t nil)))

(defun csound-repl--flash-region (errorp)
  (if errorp
      (hlt-highlight-region
       csound-repl--expression-start
       csound-repl--expression-end 'csound-font-lock-eval-flash-error)
    (hlt-highlight-region
     csound-repl--expression-start
     csound-repl--expression-end
     'csound-font-lock-eval-flash))
  (run-with-idle-timer 0.15 nil
		       (lambda ()
			 (hlt-unhighlight-region
			  csound-repl--expression-start
			  csound-repl--expression-end))))

(defun csound-repl-evaluate-orchestra-region (start end)
  (let ((expression-string (buffer-substring start end)))
    (setq-local csound-repl--expression-start start)
    (setq-local csound-repl--expression-end end)
    (setq-local csound-repl--expression-tmp-buffer-size
		(buffer-size (get-buffer csound-repl-buffer-name)))
    (csoundCompileOrc csound-repl--csound-instance expression-string)
    (run-with-idle-timer
     0.02 nil
     (lambda ()
       (if (csound-repl--errorp csound-repl--expression-tmp-buffer-size)
	   (csound-repl--flash-region t) 
	 (progn
	   (csound-repl--flash-region nil)
	   (csound-repl--insert-message
	    (concat ";; Evaluated: "
		    (buffer-substring csound-repl--expression-start
				      (save-excursion
					(goto-char csound-repl--expression-start)
					(line-end-position)))))))))))


(defun csound-repl-evaluate-score-region (start end)
  (let ((expression-string (buffer-substring start end))
	(message-buffer-size (buffer-size
			      (get-buffer csound-repl-buffer-name))))
    (setq-local csound-repl--expression-start start)
    (setq-local csound-repl--expression-end end)
    (setq-local csound-repl--expression-tmp-buffer-size
		(buffer-size (get-buffer csound-repl-buffer-name)))
    (csound-input-message csound-repl--csound-instance
			  (csound-score-trim-time
			   expression-string))
    (run-with-idle-timer
     0.02 nil
     (lambda ()
       (if (csound-repl--errorp csound-repl--expression-tmp-buffer-size)
	   (prog2 (csound-repl--flash-region t)
	       (csound-repl--insert-message (concat ";; Score: error in code")))
	 (csound-repl--flash-region nil))))))

(defun csound-repl-evaluate-region ()
  "Evaluate any csound code in region."
  (interactive)
  (if (not (csound-repl-buffer-running-p))
      (message "csound-repl is not started")
    (if (save-excursion
	  (search-backward "<CsScore" nil t 1))
	(if (region-active-p)
	    (prog2
		(csound-repl-evaluate-score-region
		 (region-beginning)
		 (region-end))
		(deactivate-mark))
	  (if (save-excursion
		(beginning-of-line 1)
		(search-forward-regexp "i[0-9\"]?" (line-end-position) t 1))
	      (apply 'csound-repl-evaluate-score-region
		     (csound-repl--newline-seperated-score-block))
	    (csound-repl-evaluate-score-region
	     (line-beginning-position)
	     (line-end-position))))
      (if (region-active-p)
	  (prog2
	      (csound-repl-evaluate-orchestra-region
	       (region-beginning)
	       (region-end))
	      (deactivate-mark))
	(apply 'csound-repl-evaluate-orchestra-region (csound-repl--expression-at-point))))))

(defun csound-repl-evaluate-line ()
  "Evaluate csound expression on current line."
  (interactive)
  (if (not (csound-repl-buffer-running-p))
      (message "csound-repl instance was not found")
    (if (save-excursion
	  (search-backward-regexp "<CsScore" nil t 1))
	(csound-repl-evaluate-score-region
	 (line-beginning-position)
	 (line-end-position))	
      (csound-repl-evaluate-orchestra-region
       (line-beginning-position)
       (line-end-position)))))



;; (let ((last-call "i 1 0 10 200"))
;;   (read-csound-repl
;;    (intern (first (split-string (csound-util-chomp last-call) " ")))
;;    (get-process "csnd") (list last-call)))

(defun csound-repl-plot-ftgen ()
  "Plots the f statement on the cursor's current line.
   This operation evaluates the f statement and reads
   table from Csound, sends it to gnuplot which sends
   the png into the REPL."
  (interactive)
  (let* ((line-str (buffer-substring-no-properties (line-beginning-position)
						   (line-end-position)))
	 (line-str (csound-util-chomp line-str))
	 (f-statement-p (if (eq 0 (length line-str))
			    nil
			  (string-equal "f" (substring line-str 0 1))))
	 (ftgen-orc-p (string-match-p "\\<ftgen\\>" line-str)))
    (when (or f-statement-p
	      ftgen-orc-p)
      (if f-statement-p
	  (csound-repl-evaluate-score-region (line-beginning-position)
					     (line-end-position))
	(csound-repl-evaluate-orchestra-region (line-beginning-position)
					       (line-end-position)))
      (sleep-for 0 50)
      (let ((table-num (if f-statement-p
			   (-> (substring line-str 1)
			       (csound-util-chomp)
			       (split-string " ")
			       first)
			 (save-current-buffer
			   (set-buffer csound-repl-buffer-name)
			   (goto-char (buffer-size))
			   (search-backward-regexp "ftable \\([0-9]+\\)\\:")
			   (match-string-no-properties 1)))))
	(csound-repl-interaction--plot (string-to-number table-num))))))


(defvar csound-repl--font-lock-list
  '((";+.*" . font-lock-comment-face)
    ("SECTION [0-9]+:" . font-lock-string-face)
    ("new alloc.*" . font-lock-comment-face)
    ("error:" . font-lock-warning-face)
    ("\\<T[^_]\\|\\<TT\\|M:" . csound-font-lock-i-rate)
    (">>>.*<<<" . csound-font-lock-s-variables)
    ("\\<\\w*[^0-9]:\\B" . csound-font-lock-a-rate)))

(define-derived-mode
  csound-interactive-mode comint-mode "CsoundInteractive"
  "Csound Interactive Message Buffer and REPL."
  :syntax-table csound-mode-syntax-table
  ;;(setq comint-prompt-regexp (concat "^" (regexp-quote elnode-ijs-prompt))) 
  (setq-local comint-input-sender 'csound-repl--input-sender)
  (iimage-mode t)
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (let ((csnd-proc (start-process "csnd" (current-buffer) "hexl"))
	  ;; (file-location )
	  (buffer-name (first (csound-repl-last-visited-csd))))
      (setq csound-repl--process-tty-name (process-tty-name csnd-proc))
      (set-process-query-on-exit-flag csnd-proc nil)
      (set-process-filter csnd-proc (lambda (_ stdin)
				      (csound-repl--insert-message stdin)))
      (setq-local font-lock-defaults '(csound-repl--font-lock-list))
      (setq-local comint-prompt-read-only t)
      (setq-local comint-scroll-to-bottom-on-input t)
      (setq-local comint-scroll-to-bottom-on-output t)
      (setq-local comint-move-point-for-output t)
      (insert (csound-repl--generate-welcome-message buffer-name))
      (set-marker
       (process-mark csnd-proc) (point))
      (comint-output-filter csnd-proc csound-repl-prompt)
      ;; (csound-repl--buffer-create)
      (csound-repl--boot-instance)
      (add-hook 'kill-buffer-hook (lambda ()
				    (when (string-equal (buffer-name) csound-repl-buffer-name)
				      (csoundStop csound-repl--csound-instance)
				      ;; Note to self, csoundCleanup causes crashes
				      (csoundDestroy csound-repl--csound-instance)
				      (sleep-for 0.1))))
      (add-hook 'kill-emacs-hook (lambda ()
				   (when csound-repl--csound-instance
				     (when (not (eq 0 (csoundGetCurrentTimeSamples csound-repl--csound-instance)))
				       (csoundStop csound-repl--csound-instance))
				     (csoundDestroy csound-repl--csound-instance)
				     (sleep-for 0.05)))))))

(provide 'csound-repl)

;;; csound-repl.el ends here
