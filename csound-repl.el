;;; csound-repl.el --- A major mode for interacting and coding Csound

;; Copyright (C) 2017  Hlöðver Sigurðsson

;; Author: Hlöðver Sigurðsson <hlolli@gmail.com>
;; Version: 0.2.2
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
;; Repl functionality for csound-mode

;;; Code:

(require 'comint)
(require 'csound-font-lock)
(require 'csound-opcodes)
(require 'csound-repl-interaction)
(require 'csound-util)
(require 'dash)
(require 'font-lock)
(require 'highlight)
(require 'shut-up)

(defvar csound-repl--csound-server)
(defvar csound-repl--udp-client-proc)
(defvar csound-repl--console-client-proc)

;; For flash effects, expression variables,
;; need to live longer than the funcall
(defvar csound-repl--expression-start 0)
(defvar csound-repl--expression-end 0)
(defvar csound-repl--expression-tmp-buffer-size 0)

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

(defcustom csound-repl-kr
  (/ csound-repl-sr csound-repl-ksmps)
  "kr value of the csound repl"
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

(defcustom csound-repl-start-server-p t
  "When non nil, start csound server."
  :group 'csound-mode-repl
  :type 'boolean)

(defun csound-repl--get-sr (buf)
  (cond
   ((and buf csound-repl-start-server-p)
    (save-excursion
      (switch-to-buffer buf)
      (goto-char (point-min))
      (search-forward-regexp
       "^\\s-*sr\\s-*=\\s-*\\([0-9]+\\)" nil t 1)
      (let ((sr (match-string-no-properties 1)))
        (switch-to-prev-buffer)
        sr))
    (number-to-string csound-repl-sr))
   (buf (number-to-string csound-repl-sr))))

(defun csound-repl--get-kr (buf)
  (cond ((and buf csound-repl-start-server-p)
         (save-excursion
           (switch-to-buffer buf)
           (goto-char (point-min))
           (search-forward-regexp
            "^\\s-*kr\\s-*=\\s-*\\([0-9]+\\)" nil t 1)
           (let ((sr (match-string-no-properties 1)))
             (switch-to-prev-buffer)
             sr))
         (number-to-string csound-repl-kr))
        (buf (number-to-string csound-repl-kr))))

(defun csound-repl--get-ksmps (buf)
  (cond ((and buf csound-repl-start-server-p)
         (save-excursion
           (switch-to-buffer buf)
           (goto-char (point-min))
           (search-forward-regexp
            "^\\s-*ksmps\\s-*=\\s-*\\([0-9]+\\)" nil t 1)
           (let ((sr (match-string-no-properties 1)))
             (switch-to-prev-buffer)
             sr))
         (number-to-string csound-repl-ksmps))
        (buf (number-to-string csound-repl-ksmps))))

(defun csound-repl--get-0dbfs (buf)
  (cond ((and buf csound-repl-start-server-p)
         (save-excursion
           (switch-to-buffer buf)
           (goto-char (point-min))
           (search-forward-regexp
            "^\\s-*0dbfs\\s-*=\\s-*\\([0-9]+\\)" nil t 1)
           (let ((sr (match-string-no-properties 1)))
             (switch-to-prev-buffer)
             sr))
         (number-to-string csound-repl-0dbfs))
        (buf (number-to-string csound-repl-0dbfs))))

(defun csound-repl--get-nchnls (buf)
  (cond ((and buf csound-repl-start-server-p)
         (save-excursion
           (switch-to-buffer buf)
           (goto-char (point-min))
           (search-forward-regexp
            "^\\s-*nchnls\\s-*=\\s-*\\([0-9]+\\)" nil t 1)
           (let ((sr (match-string-no-properties 1)))
             (switch-to-prev-buffer)
             sr))
         (number-to-string csound-repl-nchnls))
        (buf (number-to-string csound-repl-nchnls))))

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
  (interactive)
  (when (not (csound-repl-buffer-running-p))
    (let ((prev-buffer (buffer-name)))
      (save-excursion
        (generate-new-buffer
         csound-repl-buffer-name)
        (split-window-sensibly)
        (switch-to-buffer-other-window csound-repl-buffer-name)
        (with-current-buffer (buffer-name) (funcall 'csound-repl-mode))
        (switch-to-buffer-other-window prev-buffer)))))

(defun csound-repl-last-visited-csd ()
  "This decides which filename is given to repl buffer.
   Returns a list of (path buffer-name buffer)"
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
                                       (file-name-directory
                                        (buffer-file-name
                                         (nth indx--last-visited (buffer-list))))
                                       (buffer-name
                                        (nth indx--last-visited (buffer-list)))
                                       (nth indx--last-visited (buffer-list))))
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
  (unless (eq 0 (length (csound-util-chomp input)))
    (let* ((id (csound-util--generate-random-uuid))
           (buffer-read-only nil)
           (lb (- (line-beginning-position) 5))
           (input-string (-> input csound-util-chomp))
           (first-chunk (car (split-string input-string))))
      (when (and first-chunk (< 0 (length first-chunk)))
        (read-csound-repl (intern (substring-no-properties first-chunk 0 1))
                          csound-repl--udp-client-proc input-string))
      ;; (comint-output-filter proc (format "%s\n" return-val))
      (push (cons id input) csound-repl--input)
      ;; (message "%s" input)
      (puthash id (process-buffer proc) csound-repl--input-history)
      ;; (save-excursion (set-buffer csound-repl-buffer-name) (point-max))
      ))
  (comint-output-filter proc csound-repl-prompt))

(defun csound-repl--generate-welcome-message (cur-file sr ksmps nchnls 0dbfs)
  (let* ((csound-repl---welcome-title
          (concat "  __   __   __             __             __   __   __ \n"
                  " /    /    /  | /  | /| ||/  |      /|/| /  ||/  | /   \n"
                  "(    (___ (   |(   |( | ||   | ___ ( / |(   ||   |(___ \n"
                  "|   )    )|   )|   )| | )|   )     |   )|   )|   )|    \n"
                  "|__/  __/ |__/ |__/ | |/ |__/      |  / |__/ |__/ |__  \n"))
         (s (format (concat "\n"
                            "file: " cur-file "\n"
                            "sr: %s\n"
                            "ksmps: %s\n"
                            "nchnls: %s\n"
                            "0dbfs: %s\n\n\n")
                    sr
                    ksmps
                    nchnls
                    0dbfs)))
    (concat csound-repl---welcome-title s)))

;; (defun csound-repl--set-default-dir-options ()
;;   (let ((filedir (nth 1 (csound-repl-last-visited-csd))))
;;     (mapc (lambda (opt)
;;             (csoundSetOption csound-repl--csound-instance
;;                              (format "--env:%s+=;%s"
;;                                      opt filedir)))
;;           '("INCDIR" "SFDIR"
;;             "SSDIR" "SADIR"
;;             "MFDIR"))))

(defun csound-repl--expression-at-point ()
  (save-excursion
    (end-of-line)
    (let ((fallback (list (line-beginning-position) (line-end-position)))
          (beg (search-backward-regexp "^\\s-*\\<instr\\>\\|^\\s-*\\<opcode\\>" nil t))
          (end (search-forward-regexp "^\\s-*\\<endin\\>\\|^\\s-*\\<endop\\>" nil t)))
      (if (and beg end (<= beg (car fallback) end))
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

(setq csound-repl--filter-multline-hackfix nil)

(setq csound-repl--filter-multline-hackfix-rtevent nil)

(defun csound-repl--filter (_ msg)
  (save-current-buffer
    (set-buffer csound-repl-buffer-name)
    (goto-char (buffer-size))
    (let ((msg (->> msg
                    (replace-regexp-in-string "\0\\|\n" "")
                    (replace-regexp-in-string ">>>" " >>> ")
                    (replace-regexp-in-string "\\s-+rtevent:\\s-+" "rtevent: ")))
          (hackfix-p csound-repl--filter-multline-hackfix))
      (when (string-match-p "rtevent:" msg)
        (setq csound-repl--filter-multline-hackfix-rtevent 0
              csound-repl--filter-multline-hackfix t))
      (when (numberp csound-repl--filter-multline-hackfix-rtevent)
        (if (eq 2 csound-repl--filter-multline-hackfix-rtevent)
            (setq csound-repl--filter-multline-hackfix-rtevent nil
                  csound-repl--filter-multline-hackfix nil)
          (setq csound-repl--filter-multline-hackfix-rtevent
                (1+ csound-repl--filter-multline-hackfix-rtevent))))
      (when (string-match-p ">>>" msg)
        (setq csound-repl--filter-multline-hackfix t))
      (when (string-match-p "<<<" msg)
        (setq csound-repl--filter-multline-hackfix nil))
      (if (prog2 (beginning-of-line)
              (search-forward csound-repl-prompt nil t 1))
          (progn
            (beginning-of-line)
            (end-of-line 0)
            (if hackfix-p
                (insert msg)
              (insert (concat "\n" msg))))
        (progn
          (goto-char (buffer-size))
          (end-of-line 1)
          (if hackfix-p
              (insert msg)
            (insert (concat msg "\n")))))
      (when (or (string-match-p  "rtjack\\: error" msg)
                (string-match-p "rtjack\\: could not connect" msg))
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
    (process-send-string csound-repl--udp-client-proc expression-string)
    (run-with-idle-timer
     0.02 nil
     (lambda ()
       (if (csound-repl--errorp csound-repl--expression-tmp-buffer-size)
           (csound-repl--flash-region t)
         (progn
           (csound-repl--flash-region nil)
           (csound-repl--filter
            nil
            (concat ";; Evaluated: "
                    (buffer-substring csound-repl--expression-start
                                      (save-excursion
                                        (goto-char csound-repl--expression-start)
                                        (line-end-position)))))))))))


(defun csound-repl-evaluate-score-region (start end)
  (let ((expression-string (buffer-substring start end))
        (message-buffer-size (buffer-size
                              (get-buffer csound-repl-buffer-name))))
    ;; (message expression-string)
    (setq-local csound-repl--expression-start start)
    (setq-local csound-repl--expression-end end)
    (setq-local csound-repl--expression-tmp-buffer-size
                (buffer-size (get-buffer csound-repl-buffer-name)))
    (process-send-string csound-repl--udp-client-proc
                         (concat "$" (csound-score-trim-time expression-string)))
    (run-with-idle-timer
     0.02 nil
     (lambda ()
       (if (csound-repl--errorp csound-repl--expression-tmp-buffer-size)
           (prog2 (csound-repl--flash-region t)
               (csound-repl--filter nil ";; Score: error in code"))
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
      (message "csound-repl is not started")
    (if (save-excursion
          (search-backward-regexp "<CsScore" nil t 1))
        (csound-repl-evaluate-score-region
         (line-beginning-position)
         (line-end-position))
      (csound-repl-evaluate-orchestra-region
       (line-beginning-position)
       (line-end-position)))))


;; (defun csound-repl-plot-ftgen ()
;;   "Plots the f statement on the cursor's current line.
;;    This operation evaluates the f statement and reads
;;    table from Csound, sends it to gnuplot which sends
;;    the png into the REPL."
;;   ;; (interactive)
;;   (let* ((line-str (buffer-substring-no-properties (line-beginning-position)
;;               (line-end-position)))
;;   (line-str (csound-util-chomp line-str))
;;   (f-statement-p (if (eq 0 (length line-str))
;;          nil
;;        (string-equal "f" (substring line-str 0 1))))
;;   (ftgen-orc-p (string-match-p "\\<ftgen\\>" line-str)))
;;     (when (or f-statement-p
;;        ftgen-orc-p)
;;       (if f-statement-p
;;    (csound-repl-evaluate-score-region (line-beginning-position)
;;               (line-end-position))
;;  (csound-repl-evaluate-orchestra-region (line-beginning-position)
;;                 (line-end-position)))
;;       (sleep-for 0 50)
;;       (let ((table-num (if f-statement-p
;;         (-> (substring line-str 1)
;;             (csound-util-chomp)
;;             (split-string " ")
;;             first)
;;       (save-current-buffer
;;         (set-buffer csound-repl-buffer-name)
;;         (goto-char (buffer-size))
;;         (search-backward-regexp "ftable \\([0-9]+\\)\\:")
;;         (match-string-no-properties 1)))))
;;  (csound-repl-interaction--plot (string-to-number table-num))))))

(defvar csound-repl--font-lock-list
  '((";.*" . font-lock-comment-face)
    ("SECTION [0-9]+:" . font-lock-string-face)
    ("new alloc.*" . font-lock-comment-face)
    ("error:\\|instrerror:" . font-lock-warning-face)
    ;; ("\\<T[^_]\\|\\<TT\\|M:" . csound-font-lock-i-rate)
    (">>>.*<<<" . csound-font-lock-s-variables)
    ("\\<\\w*[^0-9]:\\B" . csound-font-lock-a-rate)))

(defun csound-repl--sentinel (proc msg)
  (message msg))

(defun csound-repl--start-client (port)
  (let ((port (if (stringp port) port (number-to-string port)))
        (host "127.0.0.1"))
    (make-network-process :name "csound-udp-client"
                          :type 'datagram
                          :buffer csound-repl-buffer-name
                          :family 'ipv4
                          :host host
                          :service port
                          :sentinel 'csound-repl--filter
                          :filter 'csound-repl--filter)))

(defun csound-repl--console-client (port)
  (let ((port (if (stringp port) port (number-to-string port)))
        (host "127.0.0.1"))
    (make-network-process :name "csound-console-client"
                          :server t
                          :type 'datagram
                          :buffer csound-repl-buffer-name
                          :family 'ipv4
                          :host host
                          :service port
                          :sentinel 'csound-repl--filter
                          :filter 'csound-repl--filter)))

(defun csound-repl--start-server (port console-port sr ksmps nchnls zero-db-fs)
  (start-process "Csound Server" csound-repl-buffer-name
                 "csound" "-odac"
                 (format "--port=%s" port)
                 (format "--udp-console=127.0.0.1:%s" console-port)
                 (format "--sample-rate=%s" sr)
                 (format "--ksmps=%s" ksmps)
                 (format "--nchnls=%s" nchnls)
                 (format "--0dbfs=%s" zero-db-fs)))

(setq csound-repl-map
      (let ((map comint-mode-map))
        (define-key map (kbd "<S-return>")
          (lambda ()
            (interactive)
            (insert "\n      ")))
        map))

(define-derived-mode
  csound-repl-mode comint-mode "CsoundRepl"
  "Csound Interactive Message Buffer and REPL."
  :syntax-table csound-mode-syntax-table
  (setq-local comint-input-sender 'csound-repl--input-sender)
  (iimage-mode t)
  (unless (comint-check-proc (current-buffer))
    (let* ((last-csound-buffer (csound-repl-last-visited-csd))
           (buffer-name (and last-csound-buffer (listp last-csound-buffer)
                             (nth 1 last-csound-buffer)))
           (buffer (and last-csound-buffer (listp last-csound-buffer)
                        (nth 2 last-csound-buffer)))
           (port (if csound-repl-start-server-p 6000 8099))
           (console-port (if csound-repl-start-server-p 6001 8100))
           (sr (csound-repl--get-sr buffer))
           (ksmps (csound-repl--get-ksmps buffer))
           (nchnls (csound-repl--get-nchnls buffer))
           (0dbfs (csound-repl--get-0dbfs buffer)))
      (insert (csound-repl--generate-welcome-message buffer-name sr ksmps nchnls 0dbfs))
      (if csound-repl-start-server-p
          (setq csound-repl--csound-server (csound-repl--start-server
                                            port
                                            console-port
                                            sr
                                            ksmps
                                            nchnls
                                            0dbfs))
        (let ((fake-proc
               (condition-case nil
                   (start-process "ijsm" (current-buffer) "hexl")
                 (file-error (start-process "ijsm" (current-buffer) "cat")))))
          (set-process-query-on-exit-flag fake-proc nil)
          ;; Add a silly header
          ;;  (insert "Interactive Javascript Mode\n")
          (set-marker
           (process-mark fake-proc) (point))
          (comint-output-filter fake-proc csound-repl-prompt)))
      (when csound-repl-start-server-p
        (set-process-filter csound-repl--csound-server (lambda (_ stdin) nil)))
      (setq csound-repl--udp-client-proc (csound-repl--start-client port))
      (setq csound-repl--console-client-proc (csound-repl--console-client console-port))
      (when csound-repl-start-server-p
        (set-process-query-on-exit-flag csound-repl--csound-server nil))
      (set-process-query-on-exit-flag csound-repl--udp-client-proc nil)
      (set-process-query-on-exit-flag csound-repl--console-client-proc nil)
      (setq-local font-lock-defaults '(csound-font-lock-list
                                       csound-repl--font-lock-list))
      (setq-local comment-start ";; ")
      (setq-local eldoc-documentation-function 'csound-eldoc-function)
      (add-hook 'completion-at-point-functions #'csound-util-opcode-completion-at-point nil t)
      ;; (setq-local comint-prompt-read-only t)
      (setq-local comint-scroll-to-bottom-on-input t)
      (setq-local comint-scroll-to-bottom-on-output t)
      (setq-local comint-move-point-for-output t)
      ;; (set-marker (process-mark csound-repl--console-client-proc) (point))
      ;; (comint-output-filter csound-repl--console-client-proc csound-repl-prompt)
      )))

(provide 'csound-repl)

;;; csound-repl.el ends here
