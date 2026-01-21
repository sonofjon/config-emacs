;;; aj8-pinentry.el --- Fix for pinentry.el with modern GPG agents -*- lexical-binding: t -*-

;; DESCRIPTION:
;;
;; This Emacs Lisp file patches the 'pinentry' package to work with modern
;; GPG agents. It allows Emacs to serve as the Pinentry program (Minibuffer).
;;
;; SETUP:
;;
;; The GPG Agent needs an external "bridge" script to talk to the socket
;; created by the Emacs pinentry service.
;;
;; The bridge acts as a bridge between the GPG Agent and the Emacs Pinentry
;; server. It forwards the Pinentry Assuan protocol from GPG (via
;; Stdin/Stdout) to the Emacs unix socket located at
;; /tmp/emacs<UID>/pinentry.
;;
;; INSTALLATION:
;;
;; 1. Create the bridge script at ~/.local/bin/pinentry-emacs:
;;
;;    #!/bin/sh
;;    socket="/tmp/emacs$(id -u)/pinentry"
;;    exec socat STDIO "UNIX-CONNECT:$socket"
;;
;;    (Ensure it is executable: chmod +x ~/.local/bin/pinentry-emacs)
;;
;; 2. Configure ~/.gnupg/gpg-agent.conf to use it:
;;
;;    pinentry-program <home_directory>/.local/bin/pinentry-emacs
;;    allow-emacs-pinentry
;;
;; 3. Restart Agent: gpgconf --kill gpg-agent
;;
;; REQUIREMENTS:
;;
;; 1. Emacs package 'pinentry' must be installed and running (pinentry-start).
;; 2. 'socat' must be installed.
;; 3. GPG Agent must be configured to use this script as 'pinentry-program'.
;;

(require 'pinentry)

(defun pinentry--process-filter-patched (process input)
  "Patched filter to ignore unknown GPG commands and avoid errors.

This patch addresses two issues with modern GPG Agents:

1. Handshake Fix: Ignores protocol negotiation commands (OPTION,
   GETINFO, KBD, etc.)  that the original package doesn't recognize,
   preventing immediate disconnects.

2. UI/Data Fix: Ignores metadata commands (SETKEYINFO, SETQUALITYBAR)
   that cause 'Unknown command' errors.

By replying 'OK' to these commands instead of erroring, we allow the
password prompt (GETPIN) to proceed."
  (unless (buffer-live-p (process-buffer process))
    (let ((buffer (generate-new-buffer " *pinentry*")))
      (set-process-buffer process buffer)
      (with-current-buffer buffer
        (if (fboundp 'set-buffer-multibyte)
            (set-buffer-multibyte nil))
        (make-local-variable 'pinentry--read-point)
        (setq pinentry--read-point (point-min))
        (make-local-variable 'pinentry--labels))))
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert input)
      (goto-char pinentry--read-point)
      (beginning-of-line)
      (while (looking-at ".*\n")        ;the input line finished
        (if (looking-at "\\([A-Z_]+\\) ?\\(.*\\)")
            (let ((command (match-string 1))
                  (string (pinentry--unescape-string (match-string 2))))
              (pcase command
                ((and set (guard (member set pinentry--set-label-commands)))
                 (when (> (length string) 0)
                   (let* ((symbol (intern (downcase (substring set 3))))
                          (entry (assq symbol pinentry--labels))
                          (label (decode-coding-string string 'utf-8)))
                     (if entry
                         (setcdr entry label)
                       (push (cons symbol label) pinentry--labels))))
                 (ignore-errors (process-send-string process "OK\n")))
                ;; FIX: Explicitly ignore these commands instead of erroring
                ((or "NOP" "OPTION" "BYE" "AUTH" "RESET" "KBD" "GETINFO" "SETKEYINFO" "SETDESC" "SETPROMPT" "SETQUALITYBAR" "SETERROR")
                 (ignore-errors (process-send-string process "OK\n")))
                ("GETPIN"
                 (let ((prompt
                        (or (cdr (assq 'desc pinentry--labels))
                            (cdr (assq 'prompt pinentry--labels))
                            ""))
		       (confirm (not (null (assq 'repeat pinentry--labels))))
                       entry)
                   (if (setq entry (assq 'error pinentry--labels))
                       (setq prompt (concat "Error: "
                                            (propertize
                                             (copy-sequence (cdr entry))
                                             'face 'error)
                                            "\n"
                                            prompt)))
                   (if (setq entry (assq 'title pinentry--labels))
                       (setq prompt (format "[%s] %s"
                                            (cdr entry) prompt)))
                   (if (string-match ":?[ \n]*\\'" prompt)
                       (setq prompt (concat
                                     (substring
                                      prompt 0 (match-beginning 0)) ": ")))
                   (let (passphrase escaped-passphrase encoded-passphrase)
                     (unwind-protect
                         (condition-case nil
                             (progn
                               (setq passphrase
				     (read-passwd prompt confirm))
                               (setq escaped-passphrase
                                     (pinentry--escape-string
                                      passphrase))
                               (setq encoded-passphrase (encode-coding-string
                                                         escaped-passphrase
                                                         'utf-8))
			       (ignore-errors
				 (pinentry--send-data
				  process encoded-passphrase)
				 (process-send-string process "OK\n")))
                           (error
			    (ignore-errors
			      (pinentry--send-error
			       process
			       pinentry--error-cancelled))))
                       (if passphrase
                           (clear-string passphrase))
                       (if escaped-passphrase
                           (clear-string escaped-passphrase))
                       (if encoded-passphrase
                           (clear-string encoded-passphrase))))
                   (setq pinentry--labels nil)))
                ("CONFIRM"
                 (let ((prompt
                        (or (cdr (assq 'desc pinentry--labels))
                            ""))
                       (buttons
                        (pinentry--labels-to-shortcuts
                         (list (cdr (assq 'ok pinentry--labels))
                               (cdr (assq 'notok pinentry--labels))
			       (cdr (assq 'cancel pinentry--labels)))))
                       entry)
                   (if (setq entry (assq 'error pinentry--labels))
                       (setq prompt (concat "Error: "
                                            (propertize
                                             (copy-sequence (cdr entry))
                                             'face 'error)
                                            "\n"
                                            prompt)))
                   (if (setq entry (assq 'title pinentry--labels))
                       (setq prompt (format "[%s] %s"
                                            (cdr entry) prompt)))
                   (if (remq nil buttons)
                       (progn
                         (setq prompt
                               (concat prompt " ("
                                       (mapconcat #'cdr (remq nil buttons)
                                                  ", ")
                                       ") "))
                         (condition-case nil
                             (let ((result (read-char prompt)))
                               (if (eq result (caar buttons))
				   (ignore-errors
				     (process-send-string process "OK\n"))
                                 (if (eq result (car (nth 1 buttons)))
				     (ignore-errors
				       (pinentry--send-error
					process
					pinentry--error-not-confirmed))
				   (ignore-errors
				     (pinentry--send-error
				      process
				      pinentry--error-cancelled)))))
                           (error
			    (ignore-errors
			      (pinentry--send-error
			       process
			       pinentry--error-cancelled)))))
                     (if (string-match "[ \n]*\\'" prompt)
                         (setq prompt (concat
                                       (substring
                                        prompt 0 (match-beginning 0)) " ")))
                     (if (condition-case nil
                             (y-or-n-p prompt)
                           (quit))
			 (ignore-errors
			   (process-send-string process "OK\n"))
		       (ignore-errors
			 (pinentry--send-error
			  process
			  pinentry--error-not-confirmed))))
                   (setq pinentry--labels nil)))
                (_ (ignore-errors
		     (pinentry--send-error
		      process
		      pinentry--error-not-implemented))))
              (forward-line)
              (setq pinentry--read-point (point))))))))

(advice-add 'pinentry--process-filter :override
            #'pinentry--process-filter-patched)
(message "Pinentry fix loaded.")
