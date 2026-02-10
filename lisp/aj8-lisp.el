;;; aj8-lisp.el --- My custom lisp -*- lexical-binding: t; -*-
;;
;; Author: Andreas Jonsson <ajdev8@gmail.com>
;; Maintainer: Andreas Jonsson <ajdev8@gmail.com>
;; URL: https://github.com/sonofjon/config-emacs.el
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, lisp

;;;;; FUNCTIONS

;;;; Admin

;; System package names by system type
;;   TODO: Check data for gnu/linux (enchant and pkgconf)
(defun aj8/system-package-name (package)
  "Return the appropriate system PACKAGE name based on the current system."
  (let ((system-package-alist
         '(
           ;; (enchant . ((darwin . "/opt/homebrew/lib/libenchant-2.dylib")
           (enchant . ((darwin . enchant-2)
                       (ubuntu . "/usr/lib/x86_64-linux-gnu/libenchant-2.so")
                       (fedora . "/usr/lib64/libenchant-2.so")
                       (wsl . "/usr/lib/x86_64-linux-gnu/libenchant-2.so")))
           (libvterm . ((darwin . "/opt/homebrew/lib/libvterm.dylib")
                        (ubuntu . "/usr/lib/x86_64-linux-gnu/libvterm.so")
                        (fedora . "/usr/lib64/libvterm.so")
                        (wsl . "/usr/lib/x86_64-linux-gnu/libvterm.so")))
           (ls . ((darwin . gls)
                  (ubuntu . ls)
                  (fedora . ls)
                  (wsl . ls)))
           (xclip . ((darwin . pbcopy)
                     (ubuntu . wl-copy)
                     (fedora . wl-copy)
                     (wsl . xclip))))))
    (let ((system-type (cond ((eq aj8/my-os 'macos) 'darwin)
                             ((eq aj8/my-os 'wsl) 'wsl)
                             ((eq aj8/my-os 'linux) aj8/my-linux-os)
                             (t (error "Unknown OS type: %s" aj8/my-os))))
          (package-alist (cdr (assoc package system-package-alist))))
      (or (cdr (assoc system-type package-alist))
          (error "Package '%s' not found for system type '%s'"
                 package system-type)))))

;; List available package upgrades (excludes vc packages)
(defun aj8/package-list-upgrades (&optional verbose)
  "List all packages that have upgrades available.
With prefix argument (C-u), or if VERBOSE is non-nil, show detailed
version information.  Excludes VC packages since they show as
upgradeable even when up-to-date."
  (interactive "P")
  (package-refresh-contents)
  (let* ((all-upgradable (package--upgradeable-packages))
         (upgradable (sort
                      (cl-remove-if
                       (lambda (pkg-name)
                         (let ((pkg-desc (cadr (assq pkg-name package-alist))))
                           (and pkg-desc
                                (eq (package-desc-kind pkg-desc) 'vc))))
                       all-upgradable)
                      #'string<)))
    (if upgradable
        (if verbose
            ;; Verbose version with version details on one line
            (let ((upgrade-info
                   (mapconcat
                    (lambda (pkg-name)
                      (let* ((installed (cadr (assq pkg-name package-alist)))
                             (available (cadr (assq pkg-name package-archive-contents))))
                        (format "%s:%s->%s"
                                pkg-name
                                (if installed
                                    (package-version-join (package-desc-version installed))
                                    "?")
                                (if available
                                    (package-version-join (package-desc-version available))
                                    "?"))))
                    upgradable ", ")))
              (message "Upgrades (%d): %s" (length upgradable) upgrade-info))
          ;; Simple version with just package names
          (message "Upgrades (%d): %s"
                   (length upgradable)
                   (mapconcat 'symbol-name upgradable ", ")))
      (message "All packages are up to date"))))

;; Upgrade packages verbosily (excludes vc packages)
(defun aj8/package-upgrade-all-verbose ()
  "Upgrade all packages with detailed information about what will be upgraded.
Excludes VC packages since they show as upgradeable even when
up-to-date."
  (interactive)
  (package-refresh-contents)
  (let* ((all-upgradable (package--upgradeable-packages))
         (upgradable (sort
                      (cl-remove-if
                       (lambda (pkg-name)
                         (let ((pkg-desc (cadr (assq pkg-name package-alist))))
                           (and pkg-desc
                                (eq (package-desc-kind pkg-desc) 'vc))))
                       all-upgradable)
                      #'string<)))
    (if upgradable
        (let ((package-list (mapconcat 'symbol-name upgradable ", ")))
          (when (yes-or-no-p (format "Upgrade %d packages (%s)? "
                                     (length upgradable)
                                     package-list))
            ;; Temporarily disable package-refresh-contents during upgrades
            (cl-letf (((symbol-function 'package-refresh-contents)
                       (lambda (&optional async) nil)))
              (dolist (name upgradable)
                (package-upgrade name)))))
      (message "All packages are up to date"))))

;; Make built-in package upgrade function verbose
(defun aj8/package-upgrade-all (orig-fun &rest args)
  "Advise `package-upgrade-all' to show package names in the upgrade prompt.
ORIG-FUN is the original function being advised.
ARGS are passed to ORIG-FUN."
  (let ((upgradable (sort (package--upgradeable-packages) #'string<)))
    (if upgradable
        (let ((package-list (mapconcat 'symbol-name upgradable ", ")))
          (when (yes-or-no-p (format "Upgrade %d packages (%s)? "
                                     (length upgradable)
                                     package-list))
            ;; Call the original function but skip its own prompt
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (apply orig-fun args))))
      (message "All packages are up to date"))))

;; Don't auto-remove vc packages
(defun aj8/package-autoremove-no-vc (orig-fun &rest args)
  "Advise `package-autoremove' to not remove VC packages.
This function temporarily adds packages from
`package-vc-selected-packages' to `package-selected-packages' before
calling `package-autoremove'.
ORIG-FUN is the original function being advised.
ARGS are passed to ORIG-FUN."
  (let ((package-selected-packages
         (append (mapcar #'car package-vc-selected-packages)
                 package-selected-packages)))
    (apply orig-fun args)))

;; Upgrade VC packages only when remote changes exist
(defun aj8/package-vc-upgrade-all-smart ()
  "Upgrade all installed VC packages that have remote changes.
Unlike `package-vc-upgrade-all', this checks for remote changes first
and only upgrades packages that need it."
  (interactive)
  (let ((upgraded 0)
        (checked 0)
        (total (cl-loop for package in package-alist
                        sum (cl-count-if #'package-vc-p (cdr package)))))
    (dolist (package package-alist)
      (dolist (pkg-desc (cdr package))
        (when (package-vc-p pkg-desc)
          (setq checked (1+ checked))
          (let* ((pkg-name (package-desc-name pkg-desc))
                 (pkg-dir (package-desc-dir pkg-desc))
                 (default-directory pkg-dir))
            (message "Checking %d/%d: %s..." checked total pkg-name)
            (require 'vc-git)
            (vc-git-command nil 0 nil "fetch")
            (with-temp-buffer
              (when (zerop (vc-git-command t 0 nil "status" "--porcelain=v1" "--branch"))
                (goto-char (point-min))
                (when (re-search-forward "\\[behind [0-9]+\\]" nil t)
                  (message "Upgrading %s..." pkg-name)
                  (package-vc-upgrade pkg-desc)
                  (setq upgraded (1+ upgraded))))))))))
    (message "Upgraded %d VC packages." upgraded))

;;;; AI

;; Save gptel buffers
(defun aj8/gptel-write-buffer (orig-fun &rest args)
  "Advise `gptel' to save the gptel chat buffer.

This function saves the chat buffer to the current project's root
directory.  If no project is detected, it prompts the user to choose a
directory for saving.  It constructs a filename based on the current
timestamp and the major mode of the buffer (with support for `org-mode'
and `markdown-mode').

ORIG-FUN is the original function being advised.
ARGS are passed to ORIG-FUN."
  (let ((chat-buf (apply orig-fun args)))
    (with-current-buffer chat-buf
      (unless (buffer-file-name)
        (let* ((project (project-current))
               (directory (if project
                              (project-root project)
                            (read-directory-name "No project found. Choose where to save the chat: "
                                                 default-directory nil t)))
               (suffix (format-time-string "%Y%m%dT%H%M" (current-time)))
               (extension (pcase major-mode
                            ('org-mode "org")
                            ('markdown-mode "md")
                            (_ (user-error "Unsupported major mode"))))
               (filename (expand-file-name
                          (concat "gptel-" suffix "." extension) directory)))
          (write-file filename 'confirm))))
    chat-buf))

;; Auto-save gptel buffers
;;   Also see aj8/no-backup-regexp
(defun aj8/gptel-auto-save-chat-buffer (beg end)
  "Auto-save gptel chat buffer after LLM response.
Only saves if the current buffer is a modified gptel chat buffer with a
file name.  This function is meant to be used with
`gptel-post-response-functions'.  BEG and END are the response beginning
and end positions, which are required by
`gptel-post-response-functions'."
  (when (and (bound-and-true-p gptel-mode)
             (buffer-file-name)
             (buffer-modified-p))
        (save-buffer)
    (message "Auto-saved gptel buffer")))

;; Display gptel reasoning buffer
(defun aj8/gptel-display-reasoning-buffer (beg end)
  "Display the gptel reasoning buffer.
This function displays the gptel reasoning buffer and enables
`buffer-tail-mode' (on first display only).  Use with any of gptel's
built-in hooks.  The arguments BEG and END are ignored but required by
the hook."
  (when (stringp gptel-include-reasoning)
    (let ((buf (get-buffer gptel-include-reasoning)))
      (when buf
        ;; Enable buffer-tail-mode if not already active
        (with-current-buffer buf
          (unless buffer-tail-mode (buffer-tail-mode 1)))
        ;; Now, display the buffer if it's not empty
        (when (> (buffer-size buf) 0)
          (display-buffer buf nil))))))

;; Don't query on exit for MCP server listener process
(defun aj8/mcp-server-no-query-on-exit ()
  "Set query-on-exit-flag to nil for MCP server listener process."
  (when-let ((proc (get-process "emacs-mcp-unix-server")))
    (set-process-query-on-exit-flag proc nil)))

;;;; Buffers

;;; Buffer switching

;; Alternative switch-to-prev-buffer
(defun aj8/switch-to-buffer-prev ()
  "Switch to the previous buffer.
Alternative `switch-to-prev-buffer' based on `switch-to-buffer'.  If
`switch-to-buffer-obey-display-actions' is non-nil, switch to any
window."
  (interactive)
  (let ((prev-buffer (nth 1 (buffer-list (selected-window)))))
    (when prev-buffer
      (switch-to-buffer prev-buffer))))

;; Alternative switch-to-next-buffer
(defun aj8/switch-to-buffer-next ()
  "Switch to the next buffer.
Alternative `switch-to-next-buffer' based on `switch-to-buffer'.  If
`switch-to-buffer-obey-display-actions' is non-nil, switch to any
window."
  (interactive)
  (let ((next-buffer (nth 1 (buffer-list (selected-window)))))
    (when next-buffer
      (switch-to-buffer next-buffer))))

(defcustom aj8/buffer-skip-regexp
  ;; (rx (zero-or-more anychar))     ; match anything
  ;; (rx bos (or "*scratch*") eos)   ; match a particular buffer
  ;; (rx bos "*")                    ; match buffers starting with '*'
  (rx bos (or (seq "*" (zero-or-more anything))
              (seq "magit-process" (zero-or-more anything))
              (seq "acp-client-stderr" (zero-or-more anything)))
      eos)
  "Regular expression matching ignored buffers.

The matching buffers are ignored by `next-buffer' and
`previous-buffer'."
  :type 'regexp
  :group 'aj8-lisp)

(defun aj8/buffer-side-window-p (buffer)
  "Return t if BUFFER is displayed in a side window."
  (let ((window (get-buffer-window buffer)))
    (and window (window-parameter window 'window-side))))

(defun aj8/buffer-skip-p (window buffer bury-or-kill)
  "Return t if the BUFFER should be skipped.
WINDOW is the window displaying the buffer.  BURY-OR-KILL indicates
whether to bury or kill the buffer."
  ;; Buffer name matches `aj8/buffer-skip-regexp'
  (string-match-p aj8/buffer-skip-regexp (buffer-name buffer)))
  ;; Buffer is displayed in a side window
  ;; (aj8/buffer-side-window-p buffer))

;; Custom switch-to-prev-buffer with skip
(defun aj8/switch-to-prev-buffer ()
  "Switch to the previous buffer, and skip irrelevant buffers.
If the current window is a side window use the regular
`switch-to-prev-buffer'."
  (interactive)
  (if (window-parameter nil 'window-side)
      (switch-to-prev-buffer)
    (let ((switch-to-prev-buffer-skip 'aj8/buffer-skip-p))
      (switch-to-prev-buffer))))

;; Custom previous-buffer function with skip
(defun aj8/previous-buffer ()
  "Switch to the previous buffer, and skip irrelevant buffers.
If the current window is a side window use the regular
`previous-buffer'."
  (interactive)
  (if (window-parameter nil 'window-side)
      (previous-buffer)
    (let ((switch-to-prev-buffer-skip 'aj8/buffer-skip-p))
      (previous-buffer))))

;; Custom switch-to-next-buffer with skip
(defun aj8/switch-to-next-buffer ()
  "Switch to the next buffer, and skip irrelevant buffers.
If the current window is a side window use the regular
`switch-to-next-buffer'."
  (interactive)
  (if (window-parameter nil 'window-side)
      (switch-to-next-buffer)
    (let ((switch-to-prev-buffer-skip 'aj8/buffer-skip-p))
      (switch-to-next-buffer))))

;; Custom next-buffer function with skip
(defun aj8/next-buffer ()
  "Switch to the next buffer, and skip irrelevant buffers.
If the current window is a side window use the regular
`next-buffer'."
  (interactive)
  (if (window-parameter nil 'window-side)
      (next-buffer)
    (let ((switch-to-prev-buffer-skip 'aj8/buffer-skip-p))
      (next-buffer))))

;;; Project buffer switching
;;;   (imported from projectile and adapted for project.el)

;; Find next/previous project buffer
;;   TODO: If not in a project, switch to buffers in the current dir or its children
(defun my/project--repeat-until-project-buffer (orig-fun &rest args)
  "Repeat ORIG-FUN with ARGS until the current buffer is a project buffer."
  (if (project-current)
      (let* ((other-project-buffers (make-hash-table :test 'eq))
             (project-buffers (project-buffers (project-current)))
             (max-iterations (length (buffer-list)))
             (counter 0))
        (dolist (buffer project-buffers)
          (unless (eq buffer (current-buffer))
            (puthash buffer t other-project-buffers)))
        ;; Only switch if there are other project buffers to switch to
        ;;   TODO: This does not work
        (when (and (cdr-safe project-buffers)
                   (> (hash-table-count other-project-buffers) 0))
          (while (and (< counter max-iterations)
                      (not (gethash (current-buffer) other-project-buffers)))
            (apply orig-fun args)
            (cl-incf counter))))
    (apply orig-fun args)))

;; Switch to next project buffer
;;   TODO: If not in a project, switch to buffers in the same dir
(defun my/project-next-buffer ()
  "Switch to the next project buffer, and skip irrelevant buffers.
If the current window is a side window don't skip buffers."
  (interactive)
  (if (project-current)
      (if (window-parameter nil 'window-side)
          (my/project--repeat-until-project-buffer #'next-buffer)
        (let ((switch-to-prev-buffer-skip 'aj8/buffer-skip-p))
          (my/project--repeat-until-project-buffer #'next-buffer)))
    (message "Warning: Current buffer is not a project buffer.")))

;; Switch to previous project buffer
(defun my/project-previous-buffer ()
  "Switch to the previous project buffer, and skip irrelevant buffers.
If the current window is a side window don't skip buffers."
  (interactive)
  (if (project-current)
      (if (window-parameter nil 'window-side)
          (my/project--repeat-until-project-buffer #'previous-buffer)
        (let ((switch-to-prev-buffer-skip 'aj8/buffer-skip-p))
          (my/project--repeat-until-project-buffer #'previous-buffer)))
    (message "Warning: Current buffer is not a project buffer.")))

;;; Manipulate buffer names

;; Tag buffer name
(defun aj8/tag-buffer-name (tag)
  "Add TAG to buffer name.
Renames only if the tag is missing."
  (let ((suffix-string (format " # %s" tag)))
    (unless (string-suffix-p suffix-string (buffer-name))
      (rename-buffer (concat (buffer-name) suffix-string) t))))

;; Prefix buffer name
(defun aj8/prefix-buffer-name (prefix)
  "Add PREFIX to buffer name.
Renames only if the prefix is missing."
  (let ((prefix-string (concat prefix ": ")))
    (unless (string-prefix-p prefix-string (buffer-name))
      (rename-buffer (concat prefix-string (buffer-name)) t))))

;;; Buffer matching functions

;; Make major-mode matching function
;;   Use with display-buffer-alist
(defun mp-make-display-buffer-matcher-function (major-modes)
  "Return a lambda function that matches against a list of MAJOR-MODES."
  (lambda (buffer-name action)
    (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))

;; Check if buffer belongs to a project
;; (defun mp-buffer-has-project-p (buffer action)
;;   "Return non-nil if BUFFER belongs to a project."
;;   (with-current-buffer buffer (project-current nil)))

;;; Buffer cleanup

(defcustom aj8/ediff-cleanup-buffers nil
  "If non-nil, clean up Ediff buffers on exit.
See function `aj8/ediff-cleanup-buffers' for details."
  :type 'boolean
  :group 'aj8-lisp)

;; Kill all Ediff buffers
(defun aj8/ediff-cleanup-buffers ()
  "Kill all Ediff buffers.
See also `ediff-cleanup-mess'."
  (interactive)
  (when aj8/ediff-cleanup-buffers
    (dolist (buffer (buffer-list))
      (when (string-match-p "\\*[Ee]diff.*\\*" (buffer-name buffer))
        (kill-buffer buffer)))
    (message "Killed all Ediff buffers")))

(defcustom aj8/magit-cleanup-buffers nil
  "If non-nil, clean up Magit buffers regularly.
See `aj8/magit-buffer-cleanup-timer' and
`aj8/magit-kill-process-buffers' for details."
  :type 'boolean
  :group 'aj8-lisp)

(defvar aj8/magit-buffer-cleanup-timer nil
  "Timer for automatic cleanup of Magit buffers.")

;; Kill Magit process buffers
(defun aj8/magit-kill-process-buffers ()
  "Kill all Magit process buffers."
  (interactive)
  (let ((killed-buffers 0))
    (when aj8/magit-cleanup-buffers
      (dolist (buffer (buffer-list))
        (when (string-match-p "magit-process:" (buffer-name buffer))
          (kill-buffer buffer)
          (setq killed-buffers (+ 1 killed-buffers)))))
    (if (> killed-buffers 0)
        (message "Killed %s Magit process buffer(s)" killed-buffers))))

(defun aj8/magit-start-buffer-cleanup-timer ()
  "Start the timer to run `aj8/magit-kill-process-buffers' periodically."
  (unless aj8/magit-buffer-cleanup-timer
    (setq aj8/magit-buffer-cleanup-timer
          ;; (run-with-timer 300 300 #'aj8/magit-kill-process-buffers))))
          (run-with-idle-timer 60 t #'aj8/magit-kill-process-buffers))))

(defun aj8/magit-stop-buffer-cleanup-timer ()
  "Stop the `aj8/magit-buffer-cleanup-timer' timer."
  (when aj8/magit-buffer-cleanup-timer
    (cancel-timer aj8/magit-buffer-cleanup-timer)
    (setq aj8/magit-buffer-cleanup-timer nil)))

;;; Misc

;; Kill buffer in other window
(defun my/kill-buffer-other-window ()
  "If there are multiple windows, then kill the buffer in the next window."
  (interactive)
  (unless (one-window-p)
    (let ((win (selected-window)))
      (other-window 1)
      (while (window-parameter (selected-window) 'window-side) ; skip side windows
        (other-window 1))
      (if (not (eq (selected-window) win))   ; don't kill initial window
          (kill-buffer)
        (message "No other window to kill"))
      (select-window win))))

;; Keep focus in side windows after buffer kill
(defun aj8/retain-side-window-focus (orig-fun &rest args)
  "Advise `quit-window' and `kill-current-buffer' to retain side window focus.
This ensures that when killing a buffer in a side window, focus remains
in that side window rather than shifting to another window.
ORIG-FUN is the original function being advised.
ARGS are passed to ORIG-FUN."
  (let ((current-window (selected-window))
        (is-side-window (window-parameter (selected-window) 'window-side)))
    (apply orig-fun args)
    (when (and is-side-window (window-live-p current-window))
      (select-window current-window))))

(defun aj8/switch-to-minibuffer (&optional level)
  "Switch to the currently active recursive minibuffer.
With optional LEVEL argument, switch to that specific minibuffer level.
LEVEL should be 1 for Minibuf-0, 2 for Minibuf-1, etc.  Without LEVEL,
switches to the current depth's minibuffer."
  (interactive "P")
  (let* ((depth (if level
                    (prefix-numeric-value level)
                  (minibuffer-depth)))
         (minibuf-name (format " *Minibuf-%d*" (1- depth))))
    (when (> depth 0)
      (select-window (minibuffer-window))
      (set-window-buffer (minibuffer-window) minibuf-name)
      (message "Switched to minibuffer depth %d (%s)" depth minibuf-name))))

;;;; Coding

;; Enable Eglot selectively
(defun aj8/eglot-ensure-non-remote ()
  "Enable Eglot only if the buffer is not visiting a remote file."
  (unless (file-remote-p default-directory)
    (eglot-ensure)))

;;; Copy hooks to Treesitter modes
(defun aj8/copy-hooks-to-treesitter ()
  "Copy standard mode hooks to their Treesitter equivalents.
This function copies hook functions from standard major modes to their
tree-sitter equivalents, ensuring that customizations made to standard
modes are also applied to tree-sitter modes.  Copying is deferred until
either the legacy mode's feature or the tree-sitter feature loads."
  (let ((pairs '((bash-ts-mode-hook . sh-mode-hook)
                 (css-ts-mode-hook . css-mode-hook)
                 (html-ts-mode-hook . html-mode-hook)
                 (js-ts-mode-hook . js-mode-hook)
                 (json-ts-mode-hook . json-mode-hook)
                 (lua-ts-mode-hook . lua-mode-hook)
                 (markdown-ts-mode-hook . markdown-mode-hook)
                 (python-ts-mode-hook . python-mode-hook)
                 ;; (latex-ts-mode-hook . latex-mode-hook)
                 (yaml-ts-mode-hook . yaml-mode-hook)))
        ;; Exceptions where the feature name differs from the mode name
        (hook-feature-map '((sh-mode-hook . sh-script)
                            (html-mode-hook . sgml-mode)
                            (js-mode-hook . js)
                            (json-mode-hook . json)
                            (python-mode-hook . python))))
    ;; (latex-mode-hook . tex-mode))))
    (dolist (pair pairs)
      (let* ((ts-hook (car pair))
             (orig-hook (cdr pair))
             (feature (or (alist-get orig-hook hook-feature-map)
                          ;; default: remove "-hook" from the hook name
                          (intern (substring (symbol-name orig-hook) 0 -5))))
             (ts-feature (or (alist-get orig-hook hook-feature-map)
                          (intern (substring (symbol-name orig-hook) 0 -5)))))
        (with-eval-after-load feature
          (when (boundp orig-hook)
            (dolist (fn (symbol-value orig-hook))
              (add-hook ts-hook fn))))
        (with-eval-after-load ts-feature
          (when (boundp orig-hook)
            (dolist (fn (symbol-value orig-hook))
              (add-hook ts-hook fn))))))))

(defun aj8/copy-hooks-to-treesitter-now ()
  "Copy standard mode hooks to Treesitter equivalents immediately.
This function copies hook functions from standard major modes to their
tree-sitter equivalents, ensuring that customizations made to standard
modes are also applied to tree-sitter modes."
  (let ((pairs '((bash-ts-mode-hook . sh-mode-hook)
                 (css-ts-mode-hook . css-mode-hook)
                 (html-ts-mode-hook . html-mode-hook)
                 (js-ts-mode-hook . js-mode-hook)
                 (json-ts-mode-hook . json-mode-hook)
                 (lua-ts-mode-hook . lua-mode-hook)
                 (markdown-ts-mode-hook . markdown-mode-hook)
                 (python-ts-mode-hook . python-mode-hook)
                 ;; (latex-ts-mode-hook . latex-mode-hook)
                 (yaml-ts-mode-hook . yaml-mode-hook))))
    ;; (latex-mode-hook . tex-mode))))
    (dolist (pair pairs)
      (let ((ts-hook (car pair))
            (orig-hook (cdr pair)))
        (when (boundp orig-hook)
          (dolist (fn (symbol-value orig-hook))
            (add-hook ts-hook fn)))))))

;;; Misc

;; Format XML buffers
;;   Requires: xmllint
(defun aj8/xml-format-buffer ()
  "Format current buffer using xmllint.
Much faster than `sgml-pretty-print'."
  (interactive)
  (shell-command-on-region 1 (point-max)
                           "xmllint --format -"
                           (current-buffer) t))

;; Open Ruff docs from Flymake
(defun aj8/flymake-ruff-goto-doc ()
  "Browse to the documentation for the Ruff rule on a Flymake diagnostic line.
Scans the Flymake diagnostic at point for a \"[RULE123]\"-style code and
browses to its documentation at https://docs.astral.sh/ruff/rules."
(interactive)
(unless (or (derived-mode-p 'flymake-diagnostics-buffer-mode)
            (derived-mode-p 'flymake-project-diagnostics-mode))
    (user-error "Not in a Flymake diagnostics buffer"))
  (let* ((id (tabulated-list-get-id))
         (diag (or (plist-get id :diagnostic)
                   (user-error "Bad Flymake ID: %S" id)))
         (msg (flymake-diagnostic-text diag)))
    (unless (string-match (rx "[" (group (1+ upper-case) (1+ digit)) "]")
                          msg)
      (user-error "No Ruff rule (like [RULE123]) in diagnostic: %s" msg))
    (browse-url
     (format "https://docs.astral.sh/ruff/rules/%s"
             (match-string 1 msg)))))

;;;; Completion

;;; Cape buffer functions

(defun aj8/cape-project-buffers ()
  "Return buffers belonging to the current project.
If no project is found, return only the current buffer."
  (if-let ((project (project-current)))
      (project-buffers project)
    (list (current-buffer))))

;;; Eglot capf workarounds

;; Because eglot-completion-at-point returns a valid completion table when
;; LSP has no candidates (rather than nil), it blocks subsequent capfs from
;; running. The functions below provide different solutions to this problem.

;; Separate solution: Ensure cape-file runs first
(defun aj8/eglot-ensure-cape-file-first ()
  "Ensure cape-file runs before eglot-completion-at-point.
This ensures file path completion works correctly when eglot is active."
  (when (and (memq #'eglot-completion-at-point completion-at-point-functions)
             (memq #'cape-file completion-at-point-functions))
    (setq-local completion-at-point-functions
                (cons #'cape-file
                      (remove #'cape-file completion-at-point-functions)))))

;; Solution 1: Reorder capfs so eglot runs last
(defun aj8/eglot-reorder-capf ()
  "Move eglot-completion-at-point after other capfs.
Removes both eglot and `t' from buffer-local capf list, then appends
eglot followed by `t' at the end."
  (when (memq #'eglot-completion-at-point completion-at-point-functions)
    (setq-local completion-at-point-functions
                (append (remove #'eglot-completion-at-point
                                (remove t completion-at-point-functions))
                        (list #'eglot-completion-at-point t)))
    ;; Clear hook depth information to prevent re-sorting
    ;;   See aj8/text-mode-capf for detailed explanation.
    (put 'completion-at-point-functions 'hook--depth-alist nil)))

;; Alternative implementation using add-hook
;;   This works with Emacs' hook depth system rather than bypassing it
;;   with setq-local, ensuring correct ordering is maintained when
;;   packages add capfs dynamically after this function runs.
;; (defun aj8/eglot-reorder-capf ()
;;   "Move eglot-completion-at-point after other capfs.
;; Removes both eglot and `t' from buffer-local capf list, then appends
;; eglot followed by `t' at the end."
;;   (when (memq #'eglot-completion-at-point completion-at-point-functions)
;;     (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
;;     (remove-hook 'completion-at-point-functions t t)
;;     (add-hook 'completion-at-point-functions #'eglot-completion-at-point 20 t)
;;     (add-hook 'completion-at-point-functions t 25 t)))

;; Solution 2: Merge eglot with other capfs
(defun aj8/eglot-combine-capf ()
  "Merge eglot-completion-at-point with other capfs.
Shows all candidates together by merging: eglot, unknown capfs (e.g.,
language-specific), cape-dabbrev+dict, and ispell (if present).

cape-file is not merged; it is kept separate at the front because its
completion boundaries (file paths) differ from the word-based boundaries
of the other sources."
  (let* ((current-capfs (buffer-local-value 'completion-at-point-functions
                                            (current-buffer)))
         ;; Extract unknown capfs (e.g., lang-specific)
         (unknown-capfs (seq-remove (lambda (f)
                                      (memq f '(eglot-completion-at-point
                                                cape-dabbrev+dict
                                                ispell-completion-at-point
                                                cape-file
                                                t)))
                                    current-capfs))
         ;; Keep only cape-dabbrev+dict and ispell (if present)
         (other-capfs (seq-filter (lambda (f)
                                    (memq f '(cape-dabbrev+dict
                                              ispell-completion-at-point)))
                                  current-capfs))
         ;; Build merge: eglot, unknown capfs, other capfs
         (merge-list (append (list #'eglot-completion-at-point)
                             unknown-capfs
                             other-capfs)))
    (setq-local completion-at-point-functions
                (append (when (memq #'cape-file current-capfs)
                          (list #'cape-file))
                        (list (apply #'cape-capf-super merge-list) t)))
    ;; Clear hook depth information to prevent re-sorting
    ;;   See aj8/text-mode-capf for detailed explanation.
    (put 'completion-at-point-functions 'hook--depth-alist nil)))

;; Alternative implementation using add-hook
;;   This works with Emacs' hook depth system rather than bypassing it
;;   with setq-local, ensuring correct ordering is maintained when
;;   packages add capfs dynamically after this function runs.
;; (defun aj8/eglot-combine-capf ()
;;   "Merge eglot-completion-at-point with other capfs.
;; Shows all candidates together by merging: eglot, unknown capfs (e.g.,
;; language-specific), cape-dabbrev+dict, and ispell (if present).
;;
;; cape-file is not merged; it is kept separate at the front because its
;; completion boundaries (file paths) differ from the word-based boundaries
;; of the other sources."
;;   (let* ((current-capfs (buffer-local-value 'completion-at-point-functions
;;                                             (current-buffer)))
;;          ;; Extract unknown capfs (e.g., lang-specific)
;;          (unknown-capfs (seq-remove (lambda (f)
;;                                       (memq f '(eglot-completion-at-point
;;                                                cape-dabbrev+dict
;;                                                ispell-completion-at-point
;;                                                cape-file
;;                                                t)))
;;                                     current-capfs))
;;          ;; Keep only cape-dabbrev+dict and ispell (if present)
;;          (other-capfs (seq-filter (lambda (f)
;;                                     (memq f '(cape-dabbrev+dict
;;                                              ispell-completion-at-point)))
;;                                   current-capfs))
;;          ;; Build merge: eglot, unknown capfs, other capfs
;;          (merge-list (append (list #'eglot-completion-at-point)
;;                              unknown-capfs
;;                              other-capfs)))
;;     (setq-local completion-at-point-functions nil)
;;     (when (memq #'cape-file current-capfs)
;;       (add-hook 'completion-at-point-functions #'cape-file -5 t))
;;     (add-hook 'completion-at-point-functions
;;               (apply #'cape-capf-super merge-list)
;;               0 t)
;;     (add-hook 'completion-at-point-functions t 25 t)))

;;; Cape capf functions

;; Completion at point function for prog-mode
;;   Note: LSP (eglot) handles language-specific completions including
;;   keywords, so cape-keyword is not needed. cape-elisp-symbol is also
;;   not needed since elisp-mode already has elisp-completion-at-point.
(defun aj8/prog-mode-capf ()
  "Add file, dabbrev and dict completions for programming modes.
Inserts cape-file first, and cape-dabbrev+dict before `t' in
buffer-local capf list."
  (setq-local completion-at-point-functions
              (append (list #'cape-file)
                      (remove t (buffer-local-value
                                 'completion-at-point-functions
                                 (current-buffer)))
                      (list #'cape-dabbrev+dict t)))
  ;; Clear hook depth information to prevent re-sorting
  ;;   See aj8/text-mode-capf for detailed explanation.
  (put 'completion-at-point-functions 'hook--depth-alist nil))

;; Alternative implementation using add-hook
;;   This works with Emacs' hook depth system rather than bypassing it
;;   with setq-local, ensuring correct ordering is maintained when
;;   packages add capfs dynamically after this function runs.
;; (defun aj8/prog-mode-capf ()
;;   "Add file, dabbrev and dict completions for programming modes.
;; Inserts cape-file first, and cape-dabbrev+dict before `t' in
;; buffer-local capf list."
;;   (remove-hook 'completion-at-point-functions t t)
;;   (add-hook 'completion-at-point-functions #'cape-file -5 t)
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev+dict 5 t)
;;   (add-hook 'completion-at-point-functions t 25 t))

;; Completion at point function for text-mode
(defun aj8/text-mode-capf ()
  "Add file, dabbrev and dict completions for text modes.
Inserts cape-file first, cape-dabbrev+dict before ispell and `t' in
buffer-local capf list."
  (let* ((current-capfs (buffer-local-value 'completion-at-point-functions
                                            (current-buffer)))
         (has-ispell (memq 'ispell-completion-at-point current-capfs)))
    (setq-local completion-at-point-functions
                (append (list #'cape-file)
                        (remove #'ispell-completion-at-point
                                (remove t current-capfs))
                        (list #'cape-dabbrev+dict)
                        (when has-ispell (list #'ispell-completion-at-point))
                        (list t)))
    ;; Clear hook depth information to prevent re-sorting
    ;;   Emacs' add-hook maintains depth information that causes the list to
    ;;   be re-sorted when subsequent add-hook calls occur (e.g., when eglot
    ;;   connects). Clearing this prevents ispell-completion-at-point from
    ;;   being moved to after `t' where it would be unreachable.
    (put 'completion-at-point-functions 'hook--depth-alist nil)))

;; Alternative implementation using add-hook
;;   This works with Emacs' hook depth system rather than bypassing it
;;   with setq-local, ensuring correct ordering is maintained when
;;   packages add capfs dynamically after this function runs.
;; (defun aj8/text-mode-capf ()
;;   "Add file, dabbrev and dict completions for text modes.
;; Inserts cape-file first, cape-dabbrev+dict before ispell and `t' in
;; buffer-local capf list."
;;   (let ((has-ispell (memq 'ispell-completion-at-point
;;                          completion-at-point-functions)))
;;     (remove-hook 'completion-at-point-functions t t)
;;     (add-hook 'completion-at-point-functions #'cape-file -5 t)
;;     (add-hook 'completion-at-point-functions #'cape-dabbrev+dict 5 t)
;;     (when has-ispell
;;       (add-hook 'completion-at-point-functions #'ispell-completion-at-point 10 t))
;;     (add-hook 'completion-at-point-functions t 25 t)))

;; Completion at point function for LLM chat modes
(defun aj8/llm-chat-mode-capf ()
  "Add dabbrev and dict completions for LLM chat modes.
Inserts cape-dabbrev+dict before `t' in buffer-local capf list."
  (setq-local completion-at-point-functions
              (append (remove t (buffer-local-value
                                 'completion-at-point-functions
                                 (current-buffer)))
                      (list #'cape-dabbrev+dict t)))
  ;; Clear hook depth information to prevent re-sorting
  ;;   See aj8/text-mode-capf for detailed explanation.
  (put 'completion-at-point-functions 'hook--depth-alist nil))

;;; Consult git grep exclusions

(defcustom aj8/consult-git-grep-excludes
  '("**/archive/**"       ;; exclude any archive/ tree
    "**/build/**")        ;; exclude any build/ tree
  "List of Git path globs to exclude from `consult-git-grep'."
  :type '(repeat string)
  :group 'aj8-lisp)

;; Exclude some dirs from consult-git-grep
;;   `consult--git-grep-make-builder' returns a builder function which
;;   produces: (COMMAND-LIST . HIGHLIGHT-FN), where COMMAND-LIST is the list
;;   of args to call git grep with and HIGHLIGHT-FN is the function Consult
;;   will use to fontify matches.
(defun aj8/consult-git-grep (orig-fun paths)
  "Advise `consult--git-grep-make-builder' to exclude directories.
ORIG-FUN is the original function being advised."
  ;; Call the original function to get its builder
  (let ((orig-builder (funcall orig-fun paths)))
    ;; Return a new builder that wraps the old one:
    (lambda (input)
      (let* ((res (funcall orig-builder input))
             (cmd (car res))
             (hl-fn (cdr res))
             ;; Strip off the trailing PATHS
             (fixed (butlast cmd (length paths)))
             ;; Assemble: flags, "--", each exclude, then the paths again
             (new-cmd (append fixed
                              (list "--")
                              (mapcar (lambda (g) (concat ":(exclude)" g))
                                      aj8/consult-git-grep-excludes)
                              paths)))
        (cons new-cmd hl-fn)))))

;;; Corfu navigation

(defun my/corfu-beginning-of-prompt ()
  "Move to beginning of completion input."
  (interactive)
  (corfu--goto -1)
  (goto-char (car completion-in-region--data)))

(defun my/corfu-end-of-prompt ()
  "Move to end of completion input."
  (interactive)
  (corfu--goto -1)
  (goto-char (cadr completion-in-region--data)))

;;; Orderless on the fly matching styles toggle

(defun aj8/orderless-matching-style-cycle ()
  "Cycle through orderless matching styles."
  (interactive)
  (cond
   ((eq (car orderless-matching-styles) 'orderless-literal)
    (setq-local orderless-matching-styles '(orderless-prefixes))
    (minibuffer-message "Matching: prefixes"))
   ((eq (car orderless-matching-styles) 'orderless-prefixes)
    (setq-local orderless-matching-styles '(orderless-regexp))
    (minibuffer-message "Matching: regexp"))
   ((eq (car orderless-matching-styles) 'orderless-regexp)
    (setq-local orderless-matching-styles '(orderless-flex))
    (minibuffer-message "Matching: flex"))
   ((eq (car orderless-matching-styles) 'orderless-flex)
    (setq-local orderless-matching-styles '(orderless-literal))
    (minibuffer-message "Matching: literal"))
   (t
    (setq-local orderless-matching-styles '(orderless-literal))
    (minibuffer-message "Matching: literal")))
  ;; Invalidate cached input to force candidate recomputation
  (setq vertico--input t)
  ;; Update the Vertico display with new matching style
  (vertico--exhibit))

;;; Orderless style dispatchers

;; Flex
(defun aj8/orderless-dispatch-flex-if-twiddle (pattern _index _total)
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

;; Literal / Exact
(defun aj8/orderless-dispatch-literal-if-equal (pattern _index _total)
  (when (string-suffix-p "=" pattern)
    `(orderless-regexp . ,(concat "^" (regexp-quote (substring pattern 0 -1)) "$"))))

;; Prefixes
(defun aj8/orderless-dispatch-prefixes-if-less (pattern _index _total)
  (when (string-suffix-p "<" pattern)
    `(orderless-literal-prefix . ,(substring pattern 0 -1))))

;; Regexp
(defun aj8/orderless-dispatch-regexp-if-star (pattern _index _total)
  (when (string-suffix-p "*" pattern)
    `(orderless-regexp . ,(substring pattern 0 -1))))

;; Exclude
(defun aj8/orderless-dispatch-without-if-bang (pattern _index _total)
  (when (string-suffix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 0 -1))))

;;; Vertico on the fly sort toggle

(defun aj8/vertico-sort-toggle ()
  "Cycle through sorting modes: history-length-alpha -> length-alpha -> alpha."
  (interactive)
  (cond
   ((eq vertico-sort-override-function 'vertico-sort-length-alpha)
    (setq-local vertico-sort-override-function #'vertico-sort-alpha)
    (minibuffer-message "Sorting by alpha"))
   ((eq vertico-sort-override-function 'vertico-sort-alpha)
    (setq-local vertico-sort-override-function nil)
    (minibuffer-message "Sorting by history-length-alpha"))
   (t
    (setq-local vertico-sort-override-function #'vertico-sort-length-alpha)
    (minibuffer-message "Sorting by length-alpha")))
  ;; Invalidate cached input to force candidate recomputation
  (setq vertico--input t)
  ;; Update the Vertico display with new sort order
  (vertico--exhibit))

;;;; Editing

;;; Smartparens

;; Macro for Smartparens
(defmacro my/def-pairs (pairs)
  "Define functions for pairing.  PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
  `(progn
     ,@(cl-loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))

;; Pairs for Smartparens
(my/def-pairs ((paren . "(")
               (bracket . "[")
               (brace . "{")
               (single-quote . "'")
               (double-quote . "\"")
               (back-quote . "`")))

;; Move point up and down s-expressions
(defun aj8/sp--sexp-dwim-direction (for-down-p)
  "Determine direction for sexp movement based on context.
FOR-DOWN-P should be t for down movement, nil for up movement.  Returns
the direction (1 for forward, -1 for backward) to pass to sp-down-sexp
or sp-up-sexp."
  (when-let* ((prev-paren-pos (save-excursion
                                (search-backward-regexp "[()]" nil t)))
              (next-paren-pos (save-excursion
                                (search-forward-regexp "[()]" nil t)))
              (forward-dist (- next-paren-pos (point)))
              (backward-dist (- (point) prev-paren-pos)))
    (let* ((prev-paren-left-p (= (char-after prev-paren-pos) ?\())
           (next-paren-left-p (= (char-before next-paren-pos) ?\())
           (prev-paren-right-p (= (char-after prev-paren-pos) ?\)))
           (next-paren-right-p (= (char-before next-paren-pos) ?\)))
           (point-eol-p (looking-at-p "[[:space:]]*$"))
           (point-bol-p (looking-back "^[[:space:]]*")))
      ;; Direction logic differs between down and up movement
      (if for-down-p
          ;; Down movement logic
          (cond ((and prev-paren-left-p next-paren-left-p) 1)
                ((and prev-paren-right-p next-paren-right-p) -1)
                (point-bol-p 1)
                (point-eol-p -1)
                (t (if (> forward-dist backward-dist) -1 1)))
        ;; Up movement logic
        (cond ((and prev-paren-left-p next-paren-left-p) -1)
              ((and prev-paren-right-p next-paren-right-p) 1)
              (point-bol-p -1)
              (point-eol-p 1)
              (t (if (> forward-dist backward-dist) -1 1)))))))

(defun aj8/sp-down-sexp-dwim ()
  "Move point down one level of s-expression (sexp).

The function moves point in the direction that makes sense, i.e.  in the
forward direction if point is surrounded by left parentheses, and in the
backward direction if surrounded by right parentheses.  If point is
between a right and a left parenthesis it chooses the closest direction
to move down.

Note that the logic in this function only considers parenthesis-
delimited s-expressions."
  (interactive)
  (when-let ((direction (aj8/sp--sexp-dwim-direction t)))
    (sp-down-sexp direction)))

(defun aj8/sp-up-sexp-dwim ()
  "Move point up one level of s-expression (sexp).

The function moves point in the direction that makes sense, i.e.  in the
forward direction if point is surrounded by right parentheses, and in
the backward direction if surrounded by left parentheses.  If point is
between a left and a right parenthesis it chooses the closest direction
to move up.

Note that the logic in this function only considers parenthesis-
delimited s-expressions."
  (interactive)
  (when-let ((direction (aj8/sp--sexp-dwim-direction nil)))
    (sp-up-sexp direction)))

;;; Misc

;; Copy symbol at point
(defun my/copy-symbol-at-point ()
  "Add the symbol at point to the kill ring."
  (interactive)
  (let ((symbol (thing-at-point 'symbol))
        (bounds (bounds-of-thing-at-point 'symbol)))
    (if symbol
        (progn
          (kill-new symbol)
          (if bounds
              (pulse-momentary-highlight-region (car bounds) (cdr bounds))
            (message "Copied: %s" symbol)))
      (message "No symbol at point."))))

;; Capitalize word at point
(defun aj8/capitalize-word-at-point ()
  "Capitalize word at point from the beginning of the word."
  (interactive)
  (unless (looking-at "\\<")
    (backward-word))
  (capitalize-word 1))

;; Delete blank lines in region
(defun aj8/delete-blank-lines-region (beg end)
  "Delete excess blank lines in region, leaving at most one.
BEG and END specify the region to operate on."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (replace-regexp "\\(\\(^\\s-*$\\)\n\\)\\{2,\\}" "\n" nil beg end)))

;; Remove DOS ^M characters
(defun aj8/dos2unix ()
  "Convert the end-of-line format from DOS-style to Unix-style."
  (interactive)
  (let* ((region-active (region-active-p))
         (start (if region-active (region-beginning) (point-min)))
         (end   (if region-active (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (search-forward "\r" end t) (replace-match "")))))

;; Open line above
(defun my/open-line-above ()
  "Open line above the current line."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))

;; Open line below
(defun my/open-line-below ()
  "Open line below the current line."
  (interactive)
  (end-of-line)
  (open-line 1)
  (forward-line)
  (indent-according-to-mode))

;; Wrapper for open line above/below
(defun my/open-line (arg)
  "Open a line above or below the current one.
ARG is the prefix argument.  With prefix argument
\\<global-map>\\[universal-argument], open a line above; otherwise open
a line below."
  (interactive "p")
  (if (equal arg 4)  ;; C-u gives a prefix argument of 4
      (my/open-line-above)
    (my/open-line-below)))

;;;; Files

;; Disable dired-dwim-target with prefix argument

(defun aj8/dired-do-rename-dwim (&optional arg)
  "Rename files with DWIM target behavior unless called with prefix ARG.
When called without a prefix argument, temporarily sets
`dired-dwim-target' to t, making the rename operation suggest the
directory from another visible Dired buffer as the target.  When called
with a prefix argument (C-u), temporarily sets `dired-dwim-target' to
nil, disabling the DWIM behavior and prompting for the target directory
without a default suggestion."
  (interactive "P")
  (let ((dired-dwim-target (if arg nil t)))
    (call-interactively #'dired-do-rename)))

(defun aj8/dired-dwim-target-unless-prefix ()
  "Return DWIM target directories unless last command used prefix arg.
This function is designed to be used as the value of
`dired-dwim-target'. It returns a list of target directories from other
visible Dired buffers (DWIM behavior) by default, but returns
nil (disabling DWIM) when the command was invoked with a prefix
argument (C-u)."
    (unless current-prefix-arg
      (dired-dwim-target-next)))

;;; Selective backup inibition

(defconst aj8/no-backup-regexp
  "\\`gptel-.*\\.md\\'"
  "Regexp matching file names for which we do *not* want backups.")

(defun aj8/backup-enable-predicate (file)
  "Decide whether to do backups for FILE.
We defer to `normal-backup-enable-predicate`, then skip any name
matching `aj8/no-backup-regexp`."
  (and (normal-backup-enable-predicate file)
       (not (string-match-p aj8/no-backup-regexp
                            (file-name-nondirectory file)))))

(setq backup-enable-predicate #'aj8/backup-enable-predicate)

;;; Misc

;; Keep CSV fields
(defun aj8/csv-keep-fields (fields beg end)
  "Keep specified fields of each line in the region, removing others.
If not set, the region defaults to the CSV records around point.  Fields
are separated by `csv-separators' and null fields are allowed anywhere.
Field indices increase from 1 on the left or decrease from -1 on the
right.  The removed fields are stored for use by `csv-yank-fields'.
Fields can be specified in any order but are saved in increasing index
order.  Ignore blank and comment lines.

When called interactively, a prefix argument specifies a single field,
otherwise prompt for a field list, which may include ranges in the form
m-n, where m < n and n defaults to the last field index if omitted.

When called non-interactively, FIELDS is a single field index or a list
of field indices, with ranges specified as (m.n) or (m), and BEG and END
specify the region to process."
  ;; (interactive "*P\nr")
  (interactive (csv-interactive-args 'multiple))
  (barf-if-buffer-read-only)
  ;; Invert fields: keep specified, kill the rest
  (let ((fields-to-kill
         (save-excursion
           (goto-char beg)
           (when (eolp) (error "First record is empty"))
           ;; Count total fields (same as csv-kill-many-columns)
           (let ((last 1))
             (csv-end-of-field)
             (while (not (eolp))
               (forward-char)
               (csv-end-of-field)
               (setq last (1+ last)))
             ;; Expand fields using csv-mode's logic
             (let ((f fields))
               (while f
                 (cond ((consp (car f))
                        ;; Expand range (m.n) -> m m+1 ... n
                        (let* ((range (car f)) (cdrf (cdr f))
                               (m (car range)) (n (cdr range)))
                          (if (< m 0) (setq m (+ m last 1)))
                          (if n
                              (if (< n 0) (setq n (+ n last 1)))
                            (setq n last))
                          (setq range (list n))
                          (while (> n m) (push (setq n (1- n)) range))
                          (setcar f (car range))
                          (setcdr f (cdr range))
                          (setcdr (setq f (last range)) cdrf)))
                       ((zerop (car f)) (setcar f 1))
                       ((< (car f) 0) (setcar f (+ last (car f) 1))))
                 (setq f (cdr f))))
             ;; Return fields not in keep list
             (seq-difference (number-sequence 1 last) fields)))))
    (csv-kill-fields fields-to-kill beg end)))

;;;; Help

;;;; Navigation

;;; Movement by comments

;; Move up a line, skipping comments and empty lines
(defun aj8/previous-line ()
  "Move to the previous line that is not empty and not a comment."
  (interactive)
  (previous-line)
  ;; Skip empty lines
  (if (string-match-p "^[[:space:]]*$" (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position)))
      (aj8/previous-line))
  ;; Skip comment lines
  (if (string-match-p "^[[:space:]]*\\s<" (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position)))
      (aj8/previous-line)))

;; Move down a line, skipping comments and empty lines
(defun aj8/next-line ()
  "Move to the next line that is not empty and not a comment."
  (interactive)
  (next-line) ; use forward-line?
  ;; Skip empty lines
  (if (string-match-p "^[[:space:]]*$" (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position)))
      (aj8/next-line))
  ;; Skip comment lines
  (if (string-match-p "^[[:space:]]*\\s<" (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position)))
      (aj8/next-line)))

;; Move to previous comment
(defun aj8/previous-comment ()
  "Move to the previous comment line."
  (interactive)
  (previous-line)
  ;; Skip empty lines
  (if (string-match-p "^[[:space:]]*$" (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position)))
      (aj8/previous-comment))
  ;; Skip lines that are not comments
  (if (not (string-match-p "^[[:space:]]*\\s<" (buffer-substring-no-properties
                                                (line-beginning-position)
                                                (line-end-position))))
      (aj8/previous-comment)))

;; Move to next comment
(defun aj8/next-comment ()
  "Move to the next comment line."
  (interactive)
  (next-line) ; use forward-line?
  ;; Skip empty lines
  (if (string-match-p "^[[:space:]]*$" (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position)))
      (aj8/next-comment))
  ;; Skip lines that are not comments
  (if (not (string-match-p "^[[:space:]]*\\s<" (buffer-substring-no-properties
                                                (line-beginning-position)
                                                (line-end-position))))
      (aj8/next-comment)))

;;; Helper functions for Mosey

;; Move to beginning of comment
(defun aj8/goto-beginning-of-comment ()
  "Move point to beginning of comment on current line.
See also `mosey-goto-beginning-of-comment-text'."
  (when (save-excursion
          (end-of-line)
          (re-search-backward (rx (syntax comment-start))
                              (line-beginning-position) t))
    (goto-char (- (match-end 0) 1))))

;; Combine Mosey with regular move-end-of-line
(defun aj8/mosey-eol ()
  "Move point to end of line and then cycle backward between Mosey points."
  (interactive)
  (if (or (eolp)
          (eq last-command 'aj8/mosey-eol))
      (mosey-eol-backward-cycle)
    (move-end-of-line nil)))

;; Combine Mosey with regular move-beginning-of-line
(defun aj8/mosey-bol ()
  "Move point to beginning of line and then cycle forward between Mosey points."
  (interactive)
  (if (or (bolp)
          (eq last-command 'aj8/mosey-eol))
      (mosey-bol-forward-cycle)
    (move-beginning-of-line nil)))

;;; Misc

;; Backward movement by whitespace
;;   (complements the built-in forward-whitespace)
(defun my/backward-whitespace (arg)
  "Move point to the start of the current sequence of whitespace chars.
ARG specifies the number of times to move backward."
  (interactive "^p")
  (forward-whitespace (- arg)))

;;;; Outline

;;; Set outline header format

;; Elisp files
(defun aj8/outline-headers-for-semicolon-buffers ()
  "Set outline header format for buffers with ';' comments."
  ;; Set custom outline heading format
  (setq-local outline-regexp "\\(;;+ \\)\\([^( ]\\)")
  (setq-local outline-heading-alist
              '((";;;;; " . 1)
                (";;;; " . 2)
                (";;; " . 3)
                (";; " . 4)))
  ;; Don't use 'lisp-outline-level (doesn't use outline-heading-alist)
  (setq-local outline-level 'aj8/outline-level))

;; Shell-script files
(defun aj8/outline-headers-for-hash-mark-buffers ()
  "Set outline header format for buffers with '#' comments."
  ;; Set custom outline heading format
  (setq-local outline-regexp "\\(##+ \\)\\([^( ]\\)")
  (setq-local outline-heading-alist
              '(("##### " . 1)
                ("#### " . 2)
                ("### " . 3)
                ("## " . 4)))
  ;; Use custom 'outline-level' for sh-mode
  (setq-local outline-level 'aj8/outline-level))

;; LaTeX files
;;   TODO: outline-cycle shows all sub-headings, not just one level down
(defun aj8/outline-headers-for-percentage-buffers ()
  "Set outline header format for buffers with '%' comments."
  ;; Set custom outline heading format
  (setq-local outline-regexp "\\(%+ \\)\\([^(\\ ]\\)")
  (setq-local outline-heading-alist
              '(("%%%%% " . 1)
                ("%%%% " . 2)
                ("%%% " . 3)
                ("%% " . 4)
                ("% " . 5)))
  ;; Use custom 'outline-level' for sh-mode
  (setq-local outline-level 'aj8/outline-level))

;; Xresources files
(defun aj8/outline-headers-for-exclamation-mark-buffers ()
  "Set outline header format for buffers with '!' comments."
  ;; Set custom outline heading format
  (setq-local outline-regexp "\\(!!+ \\)\\([^( ]\\)")
  (setq-local outline-heading-alist
              '(("!!!!! " . 1)
                ("!!!! " . 2)
                ("!!! " . 3)
                ("!! " . 4)))
  ;; Don't use 'conf-outline-level (doesn't use outline-heading-alist)
  (setq-local outline-level 'aj8/outline-level))

;; Return outline heading level
(defun aj8/outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.  This is the level
specified in `outline-heading-alist' and not based on the number of
characters matched by `outline-regexp'."
  ;; (outline-back-to-heading)
  (cdr (assoc (match-string 1) outline-heading-alist)))

;; Print outline level
(defun aj8/outline-level-message ()
  "Print outline level to echo area."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (let ((level (funcall outline-level)))
      (message "Level: %d" level))))

;;; Keybindings for outline-(minor-)mode

(defun aj8/outline--body-p ()
  "Check if there is a body."
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (and (not (eobp))
         (progn (forward-char 1)
                (not (outline-on-heading-p))))))

(defun aj8/outline--body-visible-p ()
  "Check if there is a visible body."
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (not (outline-invisible-p))))

(defun aj8/outline--subheadings-p ()
  "Check if there is a sub-heading."
  (save-excursion
    (outline-back-to-heading)
    (let ((level (funcall outline-level)))
      (outline-next-heading)
      (and (not (eobp))
           (< level (funcall outline-level))))))

(defun outline--subheadings-visible-p ()
  "Check if there is a visible sub-heading."
  (save-excursion
    (outline-next-heading)
    (not (outline-invisible-p))))

;; Hide more heading levels
(defun my/outline-hide-more ()
  (interactive)
  (when (outline-on-heading-p)
    (cond ((and (aj8/outline--body-p)
                (aj8/outline--body-visible-p))
           (outline-hide-entry)
           (outline-hide-leaves))
          (t
           (outline-hide-subtree)))))

;; Show more heading levels
(defun my/outline-show-more ()
  (interactive)
  (when (outline-on-heading-p)
    (cond ((and (aj8/outline--subheadings-p)
                (not (outline--subheadings-visible-p)))
           (outline-show-children))
          ((and (not (aj8/outline--subheadings-p))
                (not (aj8/outline--body-visible-p)))
           (outline-show-subtree))
          ((and (aj8/outline--body-p)
                (not (aj8/outline--body-visible-p)))
           (outline-show-entry))
          (t
           (outline-show-subtree)))))

;;; Misc

;; Copy visible region
;;   Imported from org-mode
(defun my/org-copy-visible (beg end)
  "Copy the visible parts of the region between BEG and END."
  (interactive "r")
  (let ((result ""))
    (while (/= beg end)
      (when (get-char-property beg 'invisible)
	(setq beg (next-single-char-property-change beg 'invisible nil end)))
      (let ((next (next-single-char-property-change beg 'invisible nil end)))
	(setq result (concat result (buffer-substring beg next)))
	(setq beg next)))
    (setq deactivate-mark t)
    (kill-new result)
    (message "Visible strings have been copied to the kill ring.")))

;;;; Search

;;;; Selection

;;; Better mark-word

;; Mark whole word (forward)
(defun aj8/mark-word-forward (arg)
  "Set mark ARG words forward from point.
Repeat command to select additional words.  Like `mark-word', but select
entire word at point."
  (interactive "p")
  (when (and
         (not (eq last-command this-command))
         (not (eq last-command #'aj8/mark-word-backward)))
    (if (and (looking-at "[[:alnum:]]") (looking-back "[[:alnum:]]"))
        (backward-word))
    (set-mark (point)))
  (forward-word arg))

;; Mark whole word (backward)
(defun aj8/mark-word-backward (arg)
  "Set mark ARG words backward from point.
Repeat command to select additional words.  Like `mark-word', but select
entire word at point."
  (interactive "p")
  (when (and
         (not (eq last-command this-command))
         (not (eq last-command #'aj8/mark-word-forward)))
    (if (and (looking-at "[[:alnum:]]") (looking-back "[[:alnum:]]"))
        (forward-word))
    (set-mark (point)))
  (backward-word arg))

;; Select code block
(defun aj8/select-code-block ()
  "Select the current code block delimited by triple backticks."
  (interactive)
    (when (re-search-backward "^```\\w+\n" nil t)
      (let ((content-start (match-end 0)))
        (goto-char content-start)
        (when (re-search-forward "^```" nil t)
          (set-mark (match-beginning 0))
          (goto-char content-start)))))

;;;; Spelling

;;; ispell

;; Toggle ispell program
;;   Requires: aspell, aspell-en, aspell-sv, hunspell, hunspell-sv
(defun aj8/toggle-ispell-program ()
  "Toggle `ispell' program.
If current program is `aspell', switch to `hunspell', and vice versa."
  (interactive)
  (cond
   ((string-match-p "hunspell" ispell-program-name)   ; switch to aspell
    (setq ispell-program-name "aspell")
    (setq ispell-dictionary "en_US")
    (setq ispell-extra-args '("--sug-mode=ultra"))
    (setq ispell-personal-dictionary nil))
   ((string-match-p "aspell" ispell-program-name)   ; switch to hunspell
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "en_US,sv_SE")
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_US,sv_SE")
    (setq ispell-personal-dictionary "~/.hunspell_personal")
    (unless (file-exists-p ispell-personal-dictionary)
      (with-temp-buffer (write-file ispell-personal-dictionary))))
   (t
    (user-error "`ispell-program' must be either `aspell' or `hunspell'"))))

;;; Flyspell

;; Setup for web-mode
(defun my/web-mode-flyspell-verify ()
  "Flyspell predicate of `web-mode'."
  (let* ((font-face-at-point (get-text-property (- (point) 1) 'face))
         rlt)
    ;; If rlt is t, the word at point is POSSIBLY a typo, continue checking.
    (setq rlt t)
    ;; if rlt is nil, the word at point is definitely NOT a typo.
    ;; (setq rlt nil)
    rlt))

;; Setup for web-mode (with blacklist)
;; (defun my/web-mode-flyspell-verify ()
;;   "Flyspell predicate of `web-mode'."
;;   (let* ((f (get-text-property (- (point) 1) 'face))
;;          rlt)
;;     (cond
;;      ;; Check the words with these font faces, possibly.
;;      ;; this *blacklist* will be tweaked in next condition
;;      ((not (memq f '(web-mode-html-attr-value-face
;;                      web-mode-html-tag-face
;;                      web-mode-html-attr-name-face
;;                      web-mode-constant-face
;;                      web-mode-doctype-face
;;                      web-mode-keyword-face
;;                      web-mode-comment-face ;; focus on get html label right
;;                      web-mode-function-name-face
;;                      web-mode-variable-name-face
;;                      web-mode-css-property-name-face
;;                      web-mode-css-selector-face
;;                      web-mode-css-color-face
;;                      web-mode-type-face
;;                      web-mode-block-control-face)))
;;       (setq rlt t))
;;      ;; check attribute value under certain conditions
;;      ((memq f '(web-mode-html-attr-value-face))
;;       (save-excursion
;;         (search-backward-regexp "=['\"]" (line-beginning-position) t)
;;         (backward-char)
;;         (setq rlt (string-match "^\\(value\\|class\\|ng[A-Za-z0-9-]*\\)$"
;;                                 (thing-at-point 'symbol)))))
;;      ;; finalize the blacklist
;;      (t
;;       (setq rlt nil)))
;;     rlt))

;; Goto previous flyspell error
(defun my/flyspell-goto-previous-error (arg)
  "Go to ARG previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto end of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the previous error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))))))

;; Jinx

(defun aj8/jinx-correct-backward ()
  "Start correcting misspelled words in the backward direction.
This is a wrapper for `jinx-correct-all' for consistency with
`aj8/jinx-correct-forward'."
  (interactive)
  (jinx-correct-all))

(defun aj8/jinx-correct-forward ()
  "Start correcting misspelled words in the forward direction.
Jumps to the next misspelled word after point, then starts the
correction UI."
  (interactive)
  (jinx-next 1)
  (jinx-correct-all))

;;;; Terminal

;; Setup for Eshell
(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  ;; (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  ;; (setq eshell-history-size         10000
  ;;       eshell-buffer-maximum-lines 10000
  ;;       eshell-hist-ignoredups t
  ;;       eshell-scroll-to-bottom-on-input t))
  ;; Rebind history keys
  (keymap-set eshell-hist-mode-map "C-<down>" nil)   ; unbind
  (keymap-set eshell-hist-mode-map "C-<up>" nil)   ; unbind
  (keymap-set eshell-hist-mode-map "<down>" #'eshell-next-input)
  (keymap-set eshell-hist-mode-map "<up>" #'eshell-previous-input)
  (keymap-set eshell-hist-mode-map "M-<down>"
    #'eshell-next-matching-input-from-input)
  (keymap-set eshell-hist-mode-map "M-<up>"
    #'eshell-previous-matching-input-from-input))

;;;; Theme

;;; Time functions

(require 'solar)

(setq calendar-latitude 59.336)
(setq calendar-longitude 18.08)

(defun aj8/time-sunrise ()
  "Get clean sunrise time string from `solar-sunset-sunrise'."
  (cl-first (cl-first (solar-sunrise-sunset (calendar-current-date)))))

(defun aj8/time-sunset ()
  "Get clean sunset time string from `solar-sunset-sunrise'."
  (cl-first (cl-second (solar-sunrise-sunset (calendar-current-date)))))

(defcustom aj8/time-sunrise (aj8/time-sunrise)
  "Sunrise time."
  :type 'number
  :group 'aj8-lisp)

(defcustom aj8/time-sunset (aj8/time-sunset)
  "Sunrise time."
  :type 'number
  :group 'aj8-lisp)

(defun aj8/daytime-p ()
  "Return t if current time is daytime, nil otherwise."
  (let* ((time (decode-time))
         (hours (decoded-time-hour time))
         (minutes (/ (decoded-time-minute time) 60.0))
         (seconds (/ (decoded-time-second time) 3600.0))
         (current-time (+ hours minutes seconds)))
    (and (> current-time aj8/time-sunrise)
         (< current-time aj8/time-sunset))))

;;;; Version control

;;; Ediff

(defvar my-ediff-windows nil
  "Saved window configuration.")

(defun my-ediff-save-windows ()
  "Save current window configuration."
  (setq my-ediff-windows (current-window-configuration)))

(defun my-ediff-restore-windows ()
  "Restore saved window configuration."
  (set-window-configuration my-ediff-windows))

;; Enable concatenation in ediff
;;   (when merging, use both variants A and B, one after the other)
(defun aj8/ediff--copy-both (target)
  "Copy concatenated A and B variants to TARGET buffer.
TARGET should be one of 'A, 'B, or 'C."
  (ediff-copy-diff ediff-current-difference nil target nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference
                                               'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference
                                               'B ediff-control-buffer))))

(defun aj8/ediff-copy-both-to-C ()
  "Add both variants to merge file."
  (interactive)
  (aj8/ediff--copy-both 'C))

(defun aj8/ediff-copy-B-to-A ()
  "Copy B's diff region to A, preserving A's content."
  (interactive)
  (aj8/ediff--copy-both 'A))

(defun aj8/ediff-copy-A-to-B ()
  "Copy A's diff region to B, preserving B's content."
  (interactive)
  (aj8/ediff--copy-both 'B))

(defun aj8/ediff-buffers-ignore-regexp ()
  "Run ediff on two buffers, ignoring lines that match REGEXP.
Note that matching lines, in either file, are hidden in the output."
  (interactive)
  (let* ((buffer-A (get-buffer (read-buffer "Select buffer A: ")))
         (buffer-B (get-buffer (read-buffer "Select buffer B: ")))
         (regexp (read-regexp "Ignore lines matching regexp: "))
         (temp-buffer-A (generate-new-buffer (buffer-name buffer-A)))
         (temp-buffer-B (generate-new-buffer (buffer-name buffer-B))))
    ;; Load and preprocess buffer A
    (with-current-buffer temp-buffer-A
      (insert-buffer buffer-A)
      (flush-lines regexp))
    ;; Load and preprocess buffer B
    (with-current-buffer temp-buffer-B
      (insert-buffer buffer-B)
      (flush-lines regexp))
    ;; Define cleanup function to remove buffers
    (defun cleanup-ediff-buffers ()
      (kill-buffer ediff-buffer-A)
      (kill-buffer ediff-buffer-B)
      (remove-hook 'ediff-cleanup-hook 'cleanup-ediff-buffers))
    ;; Add cleanup hook for when ediff is done
    (add-hook 'ediff-cleanup-hook 'cleanup-ediff-buffers)
    ;; Start ediff session between buffers
    (ediff-buffers temp-buffer-A temp-buffer-B)))

(defun aj8/ediff-regions-linewise-3 ()
  "Run Ediff on three regions in specified buffers.
BUFFER-A, BUFFER-B and BUFFER-C are the buffers to be compared.
Regions (i.e., point and mark) must be set in advance."
  (interactive)
  (let ((buffer-A (get-buffer (read-buffer "Select buffer A: ")))
        (buffer-B (get-buffer (read-buffer "Select buffer B: ")))
        (buffer-C (get-buffer (read-buffer "Select buffer C: "))))
    (when (and buffer-A buffer-B buffer-C) ;; Ensure all buffers are valid
      (let* ((region-A (with-current-buffer buffer-A
                     (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (error "No region selected in first buffer"))))
             (region-B (with-current-buffer buffer-B
                     (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (error "No region selected in second buffer"))))
             (region-C (with-current-buffer buffer-C
                     (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (error "No region selected in third buffer"))))
             (temp-buffer-A (generate-new-buffer (buffer-name buffer-A)))
             (temp-buffer-B (generate-new-buffer (buffer-name buffer-B)))
             (temp-buffer-C (generate-new-buffer (buffer-name buffer-C))))
        ;; Fill temp buffers with the selected regions
        (with-current-buffer temp-buffer-A (insert region-A))
        (with-current-buffer temp-buffer-B (insert region-B))
        (with-current-buffer temp-buffer-C (insert region-C))
        ;; Define cleanup function to remove buffers
        (defun cleanup-ediff-buffers ()
          (kill-buffer ediff-buffer-A)
          (kill-buffer ediff-buffer-B)
          (kill-buffer ediff-buffer-C)
          (remove-hook 'ediff-cleanup-hook 'cleanup-ediff-buffers))
        ;; Add cleanup hook for when ediff is done
        (add-hook 'ediff-cleanup-hook 'cleanup-ediff-buffers)
        ;; Start ediff-buffers3 and cleanup when done
        (ediff-buffers3 temp-buffer-A temp-buffer-B temp-buffer-C)))))

;;; Magit

;; Disable magit-wip-mode for remote (Tramp) buffers
(defun aj8/disable-magit-wip-mode-if-remote ()
  "Disable `magit-wip-mode' for remote buffers."
  (if (file-remote-p default-directory)
      (magit-wip-mode -1)  ; disable in remote directory
    (magit-wip-mode 1)))   ; enable otherwise

;; Add filenames to Magit commit messages
(defun aj8/magit-commit-add-files ()
  "Insert staged filenames into the Magit commit message buffer."
  (interactive)
  ;; (when (derived-mode-p 'git-commit-mode)
    (let* ((files (magit-staged-files))
           (filenames (mapcar #'file-name-nondirectory files)))
      (goto-char (point-min))
      (insert (string-join filenames ", ") ": ")
      (open-line 1)))
  ;; )

;; Increase diff context for magit-gptcommit
(defun aj8/magit-gptcommit-extended-context ()
  "Advise `magit-gptcommit--staged-diff' to provide extended context lines.
Replaces `magit-gptcommit--staged-diff' to provide 50 lines of context."
  (let* ((files (magit-staged-files))
         (diff (magit-git-output "diff" "--staged" "-w" "-U50"))
         ;; Split diff by file and add headers
         (diff-parts (split-string diff "^diff --git" t))
         (annotated-diff
          (mapconcat
           (lambda (part)
             (if (string-match "a/\\([^ ]+\\) b/" part)
                 (let ((filename (match-string 1 part)))
                   (format "=== File: %s ===\ndiff --git%s" filename part))
               part))
           diff-parts
           "")))
    annotated-diff))

;; Add full file contents for magit-gptcommit
(defun aj8/magit-gptcommit-add-file-context (diff)
  "Advise `magit-gptcommit--staged-diff' to augment DIFF with file contents.
DIFF is the return value from `magit-gptcommit--staged-diff'."
  (let* ((files (magit-staged-files))
         (file-contents
          (mapconcat
           (lambda (file)
             (condition-case nil
                 (let ((content (magit-git-output "show" (concat ":0:" file))))
                   (if (< (length content) 50000)  ; limit to ~50KB per file
                       (format "=== File: %s ===\n%s\n" file content)
                     (format "=== File: %s ===\n[Too large]\n" file)))
               (error "")))
           files
           "\n")))
    (concat file-contents "\n\n=== DIFF ===\n" diff)))


;;; Project

;; Filter which projects get remembered
(defcustom aj8/project-remember-predicates nil
  "List of predicates that determine if a project should be remembered.
Each predicate is called with the project root and should return non-nil
if the project should be remembered."
  :type 'list
  :group 'aj8-lisp)

(defun aj8/project-remember-project-filter (orig-fn pr &optional no-write)
  "Advise `project-remember-project' to filter which projects get remembered.
Only adds project if all predicates in `aj8/project-remember-predicates'
return non-nil for the project root.  ORIG-FN is the original function
being advised.  PR and NO-WRITE are passed to ORIG-FN."
  (let ((root (project-root pr)))
    (when (seq-every-p (lambda (pred) (funcall pred root))
                       aj8/project-remember-predicates)
      (funcall orig-fn pr no-write))))

;;;; Windows

;;; Side windows

(defcustom aj8/side-window-height 0.16
  "Top side window height."
  :type 'number
  :group 'aj8-lisp)

(defcustom aj8/side-window-width 0.40
  "Right side window width."
  :type 'number
  :group 'aj8-lisp)

(defcustom aj8/side-window-width-dynamic
  (if (>= (frame-width) 160)
      (floor (/ (frame-width) 2))
    (- (frame-width) 81))
  "Right side window width.
If the width of the frame is greater than or equal to 160 characters,
split the frame in two, if less than 160 characters set width to 80
characters."
  :type 'integer
  :group 'aj8-lisp)

(defcustom aj8/side-window-slot-top
  (if (>= (frame-width) 160) -1 0)
  "Slot for top section of right side window.
On large screens, use slot -1 to create top section. On small screens,
use slot 0 to share undivided space."
  :type 'integer
  :group 'aj8-lisp)

(defcustom aj8/side-window-slot-bottom
  (if (>= (frame-width) 160) 1 0)
  "Slot for bottom section of right side window.
On large screens, use slot 1 to create bottom section. On small screens,
use slot 0 to share undivided space."
  :type 'integer
  :group 'aj8-lisp)

(defun aj8/make-not-side-window ()
  "Move the current buffer to a regular window if it's in a side window."
  (interactive)
  (let ((original-window (selected-window))
        (buffer (current-buffer)))
    (if (window-parameter original-window 'window-side)
        (let ((display-buffer-overriding-action
               '((display-buffer-use-some-window
                  display-buffer-pop-up-window)
                 (inhibit-same-window . t)
                 (inhibit-switch-frame . t))))
          (display-buffer buffer)
          (quit-window nil original-window))
      (message "The buffer is not in a side window."))))

(defun aj8/hide-side-windows ()
  "Hide side windows if they are visible; do nothing if they are not."
  ;; (interactive)
  (let ((found-side-window nil))
    (walk-windows (lambda (w)
                    (when (window-parameter w 'window-side)
                      (setq found-side-window t)))
                  'no-mini nil)
    (when found-side-window
      (window-toggle-side-windows))))

;;; Transient window restoration

(defvar aj8/transient--saved-side-window-height nil
  "Saved height of bottom side-window before transient popup.")

(defun aj8/transient--save-config (&rest _args)
  "Advise `transient--show' to save bottom side-window height.

Transient resizes the bottom side-window but doesn't restore it
afterward.  This function saves the height so it can be restored."
  (unless aj8/transient--saved-side-window-height
    (when-let ((side-window (window-with-parameter 'window-side 'bottom)))
      (setq aj8/transient--saved-side-window-height
            (window-total-height side-window)))))

(defun aj8/transient--restore-config (&rest _args)
  "Advise `transient--post-exit' to restore bottom side-window height.

Restores the height that was saved by `aj8/transient--save-config',
ensuring the bottom side-window returns to its original size after
transient is dismissed.

Restores immediately without using an idle timer."
  (when aj8/transient--saved-side-window-height
    (let ((saved-height aj8/transient--saved-side-window-height))
      (setq aj8/transient--saved-side-window-height nil)
      (when-let ((side-window (window-with-parameter 'window-side 'bottom)))
        (let ((delta (- saved-height (window-total-height side-window))))
          (when (and (not (zerop delta))
                     (window-resizable-p side-window delta))
            (window-resize side-window delta)))))))

;;; Kill windows

;; Kill buffers by defult with quit-window

(defcustom my/quit-window-exceptions-regex "^\\*\\(Messages\\)"
  "Regexp matching buffer names for which prefix argument should not be inverted."
  :type 'regexp
  :group 'aj8-lisp)

(defcustom my/quit-window-known-wrappers '(magit-mode-bury-buffer
                                           magit-log-bury-buffer
                                           Info-exit)
  "List of commands that call `quit-window'."
  :type 'sexp
  :group 'aj8-lisp)

(defun my/quit-window (args)
  "Advise `quit-window' to kill buffer by default.

With a prefix argument, the buffer is buried instead.  This is the
inverse of the default behavior `quit-window'.

This affects all calls to `quit-window' except in buffers matching
`my/quit-window-exceptions-regex'.  Calls to `quit-window' from wrapper
functions defined by `my/quit-window-known-wrappers' are also affected.

ARGS are the arguments to `quit-window', modified and returned."
  (when (and (or (eq this-command 'quit-window)
                 (member this-command my/quit-window-known-wrappers))
             (not (string-match-p my/quit-window-exceptions-regex (buffer-name))))
    (unless (consp args)
      (setq args (list nil)))
    (setf (car args) (not current-prefix-arg)))
  args)

;; Better focus handling with quit-windows
;;   Adds winner-mode style behavior to quit-window
;;   Reference: dotemacs-karthink/init.el

(defun my/better-quit-window-save (window)
  (push (window-parameter window 'quit-restore)
        (window-parameter window 'quit-restore-stack))
  window)

(defun my/better-quit-window-restore (origfn &optional window bury-or-kill)
  (let ((sw (or window (selected-window))))
    (funcall origfn window bury-or-kill)
    (when (eq sw (selected-window))
      (pop (window-parameter nil 'quit-restore-stack))
      (setf (window-parameter nil 'quit-restore)
            (car (window-parameter nil 'quit-restore-stack))))))

(define-minor-mode my/better-quit-window-mode
  "Toggle improved `quit-window' behavior similar to `winner-mode'."
  :global t
  :lighter " BetterQuit"
  (if my-better-quit-window-mode
      (progn
        (advice-add 'display-buffer :filter-return #'my/better-quit-window-save)
        (advice-add 'quit-restore-window :around #'my/better-quit-window-restore))
    (advice-remove 'display-buffer #'my/better-quit-window-save)
    (advice-remove 'quit-restore-window #'my/better-quit-window-restore)))

;;; Better shrink/enlarge window functions

;; Wrapper for shrink-window-horizontally
(defun my/move-splitter-left (arg)
  "Move window splitter left by ARG columns."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

;; Wrapper for enlarge-window-horizontally
(defun my/move-splitter-right (arg)
  "Move window splitter right by ARG columns."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

;; Wrapper for enlarge-window
(defun my/move-splitter-up (arg)
  "Move window splitter up by ARG lines."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

;; Wrapper for shrink-window
(defun my/move-splitter-down (arg)
  "Move window splitter down by ARG lines."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

;;; Window splitting

;; Toggle window split
(defun my/toggle-window-split ()
  "If the window is split vertically, split it horizontally, and vice versa."
  (interactive)
  (unless (= (count-windows) 2)
    (user-error "Can only toggle a window split in two!"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window)   ; close current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically))   ; makes a split with the other window twice
    (switch-to-buffer nil)))   ; restore the original window
                               ; in this part of the window

;; Respect split-height-threshold always
(defun aj8/split-window-sensibly-respect-threshold (orig-fun &optional window)
  "Advise `split-window-sensibly' to respect split-height-threshold.
Prevents the fallback behavior that splits vertically ignoring
split-height-threshold when window is the only usable window.
ORIG-FUN is the original function being advised."
  (let ((window (or window (selected-window))))
    ;; Only call original function if window meets threshold criteria
    (when (or (window-splittable-p window)
              (window-splittable-p window t))
      (funcall orig-fun window))))

;;; Misc

;; Restrict windmove swap states
(defun aj8/windmove-swap-states-inhibit-side-window (orig-fun direction)
  "Advise `windmove-swap-states-in-direction' to prevent regular/side swaps.
This prevents buffer swapping when one window is a side window and the
other is a regular window.  ORIG-FUN is the original function being
advised.  DIRECTION is passed to ORIG-FUN."
  (let* ((current-window (selected-window))
         (current-is-side (window-parameter current-window 'window-side))
         (target-window (windmove-find-other-window direction))
         (target-is-side (and target-window
                              (window-parameter target-window 'window-side))))
    (if (and target-window
             (not (eq (not current-is-side) (not target-is-side))))
        (message "Cannot swap between regular and side windows")
      (funcall orig-fun direction))))

;;;; Web

;;; eww

;; Open new eww buffers in a new window
;;   M-RET
(defun aj8/eww-open-in-new-buffer ()
  "Fetch link at point in a new EWW window."
  (interactive)
  (other-window-prefix)
  (eww-open-in-new-buffer))

;; Open new eww buffers in a new window
;;   C-u RET
(defun aj8/eww-follow-link ()
  "Browse the URL under point.
Swaps the functionality of single and double prefix arguments, see
`eww-follow-link' for details."
  (interactive)
  (cond
   ((equal current-prefix-arg nil) ; no C-u
    (eww-follow-link))
   ((equal current-prefix-arg '(4)) ; C-u
    (other-window-prefix)
    (eww-follow-link '(16)))
   ((equal current-prefix-arg '(16)) ; C-u C-u
    (eww-follow-link '(4)))
   (t (user-error "Unexpected input arguments"))))

;;;; Other

;;; Custom eval-sexp functions

;; Eval next sexp
(defun my/eval-next-sexp ()
  "Evaluate the next s-expression in the buffer."
  (interactive)
  (save-excursion
    (forward-sexp)
    (eval-last-sexp nil)))

;; Eval sexp at point
(defun my/eval-sexp-at-point ()
  "Evaluate the s-expression at point."
  (interactive)
  (save-excursion
    (up-list)
    (eval-last-sexp nil)))

;;; Custom repeat-maps

;; Add repeat-mode support for any keymap
(defun my/repeatize (keymap-or-symbol)
  "Add `repeat-mode' support to a KEYMAP.
KEYMAP-OR-SYMBOL may be either a symbol (whose value is a keymap)
or a keymap object itself."
  (let ((map (if (symbolp keymap-or-symbol)
                 (symbol-value keymap-or-symbol)
               keymap-or-symbol)))
    (map-keymap
     (lambda (_key cmd)
       (when (and cmd (symbolp cmd))
         (put cmd 'repeat-map map)))
     map)))

;; Add repeat-map for Smerge
(with-eval-after-load "smerge" (my/repeatize 'smerge-basic-map))

;; Add repeat-map for Smartparens
(with-eval-after-load "smartparens"
  (let ((map (keymap-lookup smartparens-mode-map "C-c s")))
    (my/repeatize map)))

;; Add repeat-map for Hideshow
(with-eval-after-load "hideshow"
  (let ((map (keymap-lookup hs-minor-mode-map "C-c @")))
    (my/repeatize map)))

;; Create repeat-map for window resizing commands
(defvar-keymap aj8/resize-window-repeat-map
  :repeat t
  :doc "Repeat map for window resizing commands."
  "{" #'my/move-splitter-up
  "}" #'my/move-splitter-down
  ">" #'my/move-splitter-right
  "<" #'my/move-splitter-left)

;; Create repeat-map for line duplication commands
(defvar-keymap aj8/move-dup-repeat-map
  :repeat t
  :doc "Repeat map for duplication commands."
  "<up>"   #'move-dup-move-lines-up
  "C-<up>" #'move-dup-duplicate-up
  "<down>" #'move-dup-move-lines-down
  "C-<down>" #'move-dup-duplicate-down)

;; Create repeat-map for indent-relative commands
(defvar-keymap aj8/indent-repeat-map
  :repeat t
  :doc "Repeat map for `indent-relative'."
  "TAB" #'indent-relative)

;; Create repeat-map for string-inflection commands
(defvar-keymap aj8/string-inflection-repeat-map
  :repeat t
  :doc "Repeat map for string-inflection commands."
  "x" #'string-inflection-all-cycle)

;; Create repeat-map for multi-line commands
(defvar-keymap aj8/multi-line-repeat-map
  :repeat t
  :doc "Repeat map for `multi-line'."
  "n" #'multi-line)

;; Create repeat-map for next/prev-buffer
;; (defvar-keymap aj8/switch-buffer-map
;;   :repeat t
;;   :doc "Repeat map for next/prev-buffer."
;;   "<left>"  #'aj8/previous-buffer
;;   "<right>" #'aj8/next-buffer)

;; Add "/" to undo-repeat-map
;; (keymap-set undo-repeat-map "/" #'undo)

;; Repeat state for arbitrary keymaps
;;   Reference: https://karthinks.com/software/persistent-prefix-keymaps-in-emacs/

(defvar repeated-prefix-help--keymap nil
  "Keymap for the current repeated prefix help session.")

(defun repeated-prefix-help--refresh ()
  "Refresh which-key display during transient session."
  (when repeated-prefix-help--keymap
    ;; (which-key--create-buffer-and-show nil repeated-prefix-help--keymap)
    ;; Fix for initial call (on C-h key press)
    (run-with-idle-timer 0 nil #'which-key--create-buffer-and-show nil repeated-prefix-help--keymap)))

(defun repeated-prefix-help--quit ()
  "Clean up and quit the repeated prefix help session."
  (when repeated-prefix-help--keymap
    (setq repeated-prefix-help--keymap nil)
    (remove-hook 'post-command-hook #'repeated-prefix-help--refresh)))

(defun repeated-prefix-help-command ()
  "Enable repeatable prefix commands with persistent help display.

When invoked (typically as `prefix-help-command'), this captures the
current key prefix, creates a transient keymap that stays active until
you quit with \\[keyboard-quit] or execute a command outside the prefix.
A help popup shows available bindings and refreshes after each command,
allowing you to execute prefix commands repeatedly without retyping the
prefix sequence.

For example, after pressing `C-x' and then the help key, you can
repeatedly press `b', `s', `u', etc. to execute `switch-to-buffer',
`save-som-buffers', `undo', etc. without pressing `C-x' again."
  (interactive)
  (when-let* ((keys (this-command-keys-vector))
              (prefix (seq-take keys (1- (length keys))))
              (orig-keymap (key-binding prefix 'accept-default))
              (keymap (copy-keymap orig-keymap)))
    ;; Store keymap
    (setq repeated-prefix-help--keymap keymap)
    ;; Set up refresh mechanism
    (add-hook 'post-command-hook #'repeated-prefix-help--refresh)
    ;; Activate transient map with cleanup on exit
    (set-transient-map keymap t #'repeated-prefix-help--quit)))

(setq prefix-help-command #'repeated-prefix-help-command)

;;; Suppress messages with advice

(defun aj8/add-suppress-messages (functions)
  "Add advice to FUNCTIONS to suppress messages.
FUNCTIONS can be a symbol or a list of symbols representing function names."
  (let ((func-list (if (listp functions) functions (list functions))))
    (dolist (fn func-list)
      (advice-add fn :around #'aj8/suppress-messages))))

(defun aj8/remove-suppress-messages (functions)
  "Remove advice from FUNCTIONS that suppresses messages.
FUNCTIONS can be a symbol or a list of symbols representing function names."
  (let ((func-list (if (listp functions) functions (list functions))))
    (dolist (fn func-list)
      (advice-remove fn #'aj8/suppress-messages))))

(defun aj8/suppress-messages (orig-fun &rest args)
  "Advise ORIG-FUN to suppress messages during execution.
ORIG-FUN is the original function being advised.
ARGS are passed to ORIG-FUN."
  (let ((inhibit-message t)   ; disable messages in Echo area
        (message-log-max nil))   ; disable messages in Messages buffer
    (apply orig-fun args)))

;;; xterm key sequence mappings for rxvt

(defun rxvt--add-escape-key-mapping-alist (escape-prefix key-prefix suffix-alist)
  "Add mappings for a given list of escape sequences and list of keys.
ESCAPE-PREFIX is the common escape sequence prefix.  KEY-PREFIX is the
key prefix to map.  SUFFIX-ALIST is an alist of (escape-suffix
. key-suffix) pairs."
  (while suffix-alist
    (let ((escape-suffix (car (car suffix-alist)))
          (key-suffix (cdr (car suffix-alist))))
      (define-key input-decode-map (concat escape-prefix escape-suffix)
        (read-kbd-macro (concat key-prefix key-suffix))))
    (setq suffix-alist (cdr suffix-alist))))

;; Add xterm key sequence mappings
;;   See also https://github.com/CyberShadow/term-keys for a more
;;   complete solution.
(defun rxvt-input-decode-map ()
  "Map modifier and arrow keys to xterm sequences.
Each combination of modifier and up, down, left, and right keys
is mapped to the respective xterm key sequence."
  (let ((nav-key-pair-alist
         '(("A" . "<up>") ("B" . "<down>") ("C" . "<right>") ("D" . "<left>")
           ("H" . "<home>") ("F" . "<end>"))))
    (rxvt--add-escape-key-mapping-alist "\e[1;2" "S-" nav-key-pair-alist)
    (rxvt--add-escape-key-mapping-alist "\e[1;3" "M-" nav-key-pair-alist)
    (rxvt--add-escape-key-mapping-alist "\e[1;4" "M-S-" nav-key-pair-alist)
    (rxvt--add-escape-key-mapping-alist "\e[1;5" "C-" nav-key-pair-alist)
    (rxvt--add-escape-key-mapping-alist "\e[1;6" "C-S-" nav-key-pair-alist)
    (rxvt--add-escape-key-mapping-alist "\e[1;7" "M-C-" nav-key-pair-alist)
    (rxvt--add-escape-key-mapping-alist "\e[1;8" "M-C-S-" nav-key-pair-alist)))

;;; Misc

;; Swap universal prefix argument for functions
;;   (using advice)
(defun my/toggle-prefix-arg (fun)
  "Toggle universal prefix argument for the function FUN.
If called with a prefix argument, the prefix argument will be removed.
If called without a prefix argument, a prefix argument will be applied.
This only works for interactive \"P\" functions."
  (if (not (equal (interactive-form fun) '(interactive "P")))
      (error "Unexpected: must be interactive \"P\" function")
    (advice-add fun :around (lambda (x &rest args)
                              "Swap universal prefix argument for FUNCTION fun."
                              (if (called-interactively-p 'any)
                                  (apply x (cons (not (car args)) (cdr args)))
                                (apply x args))))))

;; Swap universal prefix argument for functions
;;   (during function call)
(defun aj8/call-interactively-wih-prefix-toggle (fun)
  "Call the function FUN and toggle its universal prefix argument.
If called with a prefix argument, the prefix argument will be
removed.  If called without a prefix argument, a prefix argument
will be applied."
  (if (equal current-prefix-arg nil) ; no C-u
       ;; then
       (let ((current-prefix-arg '(4)))
         (call-interactively fun))
     ;; else
     (let ((current-prefix-arg nil))
       (call-interactively fun))))

;; Reload init-file
(defun aj8/reload-init-file ()
  "Reload the Emacs configuration from `user-init-file'."
  (interactive)
  (load-file user-init-file)
  (message "Emacs configuration reloaded successfully."))

;; List parent modes
(defun my/derived-modes (mode)
  "Display a list of the ancestor modes that MODE is derived from."
  (interactive (list major-mode))
  (defun iter (mode)
    (and mode
         (cons mode (iter (get mode 'derived-mode-parent)))))
  (message "%s" (iter mode)))

;; Set up description widths for which-key
(defcustom aj8/which-key-columns 4
  "Number of columns to display in which-key popup."
  :type 'integer
  :group 'aj8-lisp)

(defun aj8/which-key-description-length ()
  "Calculate `which-key' description width based on available space and columns.
Uses `which-key--side-window-max-dimensions' and `aj8/which-key-columns'
to calculate the appropriate description width, and returns it as a
whole number.  Note that the available width is slightly less than
reported by `frame-width'.  See `which-key--side-window-max-dimensions'."
  (let* ((max-dims (which-key--side-window-max-dimensions))
         (available-width (cdr max-dims))
         ;; Account for key column and column padding
         (key-column-width 8)   ; generally >= 3
                                ; values that work well:
                                ; 4 cols : 8
                                ; 5 cols : 7
                                ; 6 cols : 7
                                ; 7 cols : 7
                                ; 8 cols : 7
         (usable-width (- available-width
                         (* aj8/which-key-columns key-column-width)
                         (* (1+ aj8/which-key-columns) which-key-add-column-padding)))
         (desc-width-per-column (/ usable-width aj8/which-key-columns)))
    ;; Ensure we return a positive whole number
    (max 10 (floor desc-width-per-column))))

;; Add indicator for Treesitter modes in the modeline
(defun aj8/treesit-mode-name ()
  "Set `mode-name' to 'Mode[TS]' if the current major mode has 'ts' in its name."
  (when (string-match-p "ts" (symbol-name major-mode))
    (setq mode-name (concat mode-name "[TS]"))))

(defun aj8/tramp-indicator ()
  "Return a remote host name if the current buffer is using TRAMP."
  (when (file-remote-p default-directory)
    (let* ((dissected (tramp-dissect-file-name default-directory))
           (host (tramp-file-name-host dissected)))
      (concat " TRAMP:" host))))


(provide 'aj8-lisp)

;;; aj8-lisp.el ends here
