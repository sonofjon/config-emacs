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
                       (gnu/linux . "/usr/lib/x86_64-linux-gnu/libenchant-2.so")
                       (wsl . "/usr/lib/x86_64-linux-gnu/libenchant-2.so")))
           (libvterm . ((darwin . "/opt/homebrew/lib/libvterm.dylib")
                        (gnu/linux . "/usr/lib64/libvterm.so")
                        (wsl . "/usr/lib/x86_64-linux-gnu/libvterm.so")))
           (xclip . ((darwin . pbcopy)
                     (gnu/linux . wl-copy)
                     (wsl . xclip))))))
    (let ((system-type (if (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
                           'wsl
                         system-type))
          (package-alist (cdr (assoc package system-package-alist))))
      (or (cdr (assoc system-type package-alist))
          (error "Package '%s' not found for system type '%s'" package system-type)))))

;;;; Buffers

;;; Undo for killed file buffers

(defvar my/reopen-killed-file-list nil
  "List of recently killed files.")

(defconst my/reopen-killed-file-max 20
  "Maximum number of killed files to store.")

(defun my/reopen-killed-file-save ()
  "Save the content of the current buffer to `my/reopen-killed-bufer-content'.
+Only save content if the buffer is not associated with a filename."
  (when buffer-file-name
    (push buffer-file-name my/reopen-killed-file-list)
    (when (> (length my/reopen-killed-file-list) my/reopen-killed-file-max)
      (setq my/reopen-killed-file-list (cl-subseq my/reopen-killed-file-list 0 my/reopen-killed-file-max)))))

;; Undo for killed buffers
(defun my/reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when my/reopen-killed-file-list
    (find-file (pop my/reopen-killed-file-list))))

;; Fancy undo for killed buffers
(defun my/reopen-killed-file-fancy ()
  "Pick a file to revisit from files killed during this session."
  (interactive)
  (if my/reopen-killed-file-list
      (let ((file (completing-read "Reopen killed file: "
                                   my/reopen-killed-file-list
                                   nil nil nil nil
                                   (car my/reopen-killed-file-list))))
        (when file
          (setq my/reopen-killed-file-list
                (cl-delete file my/reopen-killed-file-list :test #'equal))
          (find-file file)))
    (user-error "No recently-killed files to reopen")))

;;; Undo for killed non-file buffers

(defvar aj8/reopen-killed-buffer-content nil
  "Name and contents of the last killed non-file buffer.")

(defvar aj8/reopen-killed-buffer-max-size 50000
  "Maximum size of non-file buffer (in characters) to store.")

(defun aj8/reopen-killed-buffer-save ()
  "Save the content of the current buffer to `my/reopen-killed-bufer-content'.
Only save content if the buffer is not associated with a filename."
  (unless buffer-file-name
    (when (<= (buffer-size) aj8/reopen-killed-buffer-max-size)
      (setq aj8/reopen-killed-buffer-content
            (cons (buffer-name) (buffer-string))))))

(defun aj8/reopen-killed-buffer ()
  "Reopen the last killed non-file buffer, restoring its contents.
Note, this does not include window properties etc."
  (interactive)
  (if (null aj8/reopen-killed-buffer-content)
      (user-error "No recently killed non-file buffer to reopen")
    (let ((buffername (car aj8/reopen-killed-buffer-content))
          (contents (cdr aj8/reopen-killed-buffer-content)))
      (switch-to-buffer (get-buffer-create buffername))
      (insert contents)
      (setq aj8/reopen-killed-buffer-content nil))))

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
              (seq "magit-process" (zero-or-more anything)))
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
WINDOW is the window displaying the buffer.
BURY-OR-KILL indicates whether to bury or kill the buffer."
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
;;   TODO: switches to other project buffers if there is only one project buffer
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
        (when (cdr-safe project-buffers)
          (while (and (< counter max-iterations)
                      (not (gethash (current-buffer) other-project-buffers)))
            (apply orig-fun args)
            (cl-incf counter))))
    (apply orig-fun args)))

;; Switch to next project buffer
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
  "Add TAG to buffer name."
  (rename-buffer (format "%s # %s" (buffer-name) tag) t))

;; Prefix buffer name
(defun aj8/prefix-buffer-name (prefix)
  "Add PREFIX to buffer name."
  (rename-buffer (format "%s: %s" prefix (buffer-name)) t))

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

;; Keep focus in side window when killing buffer
(defun aj8/retain-side-window-focus ()
  "Retain focus in the side window after killing its buffer."
  (when-let* ((win (selected-window))
              (side-window-p (window-parameter win 'window-side)))
    (select-window win)))

(advice-add 'kill-current-buffer :after #'aj8/retain-side-window-focus)

;; Save gptel buffers
(defun aj8/gptel-write-buffer (orig-fun &rest args)
  "Advice function to save the chat buffer when starting gptel.

This function saves the chat to the current project's root directory.
If no project is detected, it prompts the user to choose a directory for
saving.  It constructs a filename based on the current timestamp and the
major mode of the buffer (with support for `org-mode' and
`markdown-mode').  This function is intended to be used as advice for
the gptel function."
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

(advice-add 'gptel :around #'aj8/gptel-write-buffer)


;;;; Coding

(defun aj8/python-mode-hook ()
  "Custom Python mode hook."
  ;; Exclude virtual environment directories from project
  ;;   Not needed: use M-s G
  ;; (setq-local project-ignored-files '(".venv/*"))
  (keymap-set python-mode-map "C-c <" nil)
  (keymap-set python-ts-mode-map "C-c <" nil))   ; unbind python-indent-shift-left

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

Scans the Flymake diagnostic at point for a “[RULE123]”-style code and
browses to its documentation at https://docs.astral.sh/ruff/rules."
(interactive)
  (unless (derived-mode-p 'flymake-diagnostics-buffer-mode)
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

(with-eval-after-load 'flymake
  (define-key flymake-diagnostics-buffer-mode-map (kbd "M-RET")
    #'aj8/flymake-ruff-goto-doc))

;;;; Completion

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

;;; Orderless matching styles

;; Cycle through orderless matching styles
;;   TODO: does not work
(defun aj8/orderless-matching-style-cycle ()
  "Cycle through orderless matching styles."
  (interactive)
  (cond
   ((eq (car orderless-matching-styles) 'orderless-literal)
    (aj8/orderless-matching-style--prefixes))
   ((eq (car orderless-matching-styles) 'orderless-prefixes)
    (aj8/orderless-matching-style--regexp))
   ((eq (car orderless-matching-styles) 'orderless-regexp)
    (aj8/orderless-matching-style--flex))
   ((eq (car orderless-matching-styles) 'orderless-flex)
    (aj8/orderless-matching-style--literal))
   (t
    (error "Unknown matching style"))))

;; Flex
(defun aj8/orderless-matching-style--flex ()
  "Components match flexy for the rest of the session."
  (setq-local orderless-matching-styles '(orderless-flex))
  (minibuffer-message "[flex]"))
  ;; (vertico--update 'interruptible))   ; TODO: What is this?

;; Literal
(defun aj8/orderless-matching-style--literal ()
  "Components match literally for the rest of the session."
  (setq-local orderless-matching-styles '(orderless-literal))
  (minibuffer-message "[literal]"))
  ;; (vertico--update 'interruptible))

;; Prefixes
(defun aj8/orderless-matching-style--prefixes ()
  "Components match prefixes for the rest of the session."
  (setq-local orderless-matching-styles '(orderless-prefixes))
  (minibuffer-message "[prefixes]"))
  ;; (vertico--update 'interruptible))

;; Regexp
(defun aj8/orderless-matching-style--regexp ()
  "Components match regexp for the rest of the session."
  (setq-local orderless-matching-styles '(orderless-regexp))
  (minibuffer-message "[regexp]"))
  ;; (vertico--update 'interruptible))

;;; Orderless style dispatchers

;; Flex
(defun aj8/orderless-dispatch-flex-if-twiddle (pattern _index _total)
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

;; Literal
(defun aj8/orderless-dispatch-literal-if-equal (pattern _index _total)
   (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

;; Prefixes
(defun aj8/orderless-dispatch-prefixes-if-less (pattern _index _total)
   (when (string-suffix-p "<" pattern)
    `(orderless-prefixes . ,(substring pattern 0 -1))))

;; Regexp
(defun aj8/orderless-dispatch-regexp-if-star (pattern _index _total)
   (when (string-suffix-p "*" pattern)
    `(orderless-regexp . ,(substring pattern 0 -1))))

;; Exclude
(defun aj8/orderless-dispatch-without-if-bang (pattern _index _total)
  (when (string-suffix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 0 -1))))

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

;; TODO: Combine both functions to reuse common code
(defun aj8/sp-down-sexp-dwim ()
  "Move point down one level of s-expression (sexp).

The function moves point in the direction that makes sense, i.e.
in the forward direction if point is surrounded by left
parentheses, and in the backward direction if surrounded by right
parentheses.  If point is between a right and a left parenthesis
it chooses the closest direction to move down.

Note that the logic in this function only considers parenthesis-
delimited s-expressions."
  (interactive)
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
           (point-bol-p (looking-back "^[[:space:]]*"))
           (direction (cond ((and prev-paren-left-p next-paren-left-p) 1)
                            ((and prev-paren-right-p next-paren-right-p) -1)
                            (point-bol-p 1)
                            (point-eol-p -1)
                            (t (if (> forward-dist backward-dist) -1 1)))))
      ;; (message "Forward distance: '%s', backward distance: '%s'"
      ;;          forward-dist backward-dist)
      ;; Move point in the appropriate direction
      (sp-down-sexp direction))))

(defun aj8/sp-up-sexp-dwim ()
  "Move point up one level of s-expression (sexp).

The function moves point in the direction that makes sense, i.e.
in the forward direction if point is surrounded by right
parentheses, and in the backward direction if surrounded by left
parentheses.  If point is between a left and a right parenthesis
it chooses the closest direction to move up.

Note that the logic in this function only considers parenthesis-
delimited s-expressions."
  (interactive)
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
           (point-bol-p (looking-back "^[[:space:]]*"))
           (direction (cond ((and prev-paren-left-p next-paren-left-p) -1)
                            ((and prev-paren-right-p next-paren-right-p) 1)
                            (point-bol-p -1)
                            (point-eol-p 1)
                            (t (if (> forward-dist backward-dist) -1 1)))))
      ;; (message "Forward distance: '%s', backward distance: '%s'"
      ;;          forward-dist backward-dist)
      ;; Move point in the appropriate direction
      (sp-up-sexp direction))))

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
ARG is the prefix argument. If called with C-u, open a line above.
Otherwise, open a line below."
  (interactive "p")
  (if (equal arg 4)  ;; C-u gives a prefix argument of 4
      (my/open-line-above)
    (my/open-line-below)))

;; Sort lines with files and directories
(defun aj8/sort-lines-custom-group (beg end)
  "Sort lines in region by custom groups.
Groups include:
1. Directories (ends with \"/\")
2. Regular files (no \"/\" or \"*\" anywhere)
3. Regular files in directories (contains \"/\" but doesn’t end with \"/\" and no \"*\")
4. Wildcards (contains \"*\")"
  (interactive "r")
  (let* ((lines (split-string (buffer-substring-no-properties beg end) "\n" t))
         (group-of
          (lambda (line)
            (cond
             ((string-match-p "\\*" line) 4)
             ((string-suffix-p "/" line) 1)
             ((string-match-p "/" line) 3)
             (t 2))))
         (sorted (sort lines
                       (lambda (a b)
                         (let ((ga (funcall group-of a))
                               (gb (funcall group-of b)))
                           (if (= ga gb)
                               (string< a b)
                             (< ga gb)))))))
    (delete-region beg end)
    (goto-char beg)
    (insert (mapconcat 'identity sorted "\n"))))

;;;; Files

;;;; Help

;;; Misc

;; Toggle Eldoc buffer
;;   TODO: does not work
(defun aj8/toggle-eldoc-buffer ()
  "Toggle the Eldoc buffer on and off."
  (interactive)
  (let ((eldoc-buffer nil)
        (found nil))
    (dolist (buf (buffer-list))
      (when (and (not found) (string-prefix-p "eldoc" (buffer-name buf)))
        (setq eldoc-buffer buf)
        (setq found t)))
    (if (and eldoc-buffer (get-buffer-window eldoc-buffer))
        ;; If the Eldoc buffer is visible, delete its window.
        (delete-window (get-buffer-window eldoc-buffer))
      ;; Otherwise, call `eldoc-doc-buffer' to show the Eldoc buffer.
      (eldoc))))

;;;; Navigation

;;; Scrolling

(defun aj8/scroll-up-paragraph (&optional move-point)
  "Scroll the selected window up one paragraph.

MOVE-POINT determines whether the point should adjust position.  With a
numeric MOVE-POINT > 0, move point to the start of the next paragraph.
With a negative MOVE-POINT, keep point centered on the window."
(interactive "P")
  (let ((move-point-value (prefix-numeric-value move-point)))
    (save-excursion
      (goto-char (window-end))
      (while (looking-at-p "^[ \t]*$")   ; scroll past empty lines
        (scroll-up-line)
        (forward-line))
      (while (not (looking-at-p "^[ \t]*$"))   ; scroll past paragraph
        (scroll-up-line)
        (forward-line))
      (scroll-up-line)   ; needed to show one empty line at the end of the window
      (forward-line -1))
    ;; Optionally, move point
    (when move-point
      (if (< move-point-value 0)
          (goto-char (/ (+ (point-min) (point-max)) 2))   ; center
        (progn   ; next paragraph
          ;;  TODO: Doesn't work when paragraphs are longer than one window height
          ;; (redisplay)   ; update buffer content in window TODO: ugly!
          ;; (force-window-update (selected-window))
          (goto-char (window-end))
          (re-search-backward "^[ \t]*$" nil t)
          (forward-line))))))

;; Scroll down one paragraph
(defun aj8/scroll-down-paragraph (&optional move-point)
  "Scroll the selected window down one paragraph.

MOVE-POINT determines whether the point should adjust position.  With a
numeric MOVE-POINT > 0, move point to the start of the next paragraph.
With a negative MOVE-POINT, keep point centered on the window."
(interactive "P")
  (let ((move-point-value (prefix-numeric-value move-point)))
    (save-excursion
      (goto-char (window-start))
      (while (looking-at-p "^[ \t]*$")   ; scroll past empty lines
        (scroll-down-line)
        (forward-line -1))
      (while (not (looking-at-p "^[ \t]*$"))   ; scroll past paragraph
        (scroll-down-line)
        (forward-line -1)))
    ;; Optionally, move point
    (when move-point
      (if (< move-point-value 0)
          (goto-char (/ (+ (point-min) (point-max)) 2))   ; center
        (progn   ; next paragraph
          (goto-char (window-start))
          (re-search-forward "^[ \t]*$" nil t)
          (forward-line))))))

;;; Movement by lines or comments

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

;;; Hideshow cycle

(defcustom aj8/hs-cycle-max-depth 3
  "The maximum depth level to reveal with `aj8/hs-cycle'.
If nil, cycle through all levels."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'hideshow)

(defvar aj8/hs-cycle--depth :if-nil
  "Current depth level for `aj8/hs-cycle'.")

(defun aj8/hs-count-levels ()
  "Return the number of nested levels within the current block."
  (save-excursion
    (let ((level 0)
          (current-pos (point))
          (end-pos (progn
                     ;; Find the end position of the current block
                     (when (hs-find-block-beginning)
                       (hs-forward-sexp (match-data t) 1))
                     (point))))
      (goto-char current-pos)
      ;; Iterate through all sub-blocks within the current block
      (while (hs-find-next-block hs-block-start-regexp end-pos nil)
        (let ((start (point)))
          ;; Move to the end of the sub-block
          (hs-forward-sexp (match-data t) 1)
          (let ((block-end (point)))
            (when (> block-end start)
              (goto-char start)
              (setq level (max level (1+ (aj8/hs-count-levels))))
              ;; Recursively count nested levels and update the maximum level
              (goto-char block-end)))))
      level)))

(defun aj8/hs-already-hidden-any-p ()
  "Return non-nil if any sub-level within the current block is hidden."
  (save-excursion
    (let ((current-pos (point))
          (end-pos (progn
                     ;; Find the end of the current code block
                     (when (hs-find-block-beginning)
                       (hs-forward-sexp (match-data t) 1))
                     (point)))
          hidden-found)
      ;; Move to the start of the current block
      (goto-char current-pos)
      ;; Iterate over all sub-blocks in the current block
      (while (and (not hidden-found)
                  (hs-find-next-block hs-block-start-regexp end-pos nil))
        ;; Check if there's a hidden sub-block at the current position
        (when (hs-already-hidden-p)
          (setq hidden-found t)))
      hidden-found)))

;; TODO:  Review that comments are accurate
;; MAYBE: Reverse direction with Shift-Tab
;;        Toggle `hide/show' completely with C-u
(defun aj8/hs-cycle ()
  "Cycle code folding, progressively revealing deeper levels.

Each invocation reveals one more nested level up to `aj8/hs-cycle-max-depth'.
Once the maximum depth is reached, fully expand the block on the next call.
If the block is fully visible, hide it entirely."
  (interactive)
  (let ((hs-functions '(hs-hide-block hs-show-block hs-hide-level))
        (max-depth (or aj8/hs-cycle-max-depth (aj8/hs-count-levels))))
    (aj8/add-suppress-messages-advice hs-functions)
    (unwind-protect
        (save-excursion
          (if (aj8/hs-already-hidden-any-p)
              ;; A block is already hidden; show one more level
              (if (or (not aj8/hs-cycle--depth)
                      (< aj8/hs-cycle--depth max-depth))
                  (progn
                    (setq aj8/hs-cycle--depth (if aj8/hs-cycle--depth
                                                 (1+ aj8/hs-cycle--depth)
                                               1))
                    (hs-hide-level aj8/hs-cycle--depth)
                    (message "hs-cycle depth: %s" aj8/hs-cycle--depth))
                ;; Max depth reached; show entire block
                (hs-show-block)
                (setq this-command nil)
                (setq aj8/hs-cycle--depth nil)
                (message "hs-cycle depth: all"))
            ;; No block is not hidden; hide entire block
            (hs-hide-block)
            (setq aj8/hs-cycle--depth 0)
            (message "hs-cycle depth: 0")))
      (aj8/remove-suppress-messages-advice hs-functions))))

(defvar aj8/hs-global-cycle--depth nil
  "Current depth level for `aj8/hs-global-cycle'.
Tracks the current level of code folding globally.")

;; TODO: Adopt recent changes to non-global version
(defun aj8/hs-global-cycle ()
  "Cycle code folding globally, progressively revealing deeper levels.

On the first call, hide all blocks.  On each subsequent call, show the
next level across all blocks, up to `aj8/hs-cycle-max-depth'.  After
reaching `aj8/hs-cycle-max-depth', fully expand all blocks on the next
call."
  (interactive)
  (let ((hs-functions '(hs-hide-all hs-show-all hs-hide-level)))
    (aj8/add-suppress-messages-advice hs-functions)
    (unwind-protect
        (save-excursion
          (cond
           ;; Initial call: hide all blocks
           ((not (eq last-command 'aj8/hs-global-cycle))
            (hs-hide-all)
            (setq aj8/hs-global-cycle--depth 0)
            (message "Global Depth: 0"))
           ;; Subsequent calls: show next level globally
           ((< aj8/hs-global-cycle--depth aj8/hs-cycle-max-depth)
            (setq aj8/hs-global-cycle--depth (1+ aj8/hs-global-cycle--depth))
            (save-excursion
              ;; MAYBE: hs-hide-level might not operate globally if
              ;; point-min is at the beginning of a block?
              (goto-char (point-min))
              (hs-hide-level aj8/hs-global-cycle--depth))
            (message "Global Depth: %s" aj8/hs-global-cycle--depth))
           ;; Last call: show all blocks
           (t
            (hs-show-all)
            (setq this-command nil)
            (setq aj8/hs-global-cycle--depth nil)
            (message "Global Depth: all"))))
      (aj8/remove-suppress-messages-advice hs-functions))))

;; Reference: dotemacs-karthink
(defun my/hs-global-cycle ()
  "Toggle between hiding and showing all blocks globally."
  (interactive)
  (pcase last-command
    ('my/hs-global-cycle
     (save-excursion (hs-show-all))
     (setq this-command 'hs-global-show))
    (_ (hs-hide-all))))

;;; Markdown

;; Move to previous row
;;   Complements the built-in markdown-table-next-row
;;   TODO: implement
(defun aj8/markdown-table-prev-row ()
  "Go to the previous row (same column) in the table.
Create new table lines if required.")

(defun aj8/markdown-table-enter ()
  "Split the current table cell at point.
The remainder of the cell is placed in a new cell below the current row."
  (interactive)
  (unless (markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let ((col (markdown-table-get-column)))
    (when (looking-at "[^|\r\n]*")
      (let* ((pos (point))
             (end (match-end 0))
             (val (buffer-substring pos end))   ;  remainder of cell
             (val (string-trim val))   ; trim whitespace
             (space-fill (make-string (- end pos) ?\ )))

        (delete-region pos end)   ; delete remainder
        (insert space-fill)   ; maintain cell width
        (markdown-table-insert-row 1)
        (markdown-table-goto-column col)
        (insert val)   ; insert remainder
        (delete-char (length val))   ; maintain cell width
        (markdown-table-goto-column col)))))  ; return to beginning of cell

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
(defun outline-headers-for-semicolon-buffers ()
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
(defun outline-headers-for-hash-mark-buffers ()
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
(defun outline-headers-for-percentage-buffers ()
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
(defun outline-headers-for-exclamation-mark-buffers ()
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
Point must be at the beginning of a header line.  This is the
level specified in `outline-heading-alist' and not based on the
number of characters matched by `outline-regexp'."
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

(defun outline--body-p ()
  "Check if there is a body."
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (and (not (eobp))
         (progn (forward-char 1)
                (not (outline-on-heading-p))))))

(defun outline--body-visible-p ()
  "Check if there is a visible body."
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (not (outline-invisible-p))))

(defun outline--subheadings-p ()
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
    (cond ((and (outline--body-p)
                (outline--body-visible-p))
           (outline-hide-entry)
           (outline-hide-leaves))
          (t
           (outline-hide-subtree)))))

;; Show more heading levels
(defun my/outline-show-more ()
  (interactive)
  (when (outline-on-heading-p)
    (cond ((and (outline--subheadings-p)
                (not (outline--subheadings-visible-p)))
           (outline-show-children))
          ((and (not (outline--subheadings-p))
                (not (outline--body-visible-p)))
           (outline-show-subtree))
          ((and (outline--body-p)
                (not (outline--body-visible-p)))
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
  "Selects the current code block delimited by triple backticks."
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
If current program is `aspell', switch to `hunspell', and vice
versa."
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

(defun aj8/jinx-correct-previous ()
  "Correct the previous visible spelling error.
The function repeatedly jumps to each previous error, corrects it,
and continues checking. When done, the point is restored to its
original position."
  (interactive)
  (save-excursion
    (while (ignore-errors (jinx-previous 1) t)
      (jinx-correct))))

(defun aj8/jinx-correct-next ()
  "Correct next visible spelling error.
The function repeatedly jumps to each next error, corrects it,
and continues checking. When done, the point is restored to its
original position."
  (interactive)
  (save-excursion
    (while (ignore-errors (jinx-next 1) t)
      (jinx-correct))))

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
(defun my/ediff-copy-both-to-C ()
  "Add both variants to merge file."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference
                                               'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference
                                               'B ediff-control-buffer))))

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

(defun my/advice--quit-window (args)
  "Advice function that makes `quit-window' quit window and kill its buffer.

With a prefix argument, the buffer is buried instead. This is the
inverse of the default behavior `quit-window'.

This affects all calls to `quit-window' except in buffers
matching `my/quit-window-exceptions-regex'. Calls to
`quit-window' from wrapper functions defined by
`my/quit-window-known-wrappers' are also affected."
  (when (and (or (eq this-command 'quit-window)
                 (member this-command my/quit-window-known-wrappers))
             (not (string-match-p my/quit-window-exceptions-regex (buffer-name))))
    (unless (consp args)
      (setq args (list nil)))
    (setf (car args) (not current-prefix-arg)))
  args)

(advice-add 'quit-window :filter-args #'my/advice--quit-window)

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

;; Uncomment to enable
;; (advice-add 'display-buffer :filter-return #'my/better-quit-window-save)
;; (advice-add 'quit-restore-window :around #'my/better-quit-window-restore)

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

;;; Misc

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
Swaps the functionality of single and double prefix arguments,
see `eww-follow-link' for details."
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

(defun prot-eww--get-current-url ()
  "Return the current-page's URL."
  (cond ((eq major-mode 'eww-mode)
         (plist-get eww-data :url))
        (t (user-error "Not a eww or elpher buffer"))))

;; Add completion to eww
;;   TODO: Fix
(defun prot-eww-browse-dwim (url &optional arg)
  "Visit a URL, maybe from `eww-prompt-history', with completion.

With optional prefix ARG (\\[universal-argument]) open URL in a
new eww buffer.  If URL does not look like a valid link, run a
web query using `eww-search-prefix'.

When called from an eww buffer, provide the current link as
\\<minibuffer-local-map>\\[next-history-element]."
  (interactive
   (let ((all-history (delete-dups
                       (append prot-eww-visited-history
                               eww-prompt-history)))
         (current-url (prot-eww--get-current-url)))
     (list
      (completing-read "Run EWW on: " all-history
                       nil nil current-url 'eww-prompt-history current-url)
      (prefix-numeric-value current-prefix-arg))))
  (prot-eww url arg))

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
(defun my/repeatize (keymap)
  "Add `repeat-mode' support to a KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap)))

;; Add repeat-map for Smerge
(with-eval-after-load "smerge" (my/repeatize 'smerge-basic-map))

;; Add repeat-map for Smartparens
;; (with-eval-after-load "smartparens" (my/repeatize 'smartparens-mode-map))

;; Create repeat-map for move-splitter commands (window resizing)
(defvar aj8/resize-window-repeat-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "{" #'my/move-splitter-up)
    (keymap-set map "}" #'my/move-splitter-down)
    (keymap-set map ">" #'my/move-splitter-right)
    (keymap-set map "<" #'my/move-splitter-left)
    map)
  "Repeat map for window resizing commands.")

;; Add repeat-map property to move-splitter map (window resizing)
(dolist (cmd '(my/move-splitter-up
               my/move-splitter-down
               my/move-splitter-right
               my/move-splitter-left))
  (put cmd 'repeat-map 'aj8/resize-window-repeat-map))

;; Create repeat-map for duplication commands
(defvar aj8/move-dup-repeat-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<up>" #'move-dup-move-lines-up)
    (keymap-set map "C-<up>" #'move-dup-duplicate-up)
    (keymap-set map "<down>" #'move-dup-move-lines-down)
    (keymap-set map "C-<down>" #'move-dup-duplicate-down)
    map)
  "Repeat map for duplication commands.")

;; Add repeat-map property to move-dup commands
(dolist (cmd '(move-dup-move-lines-up
               move-dup-duplicate-up
               move-dup-move-lines-down
               move-dup-duplicate-down))
  (put cmd 'repeat-map 'aj8/move-dup-repeat-map))

;; Create repeat-map for indent-relative commands
(defvar aj8/indent-repeat-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "TAB" #'indent-relative)
    map)
  "Repeat map for `indent-relative' commands.")

;; Add repeat-map property to indent-relative map
(dolist (cmd '(indent-relative))
  (put cmd 'repeat-map 'aj8/indent-repeat-map))

;; Create repeat-map for string-inflection commands
(defvar aj8/string-inflection-repeat-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "x" #'string-inflection-all-cycle)
    map)
  "Repeat map for string-inflection commands.")

;; Add repeat-map property to string-inflection map
(dolist (cmd '(string-inflection-all-cycle))
  (put cmd 'repeat-map 'aj8/string-inflection-repeat-map))

;; Create repeat-map for multi-line commands
(defvar aj8/multi-line-repeat-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "n" #'multi-line)
    map)
  "Repeat map for `multi-line' commands.")

;; Add repeat-map property to multi-line map
(dolist (cmd '(multi-line))
  (put cmd 'repeat-map 'aj8/multi-line-repeat-map))

;; Create repeat-map for next/prev-buffer
;; (defvar aj8/switch-buffer-map
;;   (let ((map (make-sparse-keymap)))
;;     (keymap-set map "<left>" #'aj8/previous-buffer)
;;     (keymap-set map "<right>" #'aj8/next-buffer)
;;     map)
;;     "Repeat map for next/prev-buffer.")

;; Add repeat-map property to next/prev-buffer map
;; (dolist (cmd '(aj8/previous-buffer
;;                aj8/next-buffer))
;;   (put cmd 'repeat-map 'aj8/switch-buffer-map))

;; Add "/" to undo-repeat-map
;; (keymap-set undo-repeat-map "/" #'undo)

;; Repeat state for arbitrary keymaps
;; (defun repeated-prefix-help-command ()
;;   TODO: Does not work
;;   (interactive)
;;   (when-let* ((keys (this-command-keys-vector))
;;               (prefix (seq-take keys (1- (length keys))))
;;               (orig-keymap (key-binding prefix 'accept-default))
;;               (keymap (copy-keymap orig-keymap))
;;               (exit-func (set-transient-map keymap t #'which-key-abort)))
;;     (define-key keymap [remap keyboard-quit]
;;       (lambda () (interactive) (funcall exit-func)))
;;     (which-key--create-buffer-and-show nil keymap)))

;; (setq prefix-help-command #'repeated-prefix-help-command)

;;; Suppress messages with `advice'

(defun aj8/add-suppress-messages-advice (functions)
  "Add advice to suppress messages for the provided FUNCTIONS.
FUNCTIONS can be a symbol or a list of symbols representing function names."
  (let ((func-list (if (listp functions) functions (list functions))))
    (dolist (fn func-list)
      (advice-add fn :around #'aj8/suppress-messages))))

(defun aj8/remove-suppress-messages-advice (functions)
  "Remove advice that suppresses messages for the provided FUNCTIONS.
FUNCTIONS can be a symbol or a list of symbols representing function names."
  (let ((func-list (if (listp functions) functions (list functions))))
    (dolist (fn func-list)
      (advice-remove fn #'aj8/suppress-messages))))

(defun aj8/suppress-messages (orig-fun &rest args)
  "Suppress messages during call to function ORIG-FUN.
ARGS are the arguments to be passed to ORIG-FUN."
  (let ((inhibit-message t)   ; disable messages in Echo area
        (message-log-max nil))   ; disable messages in Messages buffer
    (apply orig-fun args)))

;;; xterm key sequence mappings for rxvt

(defun rxvt--add-escape-key-mapping-alist (escape-prefix key-prefix suffix-alist)
  "Add mappings for a given list of escape sequences and list of keys."
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

;;; Re-flow buffers

(defconst aj8/reflow-bullet-regexp
  "^[ \t]*\\(•\\|[*]\\|[(]?[0-9]+[.)]\\|[(]?[a-z][.)]\\)[ \t]"
  "Regular expression matching a bullet or numbered-list marker at the start of a line.")

(defconst aj8/reflow-forbidden-regexps-info
  '(
    "^[ \t]*[-+*=—]\\{2,\\}"            ; Multiple markers
    ;; "^[ \t]*\\(;;\\|[(][^‘A-Z]\\)"   ; Elisp code and comments
    "^[ \t]*\\(;;\\|[(][^‘A-Z]*$\\)"    ; Elisp code and comments
    "^[ \t]\\{8,\\}"                    ; Excessive indentation
    )
  "Forbidden line regexps for Info buffers.")

(defconst aj8/reflow-forbidden-regexps-helpful
  '(
    "^[ \t]*\\(Signature\\|Documentation\\|References\\|Debugging\\|Source Code\\|Symbol Properties\\)[ \t]*$"
    "^[ \t]*\\(;;\\|[(][^‘A-Z]*$\\)"    ; Elisp code and comments
    )
  "Forbidden line regexps for helpful buffers.")

(defun aj8/reflow-count-matches (regexp text)
  "Return the number of non-overlapping occurrences of REGEXP in TEXT."
  (let ((count 0)
        (pos 0))
    (while (string-match regexp text pos)
      (setq count (1+ count))
      (setq pos (1+ (match-beginning 0))))
    count))

;; TODO
;;   Convert this to a defconst regexp for consistency with other regexps above
;;   Use `sentence-end' instead
(defun aj8/reflow-sentence-match-p (text)
  "Return t if TEXT starts and ends like a sentence."
  (and (string-match-p "^[[:upper:]]" text)   ; “‘
       (string-match-p "[.:)\"”]$" text)))   ; ”

(defun aj8/reflow-structure-match-p (beg end)
  "Return t if the text between BEG and END has a paragraph-like structure.

A text block is considered to have a paragraph-like structure if the
text meets either of the following two criteria:

  1. The text start with an uppercase letter and end with a dot.

  2. If the text starts with a bullet or numbered-list marker, then
     there must be no more than one such marker, and, the remaining text
     must be a sentence or series of sentences.

This function uses `aj8/reflow-bullet-regexp' to detect bullet markers."
  (let* ((text (string-trim (buffer-substring-no-properties beg end)))
         (candidate (if (string-match-p aj8/reflow-bullet-regexp text)
                        (and (<= (aj8/reflow-count-matches aj8/reflow-bullet-regexp text) 1)
                             (string-trim (replace-regexp-in-string aj8/reflow-bullet-regexp "" text)))
                      text)))
    (and candidate (aj8/reflow-sentence-match-p candidate))))

(defun aj8/reflow-forbidden-match-p (beg end regexps)
  "Return t if any of the REGEXPS matches any line between BEG and END."
  (save-excursion
    (goto-char beg)
    (let ((found nil))
      (while (and (< (point) end) (not found))
        (let ((line (thing-at-point 'line t)))
          (dolist (rx regexps)
            (when (string-match-p rx line)
              (setq found t))))
        (forward-line 1))
      found)))

;; Unused function
(defun aj8/reflow-paragraph-match-p (beg end regexp mode)
  "Return t if the paragraph between BEG and END satisfies a regexp check.
REGEXP is applied to each line. MODE determines how the results are combined:
  'all  : returns t if every line matches REGEXP
  'any  : returns t if at least one line matches REGEXP
  'none : returns t if no line matches REGEXP"
  (save-excursion
    (goto-char beg)
    (cond
     ((eq mode 'all)
      (catch 'fail
        (while (< (point) end)
          (let ((line (thing-at-point 'line t)))
            ;; Debug
            (message "\nLine:\n%s" line)
            (unless (string-match-p regexp line)
              (progn
                ;; Debug
                (message "No match: %s" regexp)
                (throw 'fail nil))))
          (forward-line 1))
        t))
     ((eq mode 'any)
      (catch 'match
        (while (< (point) end)
          (let ((line (thing-at-point 'line t)))
            ;; Debug
            (message "\nLine:\n%s" line)
            (when (string-match-p regexp line)
              (progn
                ;; Debug
                (message "Line matched: %s" regexp)
                (throw 'match t))))
          (forward-line 1))
        nil))
     ((eq mode 'none)
      ;; 'none is just the inverse of 'any
      (not (aj8/reflow-paragraph-match-p beg end regexp 'any)))
     (t
      (error "Invalid mode: %s (must be 'all, 'any, or 'none)" mode)))))

;; Optional wrappers for convenience:
(defun aj8/reflow-paragraph-match-all-p (beg end regexp)
  "Return t if every line in the paragraph between BEG and END matches REGEXP."
  (aj8/reflow-paragraph-match-p beg end regexp 'all))

(defun aj8/reflow-paragraph-match-any-p (beg end regexp)
  "Return t if any line in the paragraph between BEG and END matches REGEXP."
  (aj8/reflow-paragraph-match-p beg end regexp 'any))

(defun aj8/reflow-paragraph-match-none-p (beg end regexp)
  "Return t if no line in the paragraph between BEG and END matches REGEXP."
  (aj8/reflow-paragraph-match-p beg end regexp 'none))

(defun aj8/reflow-join-lines-in-region (beg end)
  "Join lines between BEG and END.
The function removes hard line breaks (newline characters) that split a
text into separate lines."
  (save-excursion
    (goto-char beg)
    ;; Debug
    ;; (insert "<Start>")
    (while (re-search-forward "\\([^ \n]\\)[ \t]*\n[ \t]*\\([^ \n]\\)" end t)
      (replace-match "\\1 \\2" nil nil))))

(defun aj8/reflow-buffer (forbidden-regexps)
  "Re-flow the current buffer by joining lines in each paragraph.
For paragraphs to be re-flowed, individual lines must not match any
regexp in FORBIDDEN-REGEXPS, and a structure criteria must be met.  See
`aj8/reflow-structure-match-p'."
  (with-demoted-errors "Error re-flowing text: %S"
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((p-beg (point)))
            (forward-paragraph)
            (let ((p-end (point)))
              ;; Debug:
              ;; (message "Paragraph from %d to %d:\n%s" p-beg p-end
              ;;          (buffer-substring-no-properties p-beg p-end))
              (unless (aj8/reflow-forbidden-match-p p-beg p-end forbidden-regexps)
                (when (aj8/reflow-structure-match-p p-beg p-end)
                  (aj8/reflow-join-lines-in-region p-beg p-end))))
            (when (< (point) (point-max))
              (forward-char 1))))))))

(defun aj8/reflow-info-buffer ()
  "Re-flow the current Info node, joining lines where appropriate.
Uses a common first-line rule (first non-blank character must be uppercase)
and Info-specific forbidden regexps."
  (interactive)
  (aj8/reflow-buffer aj8/reflow-forbidden-regexps-info))

(defun aj8/reflow-info-buffer-advice (orig-fun &rest args)
  "Advice function to re-flow an Info node after it is selected."
  (let ((result (apply orig-fun args)))
    (aj8/reflow-info-buffer)
    result))

(define-minor-mode aj8/reflow-info-mode
  "Minor mode that toggles automatic re-flowing of Info nodes.
When enabled, Info-select-node is advised so that after a node is
selected, the buffer’s text is re-flowed."
  :init-value nil
  :global t
  :lighter (:eval (when (derived-mode-p 'Info-mode) " RF"))
  (if aj8/reflow-info-mode
      (advice-add 'Info-select-node :around #'aj8/reflow-info-buffer-advice)
    (advice-remove 'Info-select-node #'aj8/reflow-info-buffer-advice)))

(defun aj8/reflow-helpful-buffer ()
  "Re-flow the current Helpful buffer, joining lines where appropriate.
Uses a common first-line rule (first non-blank character must be
uppercase) and Helpful-specific forbidden regexps."
  (interactive)
  (aj8/reflow-buffer aj8/reflow-forbidden-regexps-helpful))

(defun aj8/reflow-helpful-buffer-advice (orig-fun &rest args)
  "Advice function to re-flow a Helpful buffer."
  (let ((result (apply orig-fun args)))
    (aj8/reflow-helpful-buffer)
    result))

(define-minor-mode aj8/reflow-helpful-mode
  "Minor mode that toggles automatic re-flowing of Helpful buffers.
When enabled, helpful-update is advised so that after a Helpful buffer
is updated, the buffer’s text is re-flowed."
  :init-value nil
  :global t
  :lighter (:eval (when (derived-mode-p 'helpful-mode) " RF"))
  (if aj8/reflow-helpful-mode
      (advice-add 'helpful-update :around #'aj8/reflow-helpful-buffer-advice)
    (advice-remove 'helpful-update #'aj8/reflow-helpful-buffer-advice)))

;;; Misc

;; Swap universal prefix argument for functions
;;   (using advice)
(defun my/toggle-prefix-arg (fun)
  "Toggle universal prefix argument for the function FUN.
If called with a prefix argument, the prefix argument will be
removed.  If called without a prefix argument, a prefix argument
will be applied.  This only works for interactive \"P\"
functions."
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
(defun aj8/which-key-description-length (width)
  "Return `which-key' description width based on the given frame WIDTH.
Note that the available width is slightly less than reported by
`frame-width'. See `which-key--side-window-max-dimensions'"
    ;; (message "%s" width)
    (cond
     ((= width 142)   ; brain10-windows
      ;; 0.15)   ; TODO: floats don't work
      21)
     ((= width 138)   ; 98-28514
      26)
     ((= width 160)   ; brain10-windows (reduced font)
      24)
     ((= width 172)   ; macOS
      26)
     (t
      27)))   ; default value

;; Configure benchmark-init list format
(defun aj8/benchmark-init-list-format ()
  (setq-local tabulated-list-format
              (quote [("Module" 50 t)
                      ("Type" 7 t)
                      ("ms" 7 (lambda (a b) (< (string-to-number (aref (cadr a) 2))
                                               (string-to-number (aref (cadr b) 2))))
                       :right-align t)
                      ("total" 7 (lambda (a b) (< (string-to-number (aref (cadr a) 3))
                                                  (string-to-number (aref (cadr b) 3))))
                       :right-align t)])))

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
