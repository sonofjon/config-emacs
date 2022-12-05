;;;;; FUNCTIONS

;;;; Admin

;;;; Buffers

;;; Undo for killed buffers

(defvar killed-file-list nil
  "List of recently killed files.")

(defun reopen-killed-file--add-to-list ()
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))

;; Undo for killed buffers
(defun my/reopen-killed-file ()
  ;; TODO: make it work for all buffers, not just files
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when killed-file-list
    (find-file (pop killed-file-list))))

;; Fancy undo for killed buffers
(defun my/reopen-killed-file-fancy ()
  "Pick a file to revisit from a list of files killed during this
Emacs session."
  (interactive)
  (if killed-file-list
      (let ((file (completing-read "Reopen killed file: " killed-file-list
                                   nil nil nil nil (car killed-file-list))))
        (when file
          (setq killed-file-list (cl-delete file killed-file-list :test #'equal))
          (find-file file)))
    (error "No recently-killed files to reopen")))

;;; Buffer switching

(defcustom aj8/buffer-skip-regexp
  (rx bos (or (or "*Messages*" "*scratch*" "*Warnings*")
              (seq "magit-diff" (zero-or-more anything))
              (seq "magit-process" (zero-or-more anything))
              (seq "magit-revision" (zero-or-more anything))
              (seq "magit-stash" (zero-or-more anything)))
      eos)
  "Regular expression matching buffers ignored by `next-buffer'
and `previous-buffer'."
  :type 'regexp
  :group 'aj8-lisp)

(defun aj8/buffer-skip-p (window buffer bury-or-kill)
  "Return t if BUFFER name matches `aj8/buffer-skip-regexp'."
  (string-match-p aj8/buffer-skip-regexp (buffer-name buffer)))

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
  "In selected window switch to the next project buffer.
If the current buffer does not belong to a project, call `next-buffer'."
  (interactive)
  ;; (let ((switch-to-prev-buffer-skip 'aj8/buffer-skip-p))
  ;;   TODO: In Emacs 29 there is also switch-to-prev-buffer-skip-regexp
    (my/project--repeat-until-project-buffer #'next-buffer))

;; Switch to previous project buffer
(defun my/project-previous-buffer ()
  "In selected window switch to the previous project buffer.
If the current buffer does not belong to a project, call `previous-buffer'."
  (interactive)
  ;; (let ((switch-to-prev-buffer-skip 'aj8/buffer-skip-p))
    (my/project--repeat-until-project-buffer #'previous-buffer))

;;; Manipulate buffer names

;; Tag buffer name
(defun aj8/tag-buffer-name (tag)
  "Add TAG to buffer name."
  (rename-buffer (format "%s # %s" (buffer-name) tag)))

;; Prefix buffer name
(defun aj8/prefix-buffer-name (prefix)
  "Add PREFIX to buffer name."
  (rename-buffer (format "%s: %s" prefix (buffer-name))))

;;; Buffer matching functions

;; Make major-mode matching function
(defun mp-make-display-buffer-matcher-function (major-modes)
  "Return a lambda function that matches against a list of
major-modes."
  (lambda (buffer-name action)
    (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))

;; Check if buffer belongs to a project
;; (defun mp-buffer-has-project-p (buffer action)
;;   "Return non-nil if BUFFER belongs to a project."
;;   (with-current-buffer buffer (project-current nil)))

;;; Misc

;; Kill buffer in other window
(defun my/kill-buffer-other-window ()
  "If there are multiple windows, then kill the buffer in the next window."
  (interactive)
  (unless (one-window-p)
    (setq win (selected-window))
    (other-window 1)
    (while (window-parameter (selected-window) 'window-side) ; skip side windows
      (other-window 1))
    (if (not (eq (selected-window) win))   ; don't kill initial window
        (kill-buffer)
      (message "No other window to kill"))
    (select-window win)))

;;;; Coding

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

;; Literal
(defun aj8/orderless-matching-style--literal ()
  "Components match literally for the rest of the session."
  (setq-local orderless-matching-styles '(orderless-literal))
  (minibuffer-message "[literal]"))

;; Prefixes
(defun aj8/orderless-matching-style--prefixes ()
  "Components match prefixes for the rest of the session."
  (setq-local orderless-matching-styles '(orderless-prefixes))
  (minibuffer-message "[prefixes]"))

;; Regexp
(defun aj8/orderless-matching-style--regexp ()
  "Components match regexp for the rest of the session."
  (setq-local orderless-matching-styles '(orderless-regexp))
  (minibuffer-message "[regexp]"))

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
  "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
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

;;; Misc

;; Copy symbol at point
(defun my/copy-symbol-at-point ()
  "Add the symbol at point to the kill ring."
  (interactive)
  (let ((symbol (thing-at-point 'symbol))
        (bounds (bounds-of-thing-at-point 'symbol)))
    (when symbol
      (kill-new symbol)
      (pulse-momentary-highlight-region (car bounds) (cdr bounds)))))

;; Capitalize word at point
(defun aj8/capitalize-word-at-point ()
  "Capitalize ARG words from the beginning of the current word."
  (interactive)
  (unless (looking-at "\\<")
    (backward-word))
  (capitalize-word 1))

;; Delete blank lines in region
(defun aj8/delete-blank-lines-region (beg end &optional region)
  "Delete blank lines, leaving one, in region."
  (interactive (progn
                 (let ((beg (mark))
                       (end (point)))
                   (unless (and beg end)
                     (user-error "The mark is not set now, so there is no region"))
                   (list beg end 'region))))
  (replace-regexp "\\(\\(^\\s-*$\\)\n\\)\\{2,\\}" "\n" nil beg end))

;;;; Files

;;;; Help

;;; Misc

;;;; Navigation

;;; Scrolling

;; Scroll up one paragraph
(defun aj8/scroll-up-paragraph ()
  ;; TODO: Why is the scroll-up-line at the end needed?
  "Scroll text of selected window upward one paragraph."
  (interactive)
  (save-excursion
    (goto-char (window-end))
    (while (progn
             (scroll-up-line)
             (forward-line)
             (looking-at "^$")))   ; scroll past all empty lines
    (while (progn
             (scroll-up-line)
             (forward-line)
             (not (looking-at "^$"))))
    (scroll-up-line)))

;; Scroll down one paragraph
(defun aj8/scroll-down-paragraph ()
  "Scroll text of selected window downward one paragraph."
  (interactive)
  (save-excursion
    (goto-char (window-start))
    (while (progn
             (scroll-down-line)
             (forward-line -1)
             (looking-at "^$")))   ; scroll past all empty lines
    (while (progn
             (scroll-down-line)
             (forward-line -1)
             (not (looking-at "^$"))))))
    ;; (scroll-down-line)))

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
  "Move point to beginning of comment on current line. See also
`mosey-goto-beginning-of-comment-text'."
  (when (save-excursion
          (end-of-line)
          (re-search-backward (rx (syntax comment-start))
                              (line-beginning-position) t))
    (goto-char (- (match-end 0) 1))))

;; Combine Mosey with regular move-end-of-line
(defun aj8/mosey-eol ()
  "Move point to end of line and then cycle backward between mosey
points."
  (interactive)
  (if (or (eolp)
          (eq last-command 'aj8/mosey-eol))
      (mosey-eol-backward-cycle)
    (move-end-of-line nil)))

;; Combine Mosey with regular move-beginning-of-line
(defun aj8/mosey-bol ()
  "Move point to beginning of line and then cycle forward between
mosey points."
  (interactive)
  (if (or (bolp)
          (eq last-command 'aj8/mosey-eol))
      (mosey-bol-forward-cycle)
    (move-beginning-of-line nil)))

;;; Misc

;; Backward movement by whitespace
;;   (complements the built-in forward-whitespace)
(defun my/backward-whitespace (arg)
  "Move point to the beginning of the current sequence of whitespace characters."
  (interactive "^p")
  (forward-whitespace (- arg)))

;;;; Outline

;;; Set outline header format

;; Elisp files
(defun outline-headers-for-semicolon-buffers ()
  "Set outline header format for buffers using semicolon (\";\")
for comments."
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
  "Set outline header format for buffers using hash mark (\"#\")
for comments."
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
  "Set outline header format for buffers using hash mark (\"%\")
for comments."
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
  "Set outline header format for buffers using exclamation
mark (\"!\") for comments."
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
  "Copy the visible parts of the region."
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
(defun aj8/mark-word-forward (N)
  "Like mark-word, but select entire word at point."
  (interactive "p")
  (when (and
         (not (eq last-command this-command))
         (not (eq last-command #'aj8/mark-word-backward)))
    (if (and (looking-at "[[:alnum:]]") (looking-back "[[:alnum:]]"))
        (backward-word))
    (set-mark (point)))
  (forward-word N))

;; Mark whole word (backward)
(defun aj8/mark-word-backward (N)
  "Like mark-word, but select entire word at point. 
Repeat command to select additional words backwards."
  (interactive "p")
  (when (and
         (not (eq last-command this-command))
         (not (eq last-command #'aj8/mark-word-forward)))
    (if (and (looking-at "[[:alnum:]]") (looking-back "[[:alnum:]]"))
        (forward-word))
    (set-mark (point)))
  (backward-word N))

;;;; Spelling

;;; ispell

;; Toggle ispell program
;;   Requires: aspell, hunspell, hunspell-sv
(defun aj8/toggle-ispell-program ()
  "Toggle `ispell` program.
If current program is `aspell`, switch to `hunspell`, and vice
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
    (error "`ispell-program` must be either `aspell` or `hunspell`"))))


;;; Flyspell

;; Setup for web-mode
(defun my/web-mode-flyspell-verify ()
  "Fly Spell predicate of `web-mode`."
  (let* ((font-face-at-point (get-text-property (- (point) 1) 'face))
         rlt)
    ;; If rlt is t, the word at point is POSSIBLY a typo, continue checking.
    (setq rlt t)
    ;; if rlt is nil, the word at point is definitely NOT a typo.
    ;; (setq rlt nil)
    rlt))

;; Setup for web-mode (with blacklist)
;; (defun my/web-mode-flyspell-verify ()
;;   "Fly Spell predicate of `web-mode`."
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
  "Go to arg previous spelling error."
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

(setq calendar-latitude 59.33)
(setq calendar-longitude 18.07)

(defun aj8/time-sunrise ()
  ;; TODO: Should return fractional value
  "Get clean sunrise time string from `solar-sunset-sunrise`."
  (cl-first (cl-first (solar-sunrise-sunset (calendar-current-date)))))

(defun aj8/time-sunset ()
  "Get clean sunset time string from `solar-sunset-sunrise`."
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
  "Return True if current time is daytime, False otherwise."
  (and (> (decoded-time-hour (decode-time)) aj8/time-sunrise)
       (< (decoded-time-hour (decode-time)) aj8/time-sunset)))

;;; Modus

;; Custom settings for modus-themes
(defun aj8/modus-themes-custom-settings ()
  "If in terminal, change cursor color on theme switch."
  (when (not (display-graphic-p))   ; if using terminal
    (cond
     ((string-match-p "modus-operandi"
                      (symbol-name (modus-themes--current-theme)))
      (send-string-to-terminal "\033]12;black\007"))
     ((string-match-p "modus-vivendi"
                      (symbol-name (modus-themes--current-theme)))
      (send-string-to-terminal "\033]12;white\007"))
     (t
      (error "Unknown modus-theme name")))))

;;;; Version control

;;; Misc

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
  "Right side window width. If the width of the frame is greater
than or equal to 160 characters, split the frame in two, if less
than 160 characters set width to 80 characters."
  :type 'integer
  :group 'aj8-lisp)

;; TODO: finish this
(defun aj8/make-not-side-window ()
  "."
  (interactive)
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (display-buffer-use-some-window buffer '()))))

;;; Kill windows

(defcustom my/quit-window-exceptions-regex "^\\*\\(Messages\\)"
  "Regexp matching buffer names for which prefix argument should
not be inverted."
  :type 'regexp
  :group 'my-config)

(defcustom my/quit-window-known-wrappers '(magit-mode-bury-buffer
                                           magit-log-bury-buffer
                                           Info-exit)
  "List of commands that call `quit-window' for which prefix
argument should be inverted. "
  :type 'sexp
  :group 'my-config)

(defun my/advice--quit-window (args)
  "Advice function that makes `quit-window' quit WINDOW and kill
its buffer. With a prefix argument, the buffer is buried
instead. This is the inverse of the default behavior
`quit-window'.

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

;; Quit window and kill its buffer
(advice-add 'quit-window :filter-args 'my/advice--quit-window)

;;; Better shrink/enlarge window functions

;; Wrapper for shrink-window-horizontally
(defun my/move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

;; Wrapper for enlarge-window-horizontally
(defun my/move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

;; Wrapper for enlarge-window
(defun my/move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

;; Wrapper for shrink-window
(defun my/move-splitter-down (arg)
  "Move window splitter down."
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
    (error "Can only toggle a window split in two!"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window)   ; close current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically))   ; makes a split with the other window twice
    (switch-to-buffer nil)))   ; restore the original window
                               ; in this part of the window
;; Visit oldest window with Treemacs
(defun treemacs-visit-node-in-least-recently-used-window (&optional arg)
  "Open current file or tag in window selected by `get-lru-window'.
Stay in the current window with a single prefix argument ARG, or close the
Treemacs window with a double prefix argument."
  (interactive "P")
  (treemacs--execute-button-action
   :window (get-lru-window (selected-frame) nil :not-selected)
   :file-action (find-file (treemacs-safe-button-get btn :path))
   :dir-action (dired (treemacs-safe-button-get btn :path))
   :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
   :tag-action (treemacs--goto-tag btn)
   :window-arg arg
   :ensure-window-split t
   :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

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
   (t (error "Unexpected input arguments"))))

;; More useful buffer names in eww
;;   TODO: Emacs 29: Use eww-auto-rename-buffer instead
(defun prot-eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    (rename-buffer (format "*eww: %s*" name) t)))

(add-hook 'eww-after-render-hook #'prot-eww--rename-buffer)
(advice-add 'eww-back-url :after #'prot-eww--rename-buffer)
(advice-add 'eww-forward-url :after #'prot-eww--rename-buffer)

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
  (interactive)
  (save-excursion
    (forward-sexp)
    (eval-last-sexp nil)))

;; Eval sexp at point
(defun my/eval-sexp-at-point (levels)
  (interactive "p")
  (save-excursion
    (up-list (abs levels))
    (eval-last-sexp nil)))

;;; Custom repeat-maps
;;;   TODO: Add move-dup
;;;         Remove switch buffer

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

;; Create repeat-map for window resizing
(defvar aj8/resize-window-repeat-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "{" #'my/move-splitter-up)
    (keymap-set map "}" #'my/move-splitter-down)
    (keymap-set map ">" #'my/move-splitter-right)
    (keymap-set map "<" #'my/move-splitter-left)
    map))

;; Add repeat-map property to window resizing commands
(dolist (cmd '(my/move-splitter-up
               my/move-splitter-down
               my/move-splitter-right
               my/move-splitter-left))
  (put cmd 'repeat-map 'aj8/resize-window-repeat-map))

;; Add "/" to undo-repeat-map
(keymap-set undo-repeat-map "/" #'undo)

;;; xterm key sequence mappings for rxvt

(defun rxvt--add-escape-key-mapping-alist (escape-prefix key-prefix suffix-alist)
  "Add mappings for a given list of escape sequences and list of
keys."
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
  "Map each combination of modifier and up, down, left and right
keys to the the respective xterm key sequence."
  (setq nav-key-pair-alist
        '(("A" . "<up>") ("B" . "<down>") ("C" . "<right>") ("D" . "<left>")
          ("H" . "<home>") ("F" . "<end>")))

  (rxvt--add-escape-key-mapping-alist "\e[1;2" "S-" nav-key-pair-alist)
  (rxvt--add-escape-key-mapping-alist "\e[1;3" "M-" nav-key-pair-alist)
  (rxvt--add-escape-key-mapping-alist "\e[1;4" "M-S-" nav-key-pair-alist)
  (rxvt--add-escape-key-mapping-alist "\e[1;5" "C-" nav-key-pair-alist)
  (rxvt--add-escape-key-mapping-alist "\e[1;6" "C-S-" nav-key-pair-alist)
  (rxvt--add-escape-key-mapping-alist "\e[1;7" "M-C-" nav-key-pair-alist)
  (rxvt--add-escape-key-mapping-alist "\e[1;8" "M-C-S-" nav-key-pair-alist))

;;; Misc

;; Swap universal prefix argument for functions
;;   (using advice)
(defun my/toggle-prefix-arg (fun)
  "Toggle universal prefix argument for FUNCTION fun.
If called with a prefix argument, the prefix argument will be
removed. If called without a prefix argument, a prefix argument
will be applied. This only works for interactive \"P\"
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
  "Call FUNCTION fun and toggle its universal prefix argument.
If called with a prefix argument, the prefix argument will be
removed. If called without a prefix argument, a prefix argument
will be applied."
  (if (equal current-prefix-arg nil) ; no C-u
       ;; then
       (let ((current-prefix-arg '(4)))
         (call-interactively fun))
     ;; else
     (let ((current-prefix-arg nil))
       (call-interactively fun))))

;; Reload init-file
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

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
    "Return `which-key' description width for different frame
widths. Note that the available width is slightly less than
reported by `frame-width'. See
`which-key--side-window-max-dimensions'"
    ;; (message "%s" width)
    (cond
     ((= width 142)   ; brain10-windows
      ;; 0.15)   ; TODO: floats don't work
      21)
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


(provide 'aj8-lisp)
