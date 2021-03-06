;;;;; FUNCTIONS

;;;; Package management

;;;; Theme

;; Custom settings for modus-themes
(defun aj8/modus-themes-custom-settings ()
  "If in terminal, change cursor color on theme switch."
  (when (not (display-graphic-p))   ; if using terminal
    (cond
     ((string-match-p "modus-operandi" (symbol-name (modus-themes--current-theme)))
      (send-string-to-terminal "\033]12;black\007"))
     ((string-match-p "modus-vivendi" (symbol-name (modus-themes--current-theme)))
      (send-string-to-terminal "\033]12;white\007"))
     (t
      (error "Unknown modus-theme name")))))

;;;; Windows

;;; Kill windows

;; Quit window and kill its buffer
(defun aj8/quit-window ()
  "Quit WINDOW and kill its buffer.
With a prefix argument, bury the buffer instead (this is the
inverse of the default behavior of the standard `quit-windows'
function)."
  (interactive)
  (if current-prefix-arg   ; C-u
      (quit-window)        ; bury
    (quit-window 1)))      ; kill

;; Quit magit window and kill its buffer
(defun aj8/magit-mode-bury-buffer ()
  "Quit WINDOW and kill its buffer.
With a prefix argument, bury the buffer instead (this is the
inverse of the default behavior of the standard
`magit-mode-quit-window' function)."
  (interactive)
  (if current-prefix-arg             ; C-u
      (magit-mode-bury-buffer nil)   ; bury
    (magit-mode-bury-buffer 1)))     ; kill

;;; Better shrink/enlarge window functions

;; Wrapper for shrink-window-horizontally
(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

;; Wrapper for enlarge-window-horizontally
(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

;; Wrapper for enlarge-window
(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

;; Wrapper for shrink-window
(defun hydra-move-splitter-down (arg)
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

;;; Skip unimportant buffers when switching

(defcustom my/skippable-buffer-regexp
  (rx bos (or (or "*Backtrace*" "*Compile-Log*" "*Completions*" "*Help*"
                  "*Messages*" "*package*" "*Quail Completions*"
                  "*quelpa-build-checkout*" "*scratch*" "*Warnings*"
                  "*Async-native-compile-log*")
              (seq "magit-diff" (zero-or-more anything))
              (seq "magit-process" (zero-or-more anything))
              (seq "magit-revision" (zero-or-more anything)))
              eos)
  "Matching buffer names are ignored by `my/next-buffer'
and `my/previous-buffer'."
  :type 'regexp)

(defun my/change-buffer (change-buffer)
  "Call CHANGE-BUFFER until `my/skippable-buffer-regexp' doesn't match."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (string-match-p my/skippable-buffer-regexp (buffer-name))
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

;; Custom next-buffer
(defun my/next-buffer ()
  "Variant of `next-buffer' that skips `my/skippable-buffer-regexp'."
  (interactive)
  (my/change-buffer 'next-buffer))

;; Custom previous-buffer
(defun my/previous-buffer ()
  "Variant of `previous-buffer' that skips `my/skippable-buffer-regexp'."
  (interactive)
  (my/change-buffer 'previous-buffer))

;;; Misc

;; Kill buffer in other window
(defun my/kill-buffer-other-window ()
  "If there are multiple windows, then kill the buffer in the next window."
  (interactive)
  (unless (one-window-p)
    (setq win (selected-window))   ; TODO: is setq the correct syntax?
    (other-window 1)
    (when (window-parameter (selected-window) 'window-side)   ; skip side windows
      (other-window 1))
    (kill-buffer)
    (select-window win)))

;; Make a *scratch* buffer
(defun create-scratch-buffer nil
       "Create a scratch buffer."
       (interactive)
       (switch-to-buffer (get-buffer-create "*scratch*"))
       (lisp-interaction-mode))

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
  (interactive)                         ; TODO: why interactive?
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

;;; Movement by whitespace

;; Backward movement by whitespace
;;   (complements the built-in forward-whitespace)
(defun my/backward-whitespace (arg)
  "Move point to the beginning of the current sequence of whitespace characters."
  (interactive "^p")
  (forward-whitespace (- arg)))


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

;;;; Editing

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

;;;; Completion

;;; Orderless style dispatchers

;; Flex
(defun my/flex-if-twiddle (pattern _index _total)
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

;; Exclude literal
(defun my/without-if-bang (pattern _index _total)
  (when (string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1))))

;; Include literal
(defun my/with-if-equal (pattern _index _total)
   (when (string-prefix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 1))))

;;; Misc

;; Set custom completion styles
(defun my/completion-styles ()
  "Set custom completion styles."
  ;; Completion styles
  (setq completion-styles '(basic partial-completion initials))
  ;; Completion styles for files
  (setq completion-category-overrides '((file (styles . (basic substring))))))
  ;; Disable *Completions* buffer
  ;; (setq completion-show-help nil))
  ;; Cycle completions
  ;; (setq completion-cycle-threshold t)

;;;; Spelling

;;; ispell

;; Toggle ispell program
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
    (with-eval-after-load "ispell"
      (ispell-set-spellchecker-params)
      (ispell-hunspell-add-multi-dic "en_US,sv_SE"))
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

;;;; Files

;;;; Coding

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

;;;; Help

;;; Misc

;; Always open additional helpful buffers in the same window
(defun aj8/helpful-switch-to-buffer (buffer-or-name)
  "Switch to helpful BUFFER-OR-NAME.
If we are currently in the helpful buffer, reuse it's window,
otherwise create a new one.  With a prefix argument open the
buffer in a new window instead."
  (interactive)
  (if (eq major-mode 'helpful-mode)
    (cond
     ((equal current-prefix-arg nil) ; no C-u
      (switch-to-buffer buffer-or-name)
      (pop-to-buffer buffer-or-name))
     ((equal current-prefix-arg '(4)) ; C-u
      (pop-to-buffer buffer-or-name))
     (t
      (error "Unexpected input arguments")))
    (pop-to-buffer buffer-or-name)))

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
(defun aj8/eww-follow-link (&optional external mouse-event)
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
(defun prot-eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    (rename-buffer (format "*%s # eww*" name) t)))

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


(provide 'aj8-lisp)
