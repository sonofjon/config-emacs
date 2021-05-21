;;
;; STARTUP
;;

;; Check startup time
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Reduce garbage collection during startup
;;   (and reset default values after)
(setq gc-cons-threshold 100000000
      gc-cons-percentage 0.5)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000
                                            gc-cons-percentage 0.1)))


;;
;; LOCAL SETTINGS (EARLY)
;;

(cond ((equal (system-name) "MacBook-Air.lan")
       ;; Use custom-file.el for custom-* code (Chemacs setup)
       (setq custom-file "~/.emacs.default/custom-file.el"))

      ((equal (system-name) "penguin")
       ;; Use custom-file.el for custom-* code
       (setq custom-file "~/.emacs.d/custom-file.el")
       ;; Fix for Emacs bug on Chromebook (Emacs < v26.3)
       ;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
       (message "Fix for Chromebook")
       (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

      (t 
       ;; Use custom-file.el for custom-* code
       (setq custom-file "~/.emacs.d/custom-file.el")))

;; Load custom file
(load-file custom-file)


;;
;; FIXES
;;

;; Suppress warnings about unnecessary package-initialize calls
;;   Introduced when switching to Melpa Stable
;;   Possible solution: use some packages from Melpa
;;   Reference: https://github.com/cask/cask/issues/463
(setq warning-suppress-log-types '((package reinitialization)))


;;
;; PACKAGES SETUP
;;

;; Initialize package sources
(require 'package)
(if (version< emacs-version "27")
    (package-initialize))

;; Add MELPA to package-archives
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Set package-archives priorities
;; (setq package-archive-priorities
;;       '(("gnu"          . 3)
;;         ("melpa-stable" . 2)
;;         ("melpa"        . 1)))

;; Refresh packages database (in background)
;;   async argument fails with Chemacs
;; (unless package-archive-contents
;;   (package-refresh-contents t))

;; Refresh packages database (on first install)
(defun my-package-install-refresh-contents (&rest args)
  (package-refresh-contents)
  (advice-remove 'package-install 'my-package-install-refresh-contents))

(advice-add 'package-install :before 'my-package-install-refresh-contents)

;; Install use-package macro
(if (not (package-installed-p 'use-package))
    (package-install 'use-package))

;; Load use-package macro
(eval-when-compile
  (require 'use-package))

;; Always install packages if not present
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; For debugging startup
(setq use-package-verbose t)

;; Lower time threshold for package load time reporting
(setq use-package-minimum-reported-time 0.001)

;; Alternative option to prioritise archives
;; (setq use-package-always-pin "melpa-stable")


;;
;; PACKAGES
;;

;; benchmark-init (startup profiler)
(use-package benchmark-init
  ;; :disabled
  :config
  ;; Disable collection of benchmark data after init
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; auto-package-update
(use-package auto-package-update
  :config
  ;; Prompt before update
  ;; (setq auto-package-update-prompt-before-update t)
  ;; Delete old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Don't show update results
  ;; (setq auto-package-update-hide-results t)
  ;; Enable mode
  (auto-package-update-maybe))

;; diminish (hide minor modes)
(use-package diminish
  :config
  ;; (diminish 'company-mode)
  ;; (diminish 'ivy-mode)
  ;; (diminish 'counsel-mode)
  ;; (diminish 'which-key-mode)
  (diminish 'eldoc-mode))

;; paradox (improved package menu)
(use-package paradox
  :config
  ;; Disable *Paradox Report* buffer
  (remove-hook 'paradox--report-buffer-print 'paradox-after-execute-functions)
  ;; Enable mode
  (paradox-enable))

;; base16-theme
;;   Available options: https://belak.github.io/base16-emacs/
(use-package base16-theme)
  ;; :config
  ;; (setq base16-theme-256-color-source "base16-shell")
  ;; (setq base16-theme-256-color-source "colors")
  ;; (load-theme 'base16-classic-dark t))
  ;; (load-theme 'base16-classic-light t))
  ;; (load-theme 'base16-default-dark t))
  ;; (load-theme 'base16-default-light t))
  ;; (load-theme 'base16-google-dark t))
  ;; (load-theme 'base16-google-light t))
  ;; (load-theme 'base16-onedark t))
  ;; (load-theme 'base16-one-light t))
  ;; (load-theme 'base16-solarized-dark t))
  ;; (load-theme 'base16-solarized-light t))
  ;; (load-theme 'base16-summerfruit-dark t))
  ;; (load-theme 'base16-summerfruit-light t))
  ;; (load-theme 'base16-brewer t))
  ;; (load-theme 'base16-chalk t))
  ;; (load-theme 'base16-circus t))
  ;; (load-theme 'base16-dracula t))
  ;; (load-theme 'base16-eighties t))
  ;; (load-theme 'base16-flat t))
  ;; (load-theme 'base16-materia t))
  ;; (load-theme 'base16-nord t))
  ;; (load-theme 'base16-snazzy t))
  ;; (load-theme 'base16-spacemacs t))
  ;; (load-theme 'base16-zenburn t))

;; spacemacs-theme
(use-package spacemacs-theme
  :disabled
  :defer   ; Fix loading warning
  :config
  ;; Use the spacemacs-dark theme
  ;; (load-theme 'spacemacs-dark t))
  ;; Fix loading warning
  :init (load-theme 'spacemacs-dark t))

;; doom-themes
(use-package doom-themes
  :disabled
  ;; :init (load-theme 'doom-one) t)
  ;; :init (load-theme 'doom-vibrant) t)
  ;; :init (load-theme 'doom-snazzy) t)
  :init (load-theme 'doom-Iosvkem) t)

;; company (in-buffer text completion)
(use-package company
  ;; :diminish
  :hook ((prog-mode . company-mode)
         (emacs-lisp-mode . company-mode))
  ;; :bind ("TAB" . company-complete)
  :config
  ;; Provide instant autocompletion (default is 0.5 s)
  ;; (setq company-idle-delay 0.0)
  ;; Minimum prefix for completion
  ;; (setq company-minimum-prefix-length 3)
  ;; Maximum number of candidates
  ;; (setq company-tooltip-limit 10)
  ;; Use company mode everywhere
  (company-mode 1))

(use-package company-web
  :after (:all company web-mode)
  :config (add-to-list 'company-backends 'company-web))

(use-package company-php
  ;; :after (:all company php-mode)
  :after (:all company (:any php-mode web-mode))
  :config (add-to-list 'company-backends 'company-php))

;; ivy (generic completion mechanism)
(use-package ivy
  :diminish
  :config (ivy-mode 1))

;; counsel (ivy-enhanced versions of common Emacs commands)
(use-package counsel
  :diminish
  :after ivy
  :bind (("C-c SPC" . counsel-mark-ring))
  ;;        ("M-y" . counsel-yank-pop)
  ;;        :map ivy-minibuffer-map
  ;;        ("M-y" . ivy-next-line-or-call))
  :config 
  ;; Don't start searches with ^
  (setq ivy-initial-inputs-alist nil)
  ;; Enable mode
  (counsel-mode 1))

;; swiper (ivy-enhanced isearch)
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("C-c s" . swiper)
         ("C-c S" . swiper-all)
         :map swiper-isearch-map
         ("C-r" . ivy-previous-line-or-history)))

;; ivy-rich (add descriptions to ivy/counsel output)
(use-package ivy-rich
  :pin melpa  
  :after ivy
  :config (ivy-rich-mode 1))

;; helpful (alternative help)
(use-package helpful
  ;; :after (:all ivy counsel)
  :commands (helpful-key helpful-function helpful-symbol helpful-variable) 
  :custom
  ;; Use helpful with counsel
  (counsel-describe-function-function #'helpful-function)
  (counsel-describe-symbol-function #'helpful-symbol)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ("C-c h" . 'helpful-at-point))

;; prescient (base package)
(use-package prescient
  :after (:any ivy company)
  :custom
  ;; Disable sorting by length
  (prescient-sort-length-enable nil)
  :config
  ;; Remember across sessions
  (prescient-persist-mode 1))

;; ivy-prescient (sort candidates by frequency and recency)
(use-package ivy-prescient
  :after (:all ivy prescient)
  :custom
  ;; Disable prescient filtering (use ivy filtering)
  (ivy-prescient-enable-filtering nil)
  ;; Use ivy highlighting
  (ivy-prescient-retain-classic-highlighting t)
  ;; Enable mode
  :config (ivy-prescient-mode 1))

;; company-prescient (sort candidates by frequency and recency)
(use-package company-prescient
  :after (:all company prescient)
  :config (company-prescient-mode 1))

;; which-key (display available keybindings)
(use-package which-key
  :diminish
  ;; :defer
  :config
  ;; Delay (default is 1.0 s)
  ;; (setq which-key-idle-delay 0.5))
  ;; Enable mode
  (which-key-mode 1))

;; magit (user interface to git)
(use-package magit
  ;; Disable hl-line-mode
  :hook (magit-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

;; rainbow-delimiters (parentheses coloring)
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)))

;; expand-region (grow selected region by semantic units)
(use-package expand-region
  :bind ("C-c =" . er/expand-region))

;; multiple-cursors (edit at multiple points)
(use-package multiple-cursors
  :bind (("C-c c" . set-rectangular-region-anchor)
         ("C-c >" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-previous-like-this)
         ("C-c ?" . mc/mark-all-like-this)
         ("C-c C" . mc/edit-lines)))

;; whole-line-or-region (apply to current line if region is undefined)
;; (use-package whole-line-or-region
;;   :config (whole-line-or-region-global-mode 1))

;; web-mode (major-mode for editing web templates)
(use-package web-mode
  :pin melpa
  :mode (".html?$" ".php$"))

;; google-this (google search functions)
(use-package google-this
  :bind ("C-c g" . google-this-mode-submap)
  :config
  (google-this-mode 1))

;; diff-hl (highlight uncommitted changes)
(use-package diff-hl
  :disabled
  :config
  (global-diff-hl-mode 1)
  (diff-hl-margin-mode))


;;
;; THEMES
;;

;; (load-theme 'dichromacy)
;; (load-theme 'manoj-dark)
;; (load-theme 'misterioso)
;; (load-theme 'tango-dark)
;; (load-theme 'tsdh-dark)
;; (load-theme 'wheatgrass)
;; (load-theme 'wombat)


;;
;; CUSTOMIZATION
;;

;; Disable menu bar
(menu-bar-mode -1) 

;; Hide buffer list at startup when loading multiple files
(setq inhibit-startup-buffer-menu t)

;; Open up the debugger on error
(setq debug-on-error t)

;; Use Command as Meta on macOS
;; (setq mac-command-modifier 'meta)

;; Delete trailing newline character with 'kill-line
(setq kill-whole-line t)

;; Highlight current line
(global-hl-line-mode 1)

;; Deactivate highlight mode when selecting text
(add-hook 'activate-mark-hook (lambda () (global-hl-line-mode -1)))
(add-hook 'deactivate-mark-hook (lambda () (global-hl-line-mode 1)))

;; Enable line numbers
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                shell-mode-hook
                eshell-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Use spaces, not tabs
(setq-default indent-tabs-mode nil)

;; Use TAB for symbol completion (after indentation)
(setq tab-always-indent 'complete)

;; Delete selection on edit
(delete-selection-mode 1)

;; Prefer horizontal (side-by-side) window splitting
;;   Note: the thresholds need to be twice as big as the smallest
;;   window allowed, because the new windows each use half of the
;;   former window size
(setq split-width-threshold 160
      split-height-threshold nil)


;;
;; MODES
;;

;; ediff: Use horizontal (side-by-side) view by default
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-horizontally)

;; ediff: When merging, use both variants A and B, one after the other
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference
                                               'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference
                                               'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map ()
  (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

;; dired: custom listing style
;; (setq dired-listing-switches "-agho --group-directories-first")
(setq dired-listing-switches "-agho")   ; macOS version

;; ispell: set aspell suggestion mode 
(setq ispell-extra-args '("--sug-mode=ultra"))
;; (setq ispell-extra-args '("--sug-mode=fast"))
;; (setq ispell-extra-args '("--sug-mode=normal"))

;; flyspell: 
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;;
;; KEY BINDINGS
;;

(define-key input-decode-map "\e[1;6A" [C-S-up])
(define-key input-decode-map "\e[1;6B" [C-S-down])
;; (define-key input-decode-map "\e[1;8A" [C-S-M-up])
;; (define-key input-decode-map "\e[1;8B" [C-S-M-down])

;; (define-key input-decode-map "\e[127;2u" [S-backspace])
(define-key input-decode-map "\e[127;5u" [C-backspace])
(define-key input-decode-map "\e[127;6u" [C-S-backspace])

;; Navigation

(windmove-default-keybindings 'meta)
(global-set-key (kbd "C-x 9") 'window-swap-states)

(global-set-key (kbd "M-p") 'scroll-up-line)
(global-set-key (kbd "M-n") 'scroll-down-line)

;; Selection

(global-set-key (kbd "C-c @") 'mark-word)
(global-set-key (kbd "M-#") 'my-mark-word)
(global-set-key (kbd "M-@") 'my-mark-word-backward)

(global-set-key (kbd "C-c l") 'my-mark-line)

;; (global-set-key (kbd "C-S-M-<down>") 'my/select-line)
;; (global-set-key (kbd "C-S-M-<up>") 'my/select-line-up)
(global-set-key (kbd "C-S-M-<down>") 'my-mark-line)
(global-set-key (kbd "C-S-M-<up>") 'my-mark-line-up)
(global-set-key (kbd "C-x 9") 'window-swap-states)

;; Editing

(global-set-key (kbd "C-<backspace>") (lambda () (interactive) (kill-line 0)))
(global-set-key (kbd "C-c k") (lambda () (interactive) (kill-line 0)))

(global-set-key (kbd "C-c ;") 'comment-line)

(global-set-key (kbd "M-y") 'my/counsel-yank-pop-or-yank-pop)
  ; won't be needed in Emacs 28

;; Files

(global-set-key (kbd "C-c f") 'find-file-at-point)

;; Version control

(global-set-key (kbd "C-c e") 'ediff-buffers) 
(global-set-key (kbd "C-c r") 'ediff-regions-linewise)
(global-set-key (kbd "C-c w") 'ediff-regions-wordwise) 
(global-set-key (kbd "C-x v -") 'vc-ediff) 

;; Unbind keys

;; (global-set-key (kbd "C-x") nil)
;; (global-unset-key (kbd "C-x"))   ; Alternative syntax


;;
;; FUNCTIONS 
;;

;; Mark whole word (forward)
(defun my-mark-word (N)
  (interactive "p")
  (when (and
         (not (eq last-command this-command))
         (not (eq last-command 'my-mark-word-backward)))
    (if (and (looking-at "[[:alnum:]]") (looking-back "[[:alnum:]]"))
        (backward-word))
    (set-mark (point)))
  (forward-word N))

;; Mark whole word (backward)
(defun my-mark-word-backward (N)
  (interactive "p")
  (when (and
         (not (eq last-command this-command))
         (not (eq last-command 'my-mark-word)))
    (if (and (looking-at "[[:alnum:]]") (looking-back "[[:alnum:]]"))
        (forward-word))
    (set-mark (point)))
  (backward-word N))

;; Mark line and enable to grow selection
(defun my/select-line ()
  "Select current line. If region is active, extend selection downward by line."
  (interactive)
  (if (region-active-p)
      (progn
        (end-of-line)
        (forward-char))
    (progn
      (end-of-line)
      (set-mark (line-beginning-position))
      (forward-char))))

;; Mark line and enable to grow selection (up)
(defun my/select-line-up ()
  "Select current line. If region is active, extend selection upward by line."
  (interactive)
  (if (region-active-p)
      (progn
        (forward-line -1)
        (beginning-of-line))
    (progn
      (beginning-of-line)
      (set-mark (line-beginning-position 2)))))

;; Mark whole line (down)
;;   (source: http://emacs.stackexchange.com/a/22166/93)
(defun my-mark-line ()
  (interactive)
  (if (not (use-region-p))
      (beginning-of-line))
  (setq this-command-keys-shift-translated t)
  (call-interactively 'end-of-line)
  (call-interactively 'forward-char))

;; Mark whole line (up)
(defun my-mark-line-up ()
  (interactive)
  (if (not (use-region-p))
      (forward-line))
  (setq this-command-keys-shift-translated t)
  (call-interactively 'previous-line))

;; Another alternative
;;   (source: https://emacs.stackexchange.com/questions/15033/how-to-mark-current-line-and-move-cursor-to-next-line
(defun my/select-current-line-and-forward-line (arg)
  "Select the current line and move the cursor by ARG lines IF
no region is selected.

If a region is already selected when calling this command, only move
the cursor by ARG lines."
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))

;; Custom counsel-yank-pop
(defun my/counsel-yank-pop-or-yank-pop (&optional arg)
  "Call `counsel-yank-pop'. If called after a yank, call `yank-pop' instead."
  (interactive "*p")
  (if (eq last-command 'yank)
      (yank-pop arg)
    (counsel-yank-pop)))


;;
;; LOCAL SETTINGS (LATE)
;;

(cond ((equal (system-name) "MacBook-Air.lan")
       ;; Load theme
       (setq base16-theme-256-color-source "base16-shell")
       (load-theme 'base16-flat t))

      (t 
       ;; Load theme
       (load-theme 'wombat)))

;; Conditionally load host specific stuff
;; (let ((host-specific-files (concat (make-load-path-base) system-name ".el")))
;;   (if (file-exists-p host-specific-files)
;;       (load host-specific-files)
;;     (message (concat "No host specific customizations for " system-name))
;;     ))


; LocalWords:  swiper magit ediff ispell
