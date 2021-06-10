;;;
;;; STARTUP
;;;

;; Check startup time
(defun efs/display-startup-time ()
  "Display startup time."
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


;;;
;;; LOCAL SETTINGS (EARLY)
;;;

(cond ((equal (system-name) "MacBook-Air.lan")
       ;; Use custom-file.el for custom-* code (Chemacs setup)
       (setq custom-file "~/.emacs.default/custom-file.el")
       ;; GUI settings
       (when (display-graphic-p)
         ;; Add to exec-path
         ;;   TODO: Use exec-path-from-shell package?
         (dolist (dir '("/usr/local/bin"
                        "/usr/local/opt/grep/libexec/gnubin"))
           (add-to-list 'exec-path dir))))

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


;;;
;;; FIXES
;;;

;; Suppress warnings about unnecessary package-initialize calls
;;   Introduced when switching to Melpa Stable
;;   Possible solution: use some packages from Melpa
;;   Reference: https://github.com/cask/cask/issues/463
;; (setq warning-suppress-log-types '((package reinitialization)))


;;;
;;; PACKAGES SETUP
;;;

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
(defun my/package-install-refresh-contents (&rest args)
  "Refresh package database on first install."
  (package-refresh-contents)
  (advice-remove 'package-install #'my/package-install-refresh-contents))

(advice-add 'package-install :before #'my/package-install-refresh-contents)

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


;;;
;;; PACKAGES
;;;

;; benchmark-init (startup profiler)
(use-package benchmark-init
  ;; :disabled
  :config
  ;; Disable collection of benchmark data after init
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

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
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode))

;; paradox (improved package menu)
(use-package paradox
  :config
  ;; Disable *Paradox Report* buffer
  (remove-hook 'paradox--report-buffer-print
               'paradox-after-execute-functions)
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

(use-package modus-themes
  ;; Add all customizations prior to loading the themes
  :init
  ;; Use more subtle style for line numbers
  (setq modus-themes-subtle-line-numbers nil)
  ;; Color-code 'success' or 'done' as blue instead of green
  (setq modus-themes-success-deuteranopia nil)
  ;; Control the style of spelling and code checkers/linters
  ;;   Options for `modus-themes-lang-checkers': nil,
  ;;   'straight-underline, 'subtle-foreground,
  ;;   'subtle-foreground-straight-underline, 'intense-foreground,
  ;;   'intense-foreground-straight-underline, 'colored-background
  (setq modus-themes-lang-checkers nil)
  ;; Adjust the overall style of the mode line
  ;;   Options for `modus-themes-mode-line': nil, '3d, 'moody,
  ;;   'borderless, 'borderless-3d, 'borderless-moody, 'accented,
  ;;   'accented-3d, 'accented-moody, 'borderless-accented,
  ;;   'borderless-accented-3d, 'borderless-accented-moody
  ;; modus-themes-mode-line '3d
  (setq modus-themes-mode-line nil)
  ;; Control the overall style of code syntax highlighting
  ;;   Options for `modus-themes-syntax': nil, 'faint,
  ;;   'yellow-comments, 'green-strings,
  ;;   'yellow-comments-green-strings, 'alt-syntax,
  ;;   'alt-syntax-yellow-comments, 'faint-yellow-comments
  (setq modus-themes-syntax nil)
  ;; Control the current line highlight of HL-line mode
  ;;   Options for `modus-themes-hl-line': nil, 'intense-background,
  ;;   'accented-background, 'underline-neutral, 'underline-accented,
  ;;   'underline-only-neutral, 'underline-only-accented
  ;; modus-themes-hl-line 'underline-neutral
  (setq modus-themes-hl-line nil)
  ;; Choose the style of matching parentheses or delimiters
  ;;   Options for `modus-themes-hl-line': nil, 'subtle-bold,
  ;;   'intense, 'intense-bold}
  ;; modus-themes-paren-match 'subtle-bold
  (setq modus-themes-paren-match nil)
  ;; Set the style of links
  ;;   Options for `modus-themes-links': nil, 'faint,
  ;;   'neutral-underline, 'faint-neutral-underline, 'no-underline,
  ;;   'underline-only, 'neutral-underline-only
  ;; modus-themes-links 'neutral-underline
  (setq modus-themes-links nil)
  ;; Set style for minibuffer and REPL prompts
  ;;   Options for `modus-themes-prompts': nil, 'subtle-accented,
  ;;   'intense-accented, 'subtle-gray, 'intense-gray
  ;; modus-themes-prompts 'subtle-gray
  (setq modus-themes-prompts nil)
  ;; Control the style of the completion framework's interface
  ;;   Options for `modus-themes-completions': nil, 'moderate,
  ;;   'opinionated
  ;; modus-themes-completions 'moderate
  (setq modus-themes-completions nil)
  ;; Change the appearance of the active region
  ;;   Options for `modus-themes-region': nil, 'no-extend, 'bg-only,
  ;;   'bg-only-no-extend, 'accent, 'accent-no-extend
  (setq modus-themes-region nil)
  ;; Adjust the style of diffs
  ;;   Options for `modus-themes-diffs': nil, 'desaturated, 'bg-only,
  ;;   'deuteranopia, 'fg-only-deuteranopia
  ;; modus-themes-diffs 'fg-only-deuteranopia)
  (setq modus-themes-diffs nil)
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme
  ;; (modus-themes-load-operandi)
  (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

;; company (in-buffer text completion)
(use-package company
  :disabled
  ;; :diminish
  :hook (prog-mode . company-mode)
  ;; :bind ("TAB" . company-complete)
  :config
  ;; Provide instant autocompletion (default is 0.5 s)
  ;; (setq company-idle-delay 0.0)
  ;; Minimum prefix for completion
  ;; (setq company-minimum-prefix-length 3)
  ;; Maximum number of candidates
  ;; (setq company-tooltip-limit 10)
  ;; Use company mode everywhere
  (global-company-mode 1))

(use-package company-web
  :disabled
  :after (:all company web-mode)
  :config (add-to-list 'company-backends 'company-web))

(use-package company-php
  :disabled
  ;; :after (:all company php-mode)
  :after (:all company (:any php-mode web-mode))
  :config (add-to-list 'company-backends 'company-php))

;; ivy (generic completion mechanism)
(use-package ivy
  :disabled
  :diminish
  :config
  (ivy-mode 1))

;; counsel (ivy-enhanced versions of common Emacs commands)
(use-package counsel
  :disabled
  :diminish
  :after ivy
  :bind (("C-c SPC" . counsel-mark-ring))
  :custom
  ;; Don't start searches with ^
  (ivy-initial-inputs-alist nil)
  :config
  (counsel-mode 1))

;; swiper (ivy-enhanced isearch)
(use-package swiper
  :disabled
  :after ivy
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("C-c ." . swiper-isearch-thing-at-point)
         ("C-c s" . swiper)
         ("C-c S" . swiper-all)
         :map swiper-isearch-map
         ("C-r" . ivy-previous-line-or-history)   ; mimic standard isearch
         ("M-n" . ivy-next-history-element)))   ; search history navigation

;; ivy-rich (add descriptions to ivy/counsel output)
(use-package ivy-rich
  :disabled
  :pin melpa
  :after ivy
  :config
  (ivy-rich-mode 1))

;; helpful (alternative help)
(use-package helpful
  ;; :disabled
  ;; :after counsel
  :commands (helpful-key helpful-function helpful-symbol helpful-variable)
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ("C-c h" . 'helpful-at-point)
  ;; Open helpful info manuals in the same window
  :hook (helpful-mode . (lambda ()
                          (setq-local info-lookup-other-window-flag nil)))
  :custom
  ;; Maximum number of *helpful* buffers
  (helpful-max-buffers 3))
  ;; Use helpful with counsel
  ;; (counsel-describe-function-function #'helpful-function)
  ;; (counsel-describe-symbol-function #'helpful-symbol)
  ;; (counsel-describe-variable-function #'helpful-variable))

;; prescient (base package)
(use-package prescient
  :disabled
  :after (:any ivy company)
  :custom
  ;; Disable sorting by length
  (prescient-sort-length-enable nil)
  :config
  ;; Remember across sessions
  (prescient-persist-mode 1))

;; ivy-prescient (sort candidates by frequency and recency)
(use-package ivy-prescient
  :disabled
  :after (:all ivy prescient)
  :custom
  ;; Disable prescient filtering (use ivy filtering)
  (ivy-prescient-enable-filtering nil)
  ;; Use ivy highlighting
  (ivy-prescient-retain-classic-highlighting t)
  :config
  (ivy-prescient-mode 1))

;; company-prescient (sort candidates by frequency and recency)
(use-package company-prescient
  :disabled
  :after (:all company prescient)
  :config
  (company-prescient-mode 1))

;; vertico (vertical completion UI)
(use-package vertico
  ;; :diminish
  ;; TODO: Change shortcuts
  :bind (:map vertico-map
         ;; ("?" . minibuffer-completion-help)
         ("M-RET" . minibuffer-force-complete-and-exit)
         ("M-TAB" . minibuffer-complete))
  :custom
  ;; Enable cycling
  (vertico-cycle t)
  :config
  (vertico-mode 1))

;; orderless (orderless completion style)
(use-package orderless
  :config
  (setq
   ;; Use orderless for completion
   ;; completion-styles '(orderless)
   ;; Enable prefix completion
   completion-styles '(substring orderless)
   ;; More options (TODO)
   ;; completion-styles '(basic substring partial-completion flex)
   ;; Use orderless everywhere
   completion-category-defaults nil
   ;; Enable `partial-completion' for files to allow path expansion
   completion-category-overrides '((file (styles . (partial-completion))))))

;; savehist (persistent minibuffer history)
(use-package savehist
  :config
  (savehist-mode 1))

;; which-key (display available keybindings)
(use-package which-key
  :diminish
  ;; :defer
  :custom
  ;; Delay (default is 1.0 s)
  (which-key-idle-delay 0.75)
  :config
  (which-key-mode 1))

;; magit (user interface to git)
(use-package magit
  ;; Disable hl-line-mode
  :hook (magit-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :bind (("C-x g" . magit-status)
         ;; ("C-x M-g" . magit-dispatch)   ; default binding
	 ("C-c g" . magit-file-dispatch)))

;; rainbow-delimiters (parentheses coloring)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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
;;   :diminish
;;   ;; :bind
;;   ;; (:map whole-line-or-region-local-mode-map
;;   ;; TODO: function should not be quoted?
;;   ;; ([remap ispell-region] . #'whole-line-or-region-ispell-region))
;;   :config
;;   (whole-line-or-region-global-mode 1))

;; web-mode (major-mode for editing web templates)
(use-package web-mode
  :pin melpa
  :mode (".html?$" ".php$"))

;; google-this (google search functions)
(use-package google-this
  :diminish
  ;; :bind ("C-c g" . google-this-mode-submap)
  :config
  (google-this-mode 1))

;; diff-hl (highlight uncommitted changes)
(use-package diff-hl
  :disabled
  :custom
  (diff-hl-margin-mode t)
  :config
  (global-diff-hl-mode 1))

;; syntax-subword (fine-grained navigation)
(use-package syntax-subword
  :disabled
  :custom
  ;; Don't stop on spaces
  (syntax-subword-skip-spaces t)
  :config
  ;; Use syntax-subword-mode everywhere
  (global-syntax-subword-mode 1))

(use-package buffer-move
  :bind (("C-c b" . buf-move)))
         ;; ("C-S-M-<up>" . buf-move-up)
         ;; ("C-S-M-<down>" . buf-move-down)
         ;; ("C-S-M-<left>" . buf-move-left)
         ;; ("C-S-M-<right>" . buf-move-right)))


;;;
;;; THEMES
;;;

;; (load-theme 'dichromacy)
;; (load-theme 'manoj-dark)
;; (load-theme 'misterioso)
;; (load-theme 'tango-dark)
;; (load-theme 'tsdh-dark)
;; (load-theme 'wheatgrass)
;; (load-theme 'wombat)


;;;
;;; CUSTOMIZATION
;;;

;; Modes

;; Disable menu bar
(menu-bar-mode -1)

;; Disable toolbar (graphical Emacs)
(tool-bar-mode -1)

;; Mouse scrolling
;;   TODO: shift-scrolling does not work
;; (setq mouse-wheel-progressive-speed nil)   ; don't accelerate scrolling

;; Delete selection on edit
(delete-selection-mode 1)

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

;; Variables

;; Disable scroll bar (graphical Emacs)
(setq scroll-bar-mode nil)

;; Scrolling
;;   The order of priority is: ‘scroll-conservatively’, then
;;   ‘scroll-step’, and finally ‘scroll-up-aggressively’ /
;;   ‘scroll-down-aggressively’.
(setq scroll-conservatively 0)        ; default: 0
(setq scroll-step 1)                  ; default: 0
(setq scroll-up-aggressively nil)     ; default: nil
(setq scroll-down-aggressively nil)   ; default: nil
;; (setq scroll-margin 0)

;; Preserve point position when scrolling
;;   TODO: difference between 't' and "any value"?
(setq scroll-preserve-screen-position t)

;; Prefer horizontal (side-by-side) window splitting
;;   Note: the thresholds need to be twice as big as the smallest
;;   window allowed, because the new windows each use half of the
;;   former window size
(setq split-width-threshold 160
      split-height-threshold nil)

;; Use spaces, not tabs
(setq-default indent-tabs-mode nil)

;; Hide buffer list at startup when loading multiple files
(setq inhibit-startup-buffer-menu t)

;; Do not switch to buffers already shown
(setq switch-to-prev-buffer-skip 'this)

;; Open up the debugger on error
(setq debug-on-error t)

;; Use Command as Meta on macOS
;; (setq mac-command-modifier 'meta)

;; Delete trailing newline character with 'kill-line
(setq kill-whole-line t)

;; Use TAB for symbol completion (after indentation)
(setq tab-always-indent 'complete)

;; Open *info* buffers in same window
;; (setq info-lookup-other-window-flag nil)

;; Always select the help window
;; (setq help-window-select t)

;; Remove "..?*" from 'all' alias in grep-files-aliases
;;   TODO: Lisp error: (void-variable grep-files-aliases)
;; (setf (alist-get "all" grep-files-aliases nil nil #'equal) "* .[!.]*")

;; Mode variables

;; dired: custom listing style
;; (setq dired-listing-switches "-agho --group-directories-first")
(setq dired-listing-switches "-agho")   ; macOS version

;; ediff: use horizontal (side-by-side) view by default
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-merge-split-window-function #'split-window-horizontally)

;; helpful: always open additional helpful buffers in the same window
(setq helpful-switch-buffer-function #'my/helpful-switch-to-buffer)

;; isearch: interpret spaces as wildcards (with M-s SPC)
(setq search-whitespace-regexp ".*?")

;; ispell: set aspell suggestion mode
(setq ispell-extra-args '("--sug-mode=ultra"))
;; (setq ispell-extra-args '("--sug-mode=fast"))
;; (setq ispell-extra-args '("--sug-mode=normal"))

;; erc (server)
(setq erc-server "irc.libera.chat"
      erc-nick "ajdev8"
      ;; erc-user-full-name "Andreas Jonsson"
      erc-autojoin-channels-alist '(("irc.libera.chat"
                                     "#systemcrafters"
                                     "#emacs"))
      erc-kill-buffer-on-part t)

;; erc (appearance)
(setq erc-fill-function 'erc-fill-static
      erc-fill-static-center 18
      erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY"))

;; erc (tracking)
(setq erc-track-shorten-start 3
      erc-track-exclude '("#emacs")
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY")
      erc-track-exclude-server-buffer t)


;;;
;;; HOOKS
;;;

;; abbrev-mode
(add-hook 'text-mode-hook 'abbrev-mode)

;; Ediff
(add-hook 'ediff-keymap-setup-hook #'add-d-to-ediff-mode-map)

;; flyspell:
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; subword: camelCase
;;    Cannot be enabled at the same time as superword-mode
;; (add-hook 'prog-mode-hook (lambda () (subword-mode 1)))

;; superword: snake_case and kebab-case
;;    Cannot be enabled at the same time as subword-mode
;; (add-hook 'prog-mode-hook (lambda () (superword-mode 1)))


;;;
;;; KEY BINDINGS
;;;

(define-key input-decode-map "\e[1;6A" [C-S-up])
(define-key input-decode-map "\e[1;6B" [C-S-down])
;; (define-key input-decode-map "\e[1;8A" [C-S-M-up])
;; (define-key input-decode-map "\e[1;8B" [C-S-M-down])

;; (define-key input-decode-map "\e[127;2u" [S-backspace])
(define-key input-decode-map "\e[127;5u" [C-backspace])
(define-key input-decode-map "\e[127;6u" [C-S-backspace])

;; Windows

(global-set-key (kbd "C-x M-9") #'my/toggle-window-split)
(global-set-key (kbd "C-x 9") #'window-swap-states)

;; Buffers

(global-set-key (kbd "C-c k") #'my/kill-buffer-other-window)

;; Navigation

(windmove-default-keybindings 'meta)

(global-set-key (kbd "M-p") #'scroll-up-line)
(global-set-key (kbd "M-n") #'scroll-down-line)

(global-set-key (kbd "C-c <down>") 'my/next-line)
(global-set-key (kbd "C-c <up>") 'my/previous-line)

;; Selection

(global-set-key (kbd "C-c @") #'mark-word)
(global-set-key (kbd "M-#") #'my/mark-word)
(global-set-key (kbd "M-@") #'my/mark-word-backward)

(global-set-key (kbd "C-S-M-<down>") #'my/mark-line)
(global-set-key (kbd "C-S-M-<up>") #'my/mark-line-up)

;; Editing

(global-set-key (kbd "C-<backspace>") (lambda () (interactive) (kill-line 0)))

(global-set-key (kbd "C-c ;") #'comment-line)

;; (global-set-key (kbd "M-y") #'my/counsel-yank-pop-or-yank-pop)
  ; won't be needed in Emacs 28

;; Remap flyspell-mode-map keys
(add-hook 'flyspell-mode-hook (lambda ()
  ;; C-, : Go to previous error
  (define-key flyspell-mode-map (kbd "C-,") #'flyspell-goto-previous-error)
  ;; C-. : Go to next error
  (define-key flyspell-mode-map (kbd "C-.") #'flyspell-goto-next-error)
  ;; C-; : Auto correct current word
  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-auto-correct-word)))

;; Files

(global-set-key (kbd "C-c f") #'find-file-at-point)

;; Version control

(global-set-key (kbd "C-c e") #'ediff-buffers)
(global-set-key (kbd "C-c r") #'ediff-regions-linewise)
(global-set-key (kbd "C-c w") #'ediff-regions-wordwise)
(global-set-key (kbd "C-x v -") #'vc-ediff)

;; Unbind keys

;; (global-set-key (kbd "C-x") nil)
;; (global-unset-key (kbd "C-x"))   ; Alternative syntax


;;;
;;; FUNCTIONS
;;;

(defun my/next-line ()
  ;; TODO: Add functionality for empty lines
  "Move to the next line that is not empty and not a comment."
  (interactive)
  (next-line)
  ;; (save-excursion
  ;;   (beginning-of-line)
  ;; (if (string-match-p "^[[:space:]]*\\s<" (thing-at-point 'line))
  (if (string-match-p "/(^[[:space:]]*\\s<" (thing-at-point 'line))
  ;; (if (looking-at-p "^-*?\\s<")
  ;; (if (looking-at "^[[:space:]]*;")
  ;; (if (looking-at-p comment-start)
      (my/next-line))
  (if (nth 4 (syntax-ppss))
           (my/next-line)))

(defun my/previous-line ()
  "Move to the previous line that is not empty and not a comment."
  (interactive)
  (previous-line)
  ;; (save-excursion
  ;;   (beginning-of-line)
  ;; (if (string-match-p "^[[:space:]]*\\s<" (thing-at-point 'line))
  (if (string-match-p "/(^[[:space:]]*\\s<" (thing-at-point 'line))
  ;; (if (looking-at-p "^-*?\\s<")
  ;; (if (looking-at "^[[:space:]]*;")
  ;; (if (looking-at-p comment-start)
      (my/previous-line))
  (if (nth 4 (syntax-ppss))
           (my/previous-line)))

      ;; Toggle window split
(defun my/toggle-window-split ()
  "If the window is split vertically, split it horizontally or vice versa."
  (interactive)
  (unless (= (count-windows) 2)
    (error "Can only toggle a window split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window)   ; close current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically))   ; makes a split with the other window twice
    (switch-to-buffer nil)))   ; restore the original window
                               ; in this part of the window

;; Kill buffer in other window
(defun my/kill-buffer-other-window ()
  "If there are multiple windows, then kill the buffer in the next window."
  (interactive)
  (unless (one-window-p)
    (other-window 1)
    (kill-buffer)
    (other-window -1)))

;; Mark whole word (forward)
(defun my/mark-word (N)
  "Like mark-word, but select entire word at point."
  (interactive "p")
  (when (and
         (not (eq last-command this-command))
         (not (eq last-command #'my/mark-word-backward)))
    (if (and (looking-at "[[:alnum:]]") (looking-back "[[:alnum:]]"))
        (backward-word))
    (set-mark (point)))
  (forward-word N))

;; Mark whole word (backward)
(defun my/mark-word-backward (N)
  "Like mark-word, but select entire word at point. 
Repeat command to select additional words backwards."
  (interactive "p")
  (when (and
         (not (eq last-command this-command))
         (not (eq last-command #'my/mark-word)))
    (if (and (looking-at "[[:alnum:]]") (looking-back "[[:alnum:]]"))
        (forward-word))
    (set-mark (point)))
  (backward-word N))

;; Mark whole line (down)
;;   (source: http://emacs.stackexchange.com/a/22166/93)
(defun my/mark-line ()
  "Select current line. 
If region is active, extend selection downward by line."
  (interactive)
  (if (not (region-active-p))
      (beginning-of-line))
  (setq this-command-keys-shift-translated t)
  (call-interactively #'end-of-line)
  (call-interactively #'forward-char))

;; Mark whole line (up)
(defun my/mark-line-up ()
  "Select current line. 
If region is active, extend selection upward by line."
  (interactive)
  (if (not (region-active-p))
      (forward-line))
  (setq this-command-keys-shift-translated t)
  (call-interactively #'previous-line)
  (call-interactively #'beginning-of-line))

;; Custom counsel-yank-pop
;; (defun my/counsel-yank-pop-or-yank-pop (&optional arg)
;;   "Call `counsel-yank-pop'. 
;; If called after a yank, call `yank-pop' instead."
;;   (interactive "*p")
;;   (if (eq last-command #'yank)
;;       (yank-pop arg)
;;     (counsel-yank-pop)))

;; ispell-region stub for whole-line-or-region package
(defun whole-line-or-region-ispell-region (prefix)
  "Call `ispell-region' on region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-wrap-modified-region #'ispell-region prefix))

;; ediff: when merging, use both variants A and B, one after the other
(defun ediff-copy-both-to-C ()
  "Add both variants to merge file."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference
                                               'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference
                                               'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map ()
  "Define keybinding for `ediff-copy-both-to-C'."
  (define-key ediff-mode-map "d" #'ediff-copy-both-to-C))

;; helpful: always open additional helpful buffers in the same window
(defun my/helpful-switch-to-buffer (buffer-or-name)
  "Switch to helpful BUFFER-OR-NAME.  
If we are currently in the helpful buffer, reuse it's window,
otherwise create a new one."
  (if (eq major-mode 'helpful-mode)
      (switch-to-buffer buffer-or-name)
    (pop-to-buffer buffer-or-name)))

;; Goto previous flyspell error
(defun flyspell-goto-previous-error (arg)
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

;; (defun check-previous-spelling-error ()
;;   "Jump to previous spelling error and correct it."
;;   (interactive)
;;   (push-mark-no-activate)
;;   (flyspell-goto-previous-error 1)
;;   (call-interactively 'helm-flyspell-correct))

;; (defun check-next-spelling-error ()
;;   "Jump to next spelling error and correct it."
;;   (interactive)
;;   (push-mark-no-activate)
;;   (flyspell-goto-next-error)
;;   (call-interactively 'helm-flyspell-correct))

;; (defun push-mark-no-activate ()
;;   "Push `point' to `mark-ring' and do not activate the region.
;; Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled."
;;   (interactive)
;;   (push-mark (point) t nil)
;;   (message "Pushed mark to ring"))


;;;
;;; LOCAL SETTINGS (LATE)
;;;

(cond ((equal (system-name) "MacBook-Air.lan")
       (if (display-graphic-p)
           ;; GUI settings
           ;; Set default font
           (add-to-list 'default-frame-alist '(font . "Hack-14"))
         ;; Terminal settings
         ;; Load theme
         (setq base16-theme-256-color-source "base16-shell")
         (load-theme 'base16-flat t)))

      ((equal (system-name) "NT175")
       ;; Enable (default) web browser on WSL
       (setq browse-url-generic-program "wslview"
        browse-url-browser-function #'browse-url-generic)
       ;; Load theme
       (load-theme 'wombat))

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
