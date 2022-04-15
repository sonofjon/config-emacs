;;;;; STARTUP

;; Check startup time
(defun efs/display-startup-time ()
  "Display startup time."
  (message "Emacs loaded in %s seconds with %d garbage collections."
           (format "%.2f"
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

;; Add path to local files
;; (add-to-list 'load-path "~/local/share/emacs/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/")


;;;;; EARLY SETTINGS

;; System dependent settings
(cond ((string-match-p "MacBook-Air" (system-name))
       ;; GUI settings
       (when (display-graphic-p)
         ;; Add to exec-path
         ;;   TODO: Use exec-path-from-shell package?
         (dolist (dir '("/usr/local/bin"
                        "/usr/local/opt/grep/libexec/gnubin"))
           (add-to-list 'exec-path dir)))
       (message "Early settings for MacOS"))

      ((or (equal (system-name) "brain5")
	   (equal (system-name) "brain9")
	   (equal (system-name) "brain10")
	   (equal (system-name) "endeavour-lxde")
	   (equal (system-name) "kde-neon")
	   (equal (system-name) "manjaro-xfce"))
       (message "Early settings Linux"))

      ((equal (system-name) "penguin")
       (message "Early settings ChromeOS"))

      ((or (equal (system-name) "brain5-windows")
	   (equal (system-name) "brain8-windows")
	   (equal (system-name) "NT175"))
       (message "Early settings WSL"))

      (t (user-error "Unexpected system-name: %s" system-name)))

;; Use custom-file.el for custom-* code
(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))

(if (file-exists-p custom-file)
    (load-file custom-file))


;;;;; FIXES

;; Suppress warnings
;;   Fix numerous byte-comp warnings
;; (setq warning-minimum-level :error)   ; only show errors
;; (setq warning-suppress-types :error)        ; log but do not show warnings
;; (add-to-list 'warning-suppress-types '(yasnippet backquote-change))


;;;;; PACKAGES

;;;; Setup

;; Initialize package sources
(require 'package)
(if (version< emacs-version "27")
    (package-initialize))

;; Add MELPA to package-archives
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Add MELPA-STABLE to package-archives
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Set package-archives priorities
(setq package-archive-priorities
      '(("gnu"          . 1)
        ("nongnu"       . 2)
        ;; ("melpa-stable" . 2)
        ("melpa"        . 3)))

;; Refresh packages database (in background)
;;   TODO: async argument fails with Chemacs
;; (unless package-archive-contents
;;   (package-refresh-contents t))

;; Refresh packages database (on first install)
(defun my/package-install-refresh-contents (&rest args)
  "Refresh package database on first install."
  (package-refresh-contents)
  (advice-remove 'package-install #'my/package-install-refresh-contents))

(advice-add 'package-install :before #'my/package-install-refresh-contents)

;;; use-package

;; Install use-package
(if (not (package-installed-p 'use-package))
    (package-install 'use-package))

;; Load use-package
(eval-when-compile
  (require 'use-package))

;; Always install packages if not present
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; For debugging startup
(setq use-package-verbose t)

;; Lower time threshold for package load time reporting
(setq use-package-minimum-reported-time 0.001)

;; Alternative option to prioritize archives
;; (setq use-package-always-pin "melpa-stable")

;;; Quelpa

;; Install quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; Don't update local clone of the MELPA git repo
;;   TODO: where should I put this?
(setq quelpa-checkout-melpa-p nil)
;; Set upgrade interval
;;   TODO: where should I put this?
(setq quelpa-upgrade-interval 7)
(add-hook #'after-init-hook #'quelpa-upgrade-all-maybe)

;; Install quelpa-use-package
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))

;; Load quelpa-use-package
;;   TODO: configure
(require 'quelpa-use-package)

;;;; Packages

;;; Early packages 

;; benchmark-init (startup profiler)
(use-package benchmark-init
  :disabled
  :config
  ;; Disable collection of benchmark data after init
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

;;; Package management

;; auto-package-update
(use-package auto-package-update
  :custom
  ;; Prompt before update
  (auto-package-update-prompt-before-update t)
  ;; Delete old versions
  (auto-package-update-delete-old-versions t)
  ;; Don't show update results
  ;; (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;;; Theme

;; base16-theme
;;   Available options: <https://belak.github.io/base16-emacs/>
(use-package base16-theme
  :disabled)
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
  :init (load-theme 'spacemacs-dark t))
  ;; :init (load-theme 'spacemacs-light t))

;; doom-themes
(use-package doom-themes
  :disabled
  :init (load-theme 'doom-one) t)
  ;; :init (load-theme 'doom-one-light) t)

;; modus-themes
(use-package modus-themes
  :bind ("<f5>" . modus-themes-toggle)
  ;; Add all customizations prior to loading the themes
  :init
  ;; Use italic font forms in more code constructs
  (setq modus-themes-italic-constructs t)
  ;; Use bold text in more code constructs
  (setq modus-themes-bold-constructs nil)
  ;; Use more subtle style for line numbers
  (setq modus-themes-subtle-line-numbers t)
  ;; Define the visibility of fringes
  ;;   Options: `nil',`subtle', `intense'
  (setq modus-themes-fringes nil)
  ;; Control the style of spelling and code checkers/linters
  ;;   Options: `straight-underline', `text-also', `background',
  ;;            `intense', `faint'
  (setq modus-themes-lang-checkers '(straight-underline text-also))
  ;; Control the style of the mode line
  ;;   Options: `3d' OR `moody', `borderless', `accented'
  (setq modus-themes-mode-line '(borderless))
  ;(setq modus-themes-mode-line nil)
  ;; Control the style of code syntax highlighting
  ;;   Options: `faint', `yellow-comments', `green-strings',
  ;;            `alt-syntax'
  (setq modus-themes-syntax '(faint green-strings alt-syntax))
  ;; Style markup in Org, markdown, and others
  ;;   Options: `bold', `italic', `background', `intense'
  ;; (setq modus-themes-markup '(background italic))
  (setq modus-themes-markup nil)
  ;; Control the current line highlight of HL-line mode
  ;;   Options: `accented', `underline', `intense'
  (setq modus-themes-hl-line nil)
  ;; Control the style of matching parentheses or delimiters
  ;;   Options: `bold', `intense', `underline'
  (setq modus-themes-paren-match '(intense))
  ;; Set the style of links
  ;;   Options: `neutral-underline' OR `no-underline', `faint' OR
  ;;            `no-color', `bold', `italic', `background'
  (setq modus-themes-links nil)
  ;; Control the style of buttons in the Custom UI and related
  ;;   Options: `flat', `accented', `faint', `variable-pitch',
  ;;            `underline'
  ;; (setq modus-themes-box-buttons '(variable-pitch flat faint 0.9))
  (setq modus-themes-box-buttons nil)
  ;; Set the style for minibuffer and REPL prompts
  ;;   Options: `background', `bold', `gray', `intense', `italic'
  (setq modus-themes-prompts nil)
  ;; Control the style of the completion framework's interface
  ;;   Options: see manual
  (setq modus-themes-completions '((matches . (extrabold))
                                   (selection . (semibold accented))
                                   (popup . (accented intense))))
  ;; (setq modus-themes-completions nil)
  ;; Control the style of the active region
  ;;   Options: `no-extend', `bg-only', `accented'
  (setq modus-themes-region nil)
  ;; Adjust the style of diffs
  ;;   Options: `desaturated', `bg-only'
  (setq modus-themes-diffs nil)
  ;; Set the style of Org code blocks, quotes, and the like
  ;;   Options: `gray-background', `tinted-background'
  (setq modus-themes-org-blocks 'gray-background)
  ;; Org styles
  ;;   Options: see manual
  ;; (setq modus-themes-org-agenda
  ;;       '((header-block . (variable-pitch 1.3))
  ;;         (mail-header-parse-date . (grayscale workaholic bold-today 1.1))
  ;;         (event . (accented varied))
  ;;         (scheduled . uniform)
  ;;         (habit . traffic-light)))
  (setq modus-themes-org-agenda nil)
  ;; Heading styles
  ;;   Options: `rainbow', `overline', `background', `monochrome'
  ;; (setq modus-themes-headings
  ;;       '((1 . (background monochrome))
  ;;         (t . (monochrome))))
  (setq modus-themes-headings
        '((1 . (background rainbow))
          (2 . (background))
          (3 . (background monochrome))
          (t . (monochrome))))
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes))
  ;; :config
  ;; Load the theme
  ;; (modus-themes-load-operandi))
  ;; (modus-themes-load-vivendi))

;; circadian (theme-switching based on daytime)
(use-package circadian
  :after modus-themes
  :custom
  ;; (circadian-themes '(("8:00" . modus-operandi)
  ;;                     ("18:00"  . modus-vivendi)))
  (circadian-themes '((:sunrise . modus-operandi)
                      (:sunset  . modus-vivendi)))
  :config
  (setq calendar-latitude 59.33
        calendar-longitude 18.07)
  (circadian-setup))


;;; Windows

;;; Buffers

;; auto-dim-other-buffers (dim inactive windows)
(use-package auto-dim-other-buffers
  :disabled
  :custom
  ;; Don't dim on switch to minibuffer
  (auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
                                        ; TODO: doesn't work
  :config
  ;; Define attributes for dimmed windows
  (set-face-attribute 'auto-dim-other-buffers-face nil :background "color-233")
  (set-face-attribute 'auto-dim-other-buffers-face nil :foreground "color-245")
  (auto-dim-other-buffers-mode 1))

;; dimmer (dim inactive windows)
(use-package dimmer
  ;; :disabled
  :custom
  ;; What to dim
  ;; (dimmer-adjustment-mode ':both)
  ;; Adjust dimming amount
  (dimmer-fraction 0.25)                ; default is 0.20
  (dimmer-use-colorspace :rgb)          ; for use with modus themes
  :config
  (dimmer-mode 1))

;;; Outline

;; outline-minor-faces (use faces from outline-mode)
(use-package outline-minor-faces
  :after outline
  :hook (outline-minor-mode . outline-minor-faces-add-font-lock-keywords))

;;; Navigation

;; syntax-subword (fine-grained navigation)
(use-package syntax-subword
  :disabled
  :custom
  ;; Don't stop on spaces
  (syntax-subword-skip-spaces t)
  :config
  ;; Use syntax-subword-mode everywhere
  (global-syntax-subword-mode 1))

;;; Search

;;; Selection

;; expand-region (grow selected region by semantic units)
(use-package expand-region
  :bind (("C-c =" . er/expand-region)
         ("C-c -" . er/contract-region)))

;;; Editing

;; dot-mode (repeat command vim-style)
(use-package dot-mode
  :hook ((prog-mode . dot-mode-on)
         (text-mode . dot-mode-on))
  :bind (:map dot-mode-map ("C-c d" . dot-mode-execute))
  :custom
  (dot-mode-verbose t)
  :config
  (unbind-key "C-." dot-mode-map)
  (unbind-key "C-c ." dot-mode-map)
  (unbind-key "C-M-." dot-mode-map))

;; rainbow-delimiters (parentheses coloring)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; multiple-cursors (edit at multiple points)
(use-package multiple-cursors
  :bind (:prefix-map multiple-cursors
                     :prefix "C-c m"
                     :prefix-docstring "Multiple Cursors related"
                     ("SPC" . set-rectangular-region-anchor)
                     ("<" . mc/mark-previous-like-this)
                     (">" . mc/mark-next-like-this)
                     ("?" . mc/mark-all-like-this)
                     ("e" . mc/edit-lines)))

;; whole-line-or-region (apply to current line if region is undefined)
;; (use-package whole-line-or-region
;;   :diminish
;;   ;; :bind
;;   ;; (:map whole-line-or-region-local-mode-map
;;   ;; TODO: function should not be quoted?
;;   ;; ([remap ispell-region] . #'whole-line-or-region-ispell-region))
;;   :config
;;   ;; Use whole-line-or-region-mode everywhere
;;   (whole-line-or-region-global-mode 1))

;; undo-fu (linear undo with redo)
;;   Note that undo-in-region is disabled by default
(use-package undo-fu
  :after which-key
  :bind (("C-c z u" . undo-fu-only-undo)
         ("C-c z r" . undo-fu-only-redo)
         ("C-c z d" . undo-fu-disable-checkpoint))
  :init
  (which-key-add-key-based-replacements "C-c z" "undo-fu")
  :custom
  (undo-fu-ignore-keyboard-quit t))

;;; Completion

;; icomplete-vertical (show icomplete candidates vertically)
(use-package icomplete-vertical
  :disabled
  ;; Load after startup (when my/completion-styles is defined)
  :hook (emacs-startup . (lambda () (icomplete-mode) (icomplete-vertical-mode)))
  :bind (:map icomplete-minibuffer-map
              ("<up>" . icomplete-backward-completions)
              ("<down>" . icomplete-forward-completions)
              ;; ("RET" . icomplete-force-complete)
              ("RET" . icomplete-force-complete-and-exit)
              ("C-v" . icomplete-vertical-toggle))
  :config
  (my/completion-styles))

;; vertico (vertical completion UI)
(use-package vertico
  ;; Load after startup (when my/completion-styles is defined)
  :hook (emacs-startup . vertico-mode)
  :bind (:map vertico-map
              ("?" . minibuffer-completion-help)
              ;; ("C-c ?" . aj8/kill-completions-buffer)
              ("C-c ?" . minibuffer-hide-completions)
              ("M-c" . minibuffer-complete)
              ("M-f" . minibuffer-force-complete))
  :custom
  ;; Enable cycling
  (vertico-cycle t)
  :config
  ;; Enable M-x minibuffer-hide-completions (make function interactive)
  (put 'minibuffer-hide-completions 'interactive-form '(interactive))
  (my/completion-styles))

;; corfu (completion overlay)
(use-package corfu
  ;; TODO: configure
  :hook (prog-mode . corfu-mode)
  ;; :custom
  ;; (corfu-count 10)               ; Maximal number of candidates to show
  ;; (corfu-min-width 15)           ; Popup minimum width in characters
  ;; (corfu-max-width 100)          ; Popup maximum width in characters."
  ;; (corfu-cycle t)                ; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ; Enable auto completion
  ;; (corfu-commit-predicate nil)   ; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ; Automatically quit at word boundary
  ;; (corfu-quit-no-match t)        ; Automatically quit if there is no match
  ;; (corfu-echo-documentation nil) ; Do not show documentation in the echo area
  ;; (corfu-auto-prefix 3)          ; Minimum length of prefix for auto completion."
  :init
  ;; Enable Corfu globally
  ;;   (this is recommended since dabbrev can be used globally)
  (corfu-global-mode))

;; orderless (orderless completion style)
(use-package orderless
  :after (:any icomplete-vertical vertico)
  :config
  ;; Use orderless for completion
  ;;   (disables styles set for icomplete-vertical and vertico)
  (setq completion-styles '(orderless))
  ;; Use orderless everywhere
  ;; (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles . (basic substring)))))
  ;; Matching styles
  ;; (setq orderless-matching-styles '(orderless-literal orderless-regexp))   ; default
  (setq orderless-matching-styles
        '(orderless-prefixes
          orderless-initialism)))

;; consult (practical commands based on Emacs completion)
(use-package consult
  :after which-key
  :bind (;; C-c bindings (mode-specific-map)
         ;; ("C-c c h" . consult-history)
         ("C-c c m" . consult-mode-command)
         ("C-c c M" . consult-minor-mode-menu)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)   ; orig. repeat-complex-command
         ("C-x b" . consult-buffer)              ; orig. switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)              ; orig. yank-pop
         ("<help> a" . consult-apropos)          ; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)           ; orig. goto-line
         ("M-g M-g" . consult-goto-line)         ; orig. goto-line
         ("M-g o" . consult-outline)             ; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)      ; TODO: narrowing to packages
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s R" . consult-recent-file)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)       ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)     ; orig. isearch-edit-string
         ("M-s l" . consult-line)                ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)          ; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)               ; orig. next-matching-history-element
         ("M-r" . consult-history))              ; orig. previous-matching-history-element
  :init
  ;; Enhance `completing-read-multiple'
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (which-key-add-key-based-replacements "C-c c" "consult")
  :config
  ;; Configure preview
  ;; (setq consult-preview-key (kbd "M-."))   ; default is 'any (TODO: does not work)
  ;; Configure preview (on a per-command basis)
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-bookmark
   consult-recent-file consult-xref consult--source-bookmark
   consult--source-recent-file consult--source-project-recent-file
   :preview-key (kbd "M-."))
  ;; Narrowing key
  (setq consult-narrow-key "<"))
  ;; Enable narrowing help in the minibuffer
  ;;   (you may want to use `embark-prefix-help-command' or which-key instead)
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  ;; Use consult for completion in region
  ;;   Note, this does not work with LSP-mode or eglot (use corfu instead)
  ;; (setq completion-in-region-function #'consult-completion-in-region)
  ;;   TODO: enable completion-in-region to start with!

;; consult-project-extra (project extension for consult)
(use-package consult-project-extra
  :bind ("C-c p" . consult-project-extra-find))

;;; Spelling

;; flyspell-correct (wrapper for flyspell with completion)
(use-package flyspell-correct
  ;; TODO: configure
  :after flyspell
  :bind (:map flyspell-mode-map
              ;; ("C-;" . flyspell-auto-correct-word)
              ("C-;" . flyspell-correct-wrapper)
              ("C-c ," . #'my/flyspell-goto-previous-error)
              ("C-," . #'flyspell-correct-previous)
              ("C-c ." . #'flyspell-goto-next-error)
              ("C-." . #'flyspell-correct-next)))

;;; Files

;; treemacs (a tree layout file explorer)
(use-package treemacs
  ;; :after which-key
  :defer t
  :init
  (which-key-add-key-based-replacements "C-c t" "treemacs")
  :config
  (progn
    ;; (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
    ;;       treemacs-deferred-git-apply-delay        0.5
    ;;       treemacs-directory-name-transformer      #'identity
    ;;       treemacs-display-in-side-window          t
    ;;       treemacs-eldoc-display                   'simple
    ;;       treemacs-file-event-delay                5000
    ;;       treemacs-file-extension-regex            treemacs-last-period-regex-value
    ;;       treemacs-file-follow-delay               0.2
    ;;       treemacs-file-name-transformer           #'identity
    ;;       treemacs-follow-after-init               t
    ;;       treemacs-expand-after-init               t
    ;;       treemacs-find-workspace-method           'find-for-file-or-pick-first
    ;;       treemacs-git-command-pipe                ""
    ;;       treemacs-goto-tag-strategy               'refetch-index
    ;;       treemacs-indentation                     2
    ;;       treemacs-indentation-string              " "
    ;;       treemacs-is-never-other-window           nil
    ;;       treemacs-max-git-entries                 5000
    ;;       treemacs-missing-project-action          'ask
    ;;       treemacs-move-forward-on-expand          nil
    ;;       treemacs-no-png-images                   nil
    ;;       treemacs-no-delete-other-windows         t
    ;;       treemacs-project-follow-cleanup          nil
    ;;       treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
    ;;       treemacs-position                        'left
    ;;       treemacs-read-string-input               'from-child-frame
    ;;       treemacs-recenter-distance               0.1
    ;;       treemacs-recenter-after-file-follow      nil
    ;;       treemacs-recenter-after-tag-follow       nil
    ;;       treemacs-recenter-after-project-jump     'always
    ;;       treemacs-recenter-after-project-expand   'on-distance
    ;;       treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
    ;;       treemacs-show-cursor                     nil
    ;;       treemacs-show-hidden-files               t
    ;;       treemacs-silent-filewatch                nil
    ;;       treemacs-silent-refresh                  nil
    ;;       treemacs-sorting                         'alphabetic-asc
    ;;       treemacs-select-when-already-in-treemacs 'move-back
    ;;       treemacs-space-between-root-nodes        t
    ;;       treemacs-tag-follow-cleanup              t
    ;;       treemacs-tag-follow-delay                1.5
    ;;       treemacs-text-scale                      nil
    ;;       treemacs-user-mode-line-format           nil
    ;;       treemacs-user-header-line-format         nil
    ;;       treemacs-wide-toggle-width               70
    ;;       treemacs-width                           35
    ;;       treemacs-width-increment                 1
    ;;       treemacs-width-is-initially-locked       t
    ;;       treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    ;; (treemacs-follow-mode t)
    ;; (treemacs-filewatch-mode t)
    ;; (treemacs-fringe-indicator-mode 'always)
    ;; (treemacs-hide-gitignored-files-mode nil)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))

  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-c t 1"   . treemacs-delete-other-windows)
        ("C-c t t"   . treemacs)
        ("C-c t d"   . treemacs-select-directory)
        ("C-c t B"   . treemacs-bookmark)
        ("C-c t C-t" . treemacs-find-file)
        ("C-c t M-t" . treemacs-find-tag)))

;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
;;   :ensure t)

;; (use-package treemacs-magit
;;   :after (treemacs magit)
;;   :ensure t)

;;; Coding

;; php-mode (major-mode for editing PHP files)
(use-package php-mode
  :mode ".php$")

;; web-mode (major-mode for editing web templates)
(use-package web-mode
    :mode (".html?$")
    :custom
    ;; Enable auto-functionality in the terminal
    ;;   Note: this can be bad when pasting text
    (web-mode-enable-auto-expanding t)
    (web-mode-enable-auto-closing t)
    (web-mode-enable-auto-indentation t)
    (web-mode-enable-auto-opening t)
    (web-mode-enable-auto-pairing t)
    (web-mode-enable-auto-quoting t)
    :config
    ;; Use continuation lines
    (setq truncate-lines nil))

;; lsp-mode (language server protocol)
(use-package lsp-mode
  :disabled
  :commands (lsp lsp-deferred)
  :hook ((sh-mode . lsp)   ; or lsp-deferred
         (html-mode . lsp)
         (css-mode . lsp)
         (js-mode . lsp)
         (html-mode . lsp)
         ;; which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  ;; Prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l"))

  ;; TODO: Add this
  ;; (which-key-add-key-based-replacements "C-c l" "LSP")

;; lsp-ui-mode (higher level UI modules for LSP)
;; (use-package lsp-ui
;;   :commands lsp-ui-mode)

;; lsp-treemacs: (treemacs integration)
;; (use-package lsp-treemacs
;;   :commands lsp-treemacs-errors-list)

;; i3wm-config-mode (syntax highlighting for i3 config files)
(use-package i3wm-config-mode)

;; rainbow-mode (syntax highlighting for color codes)
(use-package rainbow-mode)

;;; Version control

;; magit (user interface to git)
(use-package magit
  ;; Disable hl-line-mode
  :hook (magit-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :bind (("C-c g" . magit-file-dispatch)
         ;; Kill, not bury, magit buffers
         :map magit-mode-map
         ([remap magit-mode-bury-buffer] . aj8/magit-mode-bury-buffer))
         ;; Open files in other window
         ;; :map magit-file-section-map
         ;; ("RET" . magit-diff-visit-file-other-window)
         ;; Open hunks in other window
         ;; :map magit-hunk-section-map
         ;; ("RET" . magit-diff-visit-file-other-window))
  :custom
  ;; Show refined diffs for current hunk
  (magit-diff-refine-hunk t))

;; diff-hl (highlight uncommitted changes)
(use-package diff-hl
  :disabled
  :config
  ;; Use diff-hl-mode everywhere
  (global-diff-hl-mode 1))

;;; Help

;; marginalia (add marginalia to minibuffer completions)
(use-package marginalia
  :demand
  :after (:any icomplete-vertical vertico)
  :bind (:map minibuffer-local-map
              ("M-m" . marginalia-cycle))
  :config
  (marginalia-mode 1))

;; which-key (display available keybindings)
(use-package which-key
  :diminish
  :custom
  ;; Show which-key buffer on C-h
  ;; (which-key-show-early-on-C-h t)
  ;; Delay (default is 1.0 s)
  ;; (which-key-idle-delay 10000)
  (which-key-idle-delay 0.75)
  ;; Secondary delay (default is nil)
  (which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode 1))

;; helpful (alternative help)
(use-package helpful
  ;; :demand
  :commands (helpful-key helpful-function helpful-symbol
             helpful-variable helpful-command)
  :bind (([remap describe-key] . helpful-key)
         ([remap describe-function] . helpful-function)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ("C-c h" . helpful-at-point)
         :map helpful-mode-map
         ;; Kill buffers on quit
         ([remap quit-window] . aj8/quit-window))
  :custom
  ;; Maximum number of *helpful* buffers
  ;; (helpful-max-buffers 3)
  ;; Always open additional helpful buffers in the same window
  (helpful-switch-buffer-function #'aj8/helpful-switch-to-buffer))

;;; Web

;; google-this (google search functions)
(use-package google-this
  :diminish
  :after which-key
  :config
  (which-key-add-key-based-replacements "C-c /" "google-this")
  (google-this-mode 1))

;; erc (IRC client)
(use-package erc
  :commands (erc erc-tls)
  :custom
  ;; Server settings
  (erc-server "irc.libera.chat")
  (erc-nick "ajdev8")
  (erc-user-full-name "Andreas Jonsson")
  (erc-autojoin-channels-alist '(("libera.chat" "#systemcrafters")))
  ;; Buffers
  (erc-join-buffer 'bury)
  (erc-kill-buffer-on-part t)
  ;; Appearance
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 18)
  ;; (erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY"))
  ;; Tracking
  (erc-track-shorten-start 3)
  (erc-track-exclude '("#emacs"))
  (erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY"))
  (erc-track-exclude-server-buffer t)
  :config
  ;; (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules))

(use-package erc-hl-nicks
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks)
  (erc-update-modules))

;; elfeed (web feed reader)
(use-package elfeed
  :commands elfeed
  :bind (:map elfeed-search-mode-map
              ("g" . #'elfeed-search-fetch)
              ("G" . #'elfeed-search-update--force))
  :custom
  ;; Subscriptions
  (elfeed-feeds '("https://www.reddit.com/r/emacs/.rss"
                  "https://sachachua.com/blog/category/emacs-news/feed/"))
  ;; Download folder
  (elfeed-enclosure-default-dir "~/Downloads/")
  ;; Default filter
  ;; (elfeed-search-filter "@6-months-ago +unread")
  ;; Sort order
  ;; (elfeed-sort-order 'descending)
  ;; Use unique buffer names
  (elfeed-show-unique-buffers t)
  ;; Layout
  (elfeed-search-title-max-width 50)   ; default is 70
  (elfeed-search-title-min-width 16)    ; default is 16
  (elfeed-search-trailing-width 30))    ; default is 30

;;; Other

;; diminish (hide minor modes)
(use-package diminish
  :config
  ;; Pre-loaded modes
  (diminish 'eldoc-mode)
  ;; Not pre-loaded modes
  ;; (diminish 'which-key-mode)
  (with-eval-after-load "auto-revert-mode" (diminish 'auto-revert-mode)))
                                        ; TODO: doesn't work

;; hydra (stateful keymaps)
(use-package hydra
  :defer)

;; keyfreq (command stats)
(use-package keyfreq
  :init
  (setq keyfreq-excluded-commands '(self-insert-command
                                    right-char
                                    left-char
                                    previous-line
                                    next-line
                                    magit-previous-line
                                    magit-next-line
                                    vertico-previous
                                    vertico-next))
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


;;;;; THEMES

;; (load-theme 'dichromacy)
;; (load-theme 'manoj-dark)
;; (load-theme 'misterioso)
;; (load-theme 'tango-dark)
;; (load-theme 'tsdh-dark)
;; (load-theme 'wheatgrass)
;; (load-theme 'wombat)


;;;;; CUSTOMIZATION

;;;; Modes

;;; Package management...

;;; Theme...

;; Disable menu bar
(menu-bar-mode -1)

;; Disable toolbar (graphical Emacs)
(tool-bar-mode -1)

;; Disable scroll bar
(with-eval-after-load "scroll-bar"   ; avoid error on some systems
  (scroll-bar-mode -1))

;; Enable line numbers
;; (add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

;;; Windows...

;; Enable winner mode
(setq winner-dont-bind-my-keys t)
(winner-mode 1)

;;; Buffers...

;; Persistent minibuffer history
(savehist-mode 1)

;; Show recursion depth in the minibuffer prompt
(minibuffer-depth-indicate-mode 1)

;;; Outline...

;;; Navigation...

;; Subword movement and editing: camelCase
;;    Cannot be enabled at the same time as superword-mode
;; (add-hook 'prog-mode-hook (lambda () (subword-mode 1)))

;; Superword movement and editing: snake_case and kebab-case
;;    Cannot be enabled at the same time as subword-mode
;; (add-hook 'prog-mode-hook (lambda () (superword-mode 1)))

;;; Search...

;;; Selection...

;; Delete selection on edit
(delete-selection-mode 1)

;;; Editing...

;; Highlight current line
(global-hl-line-mode 1)

;;; Completion...

;;; Spelling...

;; On-the-fly spell checking
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;; Files...

;; Track recent files
(recentf-mode 1)

;;; Coding...

;;; Version control...

;;; Help...

;;; Web...

;;; Other...

;; Expand abbreviations
(add-hook 'text-mode-hook 'abbrev-mode)

;;;; Variables

;;; Package management

;;; Theme

;; Disable welcome buffer
(setq inhibit-startup-screen t)

;;; Windows

;; Prefer horizontal (side-by-side) window splitting
;;   Note: the thresholds need to be twice as big as the smallest
;;   window allowed, because the new windows each use half of the
;;   former window size
(setq split-width-threshold 160
      split-height-threshold nil)

;; Set display-line-number-width automatically
(setq display-line-numbers-width-start t)

;; Do not display continuation lines
(setq-default truncate-lines t)

;;; Buffers

;; Hide buffer list at startup when loading multiple files
(setq inhibit-startup-buffer-menu t)

;; Do not switch to buffers already shown
;; (setq switch-to-prev-buffer-skip 'this)

;; Allow minibuffer commands while in the minibuffer
(setq enable-recursive-minibuffers t)

;; Open *info* buffers in same window
(setq info-lookup-other-window-flag nil)

;; Reuse existing help window
;; (setq help-window-select t)

;;; Outline

;; Outline minor mode prefix
(setq outline-minor-mode-prefix "\C-c\C-c")
(which-key-add-key-based-replacements "C-c C-c" "outline")

;; Use TAB and S-TAB for cycling
(setq outline-minor-mode-cycle t)

;; Highlight headings
(setq outline-minor-mode-highlight t)

;;; Navigation

;; Scrolling
;;   The order of priority is: ‘scroll-conservatively’, then
;;   ‘scroll-step’, and finally ‘scroll-up-aggressively’ /
;;   ‘scroll-down-aggressively’.
;;   TODO: Configure
(setq scroll-conservatively 0)        ; default: 0
(setq scroll-step 1)                  ; default: 0
(setq scroll-up-aggressively nil)     ; default: nil
(setq scroll-down-aggressively nil)   ; default: nil
;; (setq scroll-margin 0)

;; Preserve point position when scrolling
(setq scroll-preserve-screen-position t)

;;; Search

;; Don't search invisible text by default
(setq isearch-invisible nil)

;; Disable lax-whitespace searching by default
(setq isearch-lax-whitespace nil)

;; Interpret spaces as wildcards (with M-s SPC)
;;   TODO: Remove when adding a replacement for swiper?
(setq search-whitespace-regexp ".*?")

;;; Selection

;;; Editing

;; Use spaces, not tabs
(setq-default indent-tabs-mode nil)

;; Delete trailing newline character with 'kill-line
(setq kill-whole-line t)

;; Don't use the mark when region is inactive
;;   Note: messes with ediff
;; (setq mark-even-if-inactive nil)

;; Save clipboard text into kill ring before kill
;; (setq save-interprogram-paste-before-kill t)

;;; Completion

;; Use TAB for symbol completion (after indentation)
(setq tab-always-indent 'complete)

;;; Spelling

;; Set language
(setq ispell-dictionary "en")

;; Set aspell suggestion mode
(setq ispell-extra-args '("--sug-mode=ultra"))
;; (setq ispell-extra-args '("--sug-mode=fast"))
;; (setq ispell-extra-args '("--sug-mode=normal"))

;; Enable flyspell in web-mode
(put 'web-mode 'flyspell-mode-predicate #'my/web-mode-flyspell-verify)

;;; Files

;; Custom listing style in dired
;; (setq dired-listing-switches "-agho --group-directories-first")
(setq dired-listing-switches "-agho")   ; macOS version

;;; Coding

;;; Version control

;; Use horizontal (side-by-side) view by default in ediff
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-merge-split-window-function #'split-window-horizontally)

;;; Help

;;; Web

;; URL browser settings
;;   TODO: There is also browse-url-default-windows|macosx-browser
(setq browse-url-browser-function #'eww-browse-url)
(setq browse-url-secondary-browser-function #'browse-url-default-browser)
(setq browse-url-chrome-program "com.google.Chrome") ; TODO: doesn't exist
(setq browse-url-handlers
      '(("reddit\\.com" . browse-url-chrome)
        ;; ("google\\.com" . browse-url-default-browser)
        ("github\\.com" . browse-url-default-browser)
        ("gitlab\\.com" . browse-url-default-browser)
        ("stackexchange\\.com" . browse-url-default-browser)
        ("stackoverflow\\.com" . browse-url-default-browser)
        ;; ("sachachua\\.com" . browse-url-default-browser)
        ("." . eww-browse-url)))

;; Default search engine
(setq eww-search-prefix "https://google.com/search?q=")   ; default is duckduckgo
;; Restore eww buffers
(setq eww-restore-desktop t)
;; Don't remove duplicates in browing history
;; (setq eww-desktop-remove-duplicates nil)
;; Download folder
(setq eww-download-directory (expand-file-name "~/Downloads"))
;; Max history items
(setq eww-history-limit 100)
;; Don't shadow default eww keybindings
(with-eval-after-load "shr"
  (define-key shr-map (kbd "u") nil)
  (define-key shr-map (kbd "v") nil)
  (define-key shr-map (kbd "w") nil))
;; Open new eww buffers in a new window (M-RET)
(with-eval-after-load "eww"
  (define-key eww-mode-map
    [remap eww-open-in-new-buffer] 'aj8/eww-open-in-new-buffer))
;; Open new eww buffers in a new window (C-u RET)
(with-eval-after-load "eww"
  (define-key eww-mode-map
    [remap eww-follow-link] 'aj8/eww-follow-link))

;; Are these needed?
;; (setq shr-use-colors nil)             ; t is bad for accessibility
;; (setq shr-use-fonts nil)

;;; Other

;; Use 'y' or 'n' questions always
;; (setq use-short-answers t)

;; Open up the debugger on error
;; (setq debug-on-error t)

;; Use left Option as Meta on macOS
;; (setq mac-option-modifier 'meta)

;; Use left Command as Super on macOS
;; (setq mac-command-modifier 'super)

;; Use longer pulse
(setq pulse-delay 0.05)   ; default is 0.03

;; Use speed keys in org-mode
(setq org-use-speed-commands t)

;; Remove "..?*" from alias `all' in grep-files-aliases
(with-eval-after-load "grep"
  (setf (alist-get "all" grep-files-aliases nil nil #'equal) "* .[!.]*"))


;;;;; HOOKS

;; emacs-startup: delay evaluation until functions are defined
;;
;; More convenient "jump to mark" command (swap prefix argument)
;; (add-hook 'emacs-startup-hook
;;           (lambda () (my/toggle-prefix-arg #'set-mark-command)))


;; outline-mode: remove form-feed character (^L) from regexp
(add-hook 'outline-mode-hook
          (lambda ()
            (setq outline-regexp "[*]+")))

;; emacs-lisp-mode:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Set custom outline heading format
            (setq-local outline-regexp "\\(;;+ \\)\\([^( ]\\)")
            (setq outline-heading-alist
                  '((";;;;; " . 1)
                    (";;;; " . 2)
                    (";;; " . 3)
                    (";; " . 4)))
            ;; Don't use 'lisp-outline-level (doesn't use outline-heading-alist)
            (setq-local outline-level 'aj8/outline-level)))

;; conf-xdefaults-mode:
(add-hook 'conf-xdefaults-mode-hook
          (lambda ()
            ;; Set custom outline heading format
            (setq-local outline-regexp "\\(!!+ \\)\\([^( ]\\)")
            (setq outline-heading-alist
                  '(("!!!!! " . 1)
                    ("!!!! " . 2)
                    ("!!! " . 3)
                    ("!! " . 4)))
            ;; Don't use 'lisp-outline-level (doesn't use outline-heading-alist)
            (setq-local outline-level 'aj8/outline-level)))

;; i3wm-config-mode:
(add-hook 'i3wm-config-mode-hook
          (lambda ()
            ;; Set custom outline heading format
            (setq-local outline-regexp "\\(##+ \\)\\([^( ]\\)")
            (setq outline-heading-alist
                  '(("##### " . 1)
                    ("#### " . 2)
                    ("### " . 3)
                    ("## " . 4)))
            ;; Don't use 'lisp-outline-level (doesn't use outline-heading-alist)
            (setq-local outline-level 'aj8/outline-level)))

;; activate-mark: deactivate highlight mode when selecting text
(add-hook 'activate-mark-hook (lambda () (global-hl-line-mode -1)))
(add-hook 'deactivate-mark-hook (lambda () (global-hl-line-mode 1)))

;; Info-mode: allow multiple Info buffers
(add-hook 'Info-mode-hook #'rename-uniquely)

;; kill-buffer: collect list of killed buffers
(add-hook 'kill-buffer-hook #'reopen-killed-file--add-to-list)

;; help-mode: kill buffers on quit
(define-key help-mode-map [remap quit-window] 'aj8/quit-window)

;; Info-mode: kill buffers on quit
(with-eval-after-load "info"
  (define-key Info-mode-map [remap quit-window] 'aj8/quit-window))

;; dired-mode: kill buffers on quit
(with-eval-after-load "dired"
  (define-key dired-mode-map [remap quit-window] 'aj8/quit-window))

;; eww-mode: kill buffers on quit
(with-eval-after-load "eww"
  (define-key eww-mode-map [remap quit-window] 'aj8/quit-window))


;;;;; KEYBINDINGS

;;;; Escape codes

;; (define-key input-decode-map "\e[1;8A" [C-M-S-up])
;; (define-key input-decode-map "\e[1;8B" [C-M-S-down])

;; (define-key input-decode-map "\e[127;2u" [S-backspace])
(define-key input-decode-map "\e[127;5u" [C-backspace])
(define-key input-decode-map "\e[127;6u" [C-S-backspace])
(define-key input-decode-map "\e[107;6u" (kbd "C-S-k"))

;;;; Translations

(define-key key-translation-map (kbd "M-<up>") (kbd "M-p"))
(define-key key-translation-map (kbd "M-<down>") (kbd "M-n"))
(define-key key-translation-map (kbd "C-M-<up>") (kbd "C-M-p"))
(define-key key-translation-map (kbd "C-M-<down>") (kbd "C-M-n"))

;;;; Global
;;; Package management

;;; Theme

;;; Windows

(global-set-key (kbd "C-x 9") #'my/toggle-window-split)

(define-key winner-mode-map (kbd "C-c <") #'winner-undo)
(define-key winner-mode-map (kbd "C-c >") #'winner-redo)

;;; Buffers

(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "C-c k") #'my/kill-buffer-other-window)

;;; Outline

(global-set-key (kbd "C-c o") #'outline-minor-mode)

;;; Navigation

(windmove-default-keybindings 'ctrl)
(windmove-swap-states-default-keybindings '(ctrl shift))
(global-set-key (kbd "C-c w <up>") #'windmove-display-up)
(global-set-key (kbd "C-c w <down>") #'windmove-display-down)
(global-set-key (kbd "C-c w <left>") #'windmove-display-left)
(global-set-key (kbd "C-c w <right>") #'windmove-display-right)
(global-set-key (kbd "C-c w 0") #'windmove-display-same-window)
(which-key-add-key-based-replacements "C-c w" "windmove")

(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)

;; (global-set-key (kbd "C-c <up>") 'aj8/previous-line)
;; (global-set-key (kbd "C-c <down>") 'aj8/next-line)

(global-set-key (kbd "C-c x l") #'scroll-lock-mode)
(which-key-add-key-based-replacements "C-c x" "misc")

(global-set-key (kbd "C-c i") #'imenu)

;; (global-unset-key (kbd "C-M-<up>"))
;; (global-unset-key (kbd "C-M-<down>"))
;; (global-unset-key (kbd "C-M-<left>"))
;; (global-unset-key (kbd "C-M-<right>"))
;; (global-set-key (kbd "C-M-<left>") #'backward-up-list) ; overwrites default 'backward-sexp
;; (global-set-key (kbd "C-M-<right>") #'down-list) ; overwrites default 'backward-sexp
(global-set-key (kbd "C-M-<left>") #'backward-list) ; overwrites default 'backward-sexp
(global-set-key (kbd "C-M-<right>") #'forward-list) ; overwrites default 'backward-sexp
(global-set-key (kbd "C-M-p") #'backward-up-list)   ; overwrites default 'backward-list
(global-set-key (kbd "C-M-n") #'down-list)          ; overwrites default 'forward-list

;;; Search

;;; Selection

(global-set-key (kbd "M-#") #'aj8/mark-word-forward)
(global-set-key (kbd "M-@") #'aj8/mark-word-backward)

;;; Editing

(global-set-key (kbd "C-S-k") (lambda () (interactive) (kill-line 0)))

(global-set-key (kbd "C-c ;") #'comment-line)

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(global-set-key (kbd "C-c q") 'fill-individual-paragraphs) ; use with `C-x .'
                                                           ; for comments

(global-set-key (kbd "C-c s") 'my/copy-symbol-at-point)

;;; Completion

;;; Spelling

;;; Files

(global-set-key (kbd "C-c f") #'find-file-at-point)

;;; Coding

;;; Version control

(global-set-key (kbd "C-c e b") #'ediff-buffers)
(global-set-key (kbd "C-c e l") #'ediff-regions-linewise)
(global-set-key (kbd "C-c e w") #'ediff-regions-wordwise)
(global-set-key (kbd "C-x v -") #'vc-ediff)
(which-key-add-key-based-replacements "C-c e" "ediff")

;;; Help

(which-key-add-key-based-replacements "C-c H" "help")
(global-set-key (kbd "C-c H k") #'describe-keymap)
(global-set-key (kbd "C-c H d") #'shortdoc-display-group)

;;; Web

(global-set-key (kbd "C-c b") #'browse-url)

;;; Other

(global-set-key (kbd "C-c r") #'reload-init-file)

;; Mac-like bindings
;; (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
;; (global-set-key (kbd "s-m") 'iconify-frame)
;; (global-set-key (kbd "s-n") 'make-frame-command)
;; (global-set-key (kbd "s-s") 'save-buffer)
;; (global-set-key (kbd "s-a") 'mark-whole-buffer)
;; (global-set-key (kbd "s-z") 'undo)
;; (global-set-key (kbd "s-x") 'kill-region)
;; (global-set-key (kbd "s-c") 'kill-ring-save)
;; (global-set-key (kbd "s-v") 'yank)

;;; Unbind keys

;; (global-set-key (kbd "C-x") nil)
;; (global-unset-key (kbd "C-x"))   ; Alternative syntax

;;;; Local

;; Info-mode-map
(add-hook 'Info-mode-hook
          ;; Disable M-n
          (lambda () (define-key Info-mode-map (kbd "M-n") nil)))

;; ediff-mode-map
;;   TODO: check functionality
(add-hook 'ediff-keymap-setup-hook
          (lambda () (define-key ediff-mode-map "d" #'my/ediff-copy-both-to-C)))

;; outline-mode
(add-hook 'outline-mode-hook
          ;; Define custom keybindings
          (lambda () (aj8/outline-mode-keys outline-mode-map)))

;; outline-minor-mode
(add-hook 'outline-minor-mode-hook
          ;; Define custom keybindings
          (lambda () (aj8/outline-mode-keys outline-minor-mode-map)))

;;;; Hydras

(which-key-add-key-based-replacements "C-c y" "hydra")

;; Windows
(defhydra hydra-window (:hint nil)
  "
                                                                   ╭─────────┐
  Select  Move   Resize    Split           Do                      │ Windows │
╭──────────────────────────────────────────────────────────────────┴─────────┴─┐
  ^ ^ _↑_ ^ ^   ^ ^ _↑_ ^ ^   ^ ^ _↑_ ^ ^   ╭─┬─┐ ^ ^           ╭─┬─┐ ^ ^
  _←_ ^ ^ _→_   _←_ ^C^ _→_   _←_ ^M^ _→_   │ │ │[_v_]ertical   ├─┼─┤[_b_]alance  ↺ [_u_] undo layout 
  ^ ^ _↓_ ^ ^   ^ ^ _↓_ ^ ^   ^ ^ _↓_ ^ ^   ╰─┴─╯ ^ ^           ╰─┴─╯ ^ ^         ↻ [_r_] reset layout
  ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ╭───┐ ^ ^           ╭───┐ ^ ^         ✗ [_d_] close window
  ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ├───┤[_h_]orizontal │   │[_z_]oom     ⇋ [_w_] cycle window
  ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ╰───╯ ^ ^           ╰───╯ ^ ^
╰──────────────────────────────────────────────────────────────────────────────╯
"
  ;; ("<tab>" hydra-master/body "back")
  ("q" nil "quit")
  ("<left>" windmove-left)
  ("<right>" windmove-right)
  ("<up>" windmove-up)
  ("<down>" windmove-down)
  ("C-<left>" windmove-swap-states-left)
  ("C-<right>" windmove-swap-states-right)
  ("C-<up>" windmove-swap-states-up)
  ("C-<down>" windmove-swap-states-down)
  ;; ("S-<left>" shrink-window-horizontally)
  ;; ("S-<right>" enlarge-window-horizontally)
  ;; ("S-<up>" shrink-window)
  ;; ("S-<down>" enlarge-window)
  ("M-<left>" hydra-move-splitter-left)
  ("M-<right>" hydra-move-splitter-right)
  ;; ("M-<up>" hydra-move-splitter-up)
  ("M-p" hydra-move-splitter-up)
  ;; ("M-<down>" hydra-move-splitter-down)
  ("M-n" hydra-move-splitter-down)
  ("v" split-window-vertically)
  ("h" split-window-horizontally)
  ("b" balance-windows)
  ("z" delete-other-windows)
  ("u" winner-undo)
  ("r" winner-redo)
  ("d" delete-window)
  ("w" other-window))

(global-set-key (kbd "C-c y w") #'hydra-window/body)

;;; Scrolling
(defhydra hydra-scroll (:hint nil)
  "
^Line^            ^Paragraph^
^^^^---------------------------
_↑_: previous     _←_: previous
_↓_: next         _→_: next

Scroll by line or paragraph.
"
  ("<up>" scroll-down-line)
  ("<down>" scroll-up-line)
  ("<left>" aj8/scroll-down-paragraph)
  ("<right>" aj8/scroll-up-paragraph))

(global-set-key (kbd "C-c y s") #'hydra-scroll/body)

;;; Line navigation
(defhydra hydra-navigation (:hint nil)
  "
^Line^            ^Comment^
^^^^---------------------------
_↑_: previous     _←_: previous
_↓_: next         _→_: next

Move to the next line or comment.
"
  ("<up>" aj8/previous-line)
  ("<down>" aj8/next-line)
  ("<left>" aj8/previous-comment)
  ("<right>" aj8/next-comment))

(global-set-key (kbd "C-c y n") #'hydra-navigation/body)

;;; Outline
(defhydra hydra-outline (:color pink :hint nil)
  "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree

"
  ;; Hide
  ("q" outline-hide-sublevels)   ; Hide everything but the top-level headings
  ("t" outline-hide-body)        ; Hide everything but headings (all body lines)
  ("o" outline-hide-other)       ; Hide other branches
  ("c" outline-hide-entry)       ; Hide this entry's body
  ("l" outline-hide-leaves)      ; Hide body lines in this entry and sub-entries
  ("d" outline-hide-subtree)     ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" outline-show-all)         ; Show (expand) everything
  ("e" outline-show-entry)       ; Show this heading's body
  ("i" outline-show-children)    ; Show this heading's immediate child sub-headings
  ("k" outline-show-branches)    ; Show all sub-headings under this heading
  ("s" outline-show-subtree)     ; Show (expand) everything in this heading & below
  ;; Move
  ("u" outline-up-heading)                ; Up
  ("n" outline-next-visible-heading)      ; Next
  ("p" outline-previous-visible-heading)  ; Previous
  ("f" outline-forward-same-level)        ; Forward - same level
  ("b" outline-backward-same-level)       ; Backward - same level
  ("z" nil "leave"))

(global-set-key (kbd "C-c y o") #'hydra-outline/body)


;;;;; FUNCTIONS

;;;; Package management

;;;; Theme

;;;; Windows

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

;; Kill buffer in other window
(defun my/kill-buffer-other-window ()
  "If there are multiple windows, then kill the buffer in the next window."
  (interactive)
  (unless (one-window-p)
    (other-window 1)
    (kill-buffer)
    (other-window -1)))

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

;;;; Buffers

;; Kill *Completions* buffer
;; (defun aj8/kill-completions-buffer ()
;;   "Get rid of *Completions* buffer."
;;   (interactive)
;;   (let ((buffer "*Completions*"))
;;     (and (get-buffer buffer)
;;          (kill-buffer buffer))))

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

;; Make a *scratch* buffer
(defun create-scratch-buffer nil
       "Create a scratch buffer."
       (interactive)
       (switch-to-buffer (get-buffer-create "*scratch*"))
       (lisp-interaction-mode))

;;;; Outline

;; Set keybindings for outline-(minor-)mode
(defun aj8/outline-mode-keys (map)
  "Set keybindings for outline-mode or outline-minor-mode.
MAP should either be `outline-mode-map' or `outline-minor-mode-map'."
  (let ((m map))
    (define-key m (kbd "C-c <left>") #'my/outline-hide-more)
    (define-key m (kbd "C-c <right>") #'my/outline-show-more)
    (define-key m (kbd "C-c C-<left>") #'outline-hide-sublevels)
    (define-key m (kbd "C-c M-<left>") #'outline-hide-body)
    (define-key m (kbd "C-c C-<right>") #'outline-show-all)
    (define-key m (kbd "C-c <up>") #'outline-previous-visible-heading)
    (define-key m (kbd "C-c <down>") #'outline-next-visible-heading)
    (define-key m (kbd "C-c C-<up>") #'outline-backward-same-level)
    (define-key m (kbd "C-c C-<down>") #'outline-forward-same-level)
    (define-key m (kbd "C-c M-p") #'outline-up-heading)))

;; Check if there is a body?
(defun outline--body-p ()
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (and (not (eobp))
         (progn (forward-char 1)
                (not (outline-on-heading-p))))))

;; Check if there is a visible body
(defun outline--body-visible-p ()
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (not (outline-invisible-p))))

;; Check if there is a sub-heading
(defun outline--subheadings-p ()
  (save-excursion
    (outline-back-to-heading)
    (let ((level (funcall outline-level)))
      (outline-next-heading)
      (and (not (eobp))
           (< level (funcall outline-level))))))

;; Check if there is a visible sub-heading
(defun outline--subheadings-visible-p ()
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

;;;; Search

;;;; Selection

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

(defun my/copy-symbol-at-point ()
  "Make symbol at point the latest kill in the kill ring."
  (interactive)
  (let ((symbol (thing-at-point 'symbol))
        (bounds (bounds-of-thing-at-point 'symbol)))
    (when symbol
      (kill-new symbol)
      (pulse-momentary-highlight-region (car bounds) (cdr bounds)))))

;;;; Completion

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

;; ispell-region stub for whole-line-or-region package
(defun whole-line-or-region-ispell-region (prefix)
  "Call `ispell-region' on region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-wrap-modified-region #'ispell-region prefix))

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

;; flyspell setup for web-mode
(defun my-web-mode-flyspell-verify ()
  "Fly Spell predicate of `web-mode`."
  (let* ((font-face-at-point (get-text-property (- (point) 1) 'face))
         rlt)
    ;; If rlt is t, the word at point is POSSIBLY a typo, continue checking.
    (setq rlt t)
    ;; if rlt is nil, the word at point is definitely NOT a typo.
    ;; (setq rlt nil)
    rlt))

;; flyspell setup for web-mode (with blacklist)
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

;;;; Files

;;;; Coding

;;;; Version control

;; ediff: enable concatenation
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

;; helpful: always open additional helpful buffers in the same window
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
   (t ; all other cases, prompt
    (error "Unexpected input arguments"))))

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

;; Get current URL in eww
(defun prot-eww--get-current-url ()
  "Return the current-page's URL."
  (cond ((eq major-mode 'eww-mode)
         (plist-get eww-data :url))
        (t (user-error "Not a eww or elpher buffer"))))

;; Add completion to eww
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

;; Swap universal prefix argument for functions
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

;; Helper function for rxvt-input-decode-map
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

;; Reload init-file
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))


;;;;; LATE SETTINGS

;; System dependent settings
(cond ((string-match-p "MacBook-Air" (system-name))
       ;; GUI settings
       (when (display-graphic-p)
         ;; Set default font
         (add-to-list 'default-frame-alist '(font . "Hack-14"))
         ;; Increase line spacing
         (setq-default line-spacing 1))
       (message "Late settings MacOS"))

      ((or (equal (system-name) "brain5")
	   (equal (system-name) "brain9")
	   (equal (system-name) "brain10")
	   (equal (system-name) "endeavour-lxde")
	   (equal (system-name) "kde-neon")
	   (equal (system-name) "manjaro-xfce"))
       (message "Late settings Linux"))

      ((equal (system-name) "penguin")
       (message "Late settings ChromeOS"))

      ((or (equal (system-name) "brain5-windows")
	   (equal (system-name) "brain8-windows")
	   (equal (system-name) "NT175"))
       ;; Enable (default) web browser on WSL
       (setq browse-url-generic-program "wslview")
       (setq browse-url-secondary-browser-function #'browse-url-generic)
       (advice-add #'browse-url-default-browser :override #'browse-url-generic)
       (message "Late settings WSL"))

      (t (user-error "Unexpected system-name: %s" system-name)))

;; Conditionally load host specific stuff
;; (let ((host-specific-files (concat (make-load-path-base) system-name ".el")))
;;   (if (file-exists-p host-specific-files)
;;       (load host-specific-files)
;;     (message (concat "No host specific customizations for " system-name))
;;     ))

;; Special settings for URxvt
(when (equal "rxvt-unicode-256color"
             (getenv-internal "TERM" initial-environment))
  (rxvt-input-decode-map))


; LocalWords:  ediff flyspell isearch ispell magit minibuffer modus TODO
