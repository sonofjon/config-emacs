;;;;; LOCAL SETTINGS (EARLY)

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

      ;; ((equal (system-name) "penguin")

      (t
       ;; Use custom-file.el for custom-* code
       (setq custom-file "~/.emacs.d/custom-file.el")))


;;;;; STARTUP

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

;; Load custom file
(load-file custom-file)


;;;;; FIXES


;;;;; PACKAGES SETUP

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
;; (setq package-archive-priorities
;;       '(("gnu"          . 3)
;;         ("melpa-stable" . 2)
;;         ("melpa"        . 1)))

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


;;;;; PACKAGES

;; benchmark-init (startup profiler)
;;   TODO: Fix bug in Emacs 28
;; (use-package benchmark-init
;;   ;; :disabled
;;   :config
;;   ;; Disable collection of benchmark data after init
;;   (add-hook 'after-init-hook #'benchmark-init/deactivate))

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
  :defer
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
  :init (load-theme 'spacemacs-dark t))

;; doom-themes
(use-package doom-themes
  :disabled
  ;; :init (load-theme 'doom-one) t)
  ;; :init (load-theme 'doom-vibrant) t)
  ;; :init (load-theme 'doom-snazzy) t)
  :init (load-theme 'doom-Iosvkem) t)

;; modus-themes
(use-package modus-themes
  ;; Add all customizations prior to loading the themes
  :init
  ;; Use italic font forms in more code constructs
  (setq modus-themes-italic-constructs nil)
  ;; Use bold text in more code constructs
  (setq modus-themes-bold-constructs nil)
  ;; Use more subtle style for line numbers
  (setq modus-themes-subtle-line-numbers nil)
  ;; Define the visibility of fringes
  ;;   Options: `nil',`subtle', `intense'
  (setq modus-themes-fringes nil)
  ;; Control the style of spelling and code checkers/linters
  ;;   Options: `straight-underline', `text-also', `background',
  ;;            `intense'
  (setq modus-themes-lang-checkers nil)
  ;; Control the style of the mode line
  ;;   Options: `3d' OR `moody', `borderless', `accented', `padded'
  (setq modus-themes-mode-line nil)
  ;; Control the style of code syntax highlighting
  ;;   Options: `faint', `yellow-comments', `green-strings',
  ;;            `alt-syntax'
  (setq modus-themes-syntax nil)
  ;; Control the current line highlight of HL-line mode
  ;;   Options: `accented', `underline', `intense'
  ;; (setq modus-themes-paren-match 'subtle-bold)
  (setq modus-themes-hl-line nil)
  ;; Control the style of matching parentheses or delimiters
  ;;   Options: `bold', `intense', `underline'
  (setq modus-themes-paren-match nil)
  ;; Set the style of links
  ;;   Options: `neutral-underline' OR `no-underline', `faint' OR
  ;;            `no-color', `bold', `italic', `background'
  (setq modus-themes-links nil)
  ;; Set the style for minibuffer and REPL prompts
  ;;   Options: `background', `bold', `gray', `intense', `italic'
  (setq modus-themes-prompts nil)
  ;; Control the style of the completion framework's interface
  ;;   Options: `moderate',`opinionated'
  ;; (setq modus-themes-completions 'moderate)
  (setq modus-themes-completions nil)
  ;; Control the style of the active region
  ;;   Options: `no-extend', `bg-only', `accented'
  (setq modus-themes-region nil)
  ;; Adjust the style of diffs
  ;;   Options: `desaturated', `bg-only', `deuteranopia',
  ;;            `fg-only-deuteranopia'
  ;; (setq modus-themes-diffs 'fg-only-deuteranopia)
  (setq modus-themes-diffs nil)
  ;; Heading styles
  ;;   Options: `rainbow', `overline', `background', `no-bold',
  ;;            `monochrome'
  ;; (setq modus-themes-headings
  ;;       '((1 . (overline background))
  ;;         (2 . (rainbow overline))
  ;;         (t . (no-bold))))
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
  :custom
  ;; Provide instant autocompletion (default is 0.5 s)
  ;; (company-idle-delay 0.0)
  ;; Minimum prefix for completion
  ;; (company-minimum-prefix-length 3)
  ;; Maximum number of candidates
  ;; (company-tooltip-limit 10)
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
  :after ivy
  :config
  (ivy-rich-mode 1))

;; helpful (alternative help)
(use-package helpful
  ;; :demand
  :commands (helpful-key helpful-function helpful-symbol helpful-variable)
  ;; Open helpful info manuals in the same window
  :hook (helpful-mode . (lambda ()
                          (setq-local info-lookup-other-window-flag nil)))
  :bind (([remap describe-key] . helpful-key)
         ([remap describe-function] . helpful-function)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         ("C-c h" . helpful-at-point)
         :map helpful-mode-map
         ("q" . aj8/quit-window))
  :custom
  ;; Maximum number of *helpful* buffers
  ;; (helpful-max-buffers 3)
  ;; Always open additional helpful buffers in the same window
  (helpful-switch-buffer-function #'my/helpful-switch-to-buffer))
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
  (setq orderless-matching-styles
        ;; '(orderless-literal orderless-regexp))) ; default
        '(orderless-literal
          ;; orderless-regexp
          orderless-prefixes
          orderless-strict-full-initialism)))

;; consult (practical commands based on Emacs completion)
(use-package consult
  ;; TODO: configure
  :bind (
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
  ;;        ;; Other custom bindings
  ;;        ("M-y" . consult-yank-pop)                ;; orig. yank-pop
  ;;        ("<help> a" . consult-apropos)            ;; orig. apropos-command
  ;;        ;; M-g bindings (goto-map)

  ;;        ("M-g g" . consult-goto-line)             ;; orig. goto-line
  ;;        ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
  ;;        ("M-g m" . consult-mark)
  ;;        ("M-g k" . consult-global-mark)
  ;;        ("M-g i" . consult-imenu)
  ;;        ("M-g I" . consult-project-imenu)

  ;;        ;; M-s bindings (search-map)
  ;;        ("M-s f" . consult-find)
  ;;        ("M-s L" . consult-locate)
  ;;        ("M-s g" . consult-grep)
  ;;        ("M-s G" . consult-git-grep)
  ;;        ("M-s r" . consult-ripgrep)
  ;;        ("M-s l" . consult-line)
  ;;        ("M-s m" . consult-multi-occur)
  ;;        ("M-s k" . consult-keep-lines)
  ;;        ("M-s u" . consult-focus-lines)
  ;;        ;; Isearch integration
  ;;        ("M-s e" . consult-isearch)

  ;;        :map isearch-mode-map
  ;;        ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
  ;;        ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
  ;;        ("M-s l" . consult-line))                 ;; needed by consult-line to detect isearch
         )

  ;; Enable automatic preview at point in the *Completions* buffer
  ;;   This is relevant when you use the default completion UI,
  ;;   and not necessary for Selectrum, Vertico etc.
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)

  :config
  ;; Live preview (auto)
  ;; (setq consult-preview-key 'any)   ; auto (default)
  ;; (setq consult-preview-key (kbd "M-."))   ; manual
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; Live preview (manual)
  ;; (consult-customize
  ;;  consult-theme
  ;;  :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-file consult--source-project-file consult--source-bookmark
  ;;  :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  ;; (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  ;; (setq consult-project-root-function
  ;;       (lambda ()
  ;;         (when-let (project (project-current))
  ;;           (car (project-roots project)))))
  ;; ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

;; marginalia (add marginalia to minibuffer completions)
(use-package marginalia
  :demand
  :after (:any icomplete-vertical vertico)
  :bind (:map minibuffer-local-map
              ("M-m" . marginalia-cycle))
  :config
  (marginalia-mode 1))

;; savehist (persistent minibuffer history)
(use-package savehist
  :config
  (savehist-mode 1))

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

;; hydra (stateful keymaps)
(use-package hydra
  :defer)

;; magit (user interface to git)
(use-package magit
  ;; Disable hl-line-mode
  :hook (magit-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :bind (("C-c g" . magit-file-dispatch)
         ;; Kill, not bury, magit buffers
         :map magit-mode-map
         ("q" . aj8/magit-mode-bury-buffer))
         ;; Open files in other window
         ;; :map magit-file-section-map
         ;; ("RET" . magit-diff-visit-file-other-window)
         ;; Open hunks in other window
         ;; :map magit-hunk-section-map
         ;; ("RET" . magit-diff-visit-file-other-window))
  :custom
  ;; Show refined diffs for current hunk
  (magit-diff-refine-hunk t))

;; rainbow-delimiters (parentheses coloring)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; expand-region (grow selected region by semantic units)
(use-package expand-region
  :bind (("C-c =" . er/expand-region)
         ("C-c -" . er/contract-region)))

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

;; web-mode (major-mode for editing web templates)
(use-package web-mode
  :mode (".html?$" ".php$"))

;; google-this (google search functions)
(use-package google-this
  :diminish
  :after which-key
  :config
  (which-key-add-key-based-replacements "C-c /" "google-this")
  (google-this-mode 1))

;; diff-hl (highlight uncommitted changes)
(use-package diff-hl
  :disabled
  :config
  ;; Use diff-hl-mode everywhere
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

;; buffer-move (move buffers around)
(use-package buffer-move
  :disabled
  :bind (("C-c b" . buf-move)))
         ;; ("s-<up>" . buf-move-up)
         ;; ("s-<down>" . buf-move-down)
         ;; ("s-<left>" . buf-move-left)
         ;; ("s-<right>" . buf-move-right)))

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

;; Disable menu bar
(menu-bar-mode -1)

;; Disable toolbar (graphical Emacs)
(tool-bar-mode -1)

;; Delete selection on edit
(delete-selection-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Show recursion depth in the minibuffer prompt
(minibuffer-depth-indicate-mode 1)

;; Enable line numbers
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

;; Expand abbreviations
(add-hook 'text-mode-hook 'abbrev-mode)

;; On-the-fly spell checking
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Subword movement and editing: camelCase
;;    Cannot be enabled at the same time as superword-mode
;; (add-hook 'prog-mode-hook (lambda () (subword-mode 1)))

;; Superword movement and editing: snake_case and kebab-case
;;    Cannot be enabled at the same time as subword-mode
;; (add-hook 'prog-mode-hook (lambda () (superword-mode 1)))

;;;; Variables

;;; Windows

;; Prefer horizontal (side-by-side) window splitting
;;   Note: the thresholds need to be twice as big as the smallest
;;   window allowed, because the new windows each use half of the
;;   former window size
(setq split-width-threshold 160
      split-height-threshold nil)

;; Set display-line-number-width automatically
(setq display-line-numbers-width-start t)

;;; Buffers

;; Hide buffer list at startup when loading multiple files
(setq inhibit-startup-buffer-menu t)

;; Do not switch to buffers already shown
;; (setq switch-to-prev-buffer-skip 'this)

;; Allow minibuffer commands while in the minibuffer
(setq enable-recursive-minibuffers t)

;; Open *info* buffers in same window
;; (setq info-lookup-other-window-flag nil)

;; Reuse existing help window
;; (setq help-window-select t)

;;; Outline

;; Outline minor mode prefix
;;   (must be changed before outline-minor-mode is loaded)
(defvar outline-minor-mode-prefix "\C-c \C-c")

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

;;; Completion

;; Use TAB for symbol completion (after indentation)
(setq tab-always-indent 'complete)

;;; Spelling

;; Set aspell suggestion mode
(setq ispell-extra-args '("--sug-mode=ultra"))
;; (setq ispell-extra-args '("--sug-mode=fast"))
;; (setq ispell-extra-args '("--sug-mode=normal"))

;;; Files

;; Custom listing style in dired
;; (setq dired-listing-switches "-agho --group-directories-first")
(setq dired-listing-switches "-agho")   ; macOS version

;;; Version control

;; Use horizontal (side-by-side) view by default in ediff
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-merge-split-window-function #'split-window-horizontally)

;;; Other

;; Open up the debugger on error
;; (setq debug-on-error t)

;; Use Command as Meta on macOS
;; (setq mac-command-modifier 'meta)


;;;;; HOOKS

;; emacs-startup-hooks: delay evaluation until functions are defined
;;
;; More convenient "jump to mark" command (swap prefix argument)
;; (add-hook 'emacs-startup-hook
;;           (lambda () (my/toggle-prefix-arg #'set-mark-command)))


;; outline-mode-hook: remove form-feed character (^L) from regexp
(add-hook 'outline-mode-hook
          (lambda ()
            (setq-local outline-regexp "[*]+")))

;; emacs-lisp-mode-hook: set custom outline heading format
(add-hook 'emacs-lisp-mode-hook
          (lambda () 
            (setq-local outline-regexp "\\(;;+ \\)\\([^( ]\\)")
            (setq-local outline-heading-alist
                        '((";;;;; " . 1)
                          (";;;; " . 2)
                          (";;; " . 3)
                          (";; " . 4)))
            ;; Don't use 'lisp-outline-level (doesn't use outline-heading-alist)
            (setq-local outline-level 'aj8/outline-level)))

;; activate-mark-hook: deactivate highlight mode when selecting text
(add-hook 'activate-mark-hook (lambda () (global-hl-line-mode -1)))
(add-hook 'deactivate-mark-hook (lambda () (global-hl-line-mode 1)))

;; kill-buffer-hook: collect list of killed buffers
(add-hook 'kill-buffer-hook #'reopen-killed-file--add-to-list)

;; grep-mode-hook: remove "..?*" from alias `all' in grep-files-aliases
(with-eval-after-load "grep"
  (setf (alist-get "all" grep-files-aliases nil nil #'equal) "* .[!.]*"))

;; Activate speed keys in org-mode
(add-hook 'org-mode-hook (lambda () (setq org-use-speed-commands t)))


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

;;;; Windows

(global-set-key (kbd "C-x 9") #'my/toggle-window-split)

;;;; Buffers

(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "C-c k") #'my/kill-buffer-other-window)

;;;; Outline

(global-set-key (kbd "C-c o") #'outline-minor-mode)

;;;; Navigation

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

(global-set-key (kbd "C-c l") #'scroll-lock-mode)

(global-set-key (kbd "C-c i") #'imenu)

;;;; Search

;;;; Selection

(global-set-key (kbd "M-#") #'aj8/mark-word-forward)
(global-set-key (kbd "M-@") #'aj8/mark-word-backward)

;;;; Editing

(global-set-key (kbd "C-S-k") (lambda () (interactive) (kill-line 0)))

(global-set-key (kbd "C-c ;") #'comment-line)

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(global-set-key (kbd "C-c q") 'fill-individual-paragraphs) ; use with `C-x .'
                                                           ; for comments

(global-set-key (kbd "C-c c") 'my/copy-symbol-at-point)

;;;; Completion

;;;; Spelling

;;;; Files

(global-set-key (kbd "C-c f") #'find-file-at-point)

;;;; Version control

(global-set-key (kbd "C-c e b") #'ediff-buffers)
(global-set-key (kbd "C-c e l") #'ediff-regions-linewise)
(global-set-key (kbd "C-c e w") #'ediff-regions-wordwise)
(global-set-key (kbd "C-x v -") #'vc-ediff)
(which-key-add-key-based-replacements "C-c e" "ediff")

;;;; Other

;;;; Unbind keys

;; (global-set-key (kbd "C-x") nil)
;; (global-unset-key (kbd "C-x"))   ; Alternative syntax

;;;; Modes

;; TODO: Use with-eval-after-load instead: https://emacs.stackexchange.com/questions/12592/how-to-bind-your-keys-to-keymaps-that-arent-loaded-yet

;; Info-mode-map
(add-hook 'Info-mode-hook
          ;; Disable M-n
          (lambda () (define-key Info-mode-map (kbd "M-n") nil)))

;; ediff-mode-map
;;   TODO: check functionality
(add-hook 'ediff-keymap-setup-hook
          (lambda () (define-key ediff-mode-map "d" #'my/ediff-copy-both-to-C)))

;; flyspell-mode-map
(add-hook 'flyspell-mode-hook (lambda ()
  ;; C-, : Go to previous error
  (define-key flyspell-mode-map (kbd "C-,") #'my/flyspell-goto-previous-error)
  ;; C-. : Go to next error
  (define-key flyspell-mode-map (kbd "C-.") #'flyspell-goto-next-error)
  ;; C-; : Auto correct current word
  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-auto-correct-word)))

;; outline-mode
(add-hook 'outline-mode-hook
          (lambda () (aj8/outline-mode-keys outline-mode-map)))

;; outline-minor-mode
(add-hook 'outline-minor-mode-hook
          (lambda ()
            ;; Use TAB and S-TAB for cycling
            (setq-local outline-minor-mode-cycle t)
                                        ; TODO: fix for terminal mode
            ;; Highlight headings
            (setq-local outline-minor-mode-highlight t)
                                        ; TODO: test other settings
            ;; Define custom keybindings
            (aj8/outline-mode-keys outline-minor-mode-map)))

;;;; Hydras

(which-key-add-key-based-replacements "C-c y" "hydra")

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
  ;; TODO: this fails!
  (interactive)
  (if current-prefix-arg             ; C-u
      (magit-mode-bury-buffer nil)   ; bury
    (magit-mode-bury-buffer 1)))     ; kill

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

;;;; Outline

;; Set keybindings for outline-(minor-)mode
(defun aj8/outline-mode-keys (map)
  "Set keybindings for outline-mode or outline-minor-mode.
MAP should either be `outline-mode-map' or `outline-minor-mode-map'."
  ;; (local-set-key (kbd "C-c C-c") outline-mode-prefix-map)
  ;; (setq outline-minor-mode-prefix "\C-c \C-c") ; TODO: doesn't work
  (let ((m map))
    (define-key m (kbd "C-c <left>") #'my/outline-hide-more)
    (define-key m (kbd "C-c <right>") #'my/outline-show-more)
    (define-key m (kbd "C-c C-<left>") #'outline-hide-body)
                                        ; TODO: hide-all?
    (define-key m (kbd "C-c C-<right>") #'outline-show-all)
    (define-key m (kbd "C-c <up>") #'outline-previous-visible-heading)
                                        ; TODO: Swap bindings?
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
  ;; TODO: make keybinding disappear after exectution
  "Make symbol at point the latest kill in the kill ring."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol (kill-new symbol))))

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

;;;; Files

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

;; helpful: always open additional helpful buffers in the same window
(defun my/helpful-switch-to-buffer (buffer-or-name)
  "Switch to helpful BUFFER-OR-NAME.  
If we are currently in the helpful buffer, reuse it's window,
otherwise create a new one."
  (if (eq major-mode 'helpful-mode)
      (switch-to-buffer buffer-or-name)
    (pop-to-buffer buffer-or-name)))

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


;;;;; LOCAL SETTINGS (LATE)

(cond ((equal (system-name) "MacBook-Air.lan")
       (if (display-graphic-p)
           ;; GUI settings
           (progn
	     ;; Disable scroll bar
	     (set-scroll-bar-mode nil)
             ;; Set default font
             (add-to-list 'default-frame-alist '(font . "Hack-14"))
             ;; Increase line spacing
             (setq-default line-spacing 1))
         ;; Terminal settings
         ;; Load theme
         (setq base16-theme-256-color-source "base16-shell")
         (load-theme 'base16-flat t)))

      ((equal (system-name) "NT175")
       ;; Enable (default) web browser on WSL
       (setq browse-url-generic-program "wslview"
        browse-url-browser-function #'browse-url-generic))
       ;; Load theme
       ;; (load-theme 'wombat))

      (t
       ;; Load theme
       (load-theme 'wombat)))

;; Conditionally load host specific stuff
;; (let ((host-specific-files (concat (make-load-path-base) system-name ".el")))
;;   (if (file-exists-p host-specific-files)
;;       (load host-specific-files)
;;     (message (concat "No host specific customizations for " system-name))
;;     ))


; LocalWords:  ediff flyspell isearch ispell magit minibuffer modus
