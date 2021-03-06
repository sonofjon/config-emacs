;;; init.el -*- lexical-binding: t; -*-

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


;;;;; EARLY SETTINGS

;; System dependent settings
(cond ((featurep 'ns)
       ;; GUI settings
       (when (display-graphic-p)
         ;; Add to exec-path
         ;;   TODO: Use exec-path-from-shell package?
         (dolist (dir '("/usr/local/bin"
                        "/usr/local/opt/grep/libexec/gnubin"))
           (add-to-list 'exec-path dir)))
       (message "Early settings for macOS"))

      ((or (string-match-p "^brain[0-9]+$" (system-name))
	   (equal (system-name) "endeavour-lxde")
	   (equal (system-name) "kde-neon")
	   (equal (system-name) "manjaro-xfce"))
       (message "Early settings Linux"))

      ((equal (system-name) "penguin")
       (message "Early settings ChromeOS"))

      ((or (string-match-p "^brain[0-9]+-windows$" (system-name))
           (string-match-p "^NT[0-9]\\{3\\}$" (system-name))
	   (string-match-p "^TP[0-9]\\{3\\}$" (system-name)))
       ;; Fix cursor color in terminal on WSL
       (add-hook 'modus-themes-after-load-theme-hook #'aj8/modus-themes-custom-settings)
       (message "Early settings WSL"))

      (t (user-error "Unexpected system-name: %s" system-name)))


;;;;; PACKAGES

;;;; Setup

;;; use-package

;; Install use-package
(if (not (package-installed-p 'use-package))
    (package-install 'use-package))

;; Add use-package to imenu
(setq use-package-enable-imenu-support t)

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

;; Gather package statistics
;;   (use with use-package-report)
(setq use-package-compute-statistics t)

;;; Quelpa

;; Don't update local clone of the MELPA git repo
(setq quelpa-checkout-melpa-p nil)

;; Set upgrade interval
(setq quelpa-upgrade-interval 7)
(add-hook 'after-init-hook #'quelpa-upgrade-all-maybe)

;; Install Quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; Install quelpa-use-package
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))

;; Load quelpa-use-package
(require 'quelpa-use-package)

;;; Local

(require 'aj8-lisp)

;;;; Packages

;;; Early packages 

;; benchmark-init (startup profiler)
(use-package benchmark-init
  :disabled
  ;; Disable collection of benchmark data after init
  :hook (after-init-hook . #'benchmark-init/deactivate)
  :init
  ;; Start benchmark
  (benchmark-init/activate)
  :config
  ;; Configure list format
  (setq benchmark-init/list-format
   (quote [("Module" 50 t)
    ("Type" 7 t)
    ("ms" 7 (lambda (a b) (< (string-to-number (aref (cadr a) 2))
                             (string-to-number (aref (cadr b) 2))))
     :right-align t)
    ("total" 7 (lambda (a b) (< (string-to-number (aref (cadr a) 3))
                                (string-to-number (aref (cadr b) 3))))
     :right-align t)]))
  ;; Set sort key
  (setq benchmark-init/list-sort-key  '("ms" . t)))

;;; Package management

;; auto-package-update (automatic package updates)
(use-package auto-package-update
  :defer 60
  :custom
  ;; Prompt before update
  (auto-package-update-prompt-before-update t)
  ;; Delete old versions
  (auto-package-update-delete-old-versions t)
  ;; Don't show update results
  ;; (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;; esup (benchmark utility)
;;   TODO: Full of bugs and inactive maintainer
(use-package esup
  :disabled)

;;; Theme

;; dashboard (display a dashboard at startup)
;;   TODO: make it work in Terminal
(use-package dashboard
  ;; :disabled
  :custom
  ;; Set the title
  (dashboard-banner-logo-title "Welcome to the The Department of Productivity")
  ;; Disable init info
  ;; (dashboard-set-init-info nil)
  ;; Disable footer
  ;; (dashboard-set-footer nil)
  ;; Display heading icons
  ;; (dashboard-set-heading-icons t)
  ;; Display file icons
  (dashboard-set-file-icons t)
  ;; Center content
  (dashboard-center-content t)
  ;; Set project backend
  (dashboard-projects-backend 'project-el)
  ;; Configure layout
  (dashboard-items '((recents  . 5)
                     (projects . 5)))
  :config
  ;; Set the banner
  (if (display-graphic-p)
      (setq dashboard-startup-banner 'logo)
    (setq dashboard-startup-banner
          (expand-file-name "dashboard-banner.txt" user-emacs-directory)))
  ;; Add startup hook
  (dashboard-setup-startup-hook))

;; all-the-icons (icon support)
(use-package all-the-icons
  :if (display-graphic-p))

;; all-the-icons-dired (icon support for dired)
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

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

;; doom-themes
(use-package doom-themes
  :disabled
  :custom
  ;; Enable bold and italic
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t))
  ;; Enable custom Treemacs theme (all-the-icons must be installed!)
  ;; (setq doom-themes-treemacs-theme "doom-atom")
  ;; :config
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-one-light t)
  ;; (doom-themes-treemacs-config))

;; dracula-theme
(use-package dracula-theme
  :disabled
  :custom
  ;; Don't change the font size for some headings and titles
  (setq dracula-enlarge-headings nil))
  ;; Adjust font size of titles level 1 (default 1.3)
  ;; (setq dracula-height-title-1 1.25)
  ;; Adjust font size of titles level 2 (default 1.1)
  ;; (setq dracula-height-title-1 1.15)
  ;; Adjust font size of titles level 3 (default 1.0)
  ;; (setq dracula-height-title-1 1.05)
  ;; Adjust font size of document titles (default 1.44)
  ;; (setq dracula-height-doc-title 1.4)
  ;; Use less pink and bold on the mode-line and minibuffer (default nil)
  ;; (setq dracula-alternate-mode-line-and-minibuffer t)
  ;; :config
  ;; ;; (load-theme 'dracula-dark-medium t)
  ;; (load-theme 'dracula t))

;; gruvbox-theme
(use-package gruvbox-theme
  :disabled
  :config
  ;; (load-theme 'gruvbox-light-medium t)
  (load-theme 'gruvbox-dark-medium t))

;; modus-themes
(use-package modus-themes
  ;; :disabled
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

;; sanityinc-solarized
(use-package color-theme-sanityinc-solarized
  :disabled
  :config
  ;; (load-theme 'sanityinc-solarized-light t)
  (load-theme 'sanityinc-solarized-dark t))

;; sanityinc-tomorrow
(use-package color-theme-sanityinc-tomorrow
  :disabled
  :config
  ;; (load-theme 'sanityinc-tomorrow-day t)
  (load-theme 'sanityinc-tomorrow-night t))

;; solarized-theme
(use-package solarized-theme
  :disabled
  :init
  ;; Make the fringe stand out from the background
  ;; (setq solarized-distinct-fringe-background t)
  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)
  ;; Make the modeline high contrast
  ;; (setq solarized-high-contrast-mode-line t)
  ;; Use less bolding
  (setq solarized-use-less-bold t)
  ;; Use more italics
  (setq solarized-use-more-italic t)
  ;; Use less colors for indicators such as git:gutter, flycheck and similar
  ;; (setq solarized-emphasize-indicators nil)
  ;; Don't change size of org-mode headlines (but keep other size-changes)
  ;; (setq solarized-scale-org-headlines nil)
  ;; Change the size of markdown-mode headlines (off by default)
  ;; (setq solarized-scale-markdown-headlines t)
  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0))
  ;; :config (load-theme 'solarized-dark t))
  ;; :config (load-theme 'solarized-light t))

;; spacemacs-theme
(use-package spacemacs-theme
  :disabled
  :defer   ; Fix loading warning
  :init
  ;; Toggle italics for comments (and also apply a lighter color)
  (setq spacemacs-theme-comment-italic t)
  ;; Toggle italics for keywords
  (setq spacemacs-theme-keyword-italic t)
  ;; Toggle the underline of matching parens
  (setq spacemacs-theme-underline-parens t))
  ;; :config (load-theme 'spacemacs-dark t))
  ;; :config (load-theme 'spacemacs-light t))

;; zenburn-theme
(use-package zenburn-theme
  :disabled
  :config
  (load-theme 'zenburn t))

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

;; rotate (rotate window position and layout)
(use-package rotate
  :disabled)

;; transpose-frame (transpose window arrangement)
(use-package transpose-frame
  :defer)

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
  (dimmer-fraction 0.25)         ; default is 0.20
  (dimmer-use-colorspace :rgb)   ; for use with modus themes
  :config
  (dimmer-mode 1))

;;; Outline

;; outline-minor-faces (use faces from outline-mode)
;;   See also outline-minor-mode-highlight.
(use-package outline-minor-faces
  :defer
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
  :diminish
  :hook ((prog-mode . dot-mode-on)
         (text-mode . dot-mode-on))
  :bind (:map dot-mode-map ("C-c d" . dot-mode-execute))
  :custom
  (dot-mode-verbose t)
  :config
  (unbind-key "C-." dot-mode-map)
  (unbind-key "C-c ." dot-mode-map)
  (unbind-key "C-M-." dot-mode-map))

;; lorem-ipsum (lorem ipsum text filler)
(use-package lorem-ipsum
  :defer)

;; rainbow-delimiters (parentheses coloring)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; move-dup (minor mode for moving and duplicating lines or rectangles)
(use-package move-dup
  :bind (("C-c <up>" . move-dup-move-lines-up)
         ("C-c C-<up>" . move-dup-duplicate-up)
         ("C-c <down>" . move-dup-move-lines-down)
         ("C-c C-<down>" . move-dup-duplicate-down)))

;; multiple-cursors (edit at multiple points)
(use-package multiple-cursors
  :bind (:prefix-map multiple-cursors
                     :prefix "C-c m"
                     :prefix-docstring "Multiple Cursors related"
                     ("SPC" . set-rectangular-region-anchor)
                     ("<" . mc/mark-previous-like-this)
                     (">" . mc/mark-next-like-this)
                     ("?" . mc/mark-all-like-this)
                     ("," . mc/mark-previous-like-this-symbol)
                     ("." . mc/mark-next-like-this-symbol)
                     ("/" . mc/mark-all-like-this-dwim)
                     ("m" . mc/mark-more-like-this-extended)
                     ;; ("" . mc/mark-all-dwim)
                     ("e" . mc/edit-lines)))

;; whole-line-or-region (apply to current line if region is undefined)
(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode
  ;; :bind
  ;; (:map whole-line-or-region-local-mode-map
  ;; ([remap ispell-region] . whole-line-or-region-ispell-region))
  :config
  ;; Use whole-line-or-region-mode everywhere
  (whole-line-or-region-global-mode 1))

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

;; vundo (visual undo)
(use-package vundo
  :commands (vundo)
  :bind ("C-c v" . vundo)
  :custom
  ;; Use compact layout
  (vundo-compact-display t)
  ;; Use pretty Unicode characters
  (vundo-glyph-alist vundo-unicode-symbols))

;;; Completion

;; icomplete-vertical (show icomplete candidates vertically)
(use-package icomplete-vertical
  :disabled
  :bind (:map icomplete-minibuffer-map
              ("<up>" . icomplete-backward-completions)
              ("<down>" . icomplete-forward-completions)
              ;; ("RET" . icomplete-force-complete)
              ("RET" . icomplete-force-complete-and-exit)
              ("C-v" . icomplete-vertical-toggle))
  :init
  (icomplete-mode)
  (icomplete-vertical-mode)
  :config
  ;; Configure completion styles
  (my/completion-styles))

;; vertico (vertical completion UI)
(use-package vertico
  :bind (:map vertico-map
              ("?" . minibuffer-completion-help)
              ("C-c ?" . minibuffer-hide-completions)
              ;; ("TAB" . vertico-insert)   ; default
              ("<backtab>" . vertico-insert)
              ("TAB" . minibuffer-complete))
              ;; ("<backtab>" . minibuffer-force-complete))
  :init
  (vertico-mode)
  :custom
  ;; Enable cycling
  (vertico-cycle t)
  :config
  ;; Unbind default TAB binding
  (unbind-key "TAB" vertico-map)
  ;; Enable M-x minibuffer-hide-completions (make function interactive)
  (put 'minibuffer-hide-completions 'interactive-form '(interactive))
  ;; Configure completion styles
  (my/completion-styles))

;; corfu (completion overlay)
(use-package corfu
  ;; TODO: enable corfu-history-mode and corfu-info-mode?
  ;; :hook (prog-mode . corfu-mode)   ; not needed with corfu-global-mode
  ;; :custom
  ;; (corfu-count 10)               ; maximal number of candidates to show
  ;; (corfu-min-width 15)           ; popup minimum width in characters
  ;; (corfu-max-width 100)          ; popup maximum width in characters."
  ;; (corfu-cycle t)                ; enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ; enable auto completion
  ;; (corfu-auto-prefix 3)          ; minimum length of prefix for auto completion."
  ;; (corfu-separator ?\s)          ; orderless field separator
  ;; (corfu-quit-at-boundary t)     ; automatically quit at word boundary
  ;; (corfu-quit-no-match t)        ; automatically quit if there is no match
  ;; (corfu-preview-current nil)    ; disable current candidate preview
  ;; (corfu-preselect-first nil)    ; disable candidate preselection
  ;; (corfu-on-exact-match nil)     ; configure handling of exact matches
  ;; (corfu-echo-documentation nil) ; do not show documentation in the echo area
  ;; (corfu-scroll-margin 5)        ; use scroll margin
  :init
  ;; Enable corfu globally
  ;;   (this is useful since dabbrev can be used in all buffers)
  (global-corfu-mode))   ; TODO: note that if this is enabled in terminal mode
                         ; consult-completions wont be active

;; corfu-doc (documentation popup for corfu)
(use-package corfu-doc
  :if (display-graphic-p)
  :after corfu
  :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
              ("M-p" . corfu-doc-scroll-down)   ; corfu-next
              ("M-n" . corfu-doc-scroll-up)     ; corfu-previous
              ("M-d" . corfu-doc-toggle))
  :custom
  ;; (corfu-doc-delay 0)
  ;; (corfu-doc-hide-threshold 0)
  ;; Enable manually
  (corfu-doc-auto nil))

;; corfu-terminal (corfu popup on terminal)
;;   TODO: check again when package more mature
;; (use-package corfu-terminal
;;   :if (not (display-graphic-p))
;;   :disabled
;;   :after corfu
;;   :config
;;   (corfu-terminal-mode 1)))

;; (use-package corfu-terminal
;;   :quelpa (corfu-terminal
;;            :fetcher github
;;            :url "https://codeberg.org/akib/emacs-corfu-terminal.git"))

;; cape (completion at point extensions for corfu)
;;   TODO: Fix completion in terminal
(use-package cape
  :if (not (display-graphic-p))
  :after (corfu which-key)
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c u p" . completion-at-point) ;; capf
         ("C-c u a" . cape-abbrev)
         ("C-c u d" . cape-dabbrev)
         ("C-c u w" . cape-dict)
         ("C-c u f" . cape-file)
         ("C-c u i" . cape-ispell)
         ("C-c u k" . cape-keyword)
         ("C-c u l" . cape-line)
         ("C-c u s" . cape-symbol))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (which-key-add-key-based-replacements "C-c u" "corfu/cape"))

;; orderless (orderless completion style)
(use-package orderless
  ;; :disabled
  :after (:any icomplete-vertical vertico)
  :config
  ;; Use orderless for completion
  ;;   (disables styles set for icomplete-vertical and vertico)
  (setq completion-styles '(orderless basic))
  ;; Use orderless everywhere
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles basic partial-completion))))
  ;; Matching styles
  ;; (setq orderless-matching-styles '(orderless-literal orderless-regexp))   ; default
  (setq orderless-matching-styles
        '(orderless-prefixes
          orderless-initialism)
        orderless-style-dispatchers '(my/flex-if-twiddle
                                      my/with-if-equal
                                      my/without-if-bang)))

;; consult (practical commands based on Emacs completion)
(use-package consult
  :after which-key
  :bind (;; C-c bindings (mode-specific-map)
         ;; ("C-c c h" . consult-history)
         ("C-c c m" . consult-mode-command)      ; run a command from current modes
         ("C-c c M" . consult-minor-mode-menu)   ; enable or disable minor mode
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)   ; consult: repeat-complex-command
         ("C-x b" . consult-project-buffer)              ; consult: switch-to-buffer
         ("C-x B" . consult-buffer)              ; consult: switch-to-buffer
         ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop)              ; consult: yank-pop
         ("<help> a" . consult-apropos)          ; consult: apropos-command
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)           ; consult: goto-line
         ("M-g M-g" . consult-goto-line)         ; consult: goto-line
         ("M-g o" . consult-outline)             ; jump to an outline heading
         ("M-g m" . consult-mark)                ; jump to mark
         ("M-g k" . consult-global-mark)         ; jump to mark (global)
         ("M-g i" . consult-imenu)               ; select item from imenu
         ("M-g I" . consult-imenu-multi)         ; select item from imenu (project)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)                ; search for regexp with find
         ("M-s R" . consult-recent-file)         ; find recent file
         ("M-s F" . consult-locate)              ; search for regexp with locate
         ("M-s g" . consult-grep)                ; search for regexp with grep
         ("M-s G" . consult-git-grep)            ; search for regexp with grep (git)
         ("M-s r" . consult-ripgrep)             ; search for regexp with rg
         ("M-s l" . consult-line)                ; search for a matching line
         ("M-s L" . consult-line-multi)          ; search for a matching line (global)
         ("M-s m" . consult-multi-occur)         ; consult: multi-occur
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)     ; read a search string from Isearch history
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)       ; consult: isearch-edit-string
         ("M-s e" . consult-isearch-history)     ; consult: isearch-edit-string
         ("M-s l" . consult-line)                ; needed by consult-line to detect Isearch
         ("M-s L" . consult-line-multi)          ; needed by consult-line to detect Isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)               ; consult: next-matching-history-element
         ("M-r" . consult-history))              ; consult: previous-matching-history-element
  :init
  ;; Enhance `completing-read-multiple'
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  ;; Use consult for completion in region
  ;;   Note, this does not work with LSP or eglot
  (when (not (display-graphic-p))   ; only enable if using terminal
    (setq completion-in-region-function #'consult-completion-in-region))
  (which-key-add-key-based-replacements "C-c c" "consult")
  :config
  ;; Preview key
  ;;   TODO: does not work in terminal
  (setq consult-preview-key (kbd "M-`"))         ; default is 'any
  ;; (setq consult-preview-key (list (kbd "<down>") (kbd "<up>")))
  ;; Configure preview on a per-command basis
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-bookmark
   consult-recent-file consult-xref consult--source-bookmark
   consult--source-recent-file consult--source-project-recent-file
   :preview-key (list (kbd "<down>") (kbd "<up>")))
  ;; Narrowing key
  ;; (setq consult-narrow-key "<")
  ;; Enable narrowing help in the minibuffer
  ;;   (you may want to use `embark-prefix-help-command' or which-key instead)
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  ;; Completion
  (consult-customize
   consult-completion-in-region
   :completion-styles '(basic)))   ; disable orderless
   ;; :require-match t)

;; consult-project-extra (project extension for consult)
(use-package consult-project-extra
  :bind ("C-c p" . consult-project-extra-find))

;; embark (context aware actions)
(use-package embark
  :bind (("M-." . embark-act)
         ("M-," . embark-dwim)
         ("C-h B" . embark-bindings)) ; alternative for `describe-bindings'
  ;; :init
  ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; embark-consult (integration between embark and consult)
(use-package embark-consult
  :demand t ; only necessary if you have the hook below
  :after (embark consult)
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Spelling

;; flyspell-correct (wrapper for flyspell with completion)
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ;; ("C-;" . flyspell-auto-correct-word)
              ("C-;" . flyspell-correct-wrapper)
              ("C-," . (lambda () (interactive) (aj8/call-interactively-wih-prefix-toggle #'flyspell-correct-previous)))
              ("C-." . (lambda () (interactive) (aj8/call-interactively-wih-prefix-toggle #'flyspell-correct-next)))
              ("C-c ," . my/flyspell-goto-previous-error)
              ("C-c ." . flyspell-goto-next-error)))

;;; Files

;; treemacs (a tree layout file explorer)
(use-package treemacs
  :defer
  :after which-key
  :init
  (which-key-add-key-based-replacements "C-c t" "treemacs")
  (which-key-add-key-based-replacements "C-c C-p" "treemacs")
  (which-key-add-key-based-replacements "C-c C-w" "treemacs")
  :init
  ;; No delay when switching projects
  (add-hook 'treemacs-project-follow-mode-hook
            (lambda () (setq treemacs--project-follow-delay 0)))
  :config
  (setq
   treemacs-find-workspace-method           'find-for-file-or-manually-select
   ;; treemacs-indentation                     2
   ;; treemacs-indentation-string              " "
   ;; treemacs-is-never-other-window           nil
   ;; treemacs-no-delete-other-windows         t
   ;; treemacs-project-follow-cleanup          nil
   ;; treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
   ;; treemacs-position                        'left
   ;; treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
   ;; treemacs-show-hidden-files               t
   ;; treemacs-sorting                         'alphabetic-asc
   treemacs-width                           30)
  ;; Follow current buffer file in Treemacs window (on by default)
  ;; (treemacs-follow-mode 1)
  ;; Only display current project in Treemacs window
  (treemacs-project-follow-mode 1)
  ;; Show visual indicator in the fringe for highlighted file
  (treemacs-fringe-indicator-mode 'always)
  ;; Detect file system changes (on by default)
  ;; (treemacs-filewatch-mode 1)
  ;; Hide files ignored by Git
  ;; (treemacs-hide-gitignored-files-mode 1)
  ;; Display indentation guides
  (treemacs-indent-guide-mode 1)
  ;; Display git project annotations
  (treemacs-git-commit-diff-mode 1)
  ;; Highlight files using git status
  (treemacs-git-mode 'deferred)
  :bind (("M-0" . treemacs-select-window)   ; TODO: this conflicts with C-u 0
         ("C-c t 1" . treemacs-delete-other-windows)
         ("C-c t t" . treemacs)
         ("C-c t d" . treemacs-select-directory)
         ("C-c t f" . treemacs-find-file)
         :map treemacs-mode-map
         ;; Custom navigation
         ("<down>" . treemacs-next-line)
         ("<up>" . treemacs-previous-line)
         ;; ("M-<down>" . treemacs-next-neighbour)   ; not needed
         ;; ("M-<up>" . treemacs-previous-neighbour)   ; not needed
         ("C-c C-p <down>" . treemacs-move-project-down)
            ; TODO: update transient help with these new bindings
         ("C-c C-p <up>" . treemacs-move-project-up)
         ("o l" . treemacs-visit-node-in-least-recently-used-window)))

;; treemacs-all-the-icons (all-the-icons for Treemacs)
(use-package treemacs-all-the-icons
  :if (display-graphic-p)
  :defer)

;; treemacs-icons-dired (icons for dired)
;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once))

;; treemacs-magit (make Treemacs aware of Magit operations)
(use-package treemacs-magit
  :defer
  :after (treemacs magit))

;;; Coding

;; json-mode (major-mode for editing JSON files)
(use-package json-mode
  :mode ".json"
  :hook (json-mode . (lambda () (setq-local js-indent-level 2)))
  :bind (:map json-mode-map
              ("C-c C-b" . json-mode-beautify)
              ("C-c C-s" . json-snatcher))
  :config
  (unbind-key "C-c C-f" json-mode-map)
  (unbind-key "C-c P" json-mode-map))

;; markdown-mode (major mode for editing Markdown files)
(use-package markdown-mode
  :mode (".md" . markdown-mode)
  ;; :mode (".md" . markdown-view-mode)
  :config
  ;;TODO: Fix diminish
  (add-hook 'markdown-mode-hook (lambda () (diminish 'markdown-mode)))
  (add-hook 'markdown-view-mode-hook (lambda () (diminish 'markdown-view-mode))))

;; php-mode (major-mode for editing PHP files)
(use-package php-mode
  :mode ".php$")

;; web-mode (major-mode for editing web templates)
(use-package web-mode
    :mode (".html?$")
    :hook (web-mode . (lambda () (setq truncate-lines nil)))
    :init
    ;; Engines
    (setq web-mode-engines-alist '(("django" . "\\.html\\'")
                                   ("blade" . "\\.blade\\.")))
    :custom
    ;; Indentation
    ;; (web-mode-markup-indent-offset 2)
    ;; (web-mode-code-indent-offset 2)
    ;; Padding
    ;; (web-mode-block-padding 1)
    ;; Enable auto-functionality in the terminal
    ;;   Note: this can be bad when pasting text
    (web-mode-enable-auto-expanding t)
    (web-mode-enable-auto-closing t)
    (web-mode-enable-auto-indentation t)
    (web-mode-enable-auto-opening t)
    (web-mode-enable-auto-pairing t)
    (web-mode-enable-auto-quoting t))

;; yaml-mode (major-mode for editing YAML files)
(use-package yaml-mode
  :mode ".yml"
  :bind (:map yaml-mode-map))
              ;; ("C-m" . newline-and-indent)))

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
  (which-key-add-key-based-replacements "C-c l" "LSP")
  ;; Prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l"))

;; lsp-ui-mode (higher level UI modules for LSP)
;; (use-package lsp-ui
;;   :commands lsp-ui-mode)

;; lsp-treemacs: (Treemacs integration)
;; (use-package lsp-treemacs
;;   :commands lsp-treemacs-errors-list)

;; i3wm-config-mode (syntax highlighting for i3 config files)
(use-package i3wm-config-mode
  :defer)

;; powershell (major mode for editing and running PowerShell files)
(use-package powershell
  :defer)

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
         ([remap magit-mode-bury-buffer] . aj8/magit-mode-bury-buffer)
         ("TAB" . magit-section-cycle)
         ("<backtab>" . magit-section-cycle-global))
         ;; Open files in other window
         ;; :map magit-file-section-map
         ;; ("RET" . magit-diff-visit-file-other-window)
         ;; Open hunks in other window
         ;; :map magit-hunk-section-map
         ;; ("RET" . magit-diff-visit-file-other-window))
  :custom
  ;; Show refined diffs for current hunk
  (magit-diff-refine-hunk t)
  ;; Set path to known git repositories
  (magit-repository-directories
   '(("~/dotfiles"                . 1)
     ("~/projects"                . 1)
     ("~/git"                     . 1)))
  :config
  ;; Add status flag to repository list
  (add-to-list 'magit-repolist-columns
               '("Flag" 4 magit-repolist-column-flag (:right-align t))))

;; diff-hl (highlight uncommitted changes)
(use-package diff-hl
  :disabled
  :config
  ;; Use diff-hl-mode everywhere
  (global-diff-hl-mode 1))

;; ztree (directory-diff tool)
(use-package ztree
  :defer
  :custom
  ;; Use pretty Unicode art
  (ztree-draw-unicode-lines t))
  ;; Customize file filter (default is all dot-files) 
  ;; (setq-default ztree-diff-filter-list (cons \"^.*\\.pyc\" ztree-diff-filter-list)))

;;; Help

;; marginalia (add marginalia to minibuffer completions)
(use-package marginalia
  :demand
  :after (:any icomplete-vertical vertico)
  :bind (:map minibuffer-local-map ("M-m" . marginalia-cycle))
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
  :after which-key
  :diminish
  :init
  (which-key-add-key-based-replacements "C-c /" "google-this")
  :config
  (google-this-mode 1))

;; erc (IRC client)
(use-package erc
  :disabled
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
  :disabled
  :defer
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks)
  (erc-update-modules))

;; elfeed (web feed reader)
(use-package elfeed
  :commands elfeed
  :bind (:map elfeed-search-mode-map
              ("g" . elfeed-search-fetch)
              ("G" . elfeed-search-update--force))
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
  (elfeed-search-title-max-width 50)    ; default is 70
  (elfeed-search-title-min-width 16)    ; default is 16
  (elfeed-search-trailing-width 30))    ; default is 30

;;; Other

;; diminish (hide minor modes)
(use-package diminish
  :config
  ;; Pre-loaded modes
  (diminish 'eldoc-mode)
  ;; Not pre-loaded modes
  (add-hook 'auto-revert-mode-hook (lambda () (diminish 'auto-revert-mode)))
  (add-hook 'abbrev-mode-hook (lambda () (diminish 'abbrev-mode)))
  (add-hook 'visual-line-mode-hook (lambda () (diminish 'visual-line-mode))))

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
                                    dired-previous-line
                                    dired-next-line
                                    vertico-previous
                                    vertico-next
                                    isearch-printing-char
                                    backward-delete-char-untabify
                                    mwheel-scroll))
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; ssh-agency (manage ssh-agent)
(use-package ssh-agency
  :if (display-graphic-p))

;; keychain-environment (manage ssh-agent and gpg-agent)
(use-package keychain-environment
  :disabled
  :if (display-graphic-p)
  :config
  (keychain-refresh-environment))

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
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;;; Files...

;; Track recent files
(recentf-mode 1)

;;; Coding...

;;; Version control...

;;; Help...

;;; Web...

;;; Other...

;; Expand abbreviations
(add-hook 'text-mode-hook #'abbrev-mode)

;;;; Variables

;;; Package management

;; Natively compile packages during installation
;; (setq package-native-compile t)

;;; Theme

;;; Windows

;; Prefer horizontal (side-by-side) window splitting
;;   Note: the thresholds need to be twice as big as the smallest
;;   window allowed, because the new windows each use half of the
;;   former window size
(setq split-width-threshold 140
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
(which-key-add-key-based-replacements "C-c @" "outline")

;; Use TAB and S-TAB for cycling
(setq outline-minor-mode-cycle t)

;; Highlight headings
;;   See also outline-minor-faces.
;; (setq outline-minor-mode-highlight t)   ; alternatives: 'override and 'append

;;; Navigation

;; Scrolling
;;   The order of priority is: ???scroll-conservatively???, then
;;   ???scroll-step???, and finally ???scroll-up-aggressively??? /
;;   ???scroll-down-aggressively???.
(setq scroll-conservatively 0)        ; default: 0
(setq scroll-step 1)                  ; default: 0
(setq scroll-up-aggressively nil)     ; default: nil
(setq scroll-down-aggressively nil)   ; default: nil
;; (setq scroll-margin 0)

;; Preserve point position when scrolling
(setq scroll-preserve-screen-position t)

;; Save place in each file
(setq save-place-mode t)

;; Use "repeat-mode" for "pop-mark"
(setq set-mark-command-repeat-pop t)

;;; Search

;; Don't search invisible text by default
(setq isearch-invisible nil)

;; Disable lax-whitespace searching by default
(setq isearch-lax-whitespace nil)

;; Interpret spaces as wildcards (with M-s SPC)
(setq search-whitespace-regexp ".*?")

;; Allow movement between Isearch matches by cursor motion commands
(setq isearch-allow-motion t)
(setq isearch-motion-changes-direction t)

;;; Selection

;;; Editing

;; Use spaces, not tabs
(setq-default indent-tabs-mode nil)

;; Delete trailing newline character with kill-line
(setq kill-whole-line t)

;; Don't use the mark when region is inactive
;;   Note: messes with ediff
;; (setq mark-even-if-inactive nil)

;; Save clipboard text into kill ring before kill
;; (setq save-interprogram-paste-before-kill t)

;; Yank at point, not at pointer
(setq mouse-yank-at-point t)

;;; Completion

;; Use TAB for symbol completion (after indentation)
(setq tab-always-indent 'complete)

;; Show more details for completions
(setq completions-detailed t)

;; Let hippie-expand search for line expansions in all buffers
;; (add-to-list 'hippie-expand-try-functions-list 'try-expand-line-all-buffers t)
(setcar (nthcdr 5 hippie-expand-try-functions-list) 'try-expand-line-all-buffers)

;;; Spelling

;; Configure language environment
;; (setenv "LANG" "en_US.UTF-8")

;; Use aspell
(setq ispell-program-name "aspell")

;; Set language
(setq ispell-dictionary "en_US")

;; Set aspell suggestion mode
(setq ispell-extra-args '("--sug-mode=ultra"))
;; (setq ispell-extra-args '("--sug-mode=fast"))
;; (setq ispell-extra-args '("--sug-mode=normal"))

;; Use hunspell
;;   Can handle multiple languages simultaneously
;; (setq ispell-program-name "hunspell")

;; hunspell setup
(when (string-match-p "hunspell" ispell-program-name)
  ;; Set language(s)
  (setq ispell-dictionary "en_US,sv_SE")
  (with-eval-after-load "ispell"
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_US,sv_SE"))

  ;; Don't overwrite default personal dictionaries
  (setq ispell-personal-dictionary "~/.hunspell_personal")

  ;; Make sure personal dictionary file exists
  ;;   (otherwise hunspell will silently not use it)
  (unless (file-exists-p ispell-personal-dictionary)
    (with-temp-buffer (write-file ispell-personal-dictionary))))

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

;; Use existng frame in GUI
(when (display-graphic-p)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;; Help

;; More extensive apropos commands
(setq apropos-do-all t)

;;; Web

;; URL browser settings
;;   TODO: There is also browse-url-default-windows|macosx-browser
;;   TODO: New options in Emacs 28
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
;; Don't remove duplicates in browsing history
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
    [remap eww-open-in-new-buffer] #'aj8/eww-open-in-new-buffer))
;; Open new eww buffers in a new window (C-u RET)
(with-eval-after-load "eww"
  (define-key eww-mode-map
    [remap eww-follow-link] #'aj8/eww-follow-link))

;; Are these needed?
;; (setq shr-use-colors nil)             ; t is bad for accessibility
;; (setq shr-use-fonts nil)

;;; Other

;; Use 'y' or 'n' questions always
;; (setq use-short-answers t)

;; Open up the debugger on error
;; (setq debug-on-error t)

;; Use longer pulse
(setq pulse-delay 0.05)   ; default is 0.03

;; Use speed keys in org-mode
(setq org-use-speed-commands t)

;; Remove "..?*" from alias `all' in grep-files-aliases
(with-eval-after-load "grep"
  (setf (alist-get "all" grep-files-aliases nil nil #'equal) "* .[!.]*"))

;; Show mode headers in describe-bindings buffer
(setq describe-bindings-outline t)

;;;; Other


;;;;; MODES

;; Use sh-mode for non-standard bash config files
(add-to-list 'auto-mode-alist '("\\.bash_.*\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc_.*\\'" . sh-mode))

;; outline-mode: remove form-feed character (^L) from regexp
(add-hook 'outline-mode-hook
          (lambda () (setq outline-regexp "[*]+")))

;; emacs-lisp-mode: outline settings
(add-hook 'emacs-lisp-mode-hook
          #'outline-headers-for-semicolon-buffers)

;; conf-xdefaults-mode: outline settings
(add-hook 'conf-xdefaults-mode-hook
          #'outline-headers-for-exclamation-mark-buffers)

;; shell-scrip-mode: outline settings
(add-hook 'sh-mode-hook
          #'outline-headers-for-hash-mark-buffers)

;; i3wm-config-mode: outline settings
(add-hook 'i3wm-config-mode-hook
          #'outline-headers-for-hash-mark-buffers)

;; powershell: outline settings
(add-hook 'powershell-mode-hook
          #'outline-headers-for-hash-mark-buffers)

;; activate-mark: deactivate highlight mode when selecting text
(add-hook 'activate-mark-hook (lambda () (global-hl-line-mode -1)))
(add-hook 'deactivate-mark-hook (lambda () (global-hl-line-mode 1)))

;; Info-mode: allow multiple Info buffers
(add-hook 'Info-mode-hook #'rename-uniquely)

;; kill-buffer: collect list of killed buffers
(add-hook 'kill-buffer-hook #'reopen-killed-file--add-to-list)

;; TODO: loop over maps for quit-windows
;; (defun define-key-quit-windows (map)
;;   (define-key map [remap quit-window] 'aj8/quit-window))

;; (dolist (map '(help-mode-map Info-mode-map dired-mode-map eww-mode-map))
;;   (with-eval-after-load map
;;     (define-key (symbol-value map) [remap quit-window] 'aj8/quit-window)))

;; (dolist (str (list "help" "Info" "dired" "eww"))
;;   (with-eval-after-load str
;;     (let ((kmap (symbol-value (intern (concat str "-mode-map")))))
;;       (define-key kmap [remap quit-window] 'aj8/quit-window))))

;; help-mode: kill buffers on quit
(define-key help-mode-map [remap quit-window] #'aj8/quit-window)

;; Info-mode: kill buffers on quit
(with-eval-after-load "info"
  (define-key Info-mode-map [remap quit-window] #'aj8/quit-window))

;; dired-mode: kill buffers on quit
(with-eval-after-load "dired"
  (define-key dired-mode-map [remap quit-window] #'aj8/quit-window))

;; eww-mode: kill buffers on quit
(with-eval-after-load "eww"
  (define-key eww-mode-map [remap quit-window] #'aj8/quit-window))


;;;;; KEYBINDINGS

;;   TODO: Unbind rebound keys, e.g. M-a for back-to-indentation
;;
;;         Use minor-mode for keybindings? (https://stackoverflow.com/a/683575/1610035)

;;;; Escape codes

(when (not (display-graphic-p))   ; if using terminal
  ;; (define-key input-decode-map "\e[1;8A" [C-M-S-up])
  ;; (define-key input-decode-map "\e[1;8B" [C-M-S-down])

  ;; (define-key input-decode-map "\e[127;2u" [S-backspace])
  (define-key input-decode-map "\e[127;5u" [C-backspace])
  (define-key input-decode-map "\e[127;6u" [C-S-backspace])
  (define-key input-decode-map "\e[107;6u" (kbd "C-S-k"))

  (define-key input-decode-map "\e[118;8u" (kbd "C-M-S-v")))

;;;; Translations

(define-key key-translation-map (kbd "M-<up>") (kbd "M-p"))
(define-key key-translation-map (kbd "M-<down>") (kbd "M-n"))
(define-key key-translation-map (kbd "C-M-<up>") (kbd "C-M-p"))
(define-key key-translation-map (kbd "C-M-<down>") (kbd "C-M-n"))

;;;; Global

;;; Package management

;;; Theme

(global-set-key (kbd "C-c n") #'column-number-mode)

;;; Windows

(global-set-key (kbd "C-x 9") #'my/toggle-window-split)

(define-key winner-mode-map (kbd "C-c <") #'winner-undo)
(define-key winner-mode-map (kbd "C-c >") #'winner-redo)

;;; Buffers

(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "C-c k") #'my/kill-buffer-other-window)

(global-set-key [remap next-buffer] #'my/next-buffer)
(global-set-key [remap previous-buffer] #'my/previous-buffer)

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
(global-set-key (kbd "C-c w C-<up>") #'windmove-delete-up)
(global-set-key (kbd "C-c w C-<down>") #'windmove-delete-down)
(global-set-key (kbd "C-c w C-<left>") #'windmove-delete-left)
(global-set-key (kbd "C-c w C-<right>") #'windmove-delete-right)
(which-key-add-key-based-replacements "C-c w" "windmove")

(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)

(global-set-key (kbd "M-a") #'back-to-indentation)

;; (global-set-key (kbd "C-c <up>") #'aj8/previous-line)
;; (global-set-key (kbd "C-c <down>") #'aj8/next-line)

(global-set-key (kbd "C-c x l") #'scroll-lock-mode)
(which-key-add-key-based-replacements "C-c x" "misc")

(global-set-key (kbd "C-c i") #'imenu)

;; (global-unset-key (kbd "C-M-<up>"))
;; (global-unset-key (kbd "C-M-<down>"))
;; (global-unset-key (kbd "C-M-<left>"))
;; (global-unset-key (kbd "C-M-<right>"))
;; (global-set-key (kbd "C-M-<left>") #'backward-up-list) ; overwrites default backward-sexp
;; (global-set-key (kbd "C-M-<right>") #'down-list) ; overwrites default backward-sexp
(global-set-key (kbd "C-M-<left>") #'backward-list) ; overwrites default backward-sexp
(global-set-key (kbd "C-M-<right>") #'forward-list) ; overwrites default backward-sexp
(global-set-key (kbd "C-M-p") #'backward-up-list)   ; overwrites default backward-list
(global-set-key (kbd "C-M-n") #'down-list)          ; overwrites default forward-list

;;; Search

;;; Selection

(global-set-key (kbd "M-#") #'aj8/mark-word-forward)
(global-set-key (kbd "M-@") #'aj8/mark-word-backward)

;;; Editing

(global-set-key (kbd "C-S-k") (lambda () (interactive) (kill-line 0)))

(global-set-key (kbd "C-c ;") #'comment-line)

(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "M-c") #'capitalize-dwim)

(global-set-key (kbd "C-c q") #'fill-individual-paragraphs) ; use with `C-x .'
                                                            ; for comments

(global-set-key (kbd "C-c s") #'my/copy-symbol-at-point)

(global-set-key (kbd "C-c TAB") #'indent-relative)

;;; Completion

(global-set-key [remap dabbrev-expand] 'hippie-expand)

;;; Spelling

;;; Files

(global-set-key (kbd "C-c f") #'find-file-at-point)

(global-set-key (kbd "C-x p t") #'project-forget-project)

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
;; (global-set-key (kbd "s-q") #'save-buffers-kill-terminal)
;; (global-set-key (kbd "s-m") #'iconify-frame)
;; (global-set-key (kbd "s-n") #'make-frame-command)
;; (global-set-key (kbd "s-s") #'save-buffer)
;; (global-set-key (kbd "s-a") #'mark-whole-buffer)
;; (global-set-key (kbd "s-z") #'undo)
;; (global-set-key (kbd "s-x") #'kill-region)
;; (global-set-key (kbd "s-c") #'kill-ring-save)
;; (global-set-key (kbd "s-v") #'yank)

;;; Unbind keys

;; (global-set-key (kbd "C-x") nil)
;; (global-unset-key (kbd "C-x"))   ; alternative syntax

;;;; Local

;; Info-mode-map
(add-hook 'Info-mode-hook
          ;; Disable M-n
          (lambda () (define-key Info-mode-map (kbd "M-n") nil)))

;; sh-mode-map
(add-hook 'sh-mode-hook
          ;; Disable SMIE commands
          (lambda () (define-key sh-mode-map (kbd "C-c =") nil)
                     (define-key sh-mode-map (kbd "C-c <") nil)
                     (define-key sh-mode-map (kbd "C-c >") nil)
                     (define-key sh-mode-map (kbd "C-c ?") nil)))

;; ediff-mode-map
;;   TODO: check functionality
(add-hook 'ediff-keymap-setup-hook
          (lambda () (define-key ediff-mode-map "d" #'my/ediff-copy-both-to-C)))

;;;; Hydras

(which-key-add-key-based-replacements "C-c y" "hydra")

;;; Windows
(defhydra hydra-window (:hint nil)
  ;; TODO: Improve layout
  "
                                                                   ?????????????????????????????????
  Select   Move   Resize    Split             Do                   ??? Windows ???
????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
  ^ ^ _???_ ^ ^   ^ ^ _???_ ^ ^   ^ ^ _???_ ^ ^   ??????????????? ^ ^           ??????????????? ^ ^
  _???_ ^ ^ _???_   _???_ ^C^ _???_   _???_ ^M^ _???_   ??? ??? ???[_h_]orizontal ???????????????[_b_]alance  [_t_]ranspose
  ^ ^ _???_ ^ ^   ^ ^ _???_ ^ ^   ^ ^ _???_ ^ ^   ??????????????? ^ ^           ??????????????? ^ ^         fl[_i_]p (vertical)
  ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ??????????????? ^ ^           ??????????????? ^ ^         fl[_o_]p (horizontal)
  ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ???????????????[_v_]ertical   ???   ???[_z_]oom     [_r_]otate
  ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ??????????????? ^ ^           ??????????????? ^ ^
????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
??? [_u_]ndo layout | ??? re[_s_]set layout
??? [_n_]ext window | ??? [_c_]lose window
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
  ("M-<left>" (lambda ()
                (interactive)
                (hydra-move-splitter-left 1)
                (message "Width: %s Height: %s" (window-body-width) (window-body-height))))
  ("M-<right>" (lambda ()
                (interactive)
                (hydra-move-splitter-right 1)
                (message "Width: %s Height: %s" (window-body-width) (window-body-height))))
  ;; ("M-<up>" hydra-move-splitter-up)
  ("M-p" (lambda ()
                (interactive)
                (hydra-move-splitter-up 1)
                (message "Width: %s Height: %s" (window-body-width) (window-body-height))))
  ;; ("M-<down>" hydra-move-splitter-down)
  ("M-n" (lambda ()
                (interactive)
                (hydra-move-splitter-down 1)
                (message "Width: %s Height: %s" (window-body-width) (window-body-height))))
  ("v" split-window-vertically)
  ("h" split-window-horizontally)
  ("b" balance-windows)
  ("z" delete-other-windows)
  ("u" winner-undo)
  ("s" winner-redo)
  ("c" delete-window)
  ("n" other-window)
  ("t" transpose-frame)
  ("i" flip-frame)
  ("o" flop-frame)
  ("r" rotate-frame-clockwise))

(global-set-key (kbd "C-c y w") #'hydra-window/body)

;;; Scrolling
(defhydra hydra-scroll (:hint nil)
  "
^Line^            ^Paragraph^
^^^^---------------------------
_???_: previous     _???_: previous
_???_: next         _???_: next

Scroll by line or paragraph.
"
  ("q" nil "quit")
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
_???_: previous     _???_: previous
_???_: next         _???_: next

Move to the next line or comment.
"
  ("q" nil "quit")
  ("<up>" aj8/previous-line)
  ("<down>" aj8/next-line)
  ("<left>" aj8/previous-comment)
  ("<right>" aj8/next-comment))

(global-set-key (kbd "C-c y n") #'hydra-navigation/body)

;;; Outline
(defhydra hydra-outline (:color pink :hint nil)
  "
Hide & Show         ^^Move
------------------------------------------------------
  _???_ hide               _???_ previous visible
  _???_ show               _???_ next visible
C-_???_ hide (global)    C-_???_ backward same level
C-_???_ show (global)    C-_???_ forward same level
  ^ ^                  M-_???_ up

Hide, show and navigate outlines.
"
  ;; Hide and show
  ("<left>" my/outline-hide-more)
  ("<right>" my/outline-show-more)
  ("C-<left>" outline-hide-sublevels)
  ("C-<right>" outline-show-all)
  ;; Move
  ("<up>" outline-previous-visible-heading)
  ("<down>" outline-next-visible-heading)
  ("C-<up>" outline-backward-same-level)
  ("C-<down>" outline-forward-same-level)
  ("M-p" outline-up-heading)
  ;; Quit
  ("q" nil "quit"))

(global-set-key (kbd "C-c y o") #'hydra-outline/body)


;;;;; LATE SETTINGS

;; System dependent settings
(cond ((featurep 'ns)
       ;; Use left Option as Meta on macOS
       ;; (setq mac-option-modifier 'meta)
       ;; Use left Command as Super on macOS
       ;; (setq mac-command-modifier 'super)
       ;; GUI settings
       (when (display-graphic-p)
         ;; Set default font
         (add-to-list 'default-frame-alist '(font . "Hack-14"))
         ;; Increase line spacing
         (setq-default line-spacing 1))
       (message "Late settings macOS"))

      ((or (string-match-p "^brain[0-9]+$" (system-name))
	   (equal (system-name) "endeavour-lxde")
	   (equal (system-name) "kde-neon")
	   (equal (system-name) "manjaro-xfce"))
       (message "Late settings Linux"))

      ((equal (system-name) "penguin")
       (message "Late settings ChromeOS"))

      ((or (string-match-p "^brain[0-9]+-windows$" (system-name))
           (string-match-p "^NT[0-9]\\{3\\}$" (system-name))
           (string-match-p "^TP[0-9]\\{3\\}$" (system-name)))
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
