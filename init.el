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
(cond ((featurep 'ns)   ; macOS
       ;; GUI settings
       (when (display-graphic-p)
         ;; Import path from shell
         (exec-path-from-shell-initialize))
       (message "Early settings for macOS"))

      ((equal (system-name) "penguin")   ; ChromeOS
       (message "Early settings ChromeOS"))

      ((and (eq system-type 'gnu/linux)
            (getenv "WSLENV"))   ; WSL
       ;; Fix cursor color in terminal
       (add-hook 'modus-themes-after-load-theme-hook
                 #'aj8/modus-themes-custom-settings)
       (message "Early settings WSL"))

      ((eq system-type 'gnu/linux)   ; Linux
       (message "Early settings Linux"))

      (t (user-error "Unexpected system-name: %s" system-name)))


;;;;; PACKAGES

;;;; Setup

;;; use-package

;; Install use-package
(if (not (package-installed-p 'use-package))
    (package-install 'use-package))

;; Load use-package
(eval-when-compile
  (require 'use-package))

;; Always install packages if not present
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

;; Report about loading and configuration details
;; (setq use-package-verbose t)

;; Lower time threshold for package load time reporting
(setq use-package-minimum-reported-time 0.001)

;; Add use-package to Imenu
(setq use-package-enable-imenu-support t)

;; Alternative option to prioritize archives
;; (setq use-package-always-pin "melpa-stable")

;; Gather package statistics
;;   (use with use-package-report)
;; (setq use-package-compute-statistics t)

;;; Quelpa

;; Don't update local clone of the MELPA git repository
(setq quelpa-checkout-melpa-p nil)

;; Set upgrade interval
(setq quelpa-upgrade-interval 7)
(add-hook 'after-init-hook #'quelpa-upgrade-all-maybe)

;; Install Quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents
     "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
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

;;; Selected packages
;;;   Install packages with (package-install-selected-packages)
;;;   Remove packages with (package-autoremove)
;;;   Use use-package for configuration only
(customize-set-variable 'package-selected-packages
                        '(all-the-icons
                          all-the-icons-dired
                          auto-package-update
                          benchmark-init
                          cape
                          circadian
                          consult
                          consult-project-extra
                          corfu
                          corfu-doc
                          corfu-doc-terminal
                          corfu-terminal
                          dashboard
                          devdocs
                          devdocs-browser
                          diff-hl
                          diminish
                          dimmer
                          dot-mode
                          ef-themes
                          elfeed
                          embark
                          embark-consult
                          erc-hl-nicks
                          eshell-git-prompt
                          esup
                          eterm-256color
                          exec-path-from-shell
                          expand-region
                          flymake-aspell
                          flymake-json
                          flymake-lua
                          flyspell-correct
                          google-this
                          helpful
                          hydra
                          i3wm-config-mode
                          json-mode
                          keychain-environment
                          keyfreq
                          lorem-ipsum
                          lsp-mode
                          lsp-pyright
                          lsp-treemacs
                          lsp-ui
                          lua-mode
                          magit
                          marginalia
                          markdown-mode
                          modus-themes
                          mosey
                          move-dup
                          multiple-cursors
                          orderless
                          outline-minor-faces
                          php-mode
                          popper
                          powershell
                          rainbow-mode
                          repeat-help
                          rotate
                          smartparens
                          ssh-agency
                          syntax-subword
                          transpose-frame
                          treemacs
                          treemacs-all-the-icons
                          treemacs-magit
                          undo-fu
                          vertico
                          vterm
                          vundo
                          web-mode
                          which-key
                          whole-line-or-region
                          yaml-mode
                          ztree))

;; Install selected packages
(package-install-selected-packages)

;;; Early packages 

;; benchmark-init (benchmarks for require and load calls)
(use-package benchmark-init
  :disabled
  :init
  ;; Start benchmark
  (benchmark-init/activate)
  :config
  ;; Disable collection of benchmark data after init
  (add-hook 'after-init-hook #'benchmark-init/deactivate)
  ;; Configure list format
  (advice-add 'benchmark-init/tabulated-mode :after #'aj8/benchmark-init-list-format))

;; esup (the Emacs StartUp Profiler (ESUP))
;;   TODO: Full of bugs and inactive maintainer
(use-package esup
  :disabled)

(use-package exec-path-from-shell
  :if (equal window-system 'ns))

;;; Admin

;; auto-package-update (automatically update Emacs packages)
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

;;; Buffers

;; dimmer (visually highlight the selected buffer)
(use-package dimmer
  ;; :disabled
  :custom
  ;; What to dim
  ;; (dimmer-adjustment-mode ':both)
  ;; Adjust dimming amount
  (dimmer-fraction 0.25)   ; default is 0.20
  ;; Fix for Modus themes
  (dimmer-use-colorspace :rgb)
  ;; Don't dim these buffers
  ;; (dimmer-buffer-exclusion-regexps "")
  :config
  ;; Don't dim Hydra buffers
  (dimmer-configure-hydra)
  ;; Don't dim Magit transient buffers
  (dimmer-configure-magit)
  ;; Don't dim which-key buffers
  (dimmer-configure-which-key)
  ;; Enable dimmer-mode
  (dimmer-mode 1))

;;; Coding

;; i3wm-config-mode (major-mode for editing i3wm config files)
(use-package i3wm-config-mode
  :defer)

;; json-mode (major-mode for editing JSON files)
(use-package json-mode
  :mode "\\.json$"
  :bind (:map json-mode-map
              ("C-c C-b" . json-mode-beautify)
              ("C-c C-s" . json-snatcher))
  :config
  (add-hook 'json-mode-hook (lambda () (setq-local js-indent-level 2)))
  (unbind-key "C-c C-f" json-mode-map)
  (unbind-key "C-c P" json-mode-map))

;; lua-mode (major-mode for editing Lua scripts)
(use-package lua-mode
  :mode ("\\.lua$" . lua-mode))

;; markdown-mode (major-mode for editing Markdown files)
(use-package markdown-mode
  :mode ("\\.md$"))
  ;; :mode ("\\.md$" . markdown-view-mode))

;; php-mode (major-mode for editing PHP files)
(use-package php-mode
  :mode "\\.php$")

;; powershell (major-mode for editing PowerShell scripts)
(use-package powershell
  ;; :mode ("\\.ps1$" . powershell-mode)
  :defer)

;; web-mode (major-mode for editing web templates)
(use-package web-mode
    :mode "\\.html?$"
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
  :mode "\\.yml$")
  ;; :bind (:map yaml-mode-map
  ;;             ("C-m" . newline-and-indent)))

;; rainbow-mode (colorize color names in buffers)
(use-package rainbow-mode
  :disabled)

;; lsp-mode (language server protocol)
;;   Requirements: npm (nvm)
;;   TODO: optionally enable orderless-flex for LSP completion, see Corfu Wiki
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((sh-mode . lsp)   ; or lsp-deferred
         ;; (html-mode . lsp-deferred)
         ;; (css-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (json-mode . lsp-deferred)
         (lua-mode . lsp-deferred)
         (tex-mode . lsp-deferred)
         (yaml-mode . lsp-deferred)
         ;; which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         ;; Corfu integration
         (lsp-completion-mode . my/lsp-mode-setup-completion))
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          ;; '(flex)))   ; use flex
          '(orderless)))   ; use orderless
  ;; Prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  (which-key-add-key-based-replacements "C-c l" "lsp")
                                        ; add label for prefix key
  :custom
  ;; Use custom completion backend (Corfu)
  (lsp-completion-provider :none)
  ;; Disable snippet support (requires Yanippet)
  (lsp-enable-snippet nil))

;; lsp-ui-mode (UI modules for lsp-mode)
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

;; lsp-treemacs (LSP Treemacs)
(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list)

;; lsp-pyright (Python LSP client using Pyright)
(use-package lsp-pyright
  :commands python-mode
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (require 'lsp-pyright)
                                (lsp-deferred))))  ; or lsp-deferred

;; flymake-aspell (Aspell checker for Flycheck)
;;   Requires: aspell
(use-package flymake-aspell
  :hook (text-mode . flymake-aspell-setup)
  :config
  ;; Don't prompt for saving personal dictionary
  (setq ispell-silently-savep t))

;; flymake-json (a Flymake handler for json using jsonlint)
;;   Requires: jsonlint
(use-package flymake-json
  :hook (json-mode . flymake-json-load))

;; flymake-lua (Flymake for Lua)
;;   Requires: luac
(use-package flymake-lua
  :hook (lua-mode . flymake-lua-load))

;;; Completion

;; consult (consulting completing-read)
(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c c h" . consult-history)
         ("C-c c c" . consult-mode-command)
         ("C-c c m" . consult-minor-mode-menu)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)   ; orig. repeat-complex-command
         ("C-x b" . consult-buffer)              ; orig. switch-to-buffer
         ("C-x B" . consult-project-buffer)
         ("C-x r b" . consult-bookmark)          ; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)        ; orig. abbrev-prefix-mark
                                                 ; (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop)           ; orig. yank-pop
         ("<help> a" . consult-apropos)          ; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)           ; orig. goto-line
         ("M-g M-g" . consult-goto-line)         ; orig. goto-line
         ("M-g o" . consult-outline)             ; alt.: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s R" . consult-recent-file)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)       ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)     ; orig. isearch-edit-string
         ("M-s l" . consult-line)                ; needed by consult-line to
                                                 ; detect Isearch
         ("M-s L" . consult-line-multi)          ; needed by consult-line to
                                                 ; detect Isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)    ; orig. next-matching-history-element
         ("M-r" . consult-history))   ; orig. previous-matching-history-element
  :init
  ;; Enhance `completing-read-multiple'
  ;; (advice-add #'completing-read-multiple :override
  ;;             #'consult-completing-read-multiple)

  ;; Use Consult for completion in region
  ;;   Note, this does not work with LSP or Eglot
  ;; (when (not (display-graphic-p))   ; only enable if using terminal
  ;;   (setq completion-in-region-function #'consult-completion-in-region))

  ;; Configure the register formatting
  ;;   This improves the register preview for `consult-register',
  ;;   `consult-register-load', `consult-register-store' and the Emacs
  ;;   built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Tweak the register preview window
  ;;   This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (which-key-add-key-based-replacements "C-c c" "consult")
                                        ; add label for prefix key

  :config
  ;; Preview key
  ;; (setq consult-preview-key 'any)   ; default
  ;; (setq consult-preview-key (kbd "M-`"))
  ;; (setq consult-preview-key (list (kbd "<down>") (kbd "<up>")))

  ;; Manual preview for expensive commands
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-bookmark
   consult-recent-file consult-xref consult--source-bookmark
   consult--source-recent-file consult--source-project-recent-file
   :preview-key (kbd "M-`"))

  ;; Completion
  ;; (consult-customize
  ;;  consult-completion-in-region
  ;;  :completion-styles '(basic))   ; disable orderless
  ;;  ;; :require-match t)

  ;; Narrowing key
  (setq consult-narrow-key "<"))

;; consult-project-extra (consult integration for project.el)
(use-package consult-project-extra
  :bind ("C-c p" . consult-project-extra-find))

;; corfu (Completion Overlay Region FUnction)
;;   TODO: Use separate matching-style for Corfu and Vertico, e.g. I don't want regexp in Corfu
(use-package corfu
  ;; :if (display-graphic-p)
  ;; :hook (prog-mode . corfu-mode)   ; not needed with corfu-global-mode
  :bind (:map corfu-map
              ;; ("RET" . corfu-quit)   ; pressing Return quits completion
              ;; Navigation using standard bol and eol keybindings
              ([remap move-beginning-of-line] . my/corfu-beginning-of-prompt)
              ([remap move-end-of-line] . my/corfu-end-of-prompt))
  :custom
  ;; (corfu-count 10)               ; maximal number of candidates to show
  ;; (corfu-min-width 15)           ; popup minimum width in characters
  ;; (corfu-max-width 100)          ; popup maximum width in characters."
  ;; (corfu-cycle t)                ; enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ; enable auto completion
  ;; (corfu-auto-prefix 3)          ; min. length of prefix for auto completion."
  ;; (corfu-separator ?\s)          ; orderless field separator
  ;; (corfu-quit-at-boundary t)     ; automatically quit at word boundary
  ;; (corfu-quit-no-match t)        ; automatically quit if there is no match
  ;; (corfu-preview-current nil)    ; disable current candidate preview
  ;; (corfu-preselect-first nil)    ; disable candidate preselection
  ;; (corfu-on-exact-match 'insert)   ; configure handling of exact matches
  ;;   TODO: match not inserted with orderless
  ;; (corfu-echo-documentation nil) ; do not show documentation in the echo area
  (corfu-scroll-margin 1)        ; use scroll margin
  :init
  ;; Enable Corfu globally
  ;;   (this is useful since dabbrev can be used in all buffers)
  (global-corfu-mode)
  ;; Enable Corfu in the minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (bound-and-true-p vertico--input)
      ;; (setq-local corfu-auto nil)   ; enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  :config
  ;; Sort candidates by history
  (corfu-history-mode 1)   ; requires savehist-mode
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; corfu-doc (documentation popup for Corfu)
(use-package corfu-doc
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

;; corfu-terminal (Corfu popup on terminal)
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :after corfu
  :config
  (corfu-terminal-mode 1))

;; corfu-doc-terminal (corfu-doc popup on terminal)
;; (use-package corfu-doc-terminal
;;   :if (not (display-graphic-p))
;;   :after corfu-doc
;;   :config
;;   (corfu-doc-terminal-mode 1))

(quelpa '(corfu-doc-terminal
          :fetcher git
          :url "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"))
(corfu-doc-terminal-mode 1)

;; cape (Completion At Point Extensions)
(use-package cape
  ;; :if (display-graphic-p)
  :after corfu
  :bind (("C-c u p" . completion-at-point)   ; capf
         ("C-c u a" . cape-abbrev)
         ("C-c u d" . cape-dabbrev)          ; or dabbrev-completion
         ("C-c u h" . cape-history)          ; only in shell or minibuffer?
         ("C-c u w" . cape-dict)
         ("C-c u f" . cape-file)
         ("C-c u i" . cape-ispell)
         ("C-c u k" . cape-keyword)
         ("C-c u l" . cape-line)
         ("C-c u s" . cape-symbol))          ; complete symbols everywhere
  :init
  ;; Custom completion at point functions
  (defalias 'cape-ispell+dabbrev+symbol+keyword (cape-super-capf #'cape-ispell
    ;; TODO: symbol completion fails on hyphen when ispell precedes symbol
                                                                 #'cape-dabbrev
                                                                 #'cape-symbol
                                                                 #'cape-keyword)
    "Completion at point function for Cape, combining completions
from ispell, dabbrev, symbol and keyword. Note that the order of
the capf:s matter. Also, cape-file does not merge well with the other
capf:s, see documentation.")
  (defalias 'cape-dabbrev+symbol+keyword+ispell (cape-super-capf #'cape-dabbrev
  ;; TODO: doesnt remember recent candidates
                                                                 #'cape-symbol
                                                                 #'cape-keyword
                                                                 #'cape-ispell)
    "Completion at point function for Cape, combining completions
from dabbrev, symbol, keyword and ispell. Note that the order of
the capf:s matter. Also, cape-symbol must precede cape-ispell
(bug?). Also, cape-file does not merge well with the other
capf:s, see documentation.")
  (defalias 'cape-symbol+keyword+ispell (cape-super-capf #'cape-symbol
                                                         #'cape-keyword
                                                         #'cape-ispell)
    "Completion at point function for Cape, combining completions
from dabbrev, symbol, keyword and ispell. Note that the order of
the capf:s matter. Also, cape-symbol must precede cape-ispell
(bug?). Also, cape-file does not merge well with the other
capf:s, see documentation.")
  ;; Add `completion-at-point-functions', used by `completion-at-point'
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  ;; ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; Use a combination of capf:s instead
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  ;; (add-to-list 'completion-at-point-functions
  ;;              #'cape-dabbrev+symbol+keyword+ispell)
  (add-to-list 'completion-at-point-functions
               #'cape-symbol+keyword+ispell)
  (which-key-add-key-based-replacements "C-c u" "corfu/cape"))
                                        ; add label for prefix

;; embark (conveniently act on minibuffer completions)
(use-package embark
  :bind (("M-." . embark-act)
         ("M-," . embark-dwim)
         ("C-h B" . embark-bindings)) ; alternative for `describe-bindings'
  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :custom
  ;; Hide some actions
  (embark-verbose-indicator-excluded-actions
   '("\\(local\\|global\\)-set-key" ".*debug-.*" "elp-.*" ".*trace-.*"
     embark-history-remove))
  :config
  ;; Hide the mode line of the embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; embark-consult (Consult integration for Embark)
(use-package embark-consult
  :demand t ; only necessary if you have the hook below
  :after (embark consult)
  ;; if you want to have consult previews as you move around an
  ;; auto-updating Embark collect buffer
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; orderless (completion style for matching regexps in any order)
(use-package orderless
  ;; :disabled
  :config
  ;; Matching styles
  ;; (setq orderless-matching-styles '(orderless-literal orderless-regexp))
  ;;                                       ; default
  (setq orderless-matching-styles
        '(orderless-literal orderless-regexp)
        orderless-style-dispatchers '(aj8/orderless-dispatch-flex-if-twiddle
                                      aj8/orderless-dispatch-literal-if-equal
                                      aj8/orderless-dispatch-prefixes-if-less
                                      aj8/orderless-dispatch-regexp-if-star
                                      aj8/orderless-dispatch-without-if-bang)))

;; vertico (VERTical Interactive COmpletion)
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
  (put 'minibuffer-hide-completions 'interactive-form '(interactive)))

;;; Editing

;; dot-mode (minor mode to repeat typing or commands)
(use-package dot-mode
  :diminish
  :hook ((prog-mode . dot-mode-on)
         (text-mode . dot-mode-on))
  :bind (:map dot-mode-map ("C-c d" . dot-mode-execute))
  :config
  (setq dot-mode-verbose t)
  (unbind-key "C-." dot-mode-map)
  (unbind-key "C-c ." dot-mode-map)
  (unbind-key "C-M-." dot-mode-map))

;; lorem-ipsum (insert dummy pseudo latin text)
(use-package lorem-ipsum
  :defer)

;; move-dup (Eclipse-like moving and duplicating lines or rectangles)
(use-package move-dup
  :bind (("C-c <up>" . move-dup-move-lines-up)
         ("C-c C-<up>" . move-dup-duplicate-up)
         ("C-c <down>" . move-dup-move-lines-down)
         ("C-c C-<down>" . move-dup-duplicate-down)))

;; multiple-cursors (multiple cursors for Emacs)
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

;; repeat-help (display keybindings for repeat-mode)
(use-package repeat-help
  :hook (repeat-mode . repeat-help-mode)
  :custom
  (repeat-help-auto t))

;; smartparens (automatic insertion, wrapping and paredit-like navigation)
;;   TODO: Investigate smartparens-strict-mode-map, e.g M-d and C-w changes behaviour
;;           - M-d done! TODO: how to create sp-kill-word that doesn't kill symbol? 
;;   TODO: create a smart wrapper for sp-down/up-sexp that knows whether to go forward or backward
(use-package smartparens
  :diminish
  ;; :hook (emacs-lisp-mode smartparens-mode)
  :hook (prog-mode . smartparens-mode)
  :bind (:map smartparens-mode-map
              ;; Base set                                ; defaults:
              ;; ("C-M-f" . sp-forward-sexp)             ; forward-sexp
              ("C-M-<right>" . sp-forward-sexp)
              ;; ("C-M-b" . sp-backward-sexp)            ; backward-sexp
              ("C-M-<left>" . sp-backward-sexp)
              ("C-M-n" . sp-down-sexp)                   ; forward-list
              ;; ("C-M-a" . sp-backward-down-sexp)       ; use M-- C-M-<down>
              ("C-M-a" . sp-beginning-of-sexp)           ; beginning-of-defun
              ("C-M-e" . sp-end-of-sexp)                 ; end-of-defun
              ;; ("C-M-e" . sp-up-sexp)                  ; use M-- C-M-<up>
              ("C-M-p" . sp-backward-up-sexp)            ; backward-list
              ;; ("C-M-n" . sp-next-sexp)                ; forward-list
              ;; ("C-M-p" . sp-previous-sexp)            ; backward-list
              ("C-M-k" . sp-kill-sexp)                   ; kill-sexp
              ("C-M-w" . sp-copy-sexp)                   ; append-next-kill
              ("C-c s ]" . sp-unwrap-sexp)
              ("C-]" . sp-forward-slurp-sexp)            ; abort-recursive-edit
              ("C-c s [" . sp-backward-unwrap-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ;; ("C-[" . sp-backward-slurp-sexp)        ; ESC-prefix
                                                         ; use M-- C-]
              ("C-{" . sp-backward-barf-sexp)
              ("C-c s s" . sp-splice-sexp)
              ;; ("C-]" . sp-select-next-thing-exchange) ; abort-recursive-edit
              ("C-M-]" . sp-select-next-thing)
              ("C-M-SPC" . sp-mark-sexp)                 ; mark-sexp
              ("M-F" . sp-forward-symbol)   ; remove?
              ("M-B" . sp-backward-symbol)   ; remove?
              ;; Other
              ("C-M-<backspace>" . sp-backward-kill-sexp)
              ;; ("C-k" . sp-kill-hybrid-sexp)        ; kill-line
                                        ; (this is used anyway,
                                        ; with strict mode)
              ("C-c s a" . sp-absorb-sexp)
              ("C-c s e" . sp-emit-sexp)
              ("C-c s c" . sp-convolute-sexp)        ; alternative: C-M-c
                                                     ; (exit-recursive-edit)
              ("C-c s j" . sp-join-sexp)             ; alternative: C-M-j
                                                     ; (default-indent-new-line)
              ("C-c s r" . sp-rewrap-sexp)           ; alternative: C-M-r
              ("C-c s t" . sp-transpose-sexp)        ; alternative: C-M-t
                                                     ; (transpose-sexp)
              ("C-c s u" . sp-transpose-hybrid-sexp)
              ("C-c s w" . sp-swap-enclosing-sexp)
              ("C-c s x" . sp-extract-before-sexp)   ; alternative: C-M-x
              ("C-c s y" . sp-extract-after-sexp)    ; alternative: C-M-y
              ;; Wrappers
              ("C-c (" . sp-wrap-round)
              ("C-c [" . sp-wrap-square)
              ("C-c {" . sp-wrap-curly)
              ;; ("C-c ("  . wrap-with-parens)
              ;; ("C-c ["  . wrap-with-brackets)
              ;; ("C-c {"  . wrap-with-braces)
              ("C-c '"  . wrap-with-single-quotes)   ; TODO: Fix
              ("C-c \"" . wrap-with-double-quotes)
              ("C-c `"  . wrap-with-back-quotes)
              :map smartparens-strict-mode-map
              ([remap kill-word] . nil)           ; unbind sp-kill-word
              ([remap backward-kill-word] . nil)) ; unbind sp-backward-kill-word
  :init
  (which-key-add-key-based-replacements "C-c s" "smartparens")
  :custom
  ;; Use default keybindings
  ;; (sp-base-key-bindings 'sp)
  ;; Override some default keybindings
  ;; (sp-override-key-bindings '())
  ;; Enforce that pairs are always balanced
  (smartparens-global-strict-mode t)
  ;; Don't consider symbols and strings as expressions
  ;; (sp-navigate-consider-symbols nil)   ; WARNING: deprecated
  :config
  ;; Use default config
  (require 'smartparens-config))
  ;; Use smartparens globally
  ;; (smartparens-global-mode 1))

;; whole-line-or-region (operate on current line if region undefined)
(use-package whole-line-or-region
  :defer 1
  :diminish whole-line-or-region-local-mode
  :config
  ;; Use whole-line-or-region-mode everywhere
  (whole-line-or-region-global-mode 1))

;; undo-fu (undo helper with redo)
;;   Note that undo-in-region is disabled by default
(use-package undo-fu
  :bind (("C-c z u" . undo-fu-only-undo)
         ("C-c z r" . undo-fu-only-redo)
         ("C-c z d" . undo-fu-disable-checkpoint))
  :init
  (which-key-add-key-based-replacements "C-c z" "undo-fu")
                                        ; add label for prefix key
  :custom
  (undo-fu-ignore-keyboard-quit t))

;; vundo (visual undo tree)
(use-package vundo
  :commands (vundo)
  :bind ("C-c v" . vundo)
  :custom
  ;; Use compact layout
  (vundo-compact-display t)
  ;; Use pretty Unicode characters
  (vundo-glyph-alist vundo-unicode-symbols))

;;; Files

;; treemacs (a tree style file explorer package)
(use-package treemacs
  :commands treemacs
  :init
  (which-key-add-key-based-replacements "C-c t" "treemacs")
                                        ; add label for prefix keys
  (which-key-add-key-based-replacements "C-c C-p" "treemacs")
  (which-key-add-key-based-replacements "C-c C-w" "treemacs")
  :init
  ;; No delay when switching projects
  (add-hook 'treemacs-project-follow-mode-hook
            (lambda () (setq treemacs--project-follow-delay 0)))
  :custom
  (treemacs-find-workspace-method 'find-for-file-or-manually-select)
  ;; (treemacs-indentation-string " ")
  ;; (treemacs-is-never-other-window nil)
  ;; (treemacs-no-delete-other-windows t)
  ;; (treemacs-project-follow-cleanup nil)
  ;; (treemacs-persist-file (expand-file-name ".cache/(treemacs-persist"
  ;;                                          user-emacs-directory)))
  ;; (treemacs-position 'left)
  ;; (treemacs-litter-directories '("/node_modules" "/.venv" "/.cask"))
  ;; (treemacs-show-hidden-files t)
  ;; (treemacs-sorting 'alphabetic-asc)
  (treemacs-width 30)
  :config
  ;; Follow current buffer file in Treemacs window (on by default)
  ;; (setq treemacs-follow-mode 1)
  ;; Only display current project in Treemacs window
  (setq treemacs-project-follow-mode 1)
  ;; Show visual indicator in the fringe for highlighted file
  (setq treemacs-fringe-indicator-mode 'always)
  ;; Detect file system changes (on by default)
  ;; (setq treemacs-filewatch-mode 1)
  ;; Hide files ignored by Git
  ;; (setq treemacs-hide-gitignored-files-mode 1)
  ;; Display indentation guides
  (setq treemacs-indent-guide-mode 1)
  ;; Display git project annotations
  (setq treemacs-git-commit-diff-mode 1)
  ;; Highlight files using git status
  (setq treemacs-git-mode 'deferred)
  :bind (("M-0" . treemacs-select-window)
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
         ("C-c C-p <up>" . treemacs-move-project-up)
         ("o l" . treemacs-visit-node-in-least-recently-used-window)))

;; treemacs-all-the-icons (all-the-icons integration for Treemacs)
(use-package treemacs-all-the-icons
  :if (display-graphic-p)
  :defer)

;; treemacs-icons-dired (Treemacs icons for Dired)
;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once))

;; treemacs-magit (Magit integration for Treemacs)
(use-package treemacs-magit
  :defer
  :after (treemacs magit))

;;; Help

;; helpful (a better *help* buffer)
(use-package helpful
  ;; :disabled
  ;; :demand
  :commands (helpful-key helpful-function helpful-symbol
             helpful-variable helpful-command)
  :bind (([remap describe-key] . helpful-key)
         ([remap describe-function] . helpful-function)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ("C-c h" . helpful-at-point))
  :custom
  ;; Maximum number of *helpful* buffers
  (helpful-max-buffers nil))

;; marginalia (enrich existing commands with completion annotations)
(use-package marginalia
  :demand
  :bind (:map minibuffer-local-map ("M-m" . marginalia-cycle))
  :config
  (marginalia-mode 1))

;; which-key (display available keybindings in popup)
;;   TODO: which-key buffer overlaps with bottom side-window buffer
(use-package which-key
  :diminish
  :custom
  ;; Preserve window configuration
  (which-key-preserve-window-configuration t)
  ;; Max window height
  ;;   (default is 0.25)
  (which-key-side-window-max-height 0.5)
  ;; Max description length
  ;;   (default is 27)
  (which-key-max-description-length #'aj8/which-key-description-length)
  ;; Min description length
  ;;   (see :config section)
  ;; Column padding
  ;;   (default is 0)
  (which-key-add-column-padding 1)
  ;; Delay (default is 1.0 s)
  ;; (which-key-idle-delay 10000)
  (which-key-idle-delay 0.75)
  ;; Secondary delay (default is nil)
  (which-key-idle-secondary-delay 0.05)
  ;; Display remapped commands
  (which-key-compute-remaps t)
  ;; Don't show which-key buffer on C-h
  (which-key-use-C-h-commands nil)
  :config
  ;; Min description length
  ;;   (same as max length; default is 0)
  (customize-set-variable 'which-key-min-column-description-width
                          (eval (aj8/which-key-description-length
                                 (cdr (which-key--side-window-max-dimensions)))))
  (which-key-mode 1))

;; devdocs (Emacs viewer for DevDocs)
;;   Usage: M-x devdocs-install
(use-package devdocs
  :bind ("C-c H d" . devdocs-lookup)
  :custom
  ;; Select devdocs window
  (devdocs-window-select t)
  :config
  (add-hook 'yaml-mode-hook
            (lambda () (setq-local devdocs-current-docs '("ansible"))))
  (add-hook 'sh-mode-hook
            (lambda () (setq-local devdocs-current-docs '("bash"))))
  (add-hook 'latex-mode-hook
            (lambda () (setq-local devdocs-current-docs '("latex"))))
  (add-hook 'lua-mode-hook
            (lambda () (setq-local devdocs-current-docs '("lua~5.4"))))
  (add-hook 'python-mode-hook
            (lambda () (setq-local devdocs-current-docs '("python~3.10")))))

;; devdocs-browser (browse devdocs.io documents using EWW)
;;   Usage: M-x devdocs-browser-install-doc
(use-package devdocs-browser
  :bind ("C-c H D" . devdocs-browser-open)
  ;; :custom
  ;; ;; Select devdocs window
  ;; (devdocs-window-select t)
  :custom
  (devdocs-browser-major-mode-docs-alist '((yaml-mode "Ansible")
                                           (sh-mode "Bash")
                                           (emacs-lisp-mode "Elisp")
                                           (latex-mode "LaTeX")
                                           (lua-mode "Lua")
                                           (python-mode "Python"))))

;;; Navigation

;; mosey (mosey around your buffers)
(use-package mosey
  :bind (("C-a" . aj8/mosey-bol)
         ("C-e" . aj8/mosey-eol))
  :init
  ;; Function list for beginning of line
  (defmosey '(beginning-of-line
              back-to-indentation)
    :prefix "bol")
  ;; Function list for end of line
  (defmosey '(mosey-goto-end-of-code
              aj8/goto-beginning-of-comment
              mosey-goto-beginning-of-comment-text
              end-of-line)
    :prefix "eol"))

;; syntax-subword (make operations on words more fine-grained)
(use-package syntax-subword
  :disabled
  :custom
  ;; Don't stop on spaces
  (syntax-subword-skip-spaces t)
  :config
  ;; Use syntax-subword-mode everywhere
  (global-syntax-subword-mode 1))

;;; Outline

;; outline-minor-faces (headings faces for outline-minor-mode)
;;   See also outline-minor-mode-highlight.
(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook #'outline-minor-faces-mode))

;;; Search

;;; Selection

;; expand-region (increase selected region by semantic units)
(use-package expand-region
  :bind (("C-c =" . er/expand-region)
         ("C-c -" . er/contract-region)))

;;; Spelling

;; flyspell-correct (correcting words with Flyspell via custom interface)
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ;; ("C-;" . flyspell-auto-correct-word)
              ("C-;" . flyspell-correct-wrapper)
              ("C-," . (lambda () (interactive)
                         (aj8/call-interactively-wih-prefix-toggle
                          #'flyspell-correct-previous)))
              ("C-." . (lambda () (interactive)
                         (aj8/call-interactively-wih-prefix-toggle
                          #'flyspell-correct-next)))
              ("C-c ," . my/flyspell-goto-previous-error)
              ("C-c ." . flyspell-goto-next-error)
              ("C-c ;" . flyspell-auto-correct-word)))

;;; Terminal

;; term (terminal-emulator) - [built-in package]
(use-package term
  :disabled
  :ensure nil   ; don't install built-in packages
                ; see https://github.com/jwiegley/use-package/issues/977
  :commands term)
  ;; :config
  ;; Match the default Bash shell prompt
  ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;; eterm-256color (customizable 256 colors for term)
(use-package eterm-256color
  :disabled
  :after term
  :hook (term-mode . eterm-256color-mode))

;; vterm (fully-featured terminal emulator)
(use-package vterm
  ;; TODO: Fix M-f, M-b, M-d (note, recent breaking changes were
  ;;       applied to emacs-livterm project)
  :commands vterm
  :config
  ;; Match the default Bash shell prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;; eshell (the Emacs command shell) - [built-in package]
(use-package eshell
  :ensure nil   ; don't install built-in packages
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config
  ;; Set Eshell options here
  ;; (with-eval-after-load 'esh-opt
  ;;   (setq eshell-visual-commands '("htop" "zsh" "vim")))
  ;; Add Tramp support
  (with-eval-after-load "eshell"
    (add-to-list 'eshell-modules-list 'eshell-tramp)))

;; eshell-git-prompt (some Eshell prompt for Git users)
(use-package eshell-git-prompt
  :if (display-graphic-p)
  :after eshell
  :config
  (when (display-graphic-p)
    (eshell-git-prompt-use-theme 'powerline)))

;;; Theme

;; dashboard (a startup screen extracted from Spacemacs)
(use-package dashboard
  :disabled  ; 0.3s startup time
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

;; all-the-icons (a library for inserting developer icons)
;;   Usage: M-x all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p))

;; all-the-icons-dired (shows icons for each file in dired mode)
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; modus-themes (elegant, highly legible and customizable themes) - [built-in package]
(use-package modus-themes
  ;; :disabled
  :if (and (eq system-type 'gnu/linux)   ; WSL
           (getenv "WSLENV"))
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
  (modus-themes-load-themes)
  :config
  ;; Load the theme
  (if (aj8/daytime-p)
      (modus-themes-load-operandi)
    (modus-themes-load-vivendi)))

;; ef-themes (colorful and legible themes)
(use-package ef-themes
  ;; Themes:
  ;;   Light: `ef-day', `ef-light', `ef-duo-light', `ef-spring', `ef-summer'.
  ;;   Dark:  `ef-night', `ef-dark', `ef-duo-dark', `ef-autumn', `ef-winter'.
  ;;   All the themes are included in the variable `ef-themes-collection'.
  ;; Commands:
  ;;   `ef-themes-toggle'
  ;;   `ef-themes-select'
  ;;   `ef-themes-load-random'
  ;;   `ef-themes-preview-colors'
  ;;   `ef-themes-preview-colors-current'
  ;; :disabled
  :unless (and (eq system-type 'gnu/linux)   ; WSL
               (getenv "WSLENV"))
  :bind ("<f5>" . ef-themes-toggle)
  ;; Make customizations that affect Emacs faces before loading a theme
  :init
  (setq ef-themes-headings
        '((0 . (ultrabold))
          (1 . (extrabold))
          (2 . (bold))
          (3 . (semibold))
          (t . (regular))))
  ;; Disable all other themes to avoid awkward blending
  (mapc #'disable-theme custom-enabled-themes)
  ;; Load theme
  (if (aj8/daytime-p)
      ;; (load-theme 'ef-light :no-confirm)
      (load-theme 'ef-duo-light :no-confirm)
    ;; (load-theme 'ef-dark :no-confirm))
    (load-theme 'ef-duo-dark :no-confirm))
  :config
  ;; Specify themes for ef-themes-toggle command
  ;; (setq ef-themes-to-toggle '(ef-light ef-dark)))
  (setq ef-themes-to-toggle '(ef-duo-light ef-duo-dark)))

;; circadian (theme-switching based on daytime)
(use-package circadian
  ;; :disabled  ; 0.3s startup time
  :defer 60
  :after (:any modus-themes ef-themes)
  :config
  (if (and (eq system-type 'gnu/linux)   ; WSL
           (getenv "WSLENV"))
      ;; (circadian-themes '(("8:00" . modus-operandi)
      ;;                     ("18:00"  . modus-vivendi)))
      (customize-set-variable 'circadian-themes '((:sunrise . modus-operandi)
                                                 (:sunset  . modus-vivendi)))
    ;; (customize-set-variable 'circadian-themes '((:sunrise . ef-light)
    ;;                                            (:sunset  . ef-dark)))
    (customize-set-variable 'circadian-themes '((:sunrise . ef-duo-light)
                                               (:sunset  . ef-duo-dark))))
  (circadian-setup))

;;; Version control

;; magit (a Git porcelain inside Emacs)
(use-package magit
  :bind (("C-c g" . magit-file-dispatch)
         :map magit-mode-map
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
  ;; Set alternative display function
  ;; (magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  (magit-display-buffer-function 'display-buffer)
  ;; Clone command is transient
  (magit-clone-always-transient t)
  ;; Github mapping for user sonofjon
  ;;   Clone with "gh:repository"
  (magit-clone-name-alist '(("\\`\\(?:github:\\|gh:\\)?\\([^:]+\\)\\'"
                             "github.com" "sonofjon")))
  (magit-wip-mode-lighter "")
  :config
  ;; Back up uncommitted changes
  (magit-wip-mode 1)
  ;; Disable hl-line-mode
  (add-hook 'magit-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  ;; Add status flag to repository list
  (add-to-list 'magit-repolist-columns
               '("Flag" 4 magit-repolist-column-flag (:right-align t)))
  ;; Hide header section by default
  (add-to-list 'magit-section-initial-visibility-alist '(headers . hide))
  ;; Hide commit message section by default
  (add-to-list 'magit-section-initial-visibility-alist '(commit-message . hide))
  ;; Hide diffstat section by default
  (add-to-list 'magit-section-initial-visibility-alist '(diffstat . hide)))

;; diff-hl (highlight uncommitted changes using VC)
(use-package diff-hl
  :disabled
  :config
  ;; Integration with Magit
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  ;; Put highlights in the margin in terminal
  (when (display-graphic-p)
    (setq diff-hl-margin-mode t))
  ;; Use diff-hl-mode everywhere
  (global-diff-hl-mode 1))

;; ztree (text mode directory tree)
(use-package ztree
  :defer
  :custom
  ;; Use pretty Unicode art
  (ztree-draw-unicode-lines t))
  ;; Customize file filter (default is all dot-files) 
  ;; (setq-default ztree-diff-filter-list
  ;;               (cons \"^.*\\.pyc\" ztree-diff-filter-list)))

;;; Windows

;; popper (summon and dismiss buffers as popups)
;;   TODO: popper commands don't work well
(use-package popper
  :demand   ; note that :demand and :after can be combined
            ; (ensures popper is required, not auto-loaded, after project)
  :after project   ; needed for popper-group-by-project
  :bind (("C-`"   . popper-toggle-latest)
         ;; ("M-`"   . popper-cycle)   ; TODO: conflicts with consult
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
   '("\\*\\(Messages\\|Warnings\\)\\*"
     "\\*\\(Async-native-compile-log\\|.*ls.*\\|quelpa-build-checkout\\)\\*"
     "\\*\\(Bookmark List\\|Embark Collect:.*\\|Occur\\|.*Output\\|Semantic SymRef\\|devdocs\\|package update results\\)\\*"
     "magit.*"
     backtrace-mode
     compilation-mode
     dired-mode
     help-mode
     helpful-mode
     Info-mode
     tabulated-list-mode
     "\\*eww.*\\*"
     "\\*Apropos\\*"
     "\\*\\(Man\\|WoMan\\).*\\*"
     "^\\*eshell.*\\*$" eshell-mode
     "^\\*shell.*\\*$" shell-mode
     "^\\*term.*\\*$" term-mode
     "\\*tex-shell\\*"))
     ;; (completion-list-mode . hide)))
  ;; Group by project
  (popper-group-function #'popper-group-by-project)
  ;; No display control (use display-buffer-alist)
  (popper-display-control nil)
  ;; Disable mode-line
  ;; (popper-mode-line nil)
  :config
  ;; Enable popper
  (popper-mode 1)
  ;; Show echo area hints
  (popper-echo-mode 1))

;; rotate (rotate the layout of Emacs)
(use-package rotate
  :disabled)

;; transpose-frame (transpose window arrangement in a frame)
(use-package transpose-frame
  :defer)

;;; Web

;; elfeed (an Emacs Atom/RSS feed reader)
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

;; erc (an Emacs internet relay chat client) - [built-in package]
(use-package erc
  :disabled
  :ensure nil   ; don't install built-in packages
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

;; erc-hl-nicks (ERC nick highlighter)
(use-package erc-hl-nicks
  :disabled
  :defer
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks)
  (erc-update-modules))

;; google-this (a set of functions and bindings to google under point)
(use-package google-this
  :diminish
  :bind-keymap ("C-c /" . google-this-mode-map)
  :init
  (which-key-add-key-based-replacements "C-c /" "google-this")
                                        ; add label for prefix key
  :config
  (google-this-mode 1))

;;; Other

;; diminish (diminished modes are minor modes with no modeline display)
(use-package diminish
  :config
  ;; Pre-loaded modes
  (diminish 'eldoc-mode)
  ;; Not pre-loaded modes
  (add-hook 'auto-revert-mode-hook (lambda () (diminish 'auto-revert-mode)))
  (add-hook 'abbrev-mode-hook (lambda () (diminish 'abbrev-mode)))
  (add-hook 'visual-line-mode-hook (lambda () (diminish 'visual-line-mode))))

;; hydra (make bindings that stick around)
(use-package hydra
  :defer)

;; keyfreq (track command frequencies)
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

;; ssh-agency (manage ssh-agent from Emacs)
(use-package ssh-agency
  :if (display-graphic-p))

;; keychain-environment (load keychain environment variables)
(use-package keychain-environment
  :disabled
  :if (display-graphic-p)
  :config
  (keychain-refresh-environment))


;;;;; CUSTOMIZATION

;;;; Variables

;;; Admin

;; Natively compile packages during installation
;; (setq package-native-compile t)

;;; Buffers

;; Hide buffer list at startup when loading multiple files
(setq inhibit-startup-buffer-menu t)

;; Do not switch to buffers already shown
(setq switch-to-prev-buffer-skip 'this)

;; Skip some buffers when switching buffers
;; (setq switch-to-prev-buffer-skip 'aj8/buffer-skip-p)

;; Allow minibuffer commands while in the minibuffer
(setq enable-recursive-minibuffers t)

;; Timeout for messages in active minibuffer
(setq minibuffer-message-clear-timeout 1)

;; Open *info* buffers in same window
(setq info-lookup-other-window-flag nil)

;; Reuse existing help window
;; (setq help-window-select t)

;; Reuse dired buffers
(setf dired-kill-when-opening-new-dired-buffer t)

;; Kill customize group windows
(setq custom-buffer-done-kill t)

;; Additional variables to persist between sessions
(add-to-list 'savehist-additional-variables 'kill-ring)
(add-to-list 'savehist-additional-variables 'global-mark-ring)

;;; Coding

;; Open up the debugger on error
;; (setq debug-on-error t)

;;; Completion

;; Select completion styles
(setq completion-styles '(substring orderless basic))
                                       ; substring: needed for partial completion
                                       ; orderless: space-separated components
                                       ; basic: fallback

;; Use partial completion for files
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles basic partial-completion))))

;; Use TAB for symbol completion (after indentation)
(setq tab-always-indent 'complete)

;; Show more details for completions
(setq completions-detailed t)

;; Hide commands which do not apply to the current mode
;; (setq read-extended-command-predicate #'command-completion-default-include-p)

;; Let Dabbrev searches be case sensitive
(setq dabbrev-case-fold-search nil)

;; Let hippie-expand search for line expansions in all buffers
;; (add-to-list 'hippie-expand-try-functions-list 'try-expand-line-all-buffers t)
(setcar (nthcdr 5 hippie-expand-try-functions-list) 'try-expand-line-all-buffers)

;; Ignore some buffers with hippie-expand
;;   TODO: only consider buffers with the same mode (see
;;         dabbrev-friend-buffer-function)
(with-eval-after-load "hippie-exp"
  (add-to-list 'hippie-expand-ignore-buffers "^\\*.*\\*$")
  (add-to-list 'hippie-expand-ignore-buffers "magit:.*")
  (add-to-list 'hippie-expand-ignore-buffers aj8/buffer-skip-regexp))

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

;;; Files

;; Custom listing style in dired
;; (setq dired-listing-switches "-agho --group-directories-first")
(setq dired-listing-switches "-agho")   ; macOS version

;;; Help

;; More extensive apropos commands
(setq apropos-do-all t)

;;; Outline

;; Outline minor mode prefix
(which-key-add-key-based-replacements "C-c @" "outline")
                                        ; add label for prefix key

;; Use TAB and S-TAB for cycling
(setq outline-minor-mode-cycle t)

;; Highlight headings
;;   See also outline-minor-faces.
;; (setq outline-minor-mode-highlight t)   ; alternatives: 'override and 'append

;;; Navigation

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
(setq scroll-preserve-screen-position t)

;; Use "repeat-mode" for "pop-mark"
(setq set-mark-command-repeat-pop t)

;; Auto-save bookmarks
(setq bookmark-save-flag 1)

;;; Search

;; Don't search invisible text by default
;; (setq isearch-invisible nil)

;; Disable lax-whitespace search by default
;; (setq isearch-lax-whitespace nil)

;; Interpret spaces as wildcards (with M-s SPC)
(setq search-whitespace-regexp ".*?")

;; Allow movement between Isearch matches by cursor motion commands
(setq isearch-allow-motion t)
(setq isearch-motion-changes-direction t)

;;; Selection

;;; Spelling

;; Configure language environment
;; (setenv "LANG" "en_US.UTF-8")

;; Use aspell
;;   Requirements: aspell
(setq ispell-program-name "aspell")   ; this is already the default

;; Set language
(setq ispell-dictionary "en_US")

;; Set aspell suggestion mode
(setq ispell-extra-args '("--sug-mode=ultra"))
;; (setq ispell-extra-args '("--sug-mode=fast"))
;; (setq ispell-extra-args '("--sug-mode=normal"))

;; Enable flyspell in web-mode
(put 'web-mode 'flyspell-mode-predicate #'my/web-mode-flyspell-verify)

;;; Terminal

;;; Theme

;; Set display-line-number-width automatically
(setq display-line-numbers-width-start t)

;; Do not display continuation lines
;; (setq-default truncate-lines t)

;;; Version control

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Use horizontal (side-by-side) view by default in ediff
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-merge-split-window-function #'split-window-horizontally)

;; Let ediff use existing frame in GUI
(when (display-graphic-p)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;; Web

;; URL browser settings
(setq browse-url-browser-function #'browse-url-default-browser)
(setq browse-url-secondary-browser-function #'eww-browse-url)
;; (setq browse-url-handlers
;;       '(("github\\.com" . browse-url-default-browser)
;;         ("gitlab\\.com" . browse-url-default-browser)
;;         ("google\\.com" . browse-url-default-browser)
;;         ("reddit\\.com" . browse-url-default-browser)
;;         ("stackexchange\\.com" . browse-url-default-browser)
;;         ("stackoverflow\\.com" . browse-url-default-browser)
;;         ("." . eww-browse-url)))

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

;;; Windows

;; Don't bind keys for winner
(setq winner-dont-bind-my-keys t)

;; Prefer horizontal (side-by-side) window splitting
;;   Note: the thresholds need to be twice as big as the smallest
;;   window allowed, because the new windows each use half of the
;;   former window size
(setq split-width-threshold nil
      split-height-threshold 0)

;; Enable horizontal window fitting
;; (setq fit-window-to-buffer-horizontally t)

;; Resize window combinations proportionally
;; (setq window-combination-resize t)

;; Try to even window sizes vertically only
(setq even-window-sizes 'height-only)

;; Allow switching to buffer in strongly dedicated windows
;; (setq switch-to-buffer-in-dedicated-window 'pop)

;; Top and bottom side windows occupy full frame width (default)
(setq window-sides-vertical nil)

;; Left and right side windows occupy full frame height
;; (setq window-sides-vertical t)

;; Set maximum number of side-window slots
;; (setq window-sides-slots '(1 1 3 1))

;; Window rules
;;   TODO: Buffers that are not displayed, e.g. quelpa-build-checkout
;;   and *Compile-Log*, do not seem to be affected by
;;   display-buffer-alist, such that these buffers appear in the main
;;   buffer when applying next-buffer.
(setq display-buffer-alist
      `(;;
        ;; Example using mp-make-display-buffer-matcher-function
        ;;
        ;; (,(make-display-buffer-matcher-function '(magit-mode))
        ;;  (display-buffer-in-side-window))
        ;;
        ;; No window
        ;;
        ;; ("\\*Async Shell Command\\*"
        ;;  (display-buffer-no-window))
        ;;
        ;; Top side window
        ;;
        ("\\*\\(Messages\\|Warnings\\)\\*"
         (display-buffer-in-side-window)
         (window-height . ,aj8/side-window-height)
         (side . top)
         (window-parameters . ((no-delete-other-windows . t))))
        ;; ("\\*\\(Native-compile-Log\\)\\*"
        ("\\*\\(Async-native-compile-log\\|lsp-log\\|.*-ls\\(::.*\\)?\\|quelpa-build-checkout\\)\\*"
         (display-buffer-in-side-window)
         (window-height . ,aj8/side-window-height)
         (side . top)
         (window-parameters . ((no-delete-other-windows . t))))
        ;; ((or . ((derived-mode-p . backtrace-mode)
        ;;         (derived-mode-p . compilation-mode)
        ((lambda (buffer _alist)
           (with-current-buffer buffer (or (derived-mode-p 'backtrace-mode)
                                           (derived-mode-p 'compilation-mode))))
         (display-buffer-in-side-window)
         (window-height . ,aj8/side-window-height)
         (side . top)
         (window-parameters . ((no-delete-other-windows . t))))
        ;; ("\\*Flymake diagnostics.*\\*"
        ;;  (display-buffer-in-side-window)
        ;;  (window-height . ,aj8/side-window-height)
        ;;  (side . top)
        ;;  (window-parameters . ((no-delete-other-windows . t))))
        ("magit-process:.*"
         (display-buffer-in-side-window)
         (window-height . ,aj8/side-window-height)
         (side . top)
         (window-parameters . ((no-delete-other-windows . t))))
        ;;
        ;; Right side window
        ;;
        ;;   Magit
        ;;
        ("\\(magit:\\|magit-log.*:\\|magit-reflog:\\|magit-log-select:\\).*"
         (display-buffer-in-side-window)
         (window-width . ,aj8/side-window-width-dynamic)
         (side . right)
         (slot . -1)
         (window-parameters . ((no-delete-other-windows . t))))
        ("\\(magit-diff:\\|magit-revision:\\|magit-stash:\\).*"
         (display-buffer-in-side-window)
         (window-width . ,aj8/side-window-width-dynamic)
         (side . right)
         (slot . 1)
         (window-parameters . ((no-delete-other-windows . t))))
        ("COMMIT_EDITMSG"
         (display-buffer-in-side-window)
         (window-width . ,aj8/side-window-width-dynamic)
         (side . right)
         (slot . -1)
         (window-parameters . ((no-delete-other-windows . t))))
        ;;
        ;;   Browser
        ;;
        ;; TODO: doesn't work initially
        ("\\*eww:.*\\*"
         (display-buffer-in-side-window)
         (window-width . ,aj8/side-window-width-dynamic)
         (side . right)
         (slot . 1)
         (window-parameters . ((no-delete-other-windows . t))))
        ;;
        ;;   Help
        ;;
        ("\\*Apropos\\*"
         (display-buffer-in-side-window)
         (window-width . ,aj8/side-window-width-dynamic)
         (side . right)
         (slot . 1)
         (window-parameters . ((no-delete-other-windows . t))))
        ("\\*\\(Help\\|helpful.*\\)\\*"
         (display-buffer-in-side-window)
         (window-width . ,aj8/side-window-width-dynamic)
         (side . right)
         (slot . 1)
         (window-parameters . ((no-delete-other-windows . t))))
        ("\\*info.*\\*"
         (display-buffer-in-side-window)
         (window-width . ,aj8/side-window-width-dynamic)
         (side . right)
         (slot . 1)
         (window-parameters . ((no-delete-other-windows . t))))
        ("\\*\\(Man\\|WoMan\\).*\\*"
         (display-buffer-in-side-window)
         (window-width . ,aj8/side-window-width-dynamic)
         (side . right)
         (slot . 1)
         (window-parameters . ((no-delete-other-windows . t))))
        ;;
        ;;   Other
        ;;
        ((lambda (buffer _alist)
           (with-current-buffer buffer (or (derived-mode-p 'dired-mode)
                                           (derived-mode-p 'git-rebase-mode)
                                           (derived-mode-p 'tabulated-list-mode))))
         (display-buffer-in-side-window)
         (window-width . ,aj8/side-window-width-dynamic)
         (side . right)
         (slot . -1)
         (window-parameters . ((no-delete-other-windows . t))))
        ("\\*\\(Bookmark List\\|Benchmark Init Results.*\\|Embark Collect:.*\\|Occur\\|.*Output\\|Semantic SymRef\\|devdocs\\|package update results\\|tex-shell\\)\\*"
         (display-buffer-in-side-window)
         (window-width . ,aj8/side-window-width-dynamic)
         (side . right)
         (slot . -1)
         (window-parameters . ((no-delete-other-windows . t))))
        ;;
        ;; Bottom side window
        ;;
        ("\\*\\(e?shell\\|v?term\\)\\*"
         (display-buffer-in-side-window)
         (window-height . ,aj8/side-window-height)
         (side . bottom)
         (window-parameters . ((no-delete-other-windows . t))))
        ;;
        ;; Bottom buffer (not side window)
        ;;
        ("\\*Embark Actions\\*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . fit-window-to-buffer)
         (window-parameters . ((no-other-window . t)
                               (mode-line-format . none))))))

;; Obey display actions when switching buffers
(setq switch-to-buffer-obey-display-actions t)

;;; Other

;; Custom interface: don't convert symbols to words (tags)
(setq custom-unlispify-tag-names nil)

;; Custom interface: don't convert symbols to words (menu)
(setq custom-unlispify-menu-entries t)

;; Use 'y' or 'n' questions always
;; (setq use-short-answers t)

;; Use longer pulse
(setq pulse-delay 0.05)   ; default is 0.03

;; Use speed keys in org-mode
(setq org-use-speed-commands t)

;; Show mode headers in describe-bindings buffer
(setq describe-bindings-outline t)

;; Don't prompt with xref-find-references
(with-eval-after-load "xref"
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references))

;;;; Modes

;;; Admin...

;;; Buffers...

;; Persistent minibuffer history
(savehist-mode 1)

;; Show recursion depth in the minibuffer prompt
(minibuffer-depth-indicate-mode 1)

;;; Coding...

;;; Completion...

;;; Editing...

;; Highlight current line
(global-hl-line-mode 1)

;; Auto-insert closing parens, bracket and double-quotes
;; (electric-pair-mode 1)

;;; Files...

;; Track recent files
(recentf-mode 1)

;; Save place in each file
(save-place-mode 1)

;;; Help...

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

;;; Spelling...

;; On-the-fly spell checking
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;;; Terminal

;;; Theme...

;; Enable line numbers
;; (add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

;;; Version control...

;;; Windows...

;; Enable winner mode
(winner-mode 1)

;;; Web...

;;; Other...

;; Enable repeat mode
(repeat-mode 1)

;; Expand abbreviations
(add-hook 'text-mode-hook #'abbrev-mode)

;;;; Hooks

;;; General

;; activate-mark: deactivate highlight mode when selecting text
(add-hook 'activate-mark-hook (lambda () (global-hl-line-mode -1)))
(add-hook 'deactivate-mark-hook (lambda () (global-hl-line-mode 1)))

;; dired-after-readin: tag dired buffer names
(add-hook 'dired-after-readin-hook (lambda () (aj8/prefix-buffer-name "dired")))

;; Disable side windows before Ediff
(add-hook 'ediff-before-setup-windows-hook 'window-toggle-side-windows)

;; kill-buffer: collect list of killed buffers
(add-hook 'kill-buffer-hook #'reopen-killed-file--add-to-list)

;;; Modes

;; emacs-lisp-mode: outline settings
(add-hook 'emacs-lisp-mode-hook
          #'outline-headers-for-semicolon-buffers)

;; i3wm-config-mode: outline settings
(add-hook 'i3wm-config-mode-hook
          #'outline-headers-for-hash-mark-buffers)

;; latex-mode: outline settings
(add-hook 'latex-mode-hook
          #'outline-headers-for-percentage-buffers)

;; conf-xdefaults-mode: outline settings
(add-hook 'conf-xdefaults-mode-hook
          #'outline-headers-for-exclamation-mark-buffers)

;; Info-mode: allow multiple Info buffers
(add-hook 'Info-mode-hook #'rename-uniquely)

;; outline-mode: remove form-feed character (^L) from regexp
(add-hook 'outline-mode-hook
          (lambda () (setq outline-regexp "[*]+")))

;; powershell: outline settings
(add-hook 'powershell-mode-hook
          #'outline-headers-for-hash-mark-buffers)

;; sh-mode: for non-standard bash config files
(add-to-list 'auto-mode-alist '("\\.bash_.*\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc_.*\\'" . sh-mode))

;; [la]tex-mode:
(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))
(add-hook 'latex-mode-hook (lambda () (setq comment-add 0)))   ; use single comment
                                                               ; character only

;; shell-scrip-mode: outline settings
(add-hook 'sh-mode-hook
          #'outline-headers-for-hash-mark-buffers)

;; visual-line-mode
(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'helpful-mode-hook #'visual-line-mode)
(add-hook 'Info-mode-hook #'visual-line-mode)
;; (add-hook 'Man-mode-hook #'visual-line-mode)


;;;;; KEYBINDINGS

;;   TODO: Use minor-mode for keybindings?
;;           (https://stackoverflow.com/a/683575/1610035)
;;           (https://emacs.stackexchange.com/a/358/33325)

;;;; Escape codes

(when (not (display-graphic-p))   ; if using terminal
  ;; (define-key input-decode-map "\e[1;8A" [C-M-S-up])
  ;; (define-key input-decode-map "\e[1;8B" [C-M-S-down])

  (define-key input-decode-map "\e[8;7u" (kbd "C-M-<backspace>"))
  (define-key input-decode-map "\e[32;2u" (kbd "S-SPC"))
  (define-key input-decode-map "\e[91;7u" (kbd "C-M-["))
  (define-key input-decode-map "\e[93;7u" (kbd "C-M-]"))
  (define-key input-decode-map "\e[96;5u" (kbd "C-`"))
  (define-key input-decode-map "\e[96;7u" (kbd "C-M-`"))
  (define-key input-decode-map "\e[107;6u" (kbd "C-S-k"))
  (define-key input-decode-map "\e[118;8u" (kbd "C-M-S-v"))
  (define-key input-decode-map "\e[123;5u" (kbd "C-{"))
  (define-key input-decode-map "\e[125;5u" (kbd "C-}"))
  (define-key input-decode-map "\e[127;5u" [C-backspace])
  (define-key input-decode-map "\e[127;6u" [C-S-backspace]))

;;;; Translations

(define-key key-translation-map (kbd "M-<up>") (kbd "M-p"))
(define-key key-translation-map (kbd "M-<down>") (kbd "M-n"))
(define-key key-translation-map (kbd "C-M-<up>") (kbd "C-M-p"))
(define-key key-translation-map (kbd "C-M-<down>") (kbd "C-M-n"))

;;;; General

;;; Admin

;;; Buffers

;; Buffer navigation
(global-set-key (kbd "C-x <right>") #'next-buffer)
(global-set-key (kbd "C-x <left>") #'previous-buffer)
(global-set-key (kbd "C-x C-<right>") #'my/project-next-buffer)
(global-set-key (kbd "C-x C-<left>") #'my/project-previous-buffer)

;; Kill buffer
(global-set-key (kbd "C-x k") #'kill-this-buffer)

;; Kill buffer (other window)
(global-set-key (kbd "C-c k") #'my/kill-buffer-other-window)

;;; Coding

;;; Completion

;; Use hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Cycle through orderless matching styles on the fly
(global-set-key (kbd "M-o") #'aj8/orderless-matching-style-cycle)

;;; Editing

;; Kill line to the left
(global-set-key (kbd "C-<backspace>") (lambda () (interactive) (kill-line 0)))

;; Manipulate case
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)
;; (global-set-key (kbd "M-c") #'capitalize-dwim)
(global-set-key (kbd "M-c") #'aj8/capitalize-word-at-point)

;; Fill respecting indentation
;;   (use with `C-x .'  for comments)
(global-set-key (kbd "C-c q") #'fill-individual-paragraphs)

;; Copy symbol at point
(global-set-key (kbd "C-c o") #'my/copy-symbol-at-point)

;; Indent to next nonblank character in previous line
(global-set-key (kbd "C-c TAB") #'indent-relative)

;; Exit recursive edit
;;   Default key C-M-c overridden by custom Smartparens key
(global-set-key (kbd "C-c C-c") #'exit-recursive-edit)

;;; Files

;; Find file at point
(global-set-key (kbd "C-c f") #'find-file-at-point)

;; Forget project
(global-set-key (kbd "C-x p t") #'project-forget-project)

;;; Help

;; Display keymaps
(global-set-key (kbd "C-c H k") #'describe-keymap)

;; Display commands by category
(global-set-key (kbd "C-c H s") #'shortdoc-display-group)

(which-key-add-key-based-replacements "C-c H" "help")
                                        ; add label for prefix key

;;; Navigation

;; Paragraph navigation
(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)

;; Move to indentation point
(global-set-key (kbd "M-a") #'back-to-indentation)

;; (global-set-key (kbd "C-c <up>") #'aj8/previous-line)
;; (global-set-key (kbd "C-c <down>") #'aj8/next-line)

;; Enable scroll lock
(global-set-key (kbd "C-c L") #'scroll-lock-mode)

(which-key-add-key-based-replacements "C-c x" "misc")
                                        ; add label for prefix key

;; Display Imenu
(global-set-key (kbd "C-c i") #'imenu)

(global-set-key (kbd "C-c >") #'xref-find-definitions)  ; default M-.
(global-set-key (kbd "C-c <") #'xref-pop-marker-stack)  ; default M-,

;;; Outline

;; Toggle outline-minor-mode
(global-set-key (kbd "C-c O") #'outline-minor-mode)

;; Toggle outline-minor-mode
(global-set-key (kbd "C-c F") #'hs-minor-mode)
(global-set-key (kbd "C-c <left>") #'hs-hide-block)
(global-set-key (kbd "C-c <right>") #'hs-show-block)
(global-set-key (kbd "C-c C-<left>") #'hs-hide-all)
(global-set-key (kbd "C-c C-<right>") #'hs-show-all)

;;; Search

(define-key isearch-mode-map (kbd "TAB") #'isearch-complete)

;;; Selection

;;; Spelling

;;; Terminal
;;; Theme

;; Display column number
(global-set-key (kbd "C-c N") #'column-number-mode)

;;; Version control

;; Show diffs between buffers
(global-set-key (kbd "C-c e b") #'ediff-buffers)

;; Show diffs between regions
(global-set-key (kbd "C-c e l") #'ediff-regions-linewise)
(global-set-key (kbd "C-c e w") #'ediff-regions-wordwise)

(which-key-add-key-based-replacements "C-c e" "ediff")
                                        ; add label for prefix key

;; Show diffs between file revisions
(global-set-key (kbd "C-x v -") #'vc-ediff)

;;; Web

;; Browse URL at point
(global-set-key (kbd "C-c b") #'browse-url-at-point)

;;; Windows

;; Windmove keys
(windmove-default-keybindings 'ctrl)
(windmove-swap-states-default-keybindings '(ctrl shift))

;; Open windows
(global-set-key (kbd "C-c w <up>") #'windmove-display-up)
(global-set-key (kbd "C-c w <down>") #'windmove-display-down)
(global-set-key (kbd "C-c w <left>") #'windmove-display-left)
(global-set-key (kbd "C-c w <right>") #'windmove-display-right)
(global-set-key (kbd "C-c w 0") #'windmove-display-same-window)

;; Delete windows
(global-set-key (kbd "C-c w C-<up>") #'windmove-delete-up)
(global-set-key (kbd "C-c w C-<down>") #'windmove-delete-down)
(global-set-key (kbd "C-c w C-<left>") #'windmove-delete-left)
(global-set-key (kbd "C-c w C-<right>") #'windmove-delete-right)

;; Cycle window configurations
(define-key winner-mode-map (kbd "C-c w <") #'winner-undo)
(define-key winner-mode-map (kbd "C-c w >") #'winner-redo)

(which-key-add-key-based-replacements "C-c w" "windows")
                                        ; add label for prefix key

;; Resize windows
(global-set-key (kbd "C-x {") #'my/move-splitter-up)
(global-set-key (kbd "C-x }") #'my/move-splitter-down)
(global-set-key (kbd "C-x >") #'my/move-splitter-right) ; override `scroll-right'
(global-set-key (kbd "C-x <") #'my/move-splitter-left)  ; override `scroll-left'

;; Split parent windows
(global-set-key (kbd "C-c 2") #'mp-split-below)
(global-set-key (kbd "C-c 3") #'mp-split-right)

;; Toggle side windows
(global-set-key (kbd "C-x |") #'window-toggle-side-windows)

;; Misc window manipulation
(global-set-key (kbd "C-x !") #'delete-other-windows-vertically)
(global-set-key (kbd "C-x =") #'balance-windows)
                                        ; override `what-cursor-position'
(global-set-key (kbd "C-x +") #'balance-windows-area)
                                        ; override `balance-windows'
;; (global-set-key (kbd "C-x -") #'shrink-window-if-larger-than-buffer) ; default
(global-set-key (kbd "C-x _") #'fit-window-to-buffer)   ; enlarges and shrinks
(global-set-key (kbd "C-x 9") #'my/toggle-window-split)

;;; Other

;; Reload init.el
(global-set-key (kbd "C-c r") #'reload-init-file)

;; Evaluate next sexp
(global-set-key (kbd "C-x M-e") #'my/eval-next-sexp)

;; Evaluate sexp at point
(global-set-key (kbd "C-x C-M-e") #'my/eval-sexp-at-point)

;;; Unbind keys

;; (global-set-key (kbd "C-x") nil)
;; (global-unset-key (kbd "C-x"))   ; alternative syntax

;;;; Hooks

;; ediff-mode
;;   TODO: check functionality
(add-hook 'ediff-keymap-setup-hook
          ;; Use both versions with ediff
          (lambda () (define-key ediff-mode-map "d" #'my/ediff-copy-both-to-C)))

;; Info-mode
(add-hook 'Info-mode-hook
          ;; Disable M-n
          (lambda () (define-key Info-mode-map (kbd "M-n") nil)))

;; sh-mode
(add-hook 'sh-mode-hook
          ;; Disable SMIE commands
          (lambda () (define-key sh-mode-map (kbd "C-c =") nil)
                     (define-key sh-mode-map (kbd "C-c <") nil)
                     (define-key sh-mode-map (kbd "C-c >") nil)
                     (define-key sh-mode-map (kbd "C-c ?") nil)))

;;;; Hydras

(which-key-add-key-based-replacements "C-c y" "hydra")
                                        ; add label for prefix key

;;; Windows
(defhydra hydra-window (:hint nil)
  "
                                                                   ╭─────────┐
  Select   Move   Resize    Split             Do                   │ Windows │
╭──────────────────────────────────────────────────────────────────┴─────────┴─┐
  ^ ^ _↑_ ^ ^   ^ ^ _↑_ ^ ^   ^ ^ _↑_ ^ ^   ╭─┬─┐ ^ ^           ╭─┬─┐ ^ ^
  _←_ ^ ^ _→_   _←_ ^C^ _→_   _←_ ^M^ _→_   │ │ │[_h_]orizontal ├─┼─┤[_b_]alance  [_t_]ranspose
  ^ ^ _↓_ ^ ^   ^ ^ _↓_ ^ ^   ^ ^ _↓_ ^ ^   ╰─┴─╯ ^ ^           ╰─┴─╯ ^ ^         fl[_i_]p (vertical)
  ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ╭───┐ ^ ^           ╭───┐ ^ ^         fl[_o_]p (horizontal)
  ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ├───┤[_v_]ertical   │   │[_z_]oom     [_r_]otate
  ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^   ╰───╯ ^ ^           ╰───╯ ^ ^
╰──────────────────────────────────────────────────────────────────────────────╯
↺ [_u_]ndo layout | ↻ re[_s_]set layout
⇋ [_n_]ext window | ✗ [_c_]lose window
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
                (my/move-splitter-left 1)
                (message "Width: %s Height: %s"
                         (window-body-width) (window-body-height))))
  ("M-<right>" (lambda ()
                (interactive)
                (my/move-splitter-right 1)
                (message "Width: %s Height: %s"
                         (window-body-width) (window-body-height))))
  ;; ("M-<up>" my/move-splitter-up)
  ("M-p" (lambda ()
                (interactive)
                (my/move-splitter-up 1)
                (message "Width: %s Height: %s"
                         (window-body-width) (window-body-height))))
  ;; ("M-<down>" my/move-splitter-down)
  ("M-n" (lambda ()
                (interactive)
                (my/move-splitter-down 1)
                (message "Width: %s Height: %s"
                         (window-body-width) (window-body-height))))
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
_↑_: previous     _←_: previous
_↓_: next         _→_: next

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
_↑_: previous     _←_: previous
_↓_: next         _→_: next

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
  _←_ hide               _↑_ previous visible
  _→_ show               _↓_ next visible
C-_←_ hide (global)    C-_↑_ backward same level
C-_→_ show (global)    C-_↓_ forward same level
  ^ ^                  M-_↑_ up

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
(cond ((featurep 'ns)   ; macOS
       ;; Use left Option as Meta
       ;; (setq mac-option-modifier 'meta)
       ;; Use left Command as Super
       ;; (setq mac-command-modifier 'super)
       ;; GUI settings
       (when (display-graphic-p)
         ;; Set default font
         (add-to-list 'default-frame-alist '(font . "Hack-14"))
         ;; Increase line spacing
         (setq-default line-spacing 1))
       (message "Late settings macOS"))

      ((equal (system-name) "penguin")   ; ChromeOS
       (message "Late settings ChromeOS"))

      ((and (eq system-type 'gnu/linux)
            (getenv "WSLENV"))   ; WSL
       ;; Enable (default) web browser
       ;;   Requirements: wslu
       (setq browse-url-generic-program "wslview")
       (setq browse-url-secondary-browser-function #'browse-url-generic)
       (advice-add #'browse-url-default-browser :override #'browse-url-generic)
       (message "Late settings WSL"))

      ((eq system-type 'gnu/linux)   ; Linux
       ;; Special settings for URxvt
       (when (equal "rxvt-unicode-256color"
                    (getenv-internal "TERM" initial-environment))
         (rxvt-input-decode-map))
       (message "Late settings Linux"))

      (t (user-error "Unexpected system-name: %s" system-name)))

;; Display messages buffer
(display-buffer "*Messages*")

;; Conditionally load host specific stuff
;; (let ((host-specific-files (concat (make-load-path-base) system-name ".el")))
;;   (if (file-exists-p host-specific-files)
;;       (load host-specific-files)
;;     (message (concat "No host specific customizations for " system-name))
;;     ))
