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
(cond ((eq aj8/my-os 'macos)          ; macOS
       ;; GUI settings
       (when (display-graphic-p)
         ;; Import path from shell
         (exec-path-from-shell-initialize))
       (message "Early settings for macOS"))

      ((eq aj8/my-os 'wsl)           ; WSL
       (message "Early settings WSL"))

      ((eq aj8/my-os 'linux)     ; Linux
       (message "Early settings Linux"))

      (t (user-error "Unexpected system-name: %s" system-name)))


;;;;; PACKAGES

;;;; Setup

;;; use-package

;; Always install packages if not present
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

;; use-package-ensure-system-package (auto install system packages) - [built-in package]
;;   Enables installation with :ensure-system-package
(use-package use-package-ensure-system-package)


;;; Local

(require 'aj8-lisp)

;;;; Packages

;;; Selected packages
;;;   Install packages with (package-install-selected-packages)
;;;   Remove packages with (package-autoremove)
;;;   Use use-package for configuration only
(customize-set-variable 'package-selected-packages
                        '(ahk-mode
                          all-the-icons
                          all-the-icons-dired
                          ansible
                          auto-package-update
                          benchmark-init
                          cape
                          circadian
                          consult
                          consult-eglot
                          consult-project-extra
                          corfu
                          corfu-terminal
                          dashboard
                          devdocs
                          devdocs-browser
                          dired-sidebar
                          diff-hl
                          diminish
                          dimmer
                          drag-stuff
                          ef-themes
                          elfeed
                          embark
                          embark-consult
                          erc-hl-nicks
                          eshell-git-prompt
                          esup
                          exec-path-from-shell
                          expreg
                          flymake-aspell
                          flymake-eslint
                          flymake-json
                          flymake-ruff
                          flyspell-correct
                          google-this
                          gptel
                          helpful
                          hydra
                          keychain-environment
                          keyfreq
                          lingva
                          lorem-ipsum
                          lua-mode
                          magit
			  magit-gptcommit
			  magit-todos
                          marginalia
                          markdown-mode
                          mosey
                          multiple-cursors
                          multi-line
                          obsidian
                          orderless
                          osx-trash
                          outline-minor-faces
                          pet
                          popper
                          powershell
                          repeat-help
                          rotate
                          ruff-format
                          smartparens
                          ssh-agency
                          standard-themes
                          string-inflection
                          syntax-subword
                          transpose-frame
                          treesit-auto
                          undo-fu
                          unfill
                          vertico
                          vterm
                          vundo
                          web-mode
                          whole-line-or-region
                          xclip
                          yaml-pro
                          ztree))

;;; Selected packages sources
;;;   Install packages with (package-vc-install-selected-packages)
;;;   Use use-package for configuration only
(customize-set-variable
 'package-vc-selected-packages
 `(;; (foo . "0f39eb3fd9")   ; specific revision
   ;; (bar . nil)            ; any revision
   (combobulate :url "https://github.com/mickeynp/combobulate.git")
   (copilot :url "https://github.com/copilot-emacs/copilot.el.git")
   (gptel-quick :url "https://github.com/karthink/gptel-quick.git")
   (obsidian-yaml-tools :url "https://github.com/sonofjon/obsidian-yaml-tools.el")))
                        ;; :branch "dev")
   ;; (obsidian-yaml-tools :url ,(concat (expand-file-name "~")
   ;;                                   "/projects/obsidian-yaml-tools.el"))
   ;; (obsidian-yaml-tools :url ,(concat (expand-file-name "~")
   ;;                                    "/projects/obsidian-yaml-tools.el")
   ;;                      :branch "dev")))

;; Install selected packages
(package-install-selected-packages)
(package-vc-install-selected-packages)

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
;;   MAYBE: Full of bugs and inactive maintainer
(use-package esup
  :disabled)

(use-package exec-path-from-shell
  :if (eq aj8/my-os 'macos))   ; macOS

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

;; ahk-mode (major-mode for editing AHK (AutoHotkey and AutoHotkey_L))
(use-package ahk-mode
  :mode "\\.ahk$")

;; ansible (minor-mode for editing Ansible (YAML) files)
(use-package ansible
  :defer)
  ;; :mode "\\.yml$"
  ;; :hook (yaml-mode . (lambda () (ansible 1))))

;; copilot (an unofficial Copilot plugin for Emacs)
(use-package copilot
  :bind (:map copilot-mode-map
              ("TAB" . copilot-complete)
              ("<backtab>" . 'copilot-accept-completion)
              ("M-<right>" . 'copilot-accept-completion-by-word)
              ("M-<left>" . 'copilot-accept-completion-by-line)
              ("M-p" . 'copilot-previous-completion)
              ("M-n" . 'copilot-next-completion)
         :map copilot-completion-map
              ("C-g" . 'copilot-clear-overlay))
  ;; :hook (prog-mode . copilot-mode)
  :custom
  ;; Show completions automatically
  (copilot-enable-predicates '(copilot--buffer-changed))
  ;; Don't show completions when Corfu is active
  (copilot-disable-predicates '(corfu-popup-active-p))
  :config
  (defun corfu-popup-active-p ()
    "Return non-nil if a Corfu popup is active in terminal."
    (when (boundp 'corfu-terminal--popup)
      corfu-terminal--popup))
  ;; Clear overlay
  (add-hook 'post-command-hook #'copilot-clear-overlay))
  ;; Enable Copilot globally
  ;; (global-copilot-mode 1))

;; lua-mode (major-mode for editing Lua scripts)
(use-package lua-mode
  :mode ("\\.lua$"))

;; markdown-mode (major-mode for editing Markdown files)
(use-package markdown-mode
  :mode "\\.md$"
  ;; :mode ("\\.md$" . markdown-view-mode)
  :bind (:map markdown-mode-map
              ("M-n" . nil)         ; unbind markdown-next-link
              ("M-p" . nil)     ; unbind markdown-previous-link
              ("C-c <left>" . nil)     ; unbind markdown-promote
              ("C-c <right>" . nil)     ; unbind markdown-demote
              ("C-c C-<down>" . markdown-outline-next-same-level)
              ("C-c <down>" . markdown-outline-next)
              ("C-c C-<up>" . markdown-outline-previous-same-level)
              ("C-c <up>" . markdown-outline-previous)
              ("C-c C-M-n" . markdown-move-down)
              ("C-c C-M-p" . markdown-move-up)
              ("C-c C-M-<right>" . markdown-demote)
              ("C-c C-M-<left>" . markdown-promote)))

;; powershell (major-mode for editing PowerShell scripts)
(use-package powershell
  :defer
  :mode ("\\.ps1$" . powershell-mode))

;; web-mode (major-mode for editing web templates)
(use-package web-mode
  :disabled
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

;; yaml-pro (parser-aided YAML editing features)
(use-package yaml-pro
  :disabled
  :mode ("\\.yml$" . yaml-pro-ts-mode))

;; eglot (client for language server protocol servers)
(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c l a o" . eglot-code-action-organize-imports)
              ("C-c l a q" . eglot-code-action-quickfix)
              ("C-c l a e" . eglot-code-action-extract)
              ("C-c l a i" . eglot-code-action-inline)
              ("C-c l a r" . eglot-code-action-rewrite)
              ("C-c l f" . eglot-format)
              ("C-c l F" . eglot-format-buffer)
              ("C-c l d" . flymake-show-buffer-diagnostics)
              ("C-c l D" . flymake-show-project-diagnostics)
              ;; ("C-c l h" . eldoc)
              ("C-c h" . eldoc)
              ("C-c l r" . eglot-rename))
  :hook ((sh-mode . eglot-ensure)
         ;; (html-mode . eglot-ensure)   ;  mhtml is default for HTML
         (mhtml-mode . eglot-ensure)
         (css-mode . eglot-ensure)
         (web-mode . eglot-ensure)       ; no linting
         (js-mode . eglot-ensure)
         (json-mode . eglot-ensure)      ; no linting
         (lua-mode . eglot-ensure)
         (markdown-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (latex-mode . eglot-ensure)
         (yaml-mode . eglot-ensure))
  :init
  (which-key-add-key-based-replacements "C-c l" "lsp")
  (which-key-add-key-based-replacements "C-c l a" "action")
                                        ; add label for prefix key
  :custom
  ;; Shutdown server after buffer kill
  (eglot-autoshutdown t)
  ;; Enable eglot in code external to project
  (eglot-extend-to-xref t)
  ;; Disable events buffers
  (eglot-events-buffer-size 0)
  :config
  ;; Add server for web-mode
  (add-to-list 'eglot-server-programs
               '(web-mode . ("vscode-html-language-server" "--stdio")))
  ;; Use Orderless for Eglot (default is Flex)
  (setq completion-category-overrides '((eglot (styles orderless))))
  ;; Don't manage ELDoc
  ;; (add-to-list 'eglot-stay-out-of 'eldoc))
  ;; Limit ELDoc to a single line
  ;; (setq eldoc-echo-area-use-multiline-p nil))   ; doesn't work nicely
  ;; Don't auto-show documentation
  (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1))))

;; consult-eglot (query workspace symbol from eglot using consult)
(use-package consult-eglot
  :after (consult eglot))

;; flymake-aspell (Aspell checker for Flycheck)
(use-package flymake-aspell
  :disabled
  :ensure-system-package aspell
  :hook (text-mode . flymake-aspell-setup)
  :config
  ;; Don't prompt for saving personal dictionary
  (setq ispell-silently-savep t))

;; flymake-eslint (a Flymake backend for Javascript using eslint)
(use-package flymake-eslint
  :ensure-system-package eslint
  :disabled
  :hook (js-base-mode . flymake-eslint-enable))

;; flymake-json (a Flymake handler for json using jsonlint)
;;   TODO: doesn't work
(use-package flymake-json
  :ensure-system-package jsonlint
  :hook (json-ts-mode . flymake-json-load))

;; flymake-ruff (a Flymake plugin for python files using Ruff)
(use-package flymake-ruff
  :disabled
  :ensure-system-package ruff
  ;; :hook (python-base-mode . flymake-ruff-load)   ; without eglot
  :hook (eglot-managed-mode . flymake-ruff-load))   ; with eglot

;; ruff-format (Ruff format Python source)
(use-package ruff-format
  :disabled
  :ensure-system-package ruff)
  ;; :hook (python-base-mode . ruff-format-on-save-mode))

;; pet (executable and virtualenv tracker for python-mode)
(use-package pet
  :disabled
  :ensure-system-package dasel
  :config
  (add-hook 'python-base-mode-hook (lambda () (pet-mode -1))))

;; treesit-auto (automatically use tree-sitter enhanced major modes)
(use-package treesit-auto
  :demand t
  :custom
  ;; Prompt for install
  (treesit-auto-install 'prompt)
  :config
  ;; Add tree-sitter modes to auto-mode-alist
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;; Enable globally
  (global-treesit-auto-mode))

;;; Completion

;; consult (consulting completing-read)
(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c c h" . consult-history)
         ("C-c c c" . consult-mode-command)
         ("C-c c k" . consult-kmacro)
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
         ;; M-g bindings (goto-map)
         ("M-g f" . consult-flymake)
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
  ;; (setq consult-preview-key "M-`")
  ;; (setq consult-preview-key (list "<down>" "<up>"))

  ;; Manual preview for expensive commands
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-bookmark
   consult-recent-file consult-xref consult--source-bookmark
   consult--source-file-register consult--source-recent-file
   consult--source-project-recent-file
   :preview-key "M-`")

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
;;   TODO: Use separate matching-style for Corfu and Vertico,
;;         e.g. I don't want regexp in Corfu
(use-package corfu
  ;; :hook (prog-mode . corfu-mode)   ; not needed with corfu-global-mode
  ;; :hook (corfu-mode . my/corfu-comp-style)
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
  (corfu-quit-no-match t)        ; automatically quit if there is no match
  ;; (corfu-preview-current nil)    ; disable current candidate preview
  ;; (corfu-preselect-first nil)    ; disable candidate preselection
  ;; (corfu-on-exact-match 'insert) ; configure handling of exact matches
  ;;   TODO: match not inserted with orderless
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
      (setq-local corfu-echo-delay nil) ; disable automatic echo and popup
                  ;; corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))
  ;; :config
  ;; Sort candidates by history
  ;; (corfu-history-mode 1)   ; requires savehist-mode
  ;; (savehist-mode 1)
  ;; (add-to-list 'savehist-additional-variables 'corfu-history))

;; corfu-echo (show Corfu candidate documentation in echo area)
;;   Note, this is an extension included in the Corfu package
(use-package corfu-echo
  :after corfu
  :config
  (corfu-echo-mode 1))

;; corfu-info (show Corfu candidate information in a separate buffer)
;;   Note, this is an extension included in the Corfu package
(use-package corfu-info
  :after corfu)

;; corfu-terminal (Corfu popup on terminal)
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :after corfu
  :config
  (corfu-terminal-mode 1))

;; cape (Completion At Point Extensions)
;;   TODO: Make ispell available in comments (sh-mode, and others?)
;;         (note that it works in elisp mode) (see cape-capf-inside-string
;;         and cape-capf-inside-comment and commit:
;;         https://github.com/minad/cape/commit/9b97dbbc7624415ee25f79de9ea357feb1e2e547)
(use-package cape
  :ensure-system-package "/usr/share/dict/words"
  :after corfu
  :bind (("C-c u p" . completion-at-point)   ; capf
         ("C-c u a" . cape-abbrev)
         ("C-c u d" . cape-dabbrev)          ; or dabbrev-completion
                                             ; see also dabbrev-capf on Emacs 29
         ("C-c u e" . my/cape-elisp)
         ("C-c u h" . cape-history)          ; only in shell or minibuffer?
         ("C-c u w" . cape-dict)             ; requires wamerican (Ubuntu)
                                             ; or words (Fedora)
         ("C-c u f" . cape-file)
         ("C-c u k" . cape-keyword)
         ("C-c u l" . cape-line)
         ("C-c u s" . cape-elisp-symbol)     ; complete symbols everywhere
         ("C-c u t" . cape-tex))
  :init
  ;; Custom completion at point functions
  ;;   Note that the order of the capf:s matter. Also, cape-file does
  ;;   not merge well with the other capf:s, see documentation.
  (defalias 'cape-dabbrev+symbol+keyword+dict (cape-capf-super #'cape-dabbrev
  ;; TODO: doesnt remember recent candidates
                                                               #'cape-elisp-symbol
                                                               #'cape-keyword
                                                               #'cape-dict)
    "Completion at point function for Cape, combining completions
from Dabbrev, symbol, keyword and dictionary.")
  (defalias 'cape-dict+dabbrev+symbol+keyword (cape-capf-super #'cape-dict
    ;; TODO: symbol completion fails on hyphen when dict precedes symbol
                                                               #'cape-dabbrev
                                                               #'cape-elisp-symbol
                                                               #'cape-keyword)
    "Completion at point function for Cape, combining completions
from dictionary, Dabbrev, symbol and keyword.")
  (defalias 'cape-symbol+keyword+dict (cape-capf-super #'cape-elisp-symbol
                                                       #'cape-keyword
                                                       #'cape-dict)
    "Completion at point function for Cape, combining completions
from symbol, keyword and dictionary.")
  (defalias 'cape-dict+symbol+keyword (cape-capf-super #'cape-dict
                                                       #'cape-elisp-symbol
                                                       #'cape-keyword)
    "Completion at point function for Cape, combining completions
from dictionary, symbol and keyword.")
  (defalias 'cape-dict+dabbrev (cape-capf-super #'cape-dict
                                                #'cape-dabbrev)
    "Completion at point function for Cape, combining completions
from dictionary and Dabbrev.")
  (defalias 'cape-dabbrev+dict (cape-capf-super #'cape-dabbrev
                                                #'cape-dict)
    "Completion at point function for Cape, combining completions
from Dabbrev and dictionary.")
  (defalias 'cape-dabbrev+dict (cape-capf-super #'cape-dabbrev
                                                #'cape-dict)
    "Completion at point function for Cape, combining completions
from Dabbrev and dictionary.")
  (defalias 'my/cape-elisp (cape-capf-interactive #'elisp-completion-at-point)
    "Completion at point function for Cape, allowing for completion of
Elisp code explicitly in arbitrary buffers.")

  ;; Add `completion-at-point-functions', used by `completion-at-point'
  ;;
  ;;   Note that this adds functions to the global value of
  ;;   completion-at-point-functions. Any buffer-local values of
  ;;   completion-at-point-functions will take precedence, however, if the
  ;;   buffer-local value contains `t' the global value of
  ;;   completion-at-point-functions will be used if the buffer-local value
  ;;   doesn't provide a match. Unless the buffer-local value use exclusive
  ;;   completion functions, in which case the global value is never
  ;;   called. In such cases, you can add the global values in
  ;;   mode-hooks. E.g.
  ;;
  ;;     (defun my/register-default-capfs ()
  ;;       "Add capf to current mode. This is a workaround for the fact
  ;;     that some modes fill the buffer-local capfs with exclusive
  ;;     completion functions, so that the global capfs don't get called."
  ;;       (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;       (add-to-list 'completion-at-point-functions #'cape-file)
  ;;       (add-to-list 'completion-at-point-functions #'cape-line))
  ;;
  ;;     (add-hook 'emacs-lisp-mode #'my/register-default-caps)
  ;;
  ;;   The elisp-completion-at-point function is exclusive. To make it
  ;;   non-exclusive you can wrap it with cape-capf-properties:
  ;;
  ;;     (cape-capf-properties #'elisp-completion-at-point :exclusive 'no)
  ;;
  ;;   or more recently, use cape-wrap-nonexclusive.

  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)

  ;; Completion at point function for prog-mode
  (defun aj8/prog-mode-capf ()
    ;; (add-hook 'completion-at-point-functions
    ;;            #'cape-symbol+keyword+dict nil t))
    ;; (add-to-list 'completion-at-point-functions
    ;;              #'cape-symbol+keyword+dict))
    ;; (setq-local completion-at-point-functions
    ;;             (list #'cape-symbol+keyword+dict t)))
    ;; Insert custom capf before `t' in buffer-local value
    (setq-local completion-at-point-functions
                (append (remove t (buffer-local-value
                                   'completion-at-point-functions
                                   (current-buffer)))
                        (list #'cape-symbol+keyword+dict t))))
  (add-hook 'prog-mode-hook #'aj8/prog-mode-capf)
  ;; (add-hook 'emacs-lisp-mode-hook #'aj8/prog-mode-capf)

  ;; Completion at point function for text-mode
  (defun aj8/text-mode-capf ()
    ;; (add-hook 'completion-at-point-functions
    ;;            #'cape-dict+symbol+keyword nil t))
    ;; (add-to-list 'completion-at-point-functions
    ;;               #'cape-dict+symbol+keyword))
    ;; (setq-local completion-at-point-functions
    ;;             (list#'cape-dict+symbol+keyword t)))
    ;; Insert custom capf before `t' in buffer-local value
    (setq-local completion-at-point-functions
                (append (remove t (buffer-local-value
                                   'completion-at-point-functions
                                   (current-buffer)))
                        (list #'cape-dabbrev+dict t))))
  (add-hook 'text-mode-hook #'aj8/text-mode-capf)

  (which-key-add-key-based-replacements "C-c u" "corfu/cape")
                                        ; add label for prefix

  :custom
  ;; Sort candidates by length and alphabetically
  ;;   Also see https://github.com/minad/cape/issues/44
  (corfu-sort-override-function 'corfu-sort-length-alpha))

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
     embark-history-remove)))

;; embark-consult (Consult integration for Embark)
(use-package embark-consult
  :demand t   ; only necessary if you have the hook below
  :after (embark consult)
  ;; if you want to have consult previews as you move around an
  ;; auto-updating Embark collect buffer
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; orderless (completion style for matching regexps in any order)
(use-package orderless
  :custom
  ;; Matching styles
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  ;; Style dispatchers
  (orderless-style-dispatchers
   '(aj8/orderless-dispatch-flex-if-twiddle
     aj8/orderless-dispatch-literal-if-equal
     aj8/orderless-dispatch-prefixes-if-less
     aj8/orderless-dispatch-regexp-if-star
     aj8/orderless-dispatch-without-if-bang)))

;; vertico (VERTical Interactive COmpletion)
;;   TODO: Set up vertico-repeat
;;         Set up vertico-grid-mode (with vertico-multiform-mode?)?
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
  ;; Enable M-x minibuffer-hide-completions (make function interactive)
  (put 'minibuffer-hide-completions 'interactive-form '(interactive)))

;;; Editing

;; combobulate
;;   TODO: Missing functionality: barf, slurp, forward/backward sexp (navigate
;;         by parens), ...
;;         Don't dump tree into *Messages*
(use-package combobulate
  :after treesit
  ;; :load-path ("path-to-git-checkout-of-combobulate"))
  :hook (;; (bash-ts-mode . combobulate-mode)   ; NA
         (css-ts-mode . combobulate-mode)
         ;; (html-ts-mode . combobulate-mode)   ; require Emacs 30
         (js-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         ;; (markdown-ts-mode . combobulate-mode)   ; NA
         (python-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode))
  :bind (:map combobulate-key-map
              ("C-M-n" . combobulate-navigate-down)   ; forward-list
              ("C-M-<right>" . combobulate-navigate-next)   ; forward-sexp
              ("C-M-<left>" . combobulate-navigate-previous)   ; backward-sexp
              ("C-M-p" . combobulate-navigate-up)   ; backward-list
              ("C-c o <up>" . combobulate-splice-up)
              ("C-c o <down>" . combobulate-splice-down)
              ("C-c o <right>" . combobulate-splice-self)
              ("C-c o <left>" . combobulate-splice-parent)
              ("C-M-d" . combobulate-drag-down)   ; down-list
              ("C-M-u" . combobulate-drag-up)   ; backward-up-list
              ("M-a" . combobulate-navigate-logical-previous)
              ("M-e" . combobulate-navigate-logical-next)
              ("M-a" . combobulate-navigate-logical-previous)
              ("M-<down>" . nil)
              ("M-<left>" . nil)
              ("M-<right>" . nil)
              ("M-<up>" . nil))
  :init
  (which-key-add-key-based-replacements "C-c o" "combobulate")
  (which-key-add-key-based-replacements "C-c o B" "builder")
  (which-key-add-key-based-replacements "C-c o e" "envelop")
  (which-key-add-key-based-replacements "C-c o h" "highlight")
  (which-key-add-key-based-replacements "C-c o t" "edit")
  (which-key-add-key-based-replacements "C-c o x" "xref")
                                        ; add label for prefix
  :config
  ;; Custom prefix key
  (setq combobulate-key-prefix "C-c o"))

;; lingva (access Google Translate without tracking via lingva.ml)
(use-package lingva
  :bind ("C-c x t" . lingva-translate))

;; lorem-ipsum (insert dummy pseudo latin text)
(use-package lorem-ipsum
  :defer)

;; drag-stuff (drag stuff (lines, words, region, etc...) around)
;;   TODO: drag region moves first line after region
(use-package drag-stuff
  :diminish
  :bind (("C-c <up>" . drag-stuff-up)
         ("C-c <down>" . drag-stuff-down)
         ("C-c <left>" . drag-stuff-left)
         ("C-c <right>" . drag-stuff-right))
  :config
  ;; Enable everywhere
  (drag-stuff-global-mode 1))

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

;; multi-line (multi-line statements)
(use-package multi-line
  ;; Demand multi-line to avoid failure to load mode specific strategies
  ;;   (https://github.com/IvanMalison/multi-line/issues/8)
  :demand t)
  ;; :bind (("C-c l" . multi-line)))   ; TODO: conflicts with eglot 

;; repeat-help (display keybindings for repeat-mode)
(use-package repeat-help
  :hook (repeat-mode . repeat-help-mode))
  ;; :custom
  ;; (repeat-help-auto t))

;; smartparens (automatic insertion, wrapping and paredit-like navigation)
(use-package smartparens
  :diminish
  :hook (emacs-lisp-mode . smartparens-mode)
         ;; (prog-mode . smartparens-strict-mode)
         ;; (minibuffer-setup . turn-on-smartparens-strict-mode))
  :bind (:map smartparens-mode-map
              ;; Base set                                ; defaults:
              ;; ("C-M-f" . sp-forward-sexp)             ; forward-sexp
              ("C-M-<right>" . sp-forward-sexp)
              ;; ("C-M-b" . sp-backward-sexp)            ; backward-sexp
              ("C-M-<left>" . sp-backward-sexp)
              ;; ("C-M-n" . sp-down-sexp)                ; forward-list
              ("C-M-n" . aj8/sp-down-sexp-dwim)          ; forward-list
              ;; ("C-M-a" . sp-backward-down-sexp)       ; use M-- C-M-<down>
              ("C-M-a" . sp-beginning-of-sexp)           ; beginning-of-defun
              ("C-M-e" . sp-end-of-sexp)                 ; end-of-defun
              ;; ("C-M-e" . sp-up-sexp)                  ; use M-- C-M-<up>
              ;; ("C-M-p" . sp-backward-up-sexp)         ; backward-list
              ("C-M-p" . aj8/sp-up-sexp-dwim)            ; backward-list
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
              ;; ("C-M-[" . sp-select-previous-thing)
              ("C-M-SPC" . sp-mark-sexp)                 ; mark-sexp
              ("M-F" . sp-forward-symbol)   ; remove?
              ("M-B" . sp-backward-symbol)   ; remove?
              ;; Other
              ("C-M-<backspace>" . sp-backward-kill-sexp)
              ;; ("C-k" . sp-kill-hybrid-sexp)        ; kill-line
                                        ; (this is used anyway,
                                        ; with strict mode)
              ("C-M-;" . sp-comment)
              ("C-c s a" . sp-extract-after-sexp)
              ("C-c s b" . sp-extract-before-sexp)
              ("C-c s c" . sp-convolute-sexp)        ; alternative: C-M-c
                                                     ; (exit-recursive-edit)
              ("C-c s e" . sp-emit-sexp)
              ("C-c s h" . sp-transpose-hybrid-sexp) ; alternative: C-M-h
                                                     ; (mark-defun)
              ("C-c s j" . sp-join-sexp)             ; alternative: C-M-j
                                                     ; (default-indent-new-line)
              ("C-c s n" . sp-add-to-next-sexp)
              ("C-c s p" . sp-add-to-previous-sexp)
              ("C-c s o" . sp-absorb-sexp)           ; alternative: C-M-o
                                                     ; (split-line)
              ("C-c s r" . sp-rewrap-sexp)           ; alternative: C-M-r
              ("C-c s t" . sp-transpose-sexp)        ; alternative: C-M-t
                                                     ; (transpose-sexp)
              ("C-c s w" . sp-swap-enclosing-sexp)
              ("C-c s <" . sp-splice-sexp-killing-backward)
              ("C-c s >" . sp-splice-sexp-killing-forward)
              ("C-c s |" . sp-splice-sexp-killing-around)
              ;; Prefixes
              ("C-c s O" . sp-prefix-symbol-object)
              ("C-c s P" . sp-prefix-pair-object)
              ("C-c s T" . sp-prefix-tag-object)
              ("C-c s S" . sp-prefix-save-excursion)
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
              ;; Use standard C-w, M-d, M-<backspace> in strict mode
         :map smartparens-strict-mode-map
              ([remap kill-region] . nil))         ; unbind sp-kill-region
              ;; ([remap kill-word] . nil)           ; unbind sp-kill-word
              ;; ([remap backward-kill-word] . nil)) ; unbind sp-backward-kill-word
  :init
  (which-key-add-key-based-replacements "C-c s" "smartparens")
                                        ; add label for prefix
  ;; :custom
  ;; Use default keybindings
  ;; (sp-base-key-bindings 'sp)
  ;; Override some default keybindings
  ;; (sp-override-key-bindings '())
  ;; Enforce that pairs are always balanced
  ;; (smartparens-global-strict-mode t)
  ;; Don't consider symbols and strings as expressions
  ;; (sp-navigate-consider-symbols nil)   ; WARNING: deprecated
  :config
  ;; Use default config
  (require 'smartparens-config))
  ;; Use smartparens globally
  ;; (smartparens-global-mode 1)
  ;; (show-smartparens-global-mode 1))

;; string-inflection (underscore -> UPCASE -> CamelCase -> lowerCamelCase conversion of names)
(use-package string-inflection
  :bind (("C-c i c" . string-inflection-lower-camelcase)
         ("C-c i k" . string-inflection-kebab-case)
         ("C-c i s" . string-inflection-underscore)
         ("C-c i x" . string-inflection-all-cycle))   ; TODO: add repeat-map for this command
  :init
  (which-key-add-key-based-replacements "C-c i" "inflection"))
                                        ; add label for prefix key

;; whole-line-or-region (operate on current line if region undefined)
(use-package whole-line-or-region
  :defer 1
  :diminish whole-line-or-region-local-mode
  :config
  ;; Use whole-line-or-region-mode everywhere
  (whole-line-or-region-global-mode 1))

;; unfill (do the opposite of fill-paragraph or fill-region)
(use-package unfill
  :bind (("M-S-q" . 'unfill-paragraph)
         ("C-M-S-q" . 'unfill-region)))

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

;; xclip (copy&paste GUI clipboard from text terminal)
;;   Requires: macOS: `pbpaste/pbcopy' - installed by default
;;             X11: `xclip' or `xsel'
;;             Wayland: `wl-clipboard'
(eval `(use-package xclip
         ;; :disabled
         ;; :ensure-system-package xclip
         :ensure-system-package ,(aj8/system-package-name 'xclip)
         :config
         ;; Enable xclip globally
         (xclip-mode 1)))

;;; Files

;; dired (directory editor)  - [built-in package]
(use-package dired
  :ensure nil   ; don't install built-in packages
  :bind (:map dired-mode-map ("." . dired-omit-mode))
  :init
  ;; Enable Dired-X
  (with-eval-after-load 'dired (require 'dired-x))
  :custom
  ;; Set custom listing style
  ;; (dired-listing-switches "-agho --group-directories-first")
  (dired-listing-switches "-agho")   ; macOS version
  ;; Guess target directory
  (dired-dwim-target t)
  ;; Don't display free disk space
  (dired-free-space nil)
  ;; Reuse Dired buffers
  (dired-kill-when-opening-new-dired-buffer t)
  ;; Omit hidden (dot-) files
  (dired-omit-files "^\\.[a-zA-Z0-9]+")   ; with dired-omit-mode
  :config
  ;; Tag Dired buffer names
  (add-hook 'dired-mode-hook (lambda () (aj8/prefix-buffer-name "dired")))
  ;; Hide details by default
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  ;; Hide omitted files
  (add-hook 'dired-mode-hook #'dired-omit-mode))


;; dired-sidebar (tree browser leveraging dired)
(use-package dired-sidebar
  :bind (("C-c d" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :custom
  ;; Window rule
  ;; (dired-sidebar-display-alist
  ;;  '((side . right)
  ;;    (slot . -1)
  ;;    (window-width . aj8/side-window-width-dynamic)))
  ;;    ;; (window-parameters . ((no-delete-other-windows . t)))))
  ;; Set custom width
  (dired-sidebar-width 30)   ; default is 35
  ;; Don't delete dired-sidebar window
  (dired-sidebar-no-delete-other-windows t)
  ;; Refresh sidebar to match current file
  (dired-sidebar-should-follow-file t)
  ;; Center cursor when updating tui interface
  ;; (dired-sidebar-recenter-cursor-on-tui-update t)   ; TODO: what does this do?
  ;; The tree style to display
  (dired-sidebar-theme 'ascii)   ; none, ascii, icons (all-the-icons) (default)
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands))

;; osx-trash (system trash for OS X)
(use-package osx-trash
  :if (eq aj8/my-os 'macos)   ; macOS
  :config
  (osx-trash-setup)
  (setq delete-by-moving-to-trash t))   ; TODO: add this to all systems

;;; Help

;; helpful (a better *help* buffer)
(use-package helpful
  ;; :demand t
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
  :demand t
  :bind (:map minibuffer-local-map ("M-m" . marginalia-cycle))
  :config
  (marginalia-mode 1))

;; which-key (display available keybindings in popup) - [built-in package]
;;   MAYBE: which-key buffer overlaps with bottom side-window buffer
;;          (which might be OK?)
;;          When doing so, which-key fails to
;;            1) grow window to respect the height defined below
;;            2) display a complete list of commands
(use-package which-key
  :diminish
  :custom
  ;; Use minibuffer
  ;; (which-key-popup-type 'minibuffer)   ; default is side-window
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
  (customize-set-variable
   'which-key-min-column-description-width
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
  (add-hook 'yaml-ts-mode-hook
            (lambda () (setq-local devdocs-current-docs '("ansible"))))
  (add-hook 'bash-ts-mode-hook
            (lambda () (setq-local devdocs-current-docs '("bash"))))
  (add-hook 'latex-mode-hook
            (lambda () (setq-local devdocs-current-docs '("latex"))))
  (add-hook 'lua-mode-hook
            (lambda () (setq-local devdocs-current-docs '("lua~5.4"))))
  (add-hook 'python-ts-mode-hook
            (lambda () (setq-local devdocs-current-docs '("python~3.11")))))

;; devdocs-browser (browse devdocs.io documents using EWW)
;;   Usage: M-x devdocs-browser-install-doc
(use-package devdocs-browser
  :bind ("C-c H D" . devdocs-browser-open)
  ;; :custom
  ;; ;; Select devdocs window
  ;; (devdocs-window-select t)
  :custom
  (devdocs-browser-major-mode-docs-alist '((yaml-ts-mode "Ansible")
                                           (bash-ts-mode "Bash")
                                           (emacs-lisp-mode "Elisp")
                                           (latex-mode "LaTeX")
                                           (lua-mode "Lua")
                                           (python-ts-mode "Python"))))

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

;;; Org

;;; Outline

;; outline-minor-faces (headings faces for outline-minor-mode)
;;   See also outline-minor-mode-highlight.
(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook #'outline-minor-faces-mode))

;;; Search

;;; Selection

;; expreg (simple expand region)
(use-package expreg
  :bind (("C-c =" . expreg-expand)
         ("C-c -" . expreg-contract)))

;;; Spelling

;; flyspell-correct (correcting words with Flyspell via custom interface)
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ;; ("C-;" . flyspell-auto-correct-word)
              ("C-;" . flyspell-correct-wrapper)
              ("C-," . flyspell-correct-previous)
              ("C-." . flyspell-correct-next)
              ("C-c ," . my/flyspell-goto-previous-error)
              ("C-c ." . flyspell-goto-next-error)
              ("C-c ;" . flyspell-auto-correct-word)))

;;; Terminal

;; term (terminal-emulator) - [built-in package]
(use-package term
  :disabled
  :commands term)
  ;; :config
  ;; Match the default Bash shell prompt
  ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;; vterm (fully-featured terminal emulator)
(eval `(use-package vterm
         ;; :ensure-system-package (cmake libvterm-dev)
         :ensure-system-package (cmake ,(aj8/system-package-name 'libvterm))
         :commands vterm
         :config
         ;; Match the default Bash shell prompt
         (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")))

;; eshell (the Emacs command shell) - [built-in package]
(use-package eshell
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

;; ef-themes (colorful and legible themes)
(use-package ef-themes
  ;; Themes:
  ;;   Light: `ef-day', `ef-spring', `ef-summer', `ef-light',
  ;;          `ef-duo-light', `ef-trio-light', `ef-frost', `ef-cyprus',
  ;;          `ef-kassio', `ef-elea-light', `ef-maris-light',
  ;;          'ef-melissa-light'.
  ;;   Dark: `ef-night', `ef-autumn', `ef-winter', `ef-dark', `ef-duo-dark',
  ;;         `ef-trio-dark', `ef-bio', `ef-cherie', `ef-symbiosis',
  ;;         `ef-elea-dark', `ef-maris-dark', 'ef-melissa-dark'.
  ;;   All the themes are included in the variable `ef-themes-collection'.
  ;; Commands:
  ;;   `ef-themes-toggle'
  ;;   `ef-themes-select'
  ;;   `ef-themes-load-random'
  ;;   `ef-themes-preview-colors'
  ;;   `ef-themes-preview-colors-current'
  :if (eq aj8/my-os 'macos)   ; macOS
  :bind ("<f5>" . ef-themes-toggle)
  ;; Make customizations that affect Emacs faces before loading a theme
  :init
  (setq ef-themes-headings
        '((0 . (ultrabold))
          (1 . (extrabold))
          (2 . (bold))
          (3 . (semibold))
          (t . (regular))))
  ;; (setq ef-themes-region '(intense no-extend neutral))
  (setq ef-themes-region '(neutral))
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

;; modus-themes (elegant, highly legible and customizable themes) - [built-in package]
(use-package modus-themes
  :if (eq aj8/my-os 'linux)   ; Linux
  :bind ("<f5>" . modus-themes-toggle)
  ;; Add all customizations prior to loading the themes
  :init
  ;; Use italic font forms in more code constructs
  (setq modus-themes-italic-constructs t)
  ;; Use bold text in more code constructs
  (setq modus-themes-bold-constructs t)
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
  ;; Load the theme
  (if (aj8/daytime-p)
      (load-theme 'modus-operandi)
    (load-theme 'modus-vivendi)))

;; standard-themes (like the default theme but more consistent)
(use-package standard-themes
  :if (eq aj8/my-os 'wsl)   ; WSL
  :bind ("<f5>" . standard-themes-toggle)
  ;; Make customizations that affect Emacs faces before loading a theme
  :init
  (setq standard-themes-bold-constructs t
        standard-themes-italic-constructs t
        ;; standard-themes-mixed-fonts t
        ;; standard-themes-variable-pitch-ui t
        ;; standard-themes-mode-line-accented t
        ;; Accepts a symbol value:
        ;; standard-themes-fringes 'subtle
        ;; The following accept lists of properties
        standard-themes-links '(neutral-underline)
        ;; standard-themes-region '(no-extend neutral intense)
        standard-themes-region '(neutral)
        standard-themes-prompts '(bold italic))
        ;; More complex alist to set weight, height, and optional
        ;; `variable-pitch' per heading level (t is for any level not
        ;; specified):
        ;; standard-themes-headings
        ;; '((0 . (variable-pitch light 1.9))
        ;;   (1 . (variable-pitch light 1.8))
        ;;   (2 . (variable-pitch light 1.7))
        ;;   (3 . (variable-pitch semilight 1.6))
        ;;   (4 . (variable-pitch semilight 1.5))
        ;;   (5 . (variable-pitch 1.4))
        ;;   (6 . (variable-pitch 1.3))
        ;;   (7 . (variable-pitch 1.2))
        ;;   (t . (variable-pitch 1.1))))
  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)
  ;; Load theme
  (if (aj8/daytime-p)
      (load-theme 'standard-light :no-confirm)
    (load-theme 'standard-dark :no-confirm)))

;; circadian (theme-switching based on daytime)
(use-package circadian
  ;; :disabled  ; 0.3s startup time
  :defer 60
  :after (:any modus-themes ef-themes)
  :config
  (if (eq aj8/my-os 'wsl)   ; WSL
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
     ("~/projects"                . 2)
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
  ;; Disable magit-wip-mode lighter
  (magit-wip-mode-lighter "")
  :config
  ;; Enable word-wrap
  (add-hook 'magit-status-mode-hook (lambda () (visual-line-mode 1)))
  (add-hook 'magit-revision-mode-hook (lambda () (visual-line-mode 1)))
  ;; Back up uncommitted changes
  (magit-wip-mode 1)
  ;; Disable hl-line-mode
  (add-hook 'magit-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  ;; Disable magit-wip-mode for Tramp buffers
  (add-hook 'magit-mode-hook #'aj8/disable-magit-wip-mode-if-remote)
  ;; Add status flag to repository list
  (add-to-list 'magit-repolist-columns
               '("Flag" 4 magit-repolist-column-flag (:right-align t)))
  ;; Hide header section by default
  (add-to-list 'magit-section-initial-visibility-alist '(headers . hide))
  ;; Hide commit message section by default
  (add-to-list 'magit-section-initial-visibility-alist '(commit-message . hide))
  ;; Hide diffstat section by default
  (add-to-list 'magit-section-initial-visibility-alist '(diffstat . hide))
  ;; Add Git user to header
  (add-to-list 'magit-status-headers-hook 'magit-insert-user-header t)
  ;; Reduce log margin
  (setf (nth 4 magit-log-margin) 15)
  ;; Use abbreviated ages in log margin
  (setf (nth 1 magit-log-margin) 'age-abbreviated)
  ;; TODO: Activate Smerge hydra on file visit in smerge-mode
  ;; (add-hook 'magit-diff-visit-file-hook (lambda ()
  ;;                                  (when smerge-mode
  ;;                                    (hydra-smerge/body)))))
  ;; Clean up process buffers automatically
  (setq aj8/magit-cleanup-buffers t)
  ;; Hook for buffer cleanup
  (add-hook 'magit-mode-hook #'aj8/magit-start-buffer-cleanup-timer))

;; magit-gptcommit (Git commit with help of GPT)
;;   TODO: does not work
(use-package magit-gptcommit
  :disabled
  :demand t
  :after (gptel magit)
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-generate))
  :config
  ;; Add gptcommit transient commands to `magit-commit'
  ;;   Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))
  ;; Generate commit message automatically in magit status buffer
  ;; (magit-gptcommit-mode 1))

;; magit-todos (show source file TODOs in Magit)
;;   Available keywords: "HOLD", "TODO", NEXT","THEM", "PROG", "OKAY",
;;   "DONT", "FAIL", "DONE", "NOTE", "MAYBE" "KLUDGE, "HACK", "TEMP",
;;   "FIXME" "XXXX*"
(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))
  ;; :custom
  ;; ;; Don't require a colon after keyword
  ;; (magit-todos-keyword-suffix ""))

;; diff-hl (highlight uncommitted changes using VC)
(use-package diff-hl
  :disabled
  :config
  ;; Integration with Magit
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  ;; Put highlights in the margin in terminal
  (when (display-graphic-p)
    (setq diff-hl-margin-mode t))
  ;; Use diff-hl-mode everywhere
  (global-diff-hl-mode 1))

;; ztree (text mode directory tree)
;;   MAYBE: Alternative package: diffed
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
  :demand t  ; note that :demand and :after can be combined
             ; (ensures popper is required, not auto-loaded, after project)
  :after project   ; needed for popper-group-by-project
  :bind (("C-`"   . popper-toggle)
         ;; ("M-`"   . popper-cycle)   ; TODO: conflicts with consult
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
   '("\\*\\(Messages\\|Warnings\\)\\*"
     "\\*\\(Async-native-compile-log\\|.*ls.*\\)\\*"
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
  :commands (erc erc-tls)
  :custom
  ;; Server settings
  (erc-server "irc.libera.chat")
  (erc-nick "ajdev8")
  (erc-user-full-name user-full-name)
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

;; gptel (a simple ChatGPT client for Emacs)
(use-package gptel
  :bind (("C-c t c" . gptel)
         ("C-c t m" . gptel-menu))
  :init
  (which-key-add-key-based-replacements "C-c t" "gptel")
                                        ; add label for prefix key
  ;; :custom
  ;; Set mode
  ;; (gptel-default-mode 'text-mode)   ; defauilt: markdown-mode
  ;; Don't use Curl
  ;; (gptel-use-curl nil)
  :config
  ;; Gemini
  (gptel-make-gemini "Gemini"
    :key (gptel-api-key-from-auth-source
          "generativelanguage.googleapis.com" "apikey")
    :stream t)   ; make available
  ;; (setq gptel-backend (gptel-make-gemini "Gemini"
  ;;                       :key (gptel-api-key-from-auth-source
  ;;                             "generativelanguage.googleapis.com" "apikey")
  ;;                       :stream t))   ; set default
  ;; (setq gptel-model 'gemini-pro)
  ;; (setq gptel-model 'gemini-1.5-pro-latest)   ; default is gemini-pro
  ;; ChatGPT
  ;; (setq gptel-model 'gpt-4)   ; default is gpt-3.5-turbo
  ;; (setq gptel-model 'gpt-4-turbo)
  (setq gptel-model 'gpt-4o)
  ;; Enable word-wrap
  (add-hook 'gptel-mode-hook (lambda () (visual-line-mode 1)))
  ;; Scroll window automatically
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)
  ;; Jump to next prompt after response
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response))

;; gptel-quick (quick LLM lookups in Emacs)
(use-package gptel-quick
  :bind ("C-c t h" . gptel-quick))
  ;; :config
  ;; ;; Set approximate word count of LLM summary
  ;; (gptel-quick-word-count 12))

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

;; obsidian (Obsidian notes interface)
(use-package obsidian
  :bind (:map obsidian-mode-map
              ("C-c C-o" . obsidian-follow-link-at-point))   ; default: markdown-follow-thing-at-point
              ;; ("C-c C-b" . obsidian-backlink-jump)
              ;; ("C-c C-l" . obsidian-insert-wikilink))   ; alt: obsidian-insert-link
  :custom
  ;; Define folder for `obsidian-capture'
  (obsidian-inbox-directory "Inbox")
  :config
  ;; Define vault location
  (obsidian-specify-path "~/Dropbox/Apps/Obsidian/obsidian-vault/"))
  ;; Enable mode globally
  ;; (global-obsidian-mode t))

;; obsidian-yaml-tools ()
;; (use-package obsidian-yaml-tools
;;   :demand)

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

;; Personal settings
(setq user-full-name "Andreas Jonsson")
(setq user-mail-address "ajdev8@gmail.com")

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

;; Skip some buffers when switching buffers
;; (setq switch-to-prev-buffer-skip-regexp regex)

;; Allow minibuffer commands while in the minibuffer
(setq enable-recursive-minibuffers t)

;; Timeout for messages in active minibuffer
(setq minibuffer-message-clear-timeout 1)

;; Reuse windows for *Help* buffers
(setq help-window-keep-selected t)

;; Open *info* buffers in same window
(setq info-lookup-other-window-flag nil)

;; Always select help window
(setq help-window-select t)

;; Kill customize group windows
(setq custom-buffer-done-kill t)

;; Set default sort order in Ibuffer
(setq ibuffer-default-sorting-mode 'filename/process)

;;; Coding

;; Open up the debugger on error
;; (setq debug-on-error t)

;; Use maximum decoration detail
(setq treesit-font-lock-level 4)

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
;;   MAYBE: only consider buffers with the same mode (see
;;          dabbrev-friend-buffer-function)
(with-eval-after-load "hippie-exp"
  (add-to-list 'hippie-expand-ignore-buffers "^\\*.*\\*$")
  (add-to-list 'hippie-expand-ignore-buffers "magit:.*")
  (add-to-list 'hippie-expand-ignore-buffers aj8/buffer-skip-regexp))

;;; Editing

;; Set fill column
(setq-default fill-column 76)   ; default is 70

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

;; Move point to duplicated line
(setq duplicate-line-final-position -1)

;;; Files

;; Number of saved recent files
(setq recentf-max-saved-items 100)

;;; Help

;; More extensive apropos commands
(setq apropos-do-all t)

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

;;; Org

;; Use speed keys
(setq org-use-speed-commands t)

;; Don't require confirmation when evaluating code
(setq org-confirm-babel-evaluate nil)

;; Use shift-selection
;;   (disables org-shiftup, org-shiftdown, org-shiftleft and org-shiftright)
;; (setq org-support-shift-select t)

;;; Outline

;; Outline minor mode prefix
(which-key-add-key-based-replacements "C-c @" "outline")
                                        ; add label for prefix key

;; Use TAB and S-TAB for cycling
(setq outline-minor-mode-cycle t)

;; Highlight headings
;;   See also outline-minor-faces.
;; (setq outline-minor-mode-highlight t)   ; alternatives: 'override and 'append

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
;;   Requires: aspell
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

;; Show context around the opening paren if it is offscreen
;;   MAYBE: Make the text in the echo area persist until key press
(setq show-paren-context-when-offscreen t)

;; Show current project name on the mode line
(setq project-mode-line t)

;;; Version control

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Use horizontal (side-by-side) view by default in ediff
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-merge-split-window-function #'split-window-horizontally)

;; Let ediff use existing frame in GUI
(when (display-graphic-p)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; Kill all Ediff buffers on exit
(setq aj8/ediff-cleanup-buffers t)

;;; Web

;; URL browser settings
(setq browse-url-browser-function #'browse-url-default-browser)
(setq browse-url-secondary-browser-function #'eww-browse-url)
(setq browse-url-handlers
      '(("\\.md$" . eww-browse-url)
        ("." . browse-url-default-browser)))

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
  (keymap-set shr-map "u" nil)
  (keymap-set shr-map "v" nil)
  (keymap-set shr-map "w" nil))
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

;; Set minimum window height
;; (setq window-min-height 16)

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
        ("\\*\\(Messages\\|Warnings\\|Native-compile-Log\\|Async-native-compile-log\\)\\*"
         (display-buffer-in-side-window)
         (window-height . ,aj8/side-window-height)
         (side . top)
         (window-parameters . ((no-delete-other-windows . t))))
        ;; ("\\*\\(Native-compile-Log\\)\\*"
        ;; ("\\*\\(Async-native-compile-log\\|Native-compile-Log\\|EGLOT.*events\\|.*-ls\\(::.*\\)?\\|texlab\\(::stderr\\)?\\)\\*"
        ("\\*\\(copilot events\\|EGLOT.*events\\|Flymake diagnostics.*\\|texlab\\(::stderr\\)?\\|tramp.*\\|.*-ls\\(::.*\\)?\\)\\*"
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
        ;; MAYBE: doesn't work initially
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
           (with-current-buffer buffer (or (derived-mode-p 'git-rebase-mode)
                                           (derived-mode-p 'tabulated-list-mode))))
         (display-buffer-in-side-window)
         (window-width . ,aj8/side-window-width-dynamic)
         (side . right)
         (slot . -1)
         (window-parameters . ((no-delete-other-windows . t))))
        ("\\*\\(Bookmark List\\|Benchmark Init Results.*\\|ChatGPT.*\\|Embark Collect:.*\\|Gemini.*\\|Occur\\|.*Output\\|Semantic SymRef\\|devdocs\\|eldoc\\|package update results\\|tex-shell\\)\\*"
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
;;   Only for switch-to-buffer,  not used by switch-to-prev/next-buffer
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

;; Show mode headers in describe-bindings buffer
(setq describe-bindings-outline t)

;; Don't prompt with xref-find-references
(with-eval-after-load "xref"
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t))

;; Fill gpg passwords in the minibuffer
(setq epg-pinentry-mode 'loopback)

;;;; Modes

;;; Admin...

;;; Buffers...

;; Persistent minibuffer history
(savehist-mode 1)

;; Additional variables to persist between sessions
(add-to-list 'savehist-additional-variables 'kill-ring)
(add-to-list 'savehist-additional-variables 'global-mark-ring)

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

;;; Navigation...

;; Subword movement and editing: camelCase
;;    Cannot be enabled at the same time as superword-mode
;; (add-hook 'prog-mode-hook (lambda () (subword-mode 1)))

;; Superword movement and editing: snake_case and kebab-case
;;    Cannot be enabled at the same time as subword-mode
;; (add-hook 'prog-mode-hook (lambda () (superword-mode 1)))

;;; Org

;;; Outline...

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

;; Display continuation lines with prefixes
(global-visual-wrap-prefix-mode 1)

;; Enable line numbers
;; (add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

;;; Version control...

;;; Windows...

;; Enable winner mode
(winner-mode 1)

;;; Web...

;; Buttonize URLs
;; (global-goto-address-mode 1)

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

;; after-change-major-mode: add Treesitter indicator in the modeline
(add-hook 'after-change-major-mode-hook 'aj8/treesit-mode-name)

;; ediff-before-setup: disable side windows before Ediff
;;   TODO: Should disable/enable rather than toggle
(add-hook 'ediff-before-setup-hook 'window-toggle-side-windows)
(add-hook 'ediff-quit-hook 'window-toggle-side-windows)

;; ediff-quit-hook: Kill remaining Ediff buffers
(add-hook 'ediff-quit-hook 'aj8/ediff-cleanup-buffers)

;; kill-buffer: collect list of killed buffers
(add-hook 'kill-buffer-hook #'reopen-killed-file--add-to-list)


;; messages-buffer-mode: auto-update *Messages* buffer
;;   Note: if the cursor is left at the end of the *Messages* buffer it
;;         tails automatically
(add-hook 'messages-buffer-mode-hook #'auto-revert-tail-mode)

;;; Modes

;; emacs-lisp-mode: outline settings
(add-hook 'emacs-lisp-mode-hook
          #'outline-headers-for-semicolon-buffers)

;; conf-xdefaults-mode: outline settings
(add-hook 'conf-xdefaults-mode-hook
          #'outline-headers-for-exclamation-mark-buffers)

;; Info-mode: allow multiple Info buffers
(add-hook 'Info-mode-hook #'rename-uniquely)

;; outline-mode: remove form-feed character (^L) from regexp
(add-hook 'outline-mode-hook
          (lambda () (setq outline-regexp "[*]+")))

;; powershell-mode: outline settings
(add-hook 'powershell-mode-hook
          #'outline-headers-for-hash-mark-buffers)

;; python-mode: outline settings
(add-hook 'python-base-mode-hook
          #'outline-headers-for-hash-mark-buffers)

;; python-mode: other settings
;; (add-hook 'python-base-mode-hook 'aj8/python-mode-hook)

;; sh-mode: for non-standard bash config files
(add-to-list 'auto-mode-alist '("\\.bash_.*\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc_.*\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.profile_.*\\'" . bash-ts-mode))

;; [la]tex-mode: outline settings
(add-hook 'latex-mode-hook
          #'outline-headers-for-percentage-buffers)

;; [la]tex-mode: enable auto-mode
(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))
;; (add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-ts-mode))

;; [la]tex-mode: adjust comment style
(add-hook 'latex-mode-hook (lambda () (setq comment-add 0)))   ; use single comment
                                                               ; character only

;; shell-script-mode: outline settings
(add-hook 'sh-mode-hook
          #'outline-headers-for-hash-mark-buffers)

;; tramp-mode: add mode line indicator
(add-to-list 'global-mode-string '(:eval (aj8/tramp-indicator)) t)

;; visual-line-mode
(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'helpful-mode-hook #'visual-line-mode)
(add-hook 'Info-mode-hook #'visual-line-mode)
;; (add-hook 'Man-mode-hook #'visual-line-mode)


;;;;; KEYBINDINGS

;;   MAYBE: Use minor-mode for keybindings?
;;          (https://stackoverflow.com/a/683575/1610035)
;;          (https://emacs.stackexchange.com/a/358/33325)

;;;; Escape codes

;; Add some escape codes for different terminal emulators
(when (not (display-graphic-p))   ; if using terminal
  (cond
   ;; ((eq aj8/my-terminal-emulator 'gnome-terminal)
   ;;  ;; Add some escape codes for Gnome Terminal
   ;;  (message "Define escape codes for Gnome Terminal"))
   ((eq aj8/my-terminal-emulator 'iterm2)
    ;; Add some escape codes for iTerm2
    (message "Define escape codes for iTerm2")
    (define-key input-decode-map "\e[8;5u" (kbd "C-<backspace>"))
    (define-key input-decode-map "\e[8;6u" (kbd "C-S-<backspace>"))
    (define-key input-decode-map "\e[8;7u" (kbd "C-M-<backspace>"))
    (define-key input-decode-map "\e[32;2u" (kbd "S-SPC"))   ; fails in iTerm2
    (define-key input-decode-map "\e[91;7u" (kbd "C-M-["))   ; fails in iTerm2
    (define-key input-decode-map "\e[107;6u" (kbd "C-S-k"))   ; fails in iTerm2
    (define-key input-decode-map "\e[113;4u" (kbd "M-S-q"))
    (define-key input-decode-map "\e[113;8u" (kbd "C-M-S-q"))
    (define-key input-decode-map "\e[118;8u" (kbd "C-M-S-v"))
    (define-key input-decode-map "\e[123;5u" (kbd "C-{"))
    (define-key input-decode-map "\e[125;5u" (kbd "C-}")))
   ;; ((eq aj8/my-terminal-emulator 'konsole)
   ;;  ;; Add some escape codes for Konsole
   ;;  (message "Define escape codes for Konsole"))
   ((or (eq aj8/my-terminal-emulator 'windows-terminal)
       (eq aj8/my-terminal-emulator 'konsole))
    ;; Add some escape codes for Windows Terminal
    (message "Define escape codes for Windows Terminal")
    (define-key input-decode-map "\e[8;5u" (kbd "C-<backspace>"))
    (define-key input-decode-map "\e[8;6u" (kbd "C-S-<backspace>"))
    (define-key input-decode-map "\e[8;7u" (kbd "C-M-<backspace>"))
    (define-key input-decode-map "\e[32;2u" (kbd "S-SPC"))   ; fails in Konsole, WSL
    (define-key input-decode-map "\e[44;5u" (kbd "C-,"))
    (define-key input-decode-map "\e[44;7u" (kbd "C-M-,"))
    (define-key input-decode-map "\e[46;5u" (kbd "C-."))
    (define-key input-decode-map "\e[46;7u" (kbd "C-M-."))
    ;; (define-key input-decode-map "\e[47;5u" (kbd "C-/"))
    ;; (define-key input-decode-map "\e[47;7u" (kbd "C-M-/"))
    (define-key input-decode-map "\e[59;5u" (kbd "C-;"))   ; fails in WSL
    (define-key input-decode-map "\e[59;7u" (kbd "C-M-;"))   ; fails in WSL
    (define-key input-decode-map "\e[91;7u" (kbd "C-M-["))   ; fails in Konsole, WSL
    ;; (define-key input-decode-map "\e[93;7u" (kbd "C-M-]"))
    (define-key input-decode-map "\e[96;5u" (kbd "C-`"))   ; fails in WSL
    (define-key input-decode-map "\e[96;7u" (kbd "C-M-`"))   ; fails in WSL
    (define-key input-decode-map "\e[107;6u" (kbd "C-S-k"))   ; fails in Konsole, WSL
    (define-key input-decode-map "\e[113;4u" (kbd "M-S-q"))
    (define-key input-decode-map "\e[113;8u" (kbd "C-M-S-q"))
    (define-key input-decode-map "\e[118;8u" (kbd "C-M-S-v"))
    (define-key input-decode-map "\e[123;5u" (kbd "C-{"))   ; fails in WSL
    (define-key input-decode-map "\e[125;5u" (kbd "C-}")))))   ; fails in WSL

  ;; (define-key input-decode-map "\e[1;8A" [C-M-S-up])
  ;; (define-key input-decode-map "\e[1;8B" [C-M-S-down])

  ;; TODO: Test escape codes for all terminal emulators:
  ;;   Test:
  ;;     [ ] Gnome Terminal
  ;;     [x] iTerm2
  ;;     [x] Konsole
  ;;     [x] Windows Terminal

;;;; Translations

(keymap-set key-translation-map "M-<up>" "M-p")
(keymap-set key-translation-map "M-<down>" "M-n")
(keymap-set key-translation-map "C-M-<up>" "C-M-p")
(keymap-set key-translation-map "C-M-<down>" "C-M-n")

;;;; General

;;; Admin

;;; Buffers

;; Buffer navigation
(keymap-global-set "C-x <right>" #'aj8/next-buffer)
(keymap-global-set "C-x <left>" #'aj8/previous-buffer)
(keymap-global-set "C-x C-<right>" #'my/project-next-buffer)
(keymap-global-set "C-x C-<left>" #'my/project-previous-buffer)

;; Kill buffer
(keymap-global-set "C-x k" #'kill-current-buffer)

;; Kill buffer (other window)
(keymap-global-set "C-c k" #'my/kill-buffer-other-window)

;;; Coding

;;; Completion

;; Use hippie-expand
(keymap-global-set "<remap> <dabbrev-expand>" #'hippie-expand)

;; Cycle through orderless matching styles on the fly
(keymap-global-set "M-o" #'aj8/orderless-matching-style-cycle)

;;; Editing

;; Kill line to the left
(keymap-global-set "C-<backspace>" (lambda () (interactive) (kill-line 0)))

;; Manipulate case
(keymap-global-set "M-u" #'upcase-dwim)
(keymap-global-set "M-l" #'downcase-dwim)
;; (keymap-global-set "M-c" #'capitalize-dwim)
(keymap-global-set "M-c" #'aj8/capitalize-word-at-point)

;; Fill respecting indentation
;;   (use with `C-x .'  for comments)
(keymap-global-set "C-c q" #'fill-individual-paragraphs)

;; Copy symbol at point
(keymap-global-set "C-c x c" #'my/copy-symbol-at-point)

(which-key-add-key-based-replacements "C-c x" "misc")
                                        ; add label for prefix key

;; Indent to next nonblank character in previous line
(keymap-global-set "C-c TAB" #'indent-relative)

;; Exit recursive edit
;;   Default key C-M-c overridden by custom Smartparens key
(keymap-global-set "C-c C-c" #'exit-recursive-edit)

;; Duplicate line or region
(keymap-global-set "C-c C-<down>" #'duplicate-dwim)

;;; Files

;; Find file at point
(keymap-global-set "C-c f" #'find-file-at-point)

;; Forget project
(keymap-global-set "C-x p t" #'project-forget-project)

;;; Help

;; Display keymaps
(keymap-global-set "C-c H k" #'describe-keymap)

;; Display commands by category
(keymap-global-set "C-c H s" #'shortdoc-display-group)

(which-key-add-key-based-replacements "C-c H" "help")
                                        ; add label for prefix key

;; Set keybindings for find-function and relatives
(find-function-setup-keys)

;;; Navigation

;; Paragraph navigation
(keymap-global-set "M-p" #'backward-paragraph)
(keymap-global-set "M-n" #'forward-paragraph)

;; Move to indentation point
(keymap-global-set "M-a" #'back-to-indentation)

;; (keymap-global-set "C-c <up>" #'aj8/previous-line)
;; (keymap-global-set "C-c <down>" #'aj8/next-line)

;; Enable scroll lock
(keymap-global-set "C-c L" #'scroll-lock-mode)

;; Display Imenu
;; (keymap-global-set "C-c i" #'imenu)

;; Xref
(keymap-global-set "M-?" #'xref-find-references)      ; default
(keymap-global-set "C-c ?" #'xref-find-definitions)   ; old default M-.
(keymap-global-set "C-c <" #'xref-go-back)
(keymap-global-set "C-c >" #'xref-go-forward)         ; default C-M-,

;;; Org

;;; Outline

;; Toggle outline-minor-mode
(keymap-global-set "C-c O" #'outline-minor-mode)

;; Toggle outline-minor-moderepeat
(keymap-global-set "C-c F" #'hs-minor-mode)
(keymap-global-set "C-c @ <left>" #'hs-hide-block)
(keymap-global-set "C-c @ <right>" #'hs-show-block)
(keymap-global-set "C-c @ <up>" #'hs-hide-all)
(keymap-global-set "C-c @ <down>" #'hs-show-all)

;;; Search

(keymap-set isearch-mode-map "TAB" #'isearch-complete)

;;; Selection

;;; Spelling

;;; Terminal

;;; Theme

;; Display column number
(keymap-global-set "C-c N" #'column-number-mode)

;;; Version control

;; Show diffs between buffers
(keymap-global-set "C-c e b" #'ediff-buffers)

;; Show diffs between regions
(keymap-global-set "C-c e l" #'ediff-regions-linewise)
(keymap-global-set "C-c e w" #'ediff-regions-wordwise)

(which-key-add-key-based-replacements "C-c e" "ediff")
                                        ; add label for prefix key

;; Show diffs between file revisions
(keymap-global-set "C-x v -" #'vc-ediff)

;;; Web

;; Browse URL at point
(keymap-global-set "C-c b" #'browse-url-at-point)

;;; Windows

;; Windmove keys
(windmove-default-keybindings 'ctrl)
(windmove-swap-states-default-keybindings '(ctrl shift))

;; Open windows
(keymap-global-set "C-c w <up>" #'windmove-display-up)
(keymap-global-set "C-c w <down>" #'windmove-display-down)
(keymap-global-set "C-c w <left>" #'windmove-display-left)
(keymap-global-set "C-c w <right>" #'windmove-display-right)
(keymap-global-set "C-c w 0" #'windmove-display-same-window)

;; Delete windows
(keymap-global-set "C-c w C-<up>" #'windmove-delete-up)
(keymap-global-set "C-c w C-<down>" #'windmove-delete-down)
(keymap-global-set "C-c w C-<left>" #'windmove-delete-left)
(keymap-global-set "C-c w C-<right>" #'windmove-delete-right)

;; Cycle window configurations
(keymap-set winner-mode-map "C-c w <" #'winner-undo)
(keymap-set winner-mode-map "C-c w >" #'winner-redo)

(which-key-add-key-based-replacements "C-c w" "windows")
                                        ; add label for prefix key

;; Resize windows
(keymap-global-set "C-x {" #'my/move-splitter-up)
(keymap-global-set "C-x }" #'my/move-splitter-down)
(keymap-global-set "C-x >" #'my/move-splitter-right) ; override `scroll-right'
(keymap-global-set "C-x <" #'my/move-splitter-left)  ; override `scroll-left'

;; Toggle side windows
(keymap-global-set "C-x |" #'window-toggle-side-windows)

;; Misc window manipulation
(keymap-global-set "C-x !" #'delete-other-windows-vertically)
(keymap-global-set "C-x =" #'balance-windows)
                                        ; override `what-cursor-position'
(keymap-global-set "C-x +" #'balance-windows-area)
                                        ; override `balance-windows'
;; (keymap-global-set "C-x -" #'shrink-window-if-larger-than-buffer) ; default
(keymap-global-set "C-x _" #'fit-window-to-buffer)   ; enlarges and shrinks
(keymap-global-set "C-x 9" #'my/toggle-window-split)

;;; Other

;; Reload init.el
(keymap-global-set "C-c x r" #'reload-init-file)

;; Evaluate next sexp
(keymap-global-set "C-x M-e" #'my/eval-next-sexp)

;; Evaluate sexp at point
(keymap-global-set "C-x C-M-e" #'my/eval-sexp-at-point)

;;; Unbind keys

;; (keymap-global-set "C-x" nil)
;; (keymap-global-unset "C-x")   ; alternative syntax

;;;; Hooks

;; ediff-mode
;;   MAYBE: check functionality
(add-hook 'ediff-keymap-setup-hook
          ;; Use both versions with ediff
          (lambda () (keymap-set ediff-mode-map "d" #'my/ediff-copy-both-to-C)))

;; Info-mode
(add-hook 'Info-mode-hook
          ;; Disable M-n
          (lambda () (keymap-local-unset "M-n")))

;;[la]tex-mode
(add-hook 'tex-mode-hook
          ;; Disable commands handled by Smartparens
          (lambda ()
            (keymap-set tex-mode-map "C-c ]" nil)
            (keymap-set tex-mode-map "C-c {" nil)
            (keymap-set tex-mode-map "C-c }" nil)
            (keymap-set tex-mode-map "C-c /" nil)))

;; sh-mode
(add-hook 'sh-mode-hook
          ;; Disable SMIE commands
          (lambda ()
            (keymap-local-unset "C-c =")
            (keymap-local-unset "C-c <")
            (keymap-local-unset "C-c >")
            (keymap-local-unset "C-c ?")))

;; Treesitter
;;   Copy standard mode hooks to their Treesitter equivalents
(dolist (hook '((bash-ts-mode-hook . sh-mode-hook)
                (css-ts-mode-hook . css-mode-hook)
                ;; (html-ts-mode-hook . html-mode-hook)   ; NA
                (js-ts-mode-hook . js-mode-hook)
                (json-ts-mode-hook . json-mode-hook)
                ;; (lua-ts-mode-hook . lua-mode-hook)    ; NA
                (markdown-ts-mode-hook . markdown-mode-hook)
                (python-ts-mode-hook . python-mode-hook)
                ;; (latex-ts-mode-hook . latex-mode-hook) ; NA
                (yaml-ts-mode-hook . yaml-mode-hook)))
  (dolist (func (symbol-value (cdr hook)))
    (add-hook (car hook) func)))

;;;; Hydras

(which-key-add-key-based-replacements "C-c y" "hydra")
                                        ; add label for prefix key

;;; Editing

;; Markdown
(defhydra hydra-markdown (:hint nil :foreign-keys run)
  "
Format:         _B_: bold           _I_: italic
                _Q_: quote          _C_: code           _P_: pre-formatted
Headings:       _H_: automatic
                _1_: h1       _2_: h2       _3_: h3       _4_: h4
Lists:          _m_: new item
Outline:        _k_: move up        _j_: move down
                _h_: promote        _l_: demote
Tables:         _a_: align          _s_: sort           _t_: transpose
  Navigation    _n_: next row       _p_: previous row
                _f_: forward cell   _b_: backward cell
  Rows          ___: insert         _-_: delete
  Columns       _|_: insert         _\\_: delete
                _<_: move left      _>_: move right
"
  ("B" markdown-insert-bold)
  ("I" markdown-insert-italic)
  ("Q" markdown-insert-blockquote :color gray)
  ("C" markdown-insert-code :color gray)
  ("P" markdown-insert-pre :color gray)

  ("H" markdown-insert-header-dwim)
  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)

  ("m" markdown-insert-list-item)

  ("h" markdown-promote)
  ("l" markdown-demote)
  ("k" markdown-move-up)
  ("j" markdown-move-down)

  ("a" markdown-table-align)
  ("s" markdown-table-sort-lines)
  ("t" markdown-table-transpose)
  ("n" markdown-table-next-row)
  ("p" aj8/markdown-table-prev-row)
  ("f" markdown-table-forward-cell)
  ("b" markdown-table-backward-cell)
  ("_" markdown-table-insert-row)
  ("-" markdown-table-delete-row)
  ("|" markdown-table-insert-column)
  ("\\" markdown-table-delete-column)
  ("<" markdown-table-move-column-left)
  (">" markdown-table-move-column-right)

  ("q" nil "quit"))

(keymap-global-set "C-c y m" #'hydra-markdown/body)

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

(keymap-global-set "C-c y w" #'hydra-window/body)

;;; Navigation

;; Scrolling
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

(keymap-global-set "C-c y s" #'hydra-scroll/body)

;; Line navigation
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

(keymap-global-set "C-c y n" #'hydra-navigation/body)

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

(keymap-global-set "C-c y o" #'hydra-outline/body)

;;; Version control

;; Smerge
;;   Reference: https://github.com/alphapapa/unpackaged.el?tab=readme-ov-file#smerge-mode
(defhydra hydra-smerge
  (:color pink :hint nil :post (smerge-auto-leave))
  "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("R" smerge-refine)
  ("E" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("k" smerge-kill-current)
  ("ZZ" (lambda ()
          (interactive)
          (save-buffer)
          (bury-buffer))
   "Save and bury buffer" :color blue)
  ("q" nil "cancel" :color blue))

(keymap-global-set "C-c y e" #'hydra-smerge/body)

;;;;; LATE SETTINGS

;; System dependent settings
(cond ((eq aj8/my-os 'macos)           ; macOS
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

      ((eq aj8/my-os 'wsl)            ; WSL
       ;; Enable (default) web browser
       ;;   Requires: wslu
       (setq browse-url-generic-program "wslview")
       (setq browse-url-browser-function #'browse-url-generic)
       (advice-add #'browse-url-default-browser :override #'browse-url-generic)
       (message "Late settings WSL"))

      ((eq aj8/my-os 'linux)      ; Linux
       (cond
        ((eq aj8/my-linux-os 'fedora)
         (message "Late settings Fedora"))
        ((eq aj8/my-linux-os 'ubuntu)
         (message "Late settings Ubuntu")))
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
