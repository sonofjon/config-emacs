;;; init.el --- My custom config -*- lexical-binding: t; -*-
;;
;; Author: Andreas Jonsson <ajdev8@gmail.com>
;; Maintainer: Andreas Jonsson <ajdev8@gmail.com>
;; URL: https://github.com/sonofjon/config-emacs.el
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience

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
(cond ((eq aj8/my-os 'macos)   ; macOS
       ;; GUI settings
       (when (display-graphic-p)
         ;; Import path from shell
         (exec-path-from-shell-initialize))
       (message "Early settings for macOS"))

      ((eq aj8/my-os 'wsl)     ; WSL
       (message "Early settings WSL"))

      ((eq aj8/my-os 'linux)   ; Linux
       (message "Early settings Linux"))
       ;; Configure language environment
       ;; (setenv "LANG" "en_US.UTF-8"))

      (t (user-error "Unexpected system-name: %s" (system-name))))


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

;; Prefer the newest commit over the latest release
;; (setq use-package-vc-prefer-newest t)

;;; Local

(require 'aj8-lisp)
(require 'aj8-keys)


;;;; Packages

;;; Selected packages
;;;   Install packages with `package-install-selected-packages'
;;;   Remove packages with `package-autoremove'
;;;   `use-package' is used for configuration only
(customize-set-variable 'package-selected-packages
                        '(ahk-mode
                          all-the-icons
                          all-the-icons-dired
                          ansible
                          benchmark-init
                          cape
                          circadian
                          consult
                          consult-eglot
                          consult-project-extra
                          corfu
                          corfu-terminal
                          csv-mode
                          dashboard
                          devdocs
                          devdocs-browser
                          dired-sidebar
                          diff-hl
                          diminish
                          dimmer
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
                          gptel-magit
                          helpful
                          hydra
                          jinx
                          keychain-environment
                          keyfreq
                          lingva
                          lorem-ipsum
                          lua-mode
                          magit
                          magit-delta
			  magit-gptcommit
			  magit-todos
                          marginalia
                          markdown-mode
                          mcp
                          mosey
                          move-dup
                          multiple-cursors
                          multi-line
                          obsidian
                          orderless
                          osx-trash
                          outline-minor-faces
                          package-lint
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
                          system-packages   ; for use-package-ensure-system-package
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
(customize-set-variable
 'package-vc-selected-packages
 `(;; (foo . "0f39eb3fd9")   ; specific revision
   ;; (bar . nil)            ; any revision
   (ai-code-interface :url "https://github.com/tninja/ai-code-interface.el.git" :rev :newest)
   (claude-code :url "https://github.com/stevemolitor/claude-code.el.git" :rev :newest)
   (combobulate :url "https://github.com/mickeynp/combobulate.git" :rev :newest)
   (copilot :url "https://github.com/copilot-emacs/copilot.el.git" :rev :newest)
   (gptel-quick :url "https://github.com/karthink/gptel-quick.git" :rev :newest)
   (gptel-toolkit :url "https://github.com/sonofjon/gptel-toolkit.el.git"  :rev :newest)
   (inheritenv :url "https://github.com/purcell/inheritenv.git"  :rev :newest)
   ;; (llm-tool-collection :url "https://github.com/skissue/llm-tool-collection.git" :rev :newest)
   (markdown-links :url "https://github.com/sonofjon/markdown-links.el.git" :rev :newest)
   (obsidian-yaml-tools :url "https://github.com/sonofjon/obsidian-yaml-tools.el.git")
   (reflow :url "https://github.com/sonofjon/reflow.el" :rev :newest)))
   ;; (sinister :url "https://github.com/positron-solutions/sinister" :rev :newest)))

;; Install selected packages
(package-install-selected-packages)
(package-vc-install-selected-packages)

;; Install local packages
;; (package-vc-install-from-checkout
;;  (concat (expand-file-name "~") "/projects/obsidian-yaml-tools.el")
;;  "obsidian-yaml-tools")
;; (package-vc-install-from-checkout
;;  (concat (expand-file-name "~") "/projects/gptel-toolkit")
;;  "gptel-toolkit")

;;; Early packages

;; benchmark-init (benchmarks for require and load calls)
(use-package benchmark-init
  :disabled
  :init
  ;; Start benchmark
  (benchmark-init/activate)
  ;; Reduce "Module" column width
  (with-eval-after-load 'benchmark-init-modes
    (setf (cadr (aref benchmark-init/list-format 0)) 30))   ; default is 65
  :config
  ;; Disable collection of benchmark data after init
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

;; esup (the Emacs StartUp Profiler (ESUP))
;;   MAYBE: Full of bugs and inactive maintainer
(use-package esup
  :disabled)

(use-package exec-path-from-shell
  :if (eq aj8/my-os 'macos))   ; macOS

;;; Built-in packages

;; TODO: check package descriptions

;; abbrev (abbrev mode commands for Emacs)
(use-package abbrev
  :ensure nil   ; don't install built-in packages
  :diminish
  ;; Expand abbreviations
  :hook (text-mode . abbrev-mode))

;; apropos (apropos commands for users and programmers)
(use-package apropos
  :ensure nil   ; don't install built-in packages
  :custom
  ;; More extensive apropos commands
  (apropos-do-all t))

;; bookmark (set bookmarks, maybe annotate them, jump to them later)
(use-package bookmark
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Auto-save bookmarks
  (bookmark-save-flag 1))

;; browse-url (pass a URL to a web browser)
(use-package browse-url
  :ensure nil   ; don't install built-in packages
  :bind (("C-c b" . browse-url-at-point))
  :custom
  ;; Use default browser
  (browse-url-browser-function #'browse-url-default-browser)
  ;; Use EWW as secondary browser
  (browse-url-secondary-browser-function #'eww-browse-url)
  ;; Custom browser settings
  (browse-url-handlers
   '(("\\.md$" . eww-browse-url)
     ("." . browse-url-default-browser))))

;; custom (tools for declaring and initializing options)
(use-package custom
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Kill customize group windows
  (custom-buffer-done-kill t)
  ;; Custom interface: don't convert symbols to words (tags)
  (custom-unlispify-tag-names nil)
  ;; Custom interface: don't convert symbols to words (menu)
  (custom-unlispify-menu-entries t))

;; dabbrev (dynamic abbreviation package)
(use-package dabbrev
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Let Dabbrev searches be case sensitive
  (dabbrev-case-fold-search nil)
  :config
  ;; Don't let Dabbrev check authinfo buffers
  (with-eval-after-load 'dabbrev
    (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)))

;; desktop (save partial status of Emacs when killed)
(use-package desktop
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Don't clear any variables
  (desktop-globals-to-clear nil)
  :config
  ;; Restore desktop between sessions
  ;;   TODO: doesn't restore side-windows
  (desktop-save-mode 1))

;; diff-mode (a mode for viewing/editing context diffs)
(use-package diff-mode
  :ensure nil   ; don't install built-in packages
  :custom
  ;; No context for diff output
  (diff-switches "--unified=0"))

;; dired (directory-browsing commands)
(eval `(use-package dired
         :ensure nil   ; don't install built-in packages
         :ensure-system-package ,(aj8/system-package-name 'ls)
         :bind (:map dired-mode-map
                     ("." . dired-omit-mode)
                     :map dired-mode-map
                     ("C-c C-a d" . markdown-links-insert-from-dired))
         :init
         ;; Enable Dired-X
         (with-eval-after-load 'dired (require 'dired-x))
         :custom
         ;; Set custom listing style
         ;;   --group-directories-first requires 'gls' on macOS
         (dired-listing-switches "-agho --group-directories-first")
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
         ;;   TODO: fails sometimes with TRAMP
         (add-hook 'dired-mode-hook (lambda () (aj8/prefix-buffer-name "dired")))
         ;; Hide details by default
         (add-hook 'dired-mode-hook #'dired-hide-details-mode)
         ;; Hide omitted files
         (add-hook 'dired-mode-hook #'dired-omit-mode)))

;; display-line-numbers (interface for display-line-numbers)
(use-package display-line-numbers
  :ensure nil   ; don't install built-in packages
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  ;; Set display-line-number-width automatically
  (display-line-numbers-width-start t))

;; ediff (a comprehensive visual interface to diff & patch)
(use-package ediff
  :ensure nil   ; don't install built-in packages
  :bind (("C-c e b" . ediff-buffers)
         ("C-c e l" . ediff-regions-linewise)
         ("C-c e w" . ediff-regions-wordwise))
  :init
  (which-key-add-key-based-replacements "C-c e" "ediff")
  :custom
  ;; Use horizontal (side-by-side) view by default in ediff
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-merge-split-window-function #'split-window-horizontally)
  ;; Kill all Ediff buffers on exit
  (aj8/ediff-cleanup-buffers t)
  :config
  ;; Let ediff use existing frame in GUI
  (when (display-graphic-p)
    (setq ediff-window-setup-function 'ediff-setup-windows-plain))
  ;; Kill all Ediff buffers on exit
  (add-hook 'ediff-quit-hook #'aj8/ediff-cleanup-buffers)
  ;; Store and restore window layout after Ediff
  ;;   MAYBE: works well for ediff-buffers, but not for
  ;;   ediff-regions-linewise and ediff-regions-wordwise
  ;;   (https://emacs.stackexchange.com/a/80961/33325)
  (add-hook 'ediff-before-setup-hook #'my-ediff-save-windows)
  (add-hook 'ediff-quit-hook #'my-ediff-restore-windows)
  ;; Hide side windows before Ediff
  ;;   Passing optional argument DEPTH as t ensures the hook runs last
  ;;   (`after my-ediff-save-windows')
  (add-hook 'ediff-before-setup-hook #'aj8/hide-side-windows t)
  ;; Use both versions with ediff
  ;;   MAYBE: check functionality
  (add-hook 'ediff-keymap-setup-hook
            ;; Use both versions with ediff
            (lambda () (keymap-set ediff-mode-map "d" #'my/ediff-copy-both-to-C))))

;; eglot (the Emacs client for LSP servers)
(use-package eglot
  :ensure nil   ; don't install built-in packages
  :hook ((sh-mode . eglot-ensure)
         (html-mode . eglot-ensure)
         (mhtml-mode . eglot-ensure)
         (css-mode . eglot-ensure)
         (web-mode . eglot-ensure)       ; no linting
         (js-mode . eglot-ensure)
         ;; TODO: mode does not exist! use `json-ts-mode'? What is js-json-mode?
         (json-mode . eglot-ensure)      ; no linting
         (latex-mode . eglot-ensure)
         (lua-mode . eglot-ensure)
         (markdown-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (toml-ts-mode . eglot-ensure)
         ;; TODO: mode does not exist!
         (yaml-mode . eglot-ensure))
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
              ("C-c l r" . eglot-rename))
  :init
  (which-key-add-key-based-replacements "C-c l" "lsp")
  (which-key-add-key-based-replacements "C-c l a" "action")
                                        ; add label for prefix key
  :custom
  ;; Shutdown server after buffer kill
  (eglot-autoshutdown t)
  ;; Enable eglot in code external to project
  (eglot-extend-to-xref t)
  :config
  ;; Add server for web-mode
  (add-to-list 'eglot-server-programs
               '(web-mode . ("vscode-html-language-server" "--stdio")))
  ;; Prefer ruff-lsp for Python
  ;; (add-to-list 'eglot-server-programs
  ;;              '((python-mode python-ts-mode) . ("ruff" "server")))   ; No completion, as Eglot only supports one server
  ;; Use Orderless for Eglot (default is Flex)
  (add-to-list 'completion-category-overrides '((eglot (styles orderless)))))
  ;; Don't manage ELDoc
  ;; (add-to-list 'eglot-stay-out-of 'eldoc))
  ;; Limit ELDoc to a single line
  ;; (setq eldoc-echo-area-use-multiline-p nil))   ; doesn't work nicely
  ;; Don't auto-show documentation
  ;; (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1)))

;; electric (window maker and command loop for 'electric' modes)
(use-package electric
  :ensure nil   ; don't install built-in packages
  :config
  ;; Auto-insert closing parens, bracket and double-quotes
  (electric-pair-mode 1))

;; emacs (core Emacs settings)
(use-package emacs
  :ensure nil   ; don't install built-in packages
  :bind (("C-o" . my/open-line)
         ("C-<backspace>" . (lambda () (interactive) (kill-line 0)))
         ("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim)
         ("M-c" . aj8/capitalize-word-at-point)
         ("C-c q" . fill-individual-paragraphs)
         ("C-c x c" . my/copy-symbol-at-point)
         ("C-c TAB" . indent-relative)
         ("C-c C-c" . exit-recursive-edit)
         ("C-c f" . find-file-at-point)
         ("C-c <left>" . scroll-right)
         ("C-c <right>" . scroll-left)
         ("M-p" . backward-paragraph)
         ("M-n" . forward-paragraph)
         ("M-a" . back-to-indentation)
         ("C-c N" . column-number-mode)
         ("C-c x r" . aj8/reload-init-file)
         ("C-x M-e" . my/eval-next-sexp)
         ("C-x C-M-e" . my/eval-sexp-at-point))
  :init
  (which-key-add-key-based-replacements "C-c x" "misc")
  :custom
  ;; Personal settings
  (user-full-name "Andreas Jonsson")
  (user-mail-address "ajdev8@gmail.com")
  ;; Use spaces, not tabs
  (indent-tabs-mode nil)
  ;; Delete trailing newline character with kill-line
  (kill-whole-line t)
  ;; Set fill column
  (fill-column 76)   ; default is 70
  ;; Save clipboard text into kill ring before kill
  ;; (save-interprogram-paste-before-kill t)
  ;; Yank at point, not at pointer
  (mouse-yank-at-point t)
  ;; Reduce delay for echoing multi-character keys
  (echo-keystrokes 0.01)
  ;; Scrolling
  ;;   The order of priority is: `scroll-conservatively', then
  ;;   `scroll-step', and finally `scroll-up-aggressively' /
  ;;   `scroll-down-aggressively'.
  (scroll-conservatively 0)        ; default: 0
  (scroll-step 1)                  ; default: 0
  (scroll-up-aggressively nil)     ; default: nil
  (scroll-down-aggressively nil)   ; default: nil
  ;; (scroll-margin 0)
  ;; Preserve point position when scrolling
  (scroll-preserve-screen-position t)
  ;; Smooth horizontal scrolling
  (hscroll-step 1)
  ;; Enable `scroll-left'
  (put 'scroll-left 'disabled nil)
  ;; Use "repeat-mode" for "pop-mark"
  (set-mark-command-repeat-pop t)
  ;; Don't use the mark when region is inactive
  ;;   Note: messes with ediff
  ;; (mark-even-if-inactive nil)
  ;; Interpret spaces literally when searching
  ;; (isearch-lax-whitespace nil)
  ;; Interpret spaces as wildcards when searching
  ;; (isearch-lax-whitespace t)
  ;; Interpret any characters as wildcards when searching
  (search-whitespace-regexp ".*?")
  ;; Allow movement between Isearch matches by cursor motion commands
  (isearch-allow-motion t)
  (isearch-motion-changes-direction t)
  ;; Use longer pulse
  (pulse-delay 0.05)   ; default is 0.03
  ;; Increase maximum file size that can be opened without a warning
  (large-file-warning-threshold 50000000)
  ;; Show mode headers in describe-bindings buffer
  (describe-bindings-outline t)
  ;; Delete selection on edit
  (delete-selection-mode 1)
  ;; Select completion styles
  ;;   substring: needed for partial completion
  ;;   orderless: space-separated components
  ;;   basic: fallback
  (completion-styles '(orderless basic))
  ;; (completion-styles '(substring orderless basic))
  ;; Use TAB for symbol completion (after indentation)
  (tab-always-indent 'complete)
  ;; Show more details for completions
  (completions-detailed t)
  ;; (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Increase history length
  (history-length 10000)
  ;; Delete history duplicates
  (history-delete-duplicates nil)
  :config
  ;; Use 'y' or 'n' questions always
  ;; (setq use-short-answers t)
  ;; Use partial completion for files
  (setq completion-category-defaults nil)
  (add-to-list 'completion-category-overrides '((file (styles basic partial-completion))))
  ;; Enable auto-scrolling for the *Messages* buffer
  ;;   Note that the *Messages* buffer is created early during startup (before
  ;;   this hook is set), so this hook only applies to newly created
  ;;   *Messages* buffers.  The `with-current-buffer' statement applies the
  ;;   mode to the current *Messages* buffer.
  ;; (add-hook 'messages-buffer-mode-hook (lambda () (aj8/buffer-tail-mode 1)))
  (with-current-buffer "*Messages*"
    (aj8/buffer-tail-mode 1))
  ;; Deactivate highlight mode when selecting text
  (add-hook 'activate-mark-hook (lambda () (global-hl-line-mode -1)))
  (add-hook 'deactivate-mark-hook (lambda () (global-hl-line-mode 1)))
  ;; Add Treesitter indicator in the modeline
  (add-hook 'after-change-major-mode-hook #'aj8/treesit-mode-name)
  ;; Collect list of killed file and non-file buffers
  (add-hook 'kill-buffer-hook #'my/reopen-killed-file-save)
  (add-hook 'kill-buffer-hook #'aj8/reopen-killed-buffer-save)
  (keymap-set isearch-mode-map "TAB" #'isearch-complete))

;; epg (the EasyPG library)
(use-package epg
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Fill gpg passwords in the minibuffer
  (epg-pinentry-mode 'loopback)
  ;; Select keys in the minibuffer
  (epa-keys-select-method 'minibuffer))


;; erc (an Emacs Internet relay chat client)
(use-package erc
  :disabled
  :ensure nil   ; don't install built-in packages
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

;; eshell (the Emacs command shell)
(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config
  ;; Set Eshell options here
  ;; (with-eval-after-load 'esh-opt
  ;;   (setq eshell-visual-commands '("htop" "zsh" "vim")))
  ;; Add Tramp support
  (with-eval-after-load "eshell"
    (add-to-list 'eshell-modules-list 'eshell-tramp)))

;; EWW (Emacs Web Wowser)
(use-package eww
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Default search engine
  (eww-search-prefix "https://google.com/search?q=")   ; default is duckduckgo
  ;; Restore Eww buffers
  (eww-restore-desktop t)
  ;; Don't remove duplicates in browsing history
  ;; (eww-desktop-remove-duplicates nil)
  ;; Download folder
  (eww-download-directory (expand-file-name "~/Downloads"))
  ;; Max history items
  (eww-history-limit 100)
  ;; Auto rename eww buffers
  (eww-auto-rename-buffer t)
  :config
  ;; Are these needed?
  ;; (setq shr-use-colors nil)             ; t is bad for accessibility
  ;; (setq shr-use-fonts nil)
  ;; Don't shadow default EWW keybindings
  (with-eval-after-load "shr"
    (keymap-set shr-map "u" nil)
    (keymap-set shr-map "v" nil)
    (keymap-set shr-map "w" nil))
  ;; Open new EWW buffers in a new window (M-RET)
  ;;   TODO: Emacs 30 introduces eww-open-in-new-buffer
  (with-eval-after-load "eww"
    (define-key eww-mode-map
                [remap eww-open-in-new-buffer] #'aj8/eww-open-in-new-buffer))
  ;; Open new EWW buffers in a new window (C-u RET)
  (with-eval-after-load "eww"
    (define-key eww-mode-map
                [remap eww-follow-link] #'aj8/eww-follow-link)))

;; find-func (find the definition of the Emacs Lisp function near point)
(use-package find-func
  :ensure nil   ; don't install built-in packages
  :config
  ;; Set keybindings for find-function and relatives
  (find-function-setup-keys))

;; flymake (a universal on-the-fly syntax checker)
(use-package flymake
  :bind (:map flymake-diagnostics-buffer-mode-map
              ("h" . #'aj8/flymake-ruff-goto-doc)
         :map flymake-project-diagnostics-mode-map
              ("h" . #'aj8/flymake-ruff-goto-doc)))

;; flyspell (on-the-fly spell checker)
(use-package flyspell
  :ensure nil   ; don't install built-in packages
  :diminish
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  ;; Enable flyspell in web-mode
  (put 'web-mode 'flyspell-mode-predicate #'my/web-mode-flyspell-verify))

;; goto-addr (click to browse URL or to send to e-mail address)
(use-package goto-addr
  :ensure nil   ; don't install built-in packages
  :diminish
  :config
  ;; Buttonize URLs and e-mail addresses
  (global-goto-address-mode 1))

;; help (help commands for Emacs)
(use-package help
  :ensure nil   ; don't install built-in packages
  :bind (("C-c H e" . aj8/toggle-eldoc-buffer)
         ("C-c H k" . describe-keymap)
         ("C-c H s" . shortdoc-display-group))
  :init
  (which-key-add-key-based-replacements "C-c H" "help")
  :custom
  ;; Reuse windows for *Help* buffers
  (help-window-keep-selected t)
  ;; Always select help window
  (help-window-select t))

;; hideshow (minor mode cmds to selectively display code/comment blocks)
(use-package hideshow
  :commands (hs-cycle hs-global-cycle)
  :bind (:map hs-minor-mode-map
              ("TAB" . aj8/hs-cycle)
              ("<backtab>" . aj8/hs-global-cycle)))
  ;; :custom
  ;; ;; Hide the comments too
  ;; (setq hs-hide-comments-when-hiding-all nil))

;; hippie-exp (expand text trying various ways to find its expansion)
(use-package hippie-exp
  :ensure nil   ; don't install built-in packages
  :bind (("<remap> <dabbrev-expand>" . hippie-expand))
  :config
  ;; Let hippie-expand search for line expansions in all buffers
  ;; (add-to-list 'hippie-expand-try-functions-list 'try-expand-line-all-buffers t)
  (setcar (nthcdr 5 hippie-expand-try-functions-list) 'try-expand-line-all-buffers)
  ;; Ignore some buffers with hippie-expand
  (with-eval-after-load "hippie-exp"
    (add-to-list 'hippie-expand-ignore-buffers "^\\*.*\\*$")
    (add-to-list 'hippie-expand-ignore-buffers "magit:.*")
    (add-to-list 'hippie-expand-ignore-buffers aj8/buffer-skip-regexp)))

;; hl-line (highlight the current line)
(use-package hl-line
  :ensure nil   ; don't install built-in packages
  :config
  ;; Highlight current line
  (global-hl-line-mode 1))

;; ibuffer (operate on buffers like dired)
(use-package ibuffer
  :ensure nil   ; don't install built-in packages
  :config
  ;; Set default sort order in Ibuffer
  (require 'ibuf-ext)   ; initiate ibuffer-sorting-functions-alist
                        ; https://lists.gnu.org/archive/html/help-gnu-emacs/2008-11/msg00694.html
  (setq-default ibuffer-default-sorting-mode 'filename/process))

;; info (Info package for Emacs)
(use-package info
  :ensure nil   ; don't install built-in packages
  :hook ((Info-mode . rename-uniquely)
         (Info-mode . (lambda () (keymap-local-unset "M-n")))
         (Info-mode . visual-line-mode))
  :custom
  ;; Open *info* buffers in same window
  (info-lookup-other-window-flag nil))

;; ispell (interface to spell checkers)
(use-package ispell
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Use aspell
  ;;   Requires: aspell
  (ispell-program-name "aspell")   ; this is already the default
  ;; Set language
  (ispell-dictionary "en_US")
  ;; Set aspell suggestion mode
  (ispell-extra-args '("--sug-mode=ultra")))
  ;; (setq ispell-extra-args '("--sug-mode=fast")))
  ;; (setq ispell-extra-args '("--sug-mode=normal")))

;; lisp-mode (Lisp mode, and its idiosyncratic commands)
(use-package lisp-mode
  :ensure nil   ; don't install built-in packages
  ;; Outline settings
  :hook (emacs-lisp-mode . outline-headers-for-semicolon-buffers))

;; minibuffer (minibuffer and completion functions)
(use-package minibuffer
  :ensure nil   ; don't install built-in packages
  :bind ("M-o" . aj8/orderless-matching-style-cycle)
  :custom
  ;; Allow minibuffer commands while in the minibuffer
  (enable-recursive-minibuffers t)
  ;; Timeout for messages in active minibuffer
  (minibuffer-message-clear-timeout 1)
  :config
  ;; Show recursion depth in the minibuffer prompt
  (minibuffer-depth-indicate-mode 1))

;; modus-themes (elegant, highly legible and customizable themes)
(use-package modus-themes
  :if (eq aj8/my-os 'linux)   ; Linux
  :ensure nil   ; don't install built-in packages
  ;; :no-require   ; silence use-package-lint warning
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

;; org (outline-based notes management and organizer)
(use-package org
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Use speed keys
  (org-use-speed-commands t)
  ;; Don't require confirmation when evaluating code
  (org-confirm-babel-evaluate nil)
  ;; Use shift-selection
  ;;   (disables org-shiftup, org-shiftdown, org-shiftleft and org-shiftright)
  ;; (org-support-shift-select t)
  )

;; outline (outline mode commands for Emacs)
(use-package outline
  :ensure nil   ; don't install built-in packages
  :hook (outline-mode . (lambda () (setq outline-regexp "[*]+")))
  :bind (("C-c O" . outline-minor-mode))
  :init
  (which-key-add-key-based-replacements "C-c @" "outline")
  :custom
  ;; Use TAB and S-TAB for cycling
  ;;   See also outline-minor-faces.
  (outline-minor-mode-cycle t)   ; alternatives: 'override and 'append
  ;; (outline-minor-mode-highlight t)
  )

;; paren (highlight matching paren)
(use-package paren
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Show context around the opening paren if it is offscreen
  ;;   MAYBE: Make the text in the echo area persist until key press
  (show-paren-context-when-offscreen t))

;; project (operations on the current project)
(use-package project
  :ensure nil   ; don't install built-in packages
  :bind (("C-x C-<right>" . my/project-next-buffer)
         ("C-x C-<left>" . my/project-previous-buffer)
         ("C-x p t" . project-forget-project))
  :custom
  ;; Share file history between projects
  ;; (project-file-history-behavior 'relativize)
  ;; Exclude some dirs from projects
  ;;   Or use a dir-local file!
  ;; (add-to-list 'project-vc-ignores "archive/")
  ;; Show current project name on the mode line
  (project-mode-line t)
  :config
  ;; Add projects
  ;;   (only if projects file doesn't exist or is older than 7 days)
  (let ((projects-file (locate-user-emacs-file "projects"))
        (seven-days-ago (time-subtract (current-time) (days-to-time 7))))
    (when (or (not (file-exists-p projects-file))
              (time-less-p (nth 5 (file-attributes projects-file 'integer)) seven-days-ago))
      (project-remember-projects-under "~/git")
      (project-remember-projects-under "~/dotfiles")
      (project-remember-projects-under "~/projects" t))))

;; python (Python's flying circus support for Emacs)
(use-package python
  :bind (:map python-mode-map
              ("C-c <" . nil)
         :map python-ts-mode-map
              ("C-c <" . nil))   ; unbind python-indent-shift-left
  :config
  ;; Exclude virtual environment directories from project
  ;;   Not needed: use M-s G  ;; Outline settings
  ;; (setq-local project-ignored-files '(".venv/*"))
  ;; Use ruff as the Flymake linter
  ;;   Note, this is not to be needed with Eglot
  ;; (setq python-flymake-command
  ;;       '("ruff" "--quiet" "--stdin-filename=stdin" "-"))
  ;; Silence warnings about missing linters
  ;;   This remove python-flymake from flymake-diagnostic-functions
  (add-hook 'python-base-mode-hook
            (lambda ()
              (remove-hook 'flymake-diagnostic-functions #'python-flymake t)))
  ;; Outline settings
  (add-hook 'python-base-mode-hook #'outline-headers-for-hash-mark-buffers))

;; recentf (keep track of recently opened files)
(use-package recentf
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Number of saved recent files
  (recentf-max-saved-items 100)
  :config
  ;; Keeping track of opened files
  (recentf-mode 1))

;; repeat (convenient way to repeat the previous command)
(use-package repeat
  :ensure nil   ; don't install built-in packages
  ;; :custom
  ;; Enable repeat mode time-out
  ;; (repeat-exit-timeout 5)
  :config
  ;; Disable buffer-navigation-repeat-map
  (setq buffer-navigation-repeat-map nil)
  ;; Disable repeat-mode for undo
  (setq undo-repeat-map nil)
  ;; Enable repeat mode
  (repeat-mode 1))

;; savehist (save minibuffer history)
(use-package savehist
  :ensure nil   ; don't install built-in packages
  :config
  ;; Persistent minibuffer history
  (savehist-mode 1)
  ;; Additional variables to persist between sessions
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (add-to-list 'savehist-additional-variables 'global-mark-ring))

;; saveplace (automatically save place in files)
(use-package saveplace
  :ensure nil   ; don't install built-in packages
  :config
  (save-place-mode 1))

;; scroll-lock (scroll lock scrolling)
(use-package scroll-lock
  :ensure nil   ; don't install built-in packages
  :bind ("C-c L" . scroll-lock-mode))

;; sh-script (shell-script editing commands for Emacs)
(use-package sh-script
  :ensure nil   ; don't install built-in packages
  :hook ((sh-mode . (lambda ()
                      (keymap-local-unset "C-c =")
                      (keymap-local-unset "C-c <")
                      (keymap-local-unset "C-c >")
                      (keymap-local-unset "C-c ?"))))
  :config
  ;; Outline settings
  (add-hook 'sh-base-mode-hook #'outline-headers-for-hash-mark-buffers)
  (add-hook 'conf-xdefaults-mode-hook #'outline-headers-for-hash-mark-buffers)
  (add-hook 'powershell-mode-hook #'outline-headers-for-hash-mark-buffers)
  ;; Add common config files
  (add-to-list 'auto-mode-alist '("\\.bash_.*\\'" . bash-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.bashrc_.*\\'" . bash-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.profile_.*\\'" . bash-ts-mode)))

;; simple (basic editing commands for Emacs)
(use-package simple
  :ensure nil   ; don't install built-in packages
  :hook ((help-mode . visual-line-mode)
         (helpful-mode . visual-line-mode)
         (Info-mode . visual-line-mode))
  :bind (("C-x <right>" . aj8/next-buffer)
         ("C-x <left>" . aj8/previous-buffer)
         ("C-x k" . kill-current-buffer)
         ("C-c k" . my/kill-buffer-other-window))
  :custom
  ;; Do not switch to buffers already shown
  (switch-to-prev-buffer-skip 'this)
  ;; Skip some buffers when switching buffers
  ;; (setq switch-to-prev-buffer-skip 'aj8/buffer-skip-p)
  ;; Skip some buffers when switching buffers
  ;; (setq switch-to-prev-buffer-skip-regexp regex)
  :config
  ;; Do not display continuation lines
  ;; (setq-default truncate-lines t)
  ;; Display continuation lines with prefixes
  (global-visual-wrap-prefix-mode 1))

;; subword (handling capitalized subwords in a nomenclature)
;;   Subword movement and editing: camelCase
;;     Cannot be enabled at the same time as superword-mode
(use-package subword
  :disabled
  :ensure nil   ; don't install built-in packages
  :hook ((prog-mode . (lambda () (subword-mode 1)))))

;; superword (handling capitalized superwords in a nomenclature)
;;   Superword movement and editing: snake_case and kebab-case
;;     Cannot be enabled at the same time as subword-mode
(use-package superword
  :disabled
  :ensure nil   ; don't install built-in packages
  :hook ((prog-mode . (lambda () (superword-mode 1)))))

;; term (general command interpreter in a window stuff)
(use-package term
  :disabled
  :commands term)
  ;; :config
  ;; Match the default Bash shell prompt
  ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;; tex-mode (TeX, LaTeX, and SliTeX mode commands)
(use-package tex-mode
  :ensure nil   ; don't install built-in packages
  :hook ((latex-mode . outline-headers-for-percentage-buffers)
         (latex-mode . (lambda () (setq comment-add 0))))
  :config
  (add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode)))
  ;; (add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-ts-mode)))

;; tramp (Transparent Remote (file) Access, Multiple Protocol)
;; TODO: Prevent TRAMP commands from appearing in bash history
;;       (e.g. "exec env TERM dumb INSIDE_EMACS ENV HISTFILE ~/.tramp_history bash history")
;;       https://mail.gnu.org/archive/html/tramp-devel/2024-11/msg00002.html
;; TODO: Disable tramp debug buffer
(use-package tramp
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Override the HISTFILE
  (tramp-histfile-override t)
  :config
  ;; Add mode line indicator
  (add-to-list 'global-mode-string '(:eval (aj8/tramp-indicator)) t))

;; treesit (tree-sitter utilities)
(use-package treesit
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Use maximum decoration detail
  (treesit-font-lock-level 4))

;; uniquify (unique buffer names dependent on file name)
(use-package uniquify
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Uniquify buffer name using project
  (uniquify-dirname-transform 'project-uniquify-dirname-transform))

;; vc (drive a version-control system from within Emacs)
(use-package vc
  :ensure nil   ; don't install built-in packages
  :bind (("C-x v -" . vc-ediff))
  :custom
  ;; Follow symlinks
  (vc-follow-symlinks t))

;; which-key (display available keybindings in popup)
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
  ;;   (see :config section)
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
  ;; Max description length
  ;;   (default is 27)
  (setq which-key-max-description-length
        (aj8/which-key-description-length))
  ;; Min description length
  ;;   (default is 0)
  (setq which-key-min-column-description-width
        (aj8/which-key-description-length))
  (which-key-mode 1))

;; windmove (move between windows)
(use-package windmove
  :ensure nil   ; don't install built-in packages
  :init
  (which-key-add-key-based-replacements "C-c w" "windows")
  :config
  (windmove-default-keybindings 'ctrl)
  (windmove-swap-states-default-keybindings '(ctrl shift))
  (keymap-global-set "C-c w <up>" #'windmove-display-up)
  (keymap-global-set "C-c w <down>" #'windmove-display-down)
  (keymap-global-set "C-c w <left>" #'windmove-display-left)
  (keymap-global-set "C-c w <right>" #'windmove-display-right)
  (keymap-global-set "C-c w 0" #'windmove-display-same-window)
  (keymap-global-set "C-c w C-<up>" #'windmove-delete-up)
  (keymap-global-set "C-c w C-<down>" #'windmove-delete-down)
  (keymap-global-set "C-c w C-<left>" #'windmove-delete-left)
  (keymap-global-set "C-c w C-<right>" #'windmove-delete-right))

;; winner (undo and redo window configurations)
(use-package winner
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Don't bind keys for winner
  (winner-dont-bind-my-keys t)
  :config
  (keymap-set winner-mode-map "C-c w <" #'winner-undo)
  (keymap-set winner-mode-map "C-c w >" #'winner-redo)
  (winner-mode 1))

;; window (GNU Emacs window commands aside from those written in C)
(use-package window
  :ensure nil   ; don't install built-in packages
  :bind (("C-x {" . my/move-splitter-up)
         ("C-x }" . my/move-splitter-down)
         ("C-x >" . my/move-splitter-right)
         ("C-x <" . my/move-splitter-left)
         ("C-x |" . window-toggle-side-windows)
         ("C-x !" . delete-other-windows-vertically)
         ("C-x =" . balance-windows)
         ("C-x +" . balance-windows-area)
         ("C-x _" . fit-window-to-buffer)
         ("C-x 9" . my/toggle-window-split))
  :custom
  ;; Set minimum window height
  ;; (setq window-min-height 16)
  ;; Prefer horizontal (side-by-side) window splitting
  ;;   Note: the thresholds need to be twice as big as the smallest
  ;;   window allowed, because the new windows each use half of the
  ;;   former window size
  (split-width-threshold nil)
  (split-height-threshold 0)
  ;; Enable horizontal window fitting
  ;; (setq fit-window-to-buffer-horizontally
  ;; Try to even window sizes vertically only
  (even-window-sizes 'height-only)
  ;; Allow switching to buffer in strongly dedicated windows
  ;; (switch-to-buffer-in-dedicated-window 'pop)
  ;; Top and bottom side windows occupy full frame width (default)
  (window-sides-vertical nil)
  ;; Left and right side windows occupy full frame height
  ;; (setq window-sides-vertical t)
  ;; Obey display actions when switching buffers
  (switch-to-buffer-obey-display-actions t)
  ;; Window rules
  (display-buffer-alist
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
     ;; ((or . ((derived-mode-p . backtrace-mode)
     ;;         (derived-mode-p . compilation-mode)
     ((lambda (buffer _alist)
        (with-current-buffer buffer (or (derived-mode-p 'backtrace-mode)
                                        (derived-mode-p 'compilation-mode))))
      (display-buffer-in-side-window)
      (window-height . ,aj8/side-window-height)
      (side . top)
      (window-parameters . ((no-delete-other-windows . t))))
     ("\\*\\(Messages\\|Warnings\\|Native-compile-Log\\|Async-native-compile-log\\)\\*"
      (display-buffer-in-side-window)
      (window-height . ,aj8/side-window-height)
      (side . top)
      (window-parameters . ((no-delete-other-windows . t))))
     ("magit-process:.*"
      (display-buffer-in-side-window)
      (window-height . ,aj8/side-window-height)
      (side . top)
      (window-parameters . ((no-delete-other-windows . t))))
     ("\\*\\(copilot events\\|EGLOT.*events\\|Flymake diagnostics.*\\|texlab\\(::stderr\\)?\\|tramp.*\\|.*-ls\\(::.*\\)?\\)\\*"
      (display-buffer-in-side-window)
      (window-height . ,aj8/side-window-height)
      (side . top)
      (window-parameters . ((no-delete-other-windows . t))))
     ;;
     ;; Right side window
     ;;
     ;;   Magit
     ;;
     ("\\(COMMIT_EDITMSG\\|magit:\\|magit-log.*:\\|magit-reflog:\\|magit-log-select:\\).*"
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
     ;;
     ;;   Help
     ;;
     ("\\*\\(Apropos\\|Help\\|helpful.*\\|info.*\\|Man.*\\|WoMan.*\\)\\*"
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
     ("\\*\\(Bookmark List\\|Benchmark Init Results.*\\|ChatGPT.*\\|Claude.*\\|Deepseek.*\\|Embark Collect:.*\\|Gemini.*\\|Occur\\|.*Output\\|Semantic SymRef\\|devdocs\\|eldoc\\|package update results\\|tex-shell\\)\\*"
      (display-buffer-in-side-window)
      (window-width . ,aj8/side-window-width-dynamic)
      (side . right)
      (slot . -1)
      (window-parameters . ((no-delete-other-windows . t))))
     ("\\(gptel-.*\\.\\(org\\|md\\)\\)"
      (display-buffer-in-side-window)
      (window-width . ,aj8/side-window-width-dynamic)
      (side . right)
      (slot . -1)
      (window-parameters . ((no-delete-other-windows . t))))
     ;; MAYBE: eww doesn't work initially
     ("\\*\\(eww:.*\\|gptel-reasoning\\)\\*"
      (display-buffer-in-side-window)
      (window-width . ,aj8/side-window-width-dynamic)
      (side . right)
      (slot . 1)
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
                            (mode-line-format . none)))))))
  ;; :config
  ;; Resize window combinations proportionally
  ;;   TODO: Message buffer grows on each Magit commit
  ;; (setq window-combination-resize t))

;; xref (cross-referencing commands)
(use-package xref
  :ensure nil   ; don't install built-in packages
  :bind (("M-?" . xref-find-references)
         ("C-c ?" . xref-find-definitions)
         ("C-c <" . xref-go-back)
         ("C-c >" . xref-go-forward))
  :config
  ;; Don't prompt with xref-find-references
  (with-eval-after-load "xref"
    (add-to-list 'xref-prompt-for-identifier 'xref-find-references t)))

;;; Admin

;;; AI

;; ai-code-interface (unified emacs interface supporting LLM clients)
(use-package ai-code-interface
  :bind (("C-c a" . ai-code-menu))
  :config
  ;; Set backend
  (ai-code-set-backend 'claude-code)
  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

;; claude-code (Claude Code Emacs integration)
(use-package claude-code
  :bind-keymap ("C-c a" . claude-code-command-map)
  :custom
  ;; Set terminal backend
  (claude-code-terminal-backend 'vterm)   ; default: eat
  ;; Customize Return key
  ;; (claude-code-newline-keybinding-style 'shift-return-to-send)
  ;; Prevent Claude Code windows from being deleted
  ;; (claude-code-no-delete-other-windows t)
  ;; Automatically select the Claude buffer after opening it
  (claude-code-toggle-auto-select t)
  :config
  ;; Revert buffers on file change
  ;;   Claude edits files directly on disk, not in Emacs buffers
  ;; TODO: Implement revert with more control: see https://github.com/stevemolitor/claude-code.el?tab=readme-ov-file#tips-and-tricks
  ;; (global-auto-revert-mode 1)
  ;; Increase vterm scrollback limit
  (add-hook 'claude-code-start-hook
          (lambda ()
            (when (eq claude-code-terminal-backend 'vterm)
              (setq-local vterm-max-scrollback 10000))))
  ;; optional IDE integration with Monet
  ;; (add-hook 'claude-code-process-environment-functions
  ;;           #'monet-start-server-function)
  ;; (monet-mode 1)
  ;; Enable mode
  (claude-code-mode))

;; gptel (a simple ChatGPT client for Emacs)
(use-package gptel
  :commands (gptel gptel-send)
  :bind (("C-c t c" . gptel)
         ("C-c t m" . gptel-menu)
         ("C-c t q" . gptel-abort)
         ("C-c RET" . gptel-send)
         ("C-c C-<return>" . gptel-menu)
         ("C-c C-g" . gptel-abort)
         :map gptel-mode-map
         ("C-c M-n" . gptel-end-of-response)
         ("C-c M-p" . gptel-beginning-of-response))
  :init
  (which-key-add-key-based-replacements "C-c t" "gptel")
                                        ; add label for prefix key
  :custom
  ;; Set mode
  ;; (gptel-default-mode 'text-mode)   ; default: markdown-mode
  ;; Track media
  (gptel-track-media t)
  ;; Don't use Curl
  ;; (gptel-use-curl nil)
  ;; Don't show reasoning
  ;; (gptel-include-reasoning nil)
  ;; Ignore reasoning in requests
  ;; (gptel-include-reasoning 'ignore)
  ;; Pass reasoning to buffer
  (gptel-include-reasoning "*gptel-reasoning*")
  ;; Automatic diff after rewrite
  ;; (gptel-rewrite-default-action 'ediff)
  ;; Dispatch rewrite menu
  (gptel-rewrite-default-action 'dispatch)
  ;; Single line separator
  ;; (gptel-response-separator "\n")   ; default: "\n\n"
  ;; Pass context in user prompt
  (gptel-use-context 'user)   ; default: 'system
  ;; Cache request content
  ;;   Only for Anthropic
  (gptel-cache t)
  ;; confirm tool calls with ':confirm t' in the tool registration
  (gptel-confirm-tool-calls 'auto)
  :config
  ;; Enable experimental options
  (setq gptel-expert-commands t)
  ;; Default model
  (setq-default gptel-model 'gpt-5-mini)
  ;; === ChatGPT ===
  ;; Custom OpenAI backend
  ;; (gptel-make-openai "ChatGPT-NoStream"
  ;;   :key (gptel-api-key-from-auth-source
  ;;         "api.openai.com" "apikey")
  ;;   :stream nil   ; Disable streaming
  ;;   :models gptel--openai-models)   ; include all OpenAI models
  ;; Set as default backend
  ;; (setq gptel-backend (gptel-get-backend "ChatGPT-NoStream"))
  ;; Disable streaming
  ;; (put 'gpt-5 :request-params '(:stream :json-false))
  (put 'gpt-5-mini :request-params '(:stream :json-false))
  ;; Set reasoning effort (default: medium)
  ;; (put 'o1-mini :request-params '(:reasoning_effort "medium"))
  ;; Use Flex processing
  ;; (put 'gpt-5 :request-params '(:service_tier "flex"))
  (put 'gpt-5 :request-params '(:stream :json-false :service_tier "flex"))
  ;; === Claude ===
  (gptel-make-anthropic "Claude"
    :key (gptel-api-key-from-auth-source
          "api.anthropic.com" "apikey")
    :stream t)   ; make available
  ;; Set as default backend
  ;; (setq gptel-backend "Claude")
  ;; (setq gptel-model 'claude-sonnet-4-20250514)
  ;; === Gemini ===
  (gptel-make-gemini "Gemini"
    :key (gptel-api-key-from-auth-source
          "generativelanguage.googleapis.com" "apikey")
    :stream t)   ; make available
  ;; Set as default backend
  ;; (setq gptel-backend "Gemini")
  ;; (setq gptel-model '`gemini-pro-latest')
  ;; === Moonshot ===
  (gptel-make-openai "Moonshot"
    :host "api.moonshot.ai"   ; or "api.moonshot.cn" for the Chinese site
    :key (gptel-api-key-from-auth-source
          "api.moonshot.ai" "apikey")
    :stream t   ; make available
    :models '(kimi-k2-0905-preview))
;; === Deepseek ===
  (gptel-make-deepseek "Deepseek"
    :key (gptel-api-key-from-auth-source
          "api.deepseek.com" "apikey")
    :stream t)
  ;; === OpenRouter ====
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :key (gptel-api-key-from-auth-source
          "openrouter.ai" "apikey")
    :stream t
    :models '(anthropic/claude-sonnet-4.5
              google/gemini-2.5-pro
              openai/gpt-5
              openai/gpt-5-codex
              deepseek/deepseek-chat-v3-0324
              deepseek/deepseek-r1-0528
              moonshotai/kimi-k2-0905
              moonshotai/kimi-k2-0905:exacto
              qwen/qwen3-coder
              qwen/qwen3-coder:free
              qwen/qwen3-coder-plus
              qwen/qwen3-max))
  ;; Enable reasoning
  (dolist (provider '(deepseek/deepseek-chat-v3-0324
                      deepseek/deepseek-chat-v3.1
                      deepseek/deepseek-v3.1-terminus
                      deepseek/deepseek-v3.2-exp))
    (put provider :request-params '(:reasoning (:enabled t))))
  ;; === GitHub Copilot Chat ====
  (gptel-make-gh-copilot "Copilot")
  ;; Enable word-wrap
  (add-hook 'gptel-mode-hook (lambda () (visual-line-mode 1)))
  ;; Scroll window automatically
  (defun aj8/gptel-auto-scroll-fixed ()
    "Follow streaming output by moving point when stream continues off-screen.
Runs from `gptel-post-stream-hook'. Moves point to the streaming
insertion position (which is point while this hook runs) and recenters
the window so that the streaming position appears near the bottom."
    (when-let* ((win (get-buffer-window (current-buffer) 'visible))
                (pos (point)))
      (unless (pos-visible-in-window-p pos win)
        (run-at-time 0 nil
                     (lambda (w p)
                       (with-selected-window w
                         (goto-char p)
                         (recenter -1)))
                     win pos))))
  (add-hook 'gptel-post-stream-hook #'aj8/gptel-auto-scroll-fixed)
  ;; Jump to next prompt after response
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response)
  ;; Auto-save
  (add-hook 'gptel-post-response-functions #'aj8/gptel-auto-save-chat-buffer)
  ;; Display reasoning buffer automatically
  (add-hook 'gptel-post-response-functions #'aj8/gptel-display-reasoning-buffer)
  ;; Add directives
  (add-to-list 'gptel-directives
               `(coder . ,(with-temp-buffer
                            (insert-file-contents
                             (expand-file-name "data/gptel-directive.md"
                                               user-emacs-directory))
                            (string-trim (buffer-string))))
               t)
  (add-to-list 'gptel-directives
               '(debug . "You are a large language model and a debugger. Diagnose issues and suggest fixes.")
               t)
  ;; Preset: chat
  (gptel-make-preset 'chat
    :description "Preset for chat"
    ;; :system (alist-get 'chat gptel-directives)
    :system 'chat
    :temperature 1.0
    :model 'gemini-flash-latest
    :use-context nil
    :include-reasoning 'ignore
    :use-tools nil)
  ;; Preset: coding
  (gptel-make-preset 'coding
    :description "Preset for coding"
    ;; :system (alist-get 'coder gptel-directives)
    :system 'coder
    :model 'gpt-5-mini
    :temperature 0.1
    :use-context 'user
    :include-reasoning "*gptel-reasoning*"
    ;; :tools (gptel-tk-get-tools)
    ;; :post (gptel-tk-enable-builtin-tools)
    :use-tools t)
  ;; Enable MCP
  (require 'gptel-integrations))

;; gptel-quick (quick LLM lookups in Emacs) - [source package]
(use-package gptel-quick
  :bind ("C-c t h" . gptel-quick))
  ;; :config
  ;; ;; Set approximate word count of LLM summary
  ;; (gptel-quick-word-count 12))

(use-package gptel-toolkit
  :after gptel
  :custom
  ;; Exclude some tools
  (gptel-tk-excluded-tools '(;; Redundant
                             "list_buffers"
                             "read_function"
                             "insert_in_buffer"
                             "replace_buffer_line"
                             "delete_buffer_line"
                             "delete_buffer_string"
                             "apply_buffer_line_edits"
                             "apply_buffer_line_edits_with_review"
                             "apply_buffer_string_edits"
                             "apply_buffer_string_edits_with_review"
                             ;; Unwanted
                             "replace_buffer"
                             "ert_run_unit"))
  :config
  ;; Enable built-in tools
  ;; (gptel-tk-enable-builtin-tools)
  ;; Enable built-in tools in preset
  ;; (plist-put (gptel-get-preset 'coding) :post #'gptel-tk-enable-builtin-tools))
  ;; Set built-in tools in preset
  (plist-put (gptel-get-preset 'coding) :tools (gptel-tk-get-tool-names)))

;; llm-tool-collection (a crowdsourced collection of tools to empower Large Language Models in Emacs) - [source package]
(use-package llm-tool-collection
  :disabled
  :after gptel
  :config
  ;; Register all tools
  (mapcar (apply-partially #'apply #'gptel-make-tool)
          (llm-tool-collection-get-all)))
  ;; ;; Register one tool
  ;; (apply #'gptel-make-tool llm-tc/list-directory)
  ;; ;; Register one category
  ;; (mapcar (apply-partially #'apply #'gptel-make-tool)
  ;;       (llm-tool-collection-get-category "filesystem")))

;; mcp (Model Context Protocol)
(use-package mcp
  :after gptel
  :custom
  ;; Define servers
  (mcp-hub-servers
   `(("bifrost" . (:url "http://localhost:8008/sse"))
     ("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,(expand-file-name "~/dotfiles") ,(expand-file-name "~/projects"))))
     ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
     ("serena" . (:command "uv" :args ("run" "--directory" ,(expand-file-name "~/git/serena") "serena" "start-mcp-server" "--context" "ide-assistant")))
     ("serena-remote" . (:command "uvx" :args ("--from", "git+https://github.com/oraios/serena", "serena", "start-mcp-server", "--context", "ide-assistant")))))
  :config
  (require 'mcp-hub))
  ;; ;; Start servers
  ;; :hook (after-init . mcp-hub-start-all-server))

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

;; copilot (an unofficial Copilot plugin for Emacs) - [source package]
(use-package copilot
  ;; :hook (prog-mode . copilot-mode)
  :bind (:map copilot-mode-map
              ("TAB" . copilot-complete)
              ("<backtab>" . 'copilot-accept-completion)
              ("M-<right>" . 'copilot-accept-completion-by-word)
              ("M-<left>" . 'copilot-accept-completion-by-line)
              ("M-p" . 'copilot-previous-completion)
              ("M-n" . 'copilot-next-completion)
         :map copilot-completion-map
              ("C-g" . 'copilot-clear-overlay))
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

;; csv-mode (major mode for editing comma/char separated values)
(use-package csv-mode
  :mode ("\\.csv$")
  :bind (:map csv-mode-map
              ("C-c M-a" . my/csv-align-visible)
              ("C-M-<right>" . csv-forward-field)
              ("C-M-<left>" . csv-backward-field))
  ;; Enable modeline linenumber for wide files
  :custom
  (line-number-display-limit-width 500)   ; default is 200
  :config
  (defun my/csv-align-visible (&optional arg)
    "Align visible fields."
    (interactive "P")
    (csv-align-fields nil (window-start) (window-end))))

;; markdown-mode (major-mode for editing Markdown files)
(use-package markdown-mode
  :mode "\\.md$"
  ;; :mode ("\\.md$" . markdown-view-mode)
  :bind (:map markdown-mode-map
              ("M-p" . nil)     ; unbind markdown-previous-link
              ("M-n" . nil)         ; unbind markdown-next-link
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

;; markdown-links (insert Markdown links from various sources) - [source package]
(use-package markdown-links
  :after markdown-mode
  :commands (markdown-links-insert-from-files
             markdown-links-insert-from-buffers
             markdown-links-insert-from-project
             markdown-links-insert-from-git
             markdown-links-insert-from-dired)
  :bind (:map markdown-mode-map
         ("C-c C-a f" . markdown-links-insert-from-files)
         ("C-c C-a b" . markdown-links-insert-from-buffers)
         ("C-c C-a p" . markdown-links-insert-from-project)
         ("C-c C-a g" . markdown-links-insert-from-git)))

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

;; consult-eglot (query workspace symbol from eglot using consult)
(use-package consult-eglot
  :after (consult eglot))

;; flymake-aspell (Aspell checker for Flymake)
(use-package flymake-aspell
  :disabled
  :ensure-system-package aspell
  :hook (text-mode . flymake-aspell-setup)
  :custom
  ;; Don't prompt for saving personal dictionary
  (ispell-silently-savep t))

;; flymake-eslint (a Flymake backend for Javascript using eslint)
(use-package flymake-eslint
  :disabled
  :ensure-system-package eslint
  :hook (js-base-mode . flymake-eslint-enable))

;; flymake-json (a Flymake handler for json using jsonlint)
;;   TODO: doesn't work
(use-package flymake-json
  :ensure-system-package jsonlint
  :hook (json-mode . flymake-json-load))

;; flymake-ruff (a Flymake plugin for python files using Ruff)
(use-package flymake-ruff
  :disabled   ; already included with pylsp
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

;; package-lint (a linting library for elisp package authors)
(use-package package-lint
  :defer)

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
   :preview-key "C-c M-p")

  ;; Completion
  ;; (consult-customize
  ;;  consult-completion-in-region
  ;;  :completion-styles '(basic))   ; disable orderless
  ;;  ;; :require-match t)

  ;; Narrowing key
  (setq consult-narrow-key "<")

  ;; Exclude some dirs from consult-git-grep
  (advice-add 'consult--git-grep-make-builder :around
              #'aj8/consult-git-grep-advice))

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
  ;; (corfu-preselect nil)          ; disable candidate preselection
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
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))   ; TODO: remove 1?
  ;; :config
  ;; Sort candidates by history
  ;; (corfu-history-mode 1)   ; requires savehist-mode
  ;; (savehist-mode 1)
  ;; (add-to-list 'savehist-additional-variables 'corfu-history))
  ;;
  ;; (defun my/corfu-comp-style ()
  ;;   "Set/unset a fast completion style for corfu"
  ;;   (if corfu-mode
  ;;       ;; (setq-local completion-styles '(basic))
  ;;       (setq-local orderless-matching-styles '(orderless-literal-prefix))
  ;;     ;; (kill-local-variable 'completion-styles))))
  ;;     (kill-local-variable 'orderless-matching-styles)))

  ;; (defun enable-regexp-style-for-corfu ()
  ;;   (setq-local orderless-matching-styles '(orderless-literal orderless-regexp)))

  ;; (defun disable-regexp-style-for-corfu ()
  ;;   ;; Restore previous global value for orderless-matching-styles
  ;;   (kill-local-variable 'orderless-matching-styles))

  ;; (add-hook 'corfu-show-hook #'enable-regexp-style-for-corfu)
  ;; (add-hook 'corfu-quit-hook #'disable-regexp-style-for-corfu)

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
  ;;       (add-hook 'completion-at-point-functions #'cape-dabbrev)
  ;;       (add-hook 'completion-at-point-functions #'cape-file)
  ;;       (add-hook 'completion-at-point-functions #'cape-line))
  ;;
  ;;     (add-hook 'emacs-lisp-mode #'my/register-default-caps)
  ;;
  ;;   The elisp-completion-at-point function is exclusive. To make it
  ;;   non-exclusive you can wrap it with cape-capf-properties:
  ;;
  ;;     (cape-capf-properties #'elisp-completion-at-point :exclusive 'no)
  ;;
  ;;   or more recently, use cape-wrap-nonexclusive.

  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; (add-hook 'completion-at-point-functions #'cape-keyword)
  ;; (add-hook 'completion-at-point-functions #'cape-tex)
  ;; (add-hook 'completion-at-point-functions #'cape-abbrev)
  ;; (add-hook 'completion-at-point-functions #'cape-dict)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  ;; (add-hook 'completion-at-point-functions #'cape-line)

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
;;   TODO: Set up vertico-grid-mode (with vertico-multiform-mode?)?
(use-package vertico
  :bind (("M-R" . #'vertico-repeat)
         :map vertico-map
         ("?" . minibuffer-completion-help)
         ("C-c ?" . minibuffer-hide-completions)
         ;; ("TAB" . vertico-insert)   ; default
         ("<backtab>" . vertico-insert)
         ("TAB" . minibuffer-complete)
         ;; ("<backtab>" . minibuffer-force-complete)
         ("C-p" . vertico-previous-group)
         ("C-n" . vertico-next-group)
         ("M-P" . #'vertico-repeat-previous)   ; TODO: overlaps with S-M-p
         ("M-N" . #'vertico-repeat-next))      ; TODO: overlaps with S-M-n
  :init
  (vertico-mode)
  :custom
  ;; Enable cycling
  (vertico-cycle t)
  :config
  ;; Save repeat history across sessions
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  ;; Enable M-x minibuffer-hide-completions (make function interactive)
  (put 'minibuffer-hide-completions 'interactive-form '(interactive)))

;;; Editing

;; combobulate (structured editing and navigation in Emacs with Tree-Sitter) - [source package]
;;   Missing functionality: barf, slurp, forward/backward sexp (navigate
;;   by parens), ...
;;
;;   TODO: Don't dump tree into *Messages*
(use-package combobulate
  :after treesit
  ;; :load-path ("path-to-git-checkout-of-combobulate"))
  :hook (;; (bash-ts-mode . combobulate-mode)   ; NA
         (css-ts-mode . combobulate-mode)
         (html-ts-mode . combobulate-mode)
         (mhtml-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         ;; (markdown-ts-mode . combobulate-mode)   ; NA
         (python-ts-mode . combobulate-mode)
         (toml-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode))
  :bind (:map combobulate-key-map
              ("C-M-n" . combobulate-navigate-down)   ; forward-list
              ("C-M-<right>" . combobulate-navigate-next)   ; forward-sexp
              ("C-M-<left>" . combobulate-navigate-previous)   ; backward-sexp
              ("C-M-p" . combobulate-navigate-up)   ; backward-list
              ("M-a" . combobulate-navigate-logical-previous)
              ("M-e" . combobulate-navigate-logical-next)
              ("C-M-u" . combobulate-navigate-sequence-previous)   ; backward-up-list
              ("C-M-d" . combobulate-navigate-sequence-next)   ; down-list
              ;; ("?" . combobulate-mark-node-at-point)
              ("C-c o <up>" . combobulate-splice-up)
              ("C-c o <down>" . combobulate-splice-down)
              ("C-c o <right>" . combobulate-splice-self)
              ("C-c o <left>" . combobulate-splice-parent)
              ("M-<left>" . nil)
              ("M-<right>" . nil)
              ("M-p" . nil)
              ("M-n" . nil))
  :init
  (which-key-add-key-based-replacements "C-c o" "combobulate")
  (which-key-add-key-based-replacements "C-c o B" "builder")
  (which-key-add-key-based-replacements "C-c o e" "envelop")
  (which-key-add-key-based-replacements "C-c o h" "highlight")
  (which-key-add-key-based-replacements "C-c o t" "edit")
  (which-key-add-key-based-replacements "C-c o x" "xref")
                                        ; add label for prefix
  :custom
  ;; Custom prefix key
  (combobulate-key-prefix "C-c o"))
  ;; Disable display of syntax tree
  ;; (combobulate-flash-node nil))

;; lingva (access Google Translate without tracking via lingva.ml)
(use-package lingva
  :bind ("C-c x t" . lingva-translate))

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

;; multi-line (multi-line statements)
(use-package multi-line
  ;; Demand multi-line to avoid failure to load mode specific strategies
  ;;   (https://github.com/IvanMalison/multi-line/issues/8)
  :demand t
  :bind ("C-c n" . multi-line))

;; repeat-help (display keybindings for repeat-mode)
(use-package repeat-help
  :hook (repeat-mode . repeat-help-mode))
  ;; :custom
  ;; Custom repeat help key
  ;;   Default value interferes with access to the standard "C-h" map
  ;; (repeat-help-key "C-c H r"))   ; default: "C-h'"
  ;; Show repeat-help automatically
  ;; (repeat-help-auto t)   ; default: nil
  ;; Popup type
  ;; (repeat-help-popup-type 'embark))   ; default: which-key

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
              ("C-c '"  . wrap-with-single-quotes)
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
         ("C-c i p" . string-inflection-python-style-cycle)
         ("C-c i x" . string-inflection-all-cycle))
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

;; dired-sidebar (tree browser leveraging dired)
(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :bind ("C-c d" . dired-sidebar-toggle-sidebar)
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
  ;; (dired-sidebar-recenter-cursor-on-tui-update t)
  ;; The tree style to display
  (dired-sidebar-theme 'ascii)   ; none, ascii, icons (all-the-icons) (default)
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands))

;; osx-trash (system trash for OS X)
(use-package osx-trash
  :if (eq aj8/my-os 'macos)   ; macOS
  :custom
  (delete-by-moving-to-trash t)   ; TODO: add this to all systems
  :config
  (osx-trash-setup))

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

;; reflow (Re-flow Info and Helpful buffers)
(use-package reflow
  :after (:any helpful info)
  :commands (reflow-info-buffer
             reflow-helpful-buffer
             reflow-info-mode
             reflow-helpful-mode)
  :config
  ;; Re-flow Info buffers
  (reflow-info-mode 1)
  ;; Re-flow Helpful buffers
  (reflow-helpful-mode 1))

;; marginalia (enrich existing commands with completion annotations)
(use-package marginalia
  :demand t
  :bind (:map minibuffer-local-map ("M-m" . marginalia-cycle))
  :config
  (marginalia-mode 1))

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
  (add-hook 'sh-base-mode-hook
            (lambda () (setq-local devdocs-current-docs '("bash"))))
  (add-hook 'latex-mode-hook
            (lambda () (setq-local devdocs-current-docs '("latex"))))
  (add-hook 'lua-mode-hook
            (lambda () (setq-local devdocs-current-docs '("lua~5.4"))))
  (add-hook 'python-base-mode-hook
            (lambda () (setq-local devdocs-current-docs '("python~3.11")))))

;; devdocs-browser (browse devdocs.io documents using EWW)
;;   Usage: M-x devdocs-browser-install-doc
(use-package devdocs-browser
  :bind ("C-c H D" . devdocs-browser-open)
  ;; :custom
  ;; ;; Select devdocs window
  ;; (devdocs-window-select t)
  :custom
  (devdocs-browser-major-mode-docs-alist '((yaml-mode "Ansible")
                                           (yaml-ts-mode "Ansible")
                                           (sh-mode "Bash")
                                           (bash-ts-mode "Bash")
                                           (emacs-lisp-mode "Elisp")
                                           (latex-mode "LaTeX")
                                           (lua-mode "Lua")
                                           (lua-ts-mode "Lua")
                                           (python-mode "Python")
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
  :config
  ;; Don't stop on spaces
  (setq syntax-subword-skip-spaces t)
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
;;   TODO: Uninstall?
(use-package flyspell-correct
  :disabled
  :after flyspell
  :bind (:map flyspell-mode-map
              ;; ("C-;" . flyspell-auto-correct-word)
              ("C-;" . flyspell-correct-wrapper)
              ("C-," . flyspell-correct-previous)
              ("C-." . flyspell-correct-next)
              ("C-c ," . my/flyspell-goto-previous-error)
              ("C-c ." . flyspell-goto-next-error)
              ("C-c ;" . flyspell-auto-correct-word)))

;; jinx (enchanted spell checker)
(eval `(use-package jinx
         ;; :disabled
         ;; :after vertico
         :ensure-system-package ,(aj8/system-package-name 'enchant)
         :ensure-system-package pkgconf
         :diminish
         :hook (emacs-startup . global-jinx-mode)
         :bind (:map jinx-mode-map
                     ("M-$" . jinx-correct)
                     ("C-M-$" . jinx-languages)
                     ("C-," . aj8/jinx-correct-previous)
                     ("C-." . aj8/jinx-correct-next)
                     ("C-c ," . jinx-previous)
                     ("C-c ." . jinx-next)
                :map jinx-correct-map
                     ("C-," . aj8/jinx-correct-previous)
                     ("C-." . aj8/jinx-correct-next))))
         ;; :config
         ;; ;; Better display with Vertico
         ;; ;;   TODO: must run this after Vertico is loaded
         ;; (when (featurep 'vertico)
         ;;   (add-to-list 'vertico-multiform-categories
         ;;                '(jinx grid (vertico-grid-annotate . 20)))
         ;;   (vertico-multiform-mode 1))))

;;; Terminal

;; vterm (fully-featured terminal emulator)
(eval `(use-package vterm
         ;; :ensure-system-package (cmake libvterm-dev)
         :ensure-system-package (cmake ,(aj8/system-package-name 'libvterm))
         :commands vterm
         :config
         ;; Match the default Bash shell prompt
         (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")))

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
  :after (:any modus-themes ef-themes standard-themes)
  :config
  (cond ((eq aj8/my-os 'macos)   ; macOS
         (customize-set-variable 'circadian-themes '((:sunrise . ef-duo-light)
                                                     (:sunset  . ef-duo-dark))))
        ((eq aj8/my-os 'wsl)     ; WSL
         (customize-set-variable 'circadian-themes '((:sunrise . standard-light)
                                                     (:sunset  . standard-dark))))
        ((eq aj8/my-os 'linux)   ; Linux

         (customize-set-variable 'circadian-themes '((:sunrise . modus-operandi)
                                                     (:sunset  . modus-vivendi))))
        (t (user-error "Unexpected system-name: %s" (system-name))))
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
  ;; Add filenames to Magit commit messages
  (add-hook 'git-commit-setup-hook #'aj8/magit-commit-add-files)
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
  ;; Clean up process buffers automatically
  (setq aj8/magit-cleanup-buffers t)
  ;; Hook for buffer cleanup
  (add-hook 'magit-mode-hook #'aj8/magit-start-buffer-cleanup-timer))

;; magit-delta (use Delta when displaying diffs in Magit)
(use-package magit-delta
  :disabled
  :hook (magit-mode . magit-delta-mode)
  :custom
  ;; Default options
  (magit-delta-delta-args '("--max-line-distance" "0.6" "--color-only"))
  ;; Disable +/- markers
  ;; (magit-delta-hide-plus-minus-markers nil)
  ;; Dark syntax theme
  ;;   Options: Monokai Extended, OneHalfDark, Solarized (dark),
  ;;            gruvbox-light
  (magit-delta-default-dark-theme "OneHalfDark")
  ;; Light syntax theme
  ;;   Options: Monokai Extended Light, OneHalfLight, Solarized (light),
  ;;            gruvbox-light
  (magit-delta-default-light-theme "OneHalfLight"))

;; magit-gptcommit (Git commit with help of GPT)
(use-package magit-gptcommit
  ;; :disabled
  :demand t
  :after magit
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept))
  :init
  ;; (require 'llm-gemini)
  (require 'llm-openai)
  :custom
  ;; Supress warning
  (llm-warn-on-nonfree nil)
  (magit-gptcommit-prompt "You are an expert programmer writing a Git commit message.\n- You have carefully reviewed every file diff included in this commit.\n- Keep it to a single line, no more than 50 characters\n- Use the imperative tense (e.g., 'Add logging' not 'Added logging')\n- Ensure the message reflects a clear and cohesive change\n- Do not end the summary with a period\n- Do not use backticks (`) anywhere in the response.\n- Do not refer refer to the filename of the commit.\n- Do not state where the changes were applied.\n\nTHE FILE DIFFS:\n```\n%s\n```\nNow, write the commit message")
  :config
  ;; Set up LLM provider: OpenAI
  ;;   Use lazy setup to avoid password prompt during startup
  (setq magit-gptcommit-llm-provider
        (make-llm-openai :key (auth-info-password
                               (car (auth-source-search
                                     :host "api.openai.com"
                                     :user "apikey")))
                         :chat-model "gpt-4.1"))
  ;; Set up LLM provider: Gemini
  ;; (setq magit-gptcommit-llm-provider
  ;;       (make-llm-gemini :key (auth-info-password
  ;;                              (car (auth-source-search
  ;;                                    :host "generativelanguage.googleapis.com"
  ;;                                    :user "apikey")))
  ;;                        :chat-model "gemini-flash-latest"))
  ;; Add gptcommit transient commands to `magit-commit'
  (magit-gptcommit-status-buffer-setup))
  ;; Generate commit message automatically in Magit status buffer
  ;; (magit-gptcommit-mode 1))

;; gptel-magit (generate commit messages for Magit using gptel)
;;   MAYBE: try this package
(use-package gptel-magit
  :disabled
  :after (gptel magit)
  :hook (magit-mode . gptel-magit-install))

;; magit-todos (show source file TODOs in Magit)
;;   Available keywords: "HOLD", "TODO", NEXT","THEM", "PROG", "OKAY",
;;   "DONT", "FAIL", "DONE", "NOTE", "MAYBE" "KLUDGE, "HACK", "TEMP",
;;   "FIXME" "XXXX*"
(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode)
  ;; :custom
  ;; ;; Don't require a colon after keyword
  ;; (magit-todos-keyword-suffix ""))
  :config
  ;; Exclude some dirs from search
  (add-to-list 'magit-todos-exclude-globs "**/archive/*"))

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
  :config
  ;; Use pretty Unicode art
  (setq ztree-draw-unicode-lines t))
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

;; sinister (split-window-left superiority) - [source package]
(use-package sinister
  :disabled
  :custom
  ;; Number of lines to protect from incidental scrolling
  (sinister-stillness-margin 20)   ; default 12
  :config
  ;; Disable automatic scrolling with minibuffer use
  (sinister-stillness-mode 1))
  ;; Split window defaults
  ;; (sinister-misc-settings))

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
  :config
  (google-this-mode 1))

;;; Other

;; diminish (diminished modes are minor modes with no modeline display)
(use-package diminish
  :config
  ;; Pre-loaded modes
  (diminish 'eldoc-mode)
  (diminish 'visual-line-mode)
  ;; Not pre-loaded modes
  (add-hook 'auto-revert-mode-hook (lambda () (diminish 'auto-revert-mode)))
  (add-hook 'abbrev-mode-hook (lambda () (diminish 'abbrev-mode)))
  (add-hook 'visual-line-mode-hook (lambda () (diminish 'visual-line-mode))))

;; hydra (make bindings that stick around)
(use-package hydra
  :bind (("C-c y m" . hydra-markdown/body)
         ("C-c y w" . hydra-window/body)
         ("C-c y s" . hydra-scroll/body)
         ("C-c y n" . hydra-navigation/body)
         ("C-c y o" . hydra-outline/body)
         ("C-c y e" . hydra-smerge/body))
  :init
  (which-key-add-key-based-replacements "C-c y" "hydra")
  :config
  (require 'aj8-hydra))

;; inheritenv (make Emacs temp buffers inherit buffer-local environment variables)
;;   Requirement for claude-code
(use-package inheritenv)

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
                                    markdown-outdent-or-delete
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
  (add-hook 'keychain-environment-mode-hook (lambda () (diminish 'keychain-environment-mode)))
  (keychain-refresh-environment))


;;;;; LATE SETTINGS

;; System dependent settings
(cond ((eq aj8/my-os 'macos)   ; macOS
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

      ((eq aj8/my-os 'wsl)     ; WSL
       ;; Enable (default) web browser
       ;;   Requires: wslu
       (setq browse-url-generic-program "wslview")
       (setq browse-url-browser-function #'browse-url-generic)
       (advice-add #'browse-url-default-browser :override #'browse-url-generic)
       (message "Late settings WSL"))

      ((eq aj8/my-os 'linux)   ; Linux
       (cond
        ((eq aj8/my-linux-os 'fedora)
         (message "Late settings Fedora"))
        ((eq aj8/my-linux-os 'ubuntu)
         (message "Late settings Ubuntu")))
       (message "Late settings Linux"))

      (t (user-error "Unexpected system-name: %s" (system-name))))

;; Copy standard mode hooks to their Treesitter equivalents
;; (aj8/copy-hooks-to-treesitter)
(aj8/copy-hooks-to-treesitter-now)

;; Display *Messages* buffer
(display-buffer "*Messages*")

;; Conditionally load host specific stuff
;; (let ((host-specific-files (concat (make-load-path-base) (system-name) ".el")))
;;   (if (file-exists-p host-specific-files)
;;       (load host-specific-files)
;;     (message (concat "No host specific customizations for " (system-name)))
;;     ))

(provide 'init)

;;; init.el ends here
