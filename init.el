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
(setq package-selected-packages
      '(agent-shell
        ahk-mode
        ansible
        benchmark-init
        cape
        circadian
        claude-code
        consult
        consult-eglot
        consult-project-extra
        corfu
        corfu-terminal
        csv-mode
        dired-sidebar
        diff-hl
        diminish
        dimmer
        eat
        ;; ef-themes
        elisp-dev-mcp
        embark
        embark-consult
        esup
        exec-path-from-shell
        expreg
        flymake-aspell
        flymake-eslint
        flymake-json
        flymake-ruff
        google-this
        gptel
        gptel-agent
        gptel-magit
        helpful
        hydra
        inheritenv
        jinx
        keychain-environment
        keyfreq
        lua-mode
        magit
	magit-gptcommit
	magit-todos
        marginalia
        markdown-mode
        mcp
        mosey
        move-dup
        multiple-cursors
        obsidian
        orderless
        osx-trash
        outline-minor-faces
        package-lint
        pinentry
        powershell
        repeat-help
        ruff-format
        smartparens
        ssh-agency
        ;; standard-themes
        string-inflection
        system-packages   ; for use-package-ensure-system-package
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

;; Workaround: Prevent `package.el' from saving this list to `custom-file'.
;; This avoids repeated problems with `package-selected-packages' being
;; saved incorrectly, causing `package-autoremove' to delete too many
;; packages.
(put 'package-selected-packages 'saved-value nil)

;;; Selected packages sources
;;;   Install packages with (package-vc-install-selected-packages)
(setq package-vc-selected-packages
      `(;; (foo . "0f39eb3fd9")   ; specific revision
        ;; (bar . nil)            ; any revision
        (agent-shell-manager :url "https://github.com/jethrokuan/agent-shell-manager.git" :rev :newest)
        (auto-width-mode :url "https://github.com/sonofjon/auto-width-mode.el.git" :rev :newest)
        ;; (ai-code-interface :url "https://github.com/tninja/ai-code-interface.el.git" :rev :newest)
        (buffer-tail-mode :url "https://github.com/sonofjon/buffer-tail-mode.el.git" :rev :newest)
        (claude-code-ide :url "https://github.com/manzaltu/claude-code-ide.el.git" :rev :newest)
        (combobulate :url "https://github.com/mickeynp/combobulate.git" :rev :newest)
        (flymake-jsonlint :url "https://github.com/sonofjon/flymake-jsonlint.el.git" :rev :newest)
        (gptel-quick :url "https://github.com/karthink/gptel-quick.git" :rev :newest)
        (gptel-toolkit :url "https://github.com/sonofjon/gptel-toolkit.el.git"  :rev :newest)
        (hideshow-cycle :url "https://github.com/sonofjon/hideshow-cycle.el" :rev :newest)
        (markdown-links :url "https://github.com/sonofjon/markdown-links.el.git" :rev :newest)
        (mcp-server :url "https://github.com/rhblind/emacs-mcp-server.git" :rev :newest)
        (minibuffer-side-window-mode :url "https://github.com/sonofjon/minibuffer-side-window-mode.el.git" :rev :newest)
        (monet :url "https://github.com/stevemolitor/monet.git" :rev :newest)
        (obsidian-yaml-tools :url "https://github.com/sonofjon/obsidian-yaml-tools.el.git")
        (reflow :url "https://github.com/sonofjon/reflow.el" :rev :newest)
        (restore-killed :url "https://github.com/sonofjon/restore-killed.el" :rev :newest)))

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
  ;;   Note: doesn't restore side-windows
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
         ;; (dired-dwim-target t)
         ;; Guess target directory (unless prefix)
         (dired-dwim-target #'aj8/dired-dwim-target-unless-prefix)
         ;; Don't display free disk space
         (dired-free-space nil)
         ;; Reuse Dired buffers
         (dired-kill-when-opening-new-dired-buffer t)
         ;; Omit hidden (dot-) files
         (dired-omit-files "^\\.[a-zA-Z0-9]+")   ; with dired-omit-mode
         ;; Register file renames in VC system
         (dired-vc-rename-file t)
         :config
         ;; Tag Dired buffer names
         ;;   Use idle timer to avoid renaming while Dired is processing the buffer
         (add-hook 'dired-mode-hook
                   (lambda ()
                     (let ((buf (current-buffer)))
                       (run-with-idle-timer
                        0 nil
                        (lambda ()
                          (when (buffer-live-p buf)
                            (with-current-buffer buf
                              (aj8/prefix-buffer-name "dired"))))))))
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
            (lambda ()
              (keymap-set ediff-mode-map "c" #'aj8/ediff-copy-both-to-C)
              (keymap-set ediff-mode-map "A" #'aj8/ediff-copy-A-to-B)
              (keymap-set ediff-mode-map "B" #'aj8/ediff-copy-B-to-A))))

;; eglot (the Emacs client for LSP servers)
(use-package eglot
  :ensure nil   ; don't install built-in packages
  ;; Apply hooks to legacy modes (even for non-existing modes, for
  ;; consistency reasons). The function `aj8/copy-hooks-to-treesitter-now'
  ;; (called at the end of this file) automatically copies these hooks to
  ;; their modern `*-ts-mode' equivalents.
  :hook ((sh-mode . aj8/eglot-ensure-non-remote)
         (html-mode . aj8/eglot-ensure-non-remote)
         (mhtml-mode . aj8/eglot-ensure-non-remote)
         (css-mode . aj8/eglot-ensure-non-remote)
         (web-mode . aj8/eglot-ensure-non-remote)   ; no linting
         (js-mode . aj8/eglot-ensure-non-remote)
         (json-mode . aj8/eglot-ensure-non-remote)   ; non-existing mode
         (latex-mode . aj8/eglot-ensure-non-remote)
         (lua-mode . aj8/eglot-ensure-non-remote)
         (markdown-mode . aj8/eglot-ensure-non-remote)
         (python-mode . aj8/eglot-ensure-non-remote)
         (toml-ts-mode . aj8/eglot-ensure-non-remote)
         (yaml-mode . aj8/eglot-ensure-non-remote))   ; non-existing mode
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
  ;; Use Orderless for Eglot (default is Flex)
  (add-to-list 'completion-category-overrides '((eglot (styles orderless))))
  ;; Eglot completion-at-point-functions workarounds
  ;;   Eglot prepends its capf to the front of
  ;;   completion-at-point-functions. Since eglot returns an empty list
  ;;   when LSP has no candidates (rather than nil), it blocks subsequent
  ;;   capfs from running. The functions below provide different solutions
  ;;   to this problem.
  ;; Solution 1: Reorder capfs so eglot runs last
  ;;   This allows other capfs (dabbrev, dict) to provide completions when
  ;;   LSP has none.
  (defun aj8/reorder-eglot-capf ()
    "Move eglot-completion-at-point after other capfs.
Removes both eglot and `t' from buffer-local capf list, then appends
eglot followed by `t' at the end."
    (when (memq #'eglot-completion-at-point completion-at-point-functions)
      (setq-local completion-at-point-functions
                  (append (remove #'eglot-completion-at-point
                                  (remove t completion-at-point-functions))
                          (list #'eglot-completion-at-point t)))))
  ;; (add-hook 'eglot-managed-mode-hook #'aj8/reorder-eglot-capf)
  ;; Solution 2: Merge eglot with other capfs
  ;;   Uses cape-capf-super to merge LSP completions with other capfs
  ;;   (dabbrev, dict), so all candidates appear together regardless of
  ;;   whether LSP has results.
  (defun aj8/combine-eglot-capf ()
    "Merge eglot-completion-at-point with other capfs.
Replaces buffer-local capf list with a merged capf using cape-capf-super,
followed by `t'."
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'eglot-completion-at-point
                                       #'cape-dabbrev
                                       #'cape-dict)
                      t)))
  (add-hook 'eglot-managed-mode-hook #'aj8/combine-eglot-capf))
  ;; Don't manage ELDoc
  ;; (add-to-list 'eglot-stay-out-of 'eldoc))
  ;; Limit ELDoc to a single line
  ;; (setq eldoc-echo-area-use-multiline-p nil))   ; TODO: doesn't work nicely
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
  :bind (("C-<backspace>" . (lambda () (interactive) (kill-line 0)))
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
         ("C-x M-e" . my/eval-next-sexp)
         ("C-x C-M-e" . my/eval-sexp-at-point))
  :init
  (which-key-add-key-based-replacements "C-c x" "misc")
  :custom
  ;; Personal settings
  (user-full-name "Andreas Jonsson")
  (user-mail-address "ajdev8@gmail.com")
  ;; Set fill column
  (fill-column 76)   ; default is 70
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
  ;; Don't use the mark when region is inactive
  ;;   Note: messes with ediff
  ;; (mark-even-if-inactive nil)
  ;; Use longer pulse
  (pulse-delay 0.05)   ; default is 0.03
  ;; Delete selection on edit
  (delete-selection-mode 1)
  ;; Use TAB for symbol completion (after indentation)
  (tab-always-indent 'complete)
  ;; Increase history length
  (history-length 10000)
  ;; Delete history duplicates
  (history-delete-duplicates nil)
  ;; Use the system's trash can
  (delete-by-moving-to-trash t)
  :config
  ;; Use 'y' or 'n' questions always
  ;; (setq use-short-answers t)
  ;; Add Treesitter indicator in the modeline
  (add-hook 'after-change-major-mode-hook #'aj8/treesit-mode-name))

;; epg (the EasyPG library)
(use-package epg
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Fill gpg passwords in the minibuffer
  ;;   NOTE: loopback does not support password caching with gpg-agent
  ;; (epg-pinentry-mode 'loopback)   ; default is nil/ask (Emacs/gpg)
  ;; Select keys in the minibuffer
  (epa-keys-select-method 'minibuffer))

;; pinentry (GnuPG Pinentry server implementation)
(use-package pinentry
  :if (eq aj8/my-os 'wsl)
  :ensure t
  :init
  (load (expand-file-name "lisp/aj8-pinentry" user-emacs-directory))
  (pinentry-start))

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
  ;; (eww-search-prefix "https://kagi.com/search?q=")   ; default is duckduckgo
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

;; files (file input and output commands)
(use-package files
  :ensure nil   ; don't install built-in packages
  :bind (("C-c x r" . aj8/reload-init-file))
  :custom
  ;; Revert without querying
  (revert-without-query '(".*"))
  ;; Increase maximum file size that can be opened without a warning
  (large-file-warning-threshold 50000000))

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
  (help-window-select t)
  ;; Show mode headers in describe-bindings buffer
  (describe-bindings-outline t))

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
  (global-hl-line-mode 1)
  ;; Deactivate highlight mode when selecting text
  (add-hook 'activate-mark-hook (lambda () (global-hl-line-mode -1)))
  (add-hook 'deactivate-mark-hook (lambda () (global-hl-line-mode 1))))

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

;; isearch (incremental search minor mode)
(use-package isearch
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Interpret spaces literally when searching
  ;; (isearch-lax-whitespace nil)
  ;; Interpret spaces as wildcards when searching
  ;; (isearch-lax-whitespace t)
  ;; Interpret any characters as wildcards when searching
  (search-whitespace-regexp ".*?")
  ;; Allow movement between Isearch matches by cursor motion commands
  (isearch-allow-motion t)
  (isearch-motion-changes-direction t)
  :config
  (keymap-set isearch-mode-map "TAB" #'isearch-complete))

;; ispell (interface to spell checkers)
(use-package ispell
  :ensure nil   ; don't install built-in packages
  :ensure-system-package aspell
  :custom
  ;; Use aspell
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
  :hook (emacs-lisp-mode . aj8/outline-headers-for-semicolon-buffers))

;; minibuffer (minibuffer and completion functions)
(use-package minibuffer
  :ensure nil   ; don't install built-in packages
  :bind ("M-o" . aj8/orderless-matching-style-cycle)
  :custom
  ;; Allow minibuffer commands while in the minibuffer
  (enable-recursive-minibuffers t)
  ;; Timeout for messages in active minibuffer
  (minibuffer-message-clear-timeout 1)
  ;; Select completion styles
  ;;   orderless: space-separated components
  ;;   substring: needed for partial completion
  ;;   basic: fallback
  ;; Order from most specific to least specific
  ;; (completion-styles '(orderless substring  basic))
  ;; (completion-styles '(orderless basic))
  (completion-styles '(orderless))
  ;; Show more details for completions
  (completions-detailed t)
  :config
  ;; Show recursion depth in the minibuffer prompt
  (minibuffer-depth-indicate-mode 1)
  ;; Use partial completion for files
  (setq completion-category-defaults nil)
  (add-to-list 'completion-category-overrides
               '((file (styles basic partial-completion)))))

;; modus-themes (elegant, highly legible and customizable themes)
(use-package modus-themes
  ;; :if (eq aj8/my-os 'linux)   ; Linux
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
  ;; (setq modus-themes-subtle-line-numbers t)
  ;; Define the visibility of fringes
  ;;   Options: `nil',`subtle', `intense'
  ;; (setq modus-themes-fringes nil)
  ;; Control the style of spelling and code checkers/linters
  ;;   Options: `straight-underline', `text-also', `background',
  ;;            `intense', `faint'
  ;; (setq modus-themes-lang-checkers '(straight-underline text-also))
  ;; Control the style of the mode line
  ;;   Options: `3d' OR `moody', `borderless', `accented'
  ;; (setq modus-themes-mode-line '(borderless))
  ;(setq modus-themes-mode-line nil)
  ;; Control the style of code syntax highlighting
  ;;   Options: `faint', `yellow-comments', `green-strings',
  ;;            `alt-syntax'
  ;; (setq modus-themes-syntax '(faint green-strings alt-syntax))
  ;; Style markup in Org, markdown, and others
  ;;   Options: `bold', `italic', `background', `intense'
  ;; (setq modus-themes-markup '(background italic))
  ;; (setq modus-themes-markup nil)
  ;; Control the current line highlight of HL-line mode
  ;;   Options: `accented', `underline', `intense'
  ;; (setq modus-themes-hl-line nil)
  ;; Control the style of matching parentheses or delimiters
  ;;   Options: `bold', `intense', `underline'
  ;; (setq modus-themes-paren-match '(intense))
  ;; Set the style of links
  ;;   Options: `neutral-underline' OR `no-underline', `faint' OR
  ;;            `no-color', `bold', `italic', `background'
  ;; (setq modus-themes-links nil)
  ;; Control the style of buttons in the Custom UI and related
  ;;   Options: `flat', `accented', `faint', `variable-pitch',
  ;;            `underline'
  ;; (setq modus-themes-box-buttons '(variable-pitch flat faint 0.9))
  ;; (setq modus-themes-box-buttons nil)
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
  ;; (setq modus-themes-region nil)
  ;; Adjust the style of diffs
  ;;   Options: `desaturated', `bg-only'
  ;; (setq modus-themes-diffs nil)
  ;; Set the style of Org code blocks, quotes, and the like
  ;;   Options: `gray-background', `tinted-background'
  ;; (setq modus-themes-org-blocks 'gray-background)
  ;; Org styles
  ;;   Options: see manual
  ;; (setq modus-themes-org-agenda
  ;;       '((header-block . (variable-pitch 1.3))
  ;;         (mail-header-parse-date . (grayscale workaholic bold-today 1.1))
  ;;         (event . (accented varied))
  ;;         (scheduled . uniform)
  ;;         (habit . traffic-light)))
  ;; (setq modus-themes-org-agenda nil)
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

;; package (simple package system for Emacs)
(use-package package
  :ensure nil   ; don't install built-in packages
  :init
  ;; Show package names in upgrade prompt
  (advice-add 'package-upgrade-all :around #'aj8/package-upgrade-all)
  ;; Prevent autoremove from removing VC packages
  (advice-add #'package-autoremove :around #'aj8/package-autoremove-no-vc))

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
  :bind (("C-x C-<left>" . my/project-previous-buffer)
         ("C-x C-<right>" . my/project-next-buffer)
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
  ;;   Not needed: use M-s G
  ;; (setq-local project-ignored-files '(".venv/*"))
  ;; Use ruff as the Flymake linter
  ;;   Note, this is not to be needed with Eglot
  ;; (setq python-flymake-command
  ;;       '("ruff" "--quiet" "--stdin-filename=stdin" "-"))
  ;; Silence warnings about missing linters
  ;;   This remove python-flymake from flymake-diagnostic-functions
  ;;   TODO: Why is this needed?
  (add-hook 'python-base-mode-hook
            (lambda ()
              (remove-hook 'flymake-diagnostic-functions #'python-flymake t)))
  ;; Outline settings
  (add-hook 'python-base-mode-hook #'aj8/outline-headers-for-hash-mark-buffers))

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
  (add-hook 'sh-base-mode-hook #'aj8/outline-headers-for-hash-mark-buffers)
  (add-hook 'conf-xdefaults-mode-hook #'aj8/outline-headers-for-hash-mark-buffers)
  (add-hook 'powershell-mode-hook #'aj8/outline-headers-for-hash-mark-buffers)
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
  :bind (("C-o" . my/open-line)
         ("C-x <left>" . aj8/previous-buffer)
         ("C-x <right>" . aj8/next-buffer)
         ("C-x k" . kill-current-buffer)
         ("C-c k" . my/kill-buffer-other-window)
         ("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim)
         ("M-a" . back-to-indentation)
         ("C-c N" . column-number-mode))
  :custom
  ;; Do not switch to buffers already shown
  (switch-to-prev-buffer-skip 'this)
  ;; Skip some buffers when switching buffers
  ;; (setq switch-to-prev-buffer-skip 'aj8/buffer-skip-p)
  ;; Skip some buffers when switching buffers
  ;; (setq switch-to-prev-buffer-skip-regexp regex)
  ;; Use spaces, not tabs
  (indent-tabs-mode nil)
  ;; Delete trailing newline character with kill-line
  (kill-whole-line t)
  ;; Use "repeat-mode" for "pop-mark"
  (set-mark-command-repeat-pop t)
  ;; Save clipboard text into kill ring before kill
  ;; (save-interprogram-paste-before-kill t)
  ;; Exclude mode-specific commands from M-x completion
  ;; (read-extended-command-predicate #'command-completion-default-include-p)
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
  :hook ((latex-mode . aj8/outline-headers-for-percentage-buffers)
         (latex-mode . (lambda () (setq comment-add 0))))
  :config
  (add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode)))
  ;; (add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-ts-mode)))

;; tramp (Transparent Remote (file) Access, Multiple Protocol)
;;   References: https://www.gnu.org/software/tramp/#Frequently-Asked-Questions-1
;;               https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(use-package tramp
  :ensure nil   ; don't install built-in packages
  :custom
  ;; Override the HISTFILE
  (tramp-histfile-override t)
  ;; Only use Git
  (vc-handled-backends '(Git))
  ;; Disable file lock
  (remote-file-name-inhibit-locks t)
  ;; Don't auto save
  (remote-file-name-inhibit-auto-save t)
  (remote-file-name-inhibit-auto-save-visited t)
  ;; Reduce message verbosity
  (tramp-verbose 2)
  ;; Use rsync for local-remote copying
  (tramp-default-method "rsync")   ; default is "scp"
  ;; Use scp for remote-remote copying
  (tramp-use-scp-direct-remote-copying t)
  ;; Optimize copy method
  (tramp-copy-size-limit (* 1024 1024))   ; 1MB
  :config
  ;; Use direct async method
  ;;   May cause problems with files that use DOS line endings:
  ;;   https://github.com/magit/magit/issues/5220
  ;; (connection-local-set-profile-variables
  ;;  'remote-direct-async-process
  ;;  '((tramp-direct-async-process . t)))
  ;; (connection-local-set-profiles
  ;;  '(:application tramp :protocol "scp")
  ;;  'remote-direct-async-process)
  ;; (setq magit-tramp-pipe-stty-settings 'pty)
  ;; Add mode line indicator
  (add-to-list 'global-mode-string '(:eval (aj8/tramp-indicator)) t))

;; transient (transient commands)
(use-package transient
  :ensure nil   ; don't install built-in packages
  :config
  ;; Restore side-window config after transient
  (advice-add 'transient--show :before
              #'aj8/transient--save-config)
  (advice-add 'transient--delete-window :after
              #'aj8/transient--restore-config))

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
(use-package which-key
  :diminish
  :custom
  ;; Use minibuffer
  (which-key-popup-type 'minibuffer)   ; default is side-window
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
  :init
  ;; Keep focus in side windows after quit-window
  (advice-add 'quit-window :around #'aj8/retain-side-window-focus)
  ;; Keep focus in side windows after kill-current-buffer
  (advice-add 'kill-current-buffer :around #'aj8/retain-side-window-focus)
  ;; Make quit-window kill buffer by default (inverse behavior)
  (advice-add 'quit-window :filter-args #'my/quit-window)
  ;; Always respect split-height-threshold
  (advice-add 'split-window-sensibly :around
              #'aj8/split-window-sensibly-respect-threshold)
  ;; Enable better quit-window behavior
  ;; (advice-add 'display-buffer :filter-return #'my/better-quit-window-save)
  ;; (advice-add 'quit-restore-window :around #'my/better-quit-window-restore)
  :custom
  ;; Set minimum window height
  ;; (window-min-height 16)
  ;; Prefer horizontal (side-by-side) window splitting
  ;;   Note: the thresholds need to be twice as big as the smallest
  ;;   window allowed, because the new windows each use half of the
  ;;   former window size
  (split-width-threshold nil)
  (split-height-threshold 32)
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
  ;;   Note: Patterns match case-insensitively by default.  To make patterns
  ;;   match case-sensitively, define a helper function:
  ;;     (defun aj8/case-sensitive-matcher (regexp)
  ;;       "Return a case-sensitive string matcher for REGEXP."
  ;;       (lambda (str _alist)
  ;;         (let ((case-fold-search nil))
  ;;           (string-match-p regexp str))))
  ;;   Then use: (,(aj8/case-sensitive-matcher "^\\*Messages\\*") ...)
  ;;   instead of: ("^\\*Messages\\*" ...)
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
     ("^\\*\\(Messages\\|Warnings\\|Native-compile-Log\\|Async-native-compile-log\\)\\*"
      (display-buffer-in-side-window)
      (window-height . ,aj8/side-window-height)
      (side . top)
      (window-parameters . ((no-delete-other-windows . t))))
     ("^\\(magit-process:.*\\|acp-client-stderr.*\\)"
      (display-buffer-in-side-window)
      (window-height . ,aj8/side-window-height)
      (side . top)
      (window-parameters . ((no-delete-other-windows . t))))
     ("^\\*\\(acp error\\|EGLOT.*events\\|Flymake diagnostics.*\\|texlab\\(::stderr\\)?\\|tramp.*\\|vc-git.*\\|.*-ls\\(::.*\\)?\\)\\*"
      (display-buffer-in-side-window)
      (window-height . ,aj8/side-window-height)
      (side . top)
      (window-parameters . ((no-delete-other-windows . t))))
     ;;
     ;; Right side window
     ;;
     ;;   Magit
     ;;
     ;; ("\\(COMMIT_EDITMSG\\).*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-width . ,aj8/side-window-width-dynamic)
     ;;  (side . right)
     ;;  (slot . -1)
     ;;  (window-parameters . ((no-delete-other-windows . t))))
     ("^\\(magit-diff:\\).*"
      (display-buffer-in-side-window)
      (window-width . ,aj8/side-window-width-dynamic)
      (side . right)
      (slot . 1)
      (window-parameters . ((no-delete-other-windows . t))))
     ("^\\(COMMIT_EDITMSG\\|magit:\\|magit-log.*:\\|magit-reflog:\\|magit-log-select:\\).*"
      (display-buffer-in-side-window)
      (window-width . ,aj8/side-window-width-dynamic)
      (side . right)
      (slot . ,aj8/side-window-slot-top)
      (window-parameters . ((no-delete-other-windows . t))))
     ("^\\(magit-revision:\\|magit-stash:\\).*"
      (display-buffer-in-side-window)
      (window-width . ,aj8/side-window-width-dynamic)
      (side . right)
      (slot . ,aj8/side-window-slot-bottom)
      (window-parameters . ((no-delete-other-windows . t))))
     ;;
     ;;   Help
     ;;
     ("^\\*\\(Apropos\\|Help\\|helpful.*\\|info.*\\|Man.*\\|WoMan.*\\)\\*"
      (display-buffer-in-side-window)
      (window-width . ,aj8/side-window-width-dynamic)
      (side . right)
      (slot . ,aj8/side-window-slot-bottom)
      (window-parameters . ((no-delete-other-windows . t))))
     ;;
     ;;   Other
     ;;
     ("^\\*\\(Bookmark List\\|Benchmark Init Results.*\\|ChatGPT.*\\|Claude.*\\|claude:.*\\|claude-code.*\\|Deepseek.*\\|Embark Collect:.*\\|Gemini.*\\|gptel-agent:.*\\|Occur\\|.*Output\\|Semantic SymRef\\|devdocs\\|eldoc\\|package update results\\|tex-shell\\)\\*"
      (display-buffer-in-side-window)
      (window-width . ,aj8/side-window-width-dynamic)
      (side . right)
      (slot . ,aj8/side-window-slot-top)
      (window-parameters . ((no-delete-other-windows . t))))
     ("^\\(Claude Code Agent.*\\|Copilot Agent.*\\|Gemini CLI Agent.*\\|gptel-.*\\.\\(org\\|md\\)\\)"
      (display-buffer-in-side-window)
      (window-width . ,aj8/side-window-width-dynamic)
      (side . right)
      (slot . ,aj8/side-window-slot-top)
      (window-parameters . ((no-delete-other-windows . t))))
     ;; MAYBE: eww doesn't work initially
     ("^\\*\\(eww:.*\\|gptel-reasoning\\)\\*"
      (display-buffer-in-side-window)
      (window-width . ,aj8/side-window-width-dynamic)
      (side . right)
      (slot . ,aj8/side-window-slot-bottom)
      (window-parameters . ((no-delete-other-windows . t))))
     ;;
     ;; Bottom side window
     ;;
     ("^\\*\\(eat\\|e?shell\\|v?term\\|Agent-Shell Buffers\\)\\*"
      (display-buffer-in-side-window)
      (window-height . ,aj8/side-window-height)
      (side . bottom)
      (window-parameters . ((no-delete-other-windows . t))))
     ;;
     ;; Catch-all rules based on derived modes (lowest priority)
     ;;
     ((lambda (buffer _alist)
        (with-current-buffer buffer (or (derived-mode-p 'backtrace-mode)
                                        (derived-mode-p 'compilation-mode))))
      (display-buffer-in-side-window)
      (window-height . ,aj8/side-window-height)
      (side . top)
      (window-parameters . ((no-delete-other-windows . t))))
     ((lambda (buffer _alist)
        (with-current-buffer buffer (or (derived-mode-p 'git-rebase-mode)
                                        (derived-mode-p 'tabulated-list-mode))))
      (display-buffer-in-side-window)
      (window-width . ,aj8/side-window-width-dynamic)
      (side . right)
      (slot . ,aj8/side-window-slot-top)
      (window-parameters . ((no-delete-other-windows . t)))))))
  ;; :config
  ;; Resize window combinations proportionally
  ;;   MAYBE: Makes *Messages* buffer grow on each Magit commit
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

;; agent-shell (an agent shell powered by ACP)
(use-package agent-shell
  :bind (("C-c C-a" . agent-shell)
         :map agent-shell-mode-map
         ;; ("TAB" . agent-shell-next-item)   ; fix for terminal
         ("C-c C-v" . nil)   ; unbind agent-shell-set-session-model
         ("C-c M" . agent-shell-set-session-model)
         ("C-c <up>" . agent-shell-previous-item)
         ("C-c <down>" . agent-shell-next-item)
         ("C-c C-<up>" . comint-previous-prompt)
         ("C-c C-<down>" . comint-next-prompt))
  :custom
  ;; Don't add line as context
  (agent-shell-context-sources '(files region error))
  ;; Enable viewport mode
  ;; (agent-shell-prefer-viewport-interaction t))
  :config
  ;; Disable comint completion in agent-shell
  (add-hook 'agent-shell-mode-hook
            (lambda () (setq-local comint-dynamic-complete-functions nil))))

;; agent-shell-manager (buffer manager for agent-shell)
(use-package agent-shell-manager
  :custom
  ;; Disable side-window placement
  (agent-shell-manager-side nil)
  :config
  (agent-shell-manager-toggle))

;; ai-code-interface (unified emacs interface supporting LLM clients)
(use-package ai-code-interface
  :disabled
  :bind (("C-c a" . ai-code-menu))
  :init
  (which-key-add-key-based-replacements "C-c a" "ai")
                                        ; add label for prefix key
  :config
  ;; Set backend
  (ai-code-set-backend 'claude-code)
  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

;; monet (Claude Code MCP over websockets)
(use-package monet
  ;; :disabled
  :diminish
  :custom
  ;; Disable prefix key
  (monet-prefix-key nil)
  ;; (monet-prefix-key "C-c a C-m"))
  ;; Use Ediff
  ;;   Requires proper auto-revert, see claude-code section
  ;; (monet-diff-tool #'monet-ediff-tool)
  ;; (monet-diff-cleanup-tool #'monet-ediff-cleanup-tool)
  ;; Disable diff tool
  (monet-diff-tool nil))

;; claude-code (Claude Code Emacs integration)
(use-package claude-code
  ;; :disabled
  :diminish
  :bind-keymap ("C-c a" . claude-code-command-map)
  :init
  (which-key-add-key-based-replacements "C-c a" "ai")
                                        ; add label for prefix key
  :custom
  ;; Set terminal backend
  ;; (claude-code-terminal-backend 'vterm)   ; default: eat
  ;; Customize Return key
  (claude-code-newline-keybinding-style 'newline-on-alt-return)
  ;; Prevent Claude Code windows from being deleted
  (claude-code-no-delete-other-windows t)
  ;; Automatically select the Claude buffer after opening it
  (claude-code-toggle-auto-select t)
  :config
  ;; Revert buffers on file change
  ;;   Claude edits files directly on disk, not in Emacs buffers
  ;; MAYBE: Implement revert with more control: see https://github.com/stevemolitor/claude-code.el?tab=readme-ov-file#tips-and-tricks
  ;; (global-auto-revert-mode 1)
  ;; Adapt eat settings
  (add-hook 'claude-code-start-hook
          (lambda ()
            (when (eq claude-code-terminal-backend 'eat)
              ;; Increase scrollback limit
              (setq-local eat-term-scrollback-size 500000))))
  ;; Adapt vterm settings
  (add-hook 'claude-code-start-hook
          (lambda ()
            (when (eq claude-code-terminal-backend 'vterm)
              ;; Increase scrollback limit
              (setq-local vterm-max-scrollback 10000)
              ;; Reduce minimum window limit
              (setq-local vterm-min-window-width 40))))
  ;; Optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions
            #'monet-start-server-function)
  (monet-mode 1)
  ;; Enable mode
  (claude-code-mode))

;; claude-code-ide (Claude Code integration for Emacs)
(use-package claude-code-ide
  :disabled
  :bind ("C-c a" . claude-code-ide-menu)
  :init
  (which-key-add-key-based-replacements "C-c a" "ai")
                                        ; add label for prefix key
  :custom
  ;; Use regular buffer
  (claude-code-ide-use-side-window nil)
  ;; Hide Claude window during Ediff
  (claude-code-ide-show-claude-window-in-ediff nil)
  :config
  ; Optionally enable Emacs MCP tools
  (claude-code-ide-emacs-tools-setup))

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
  ;; Auto-save gptel buffers with timestamped filenames
  (advice-add 'gptel :around #'aj8/gptel-write-buffer)
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
  ;; Highlight LLM responses
  (gptel-highlight-methods '(face))   ; default is '(margin)
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
    :key (gptel-api-key-from-auth-source "api.anthropic.com" "apikey")
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
  ;; === Alibaba ===
  (gptel-make-openai "Alibaba"
    ;; :host "dashscope-intl.aliyuncs.com"
    :host "dashscope-intl.aliyuncs.com"
    :protocol "https"
    :endpoint "/compatible-mode/v1/chat/completions"
    :key (gptel-api-key-from-auth-source
          "dashscope-intl.aliyuncs.com" "apikey")
    :stream t
    :models '(qwen3-coder-plus-2025-09-23))
  ;; === Deepseek ===
  (gptel-make-deepseek "Deepseek"
    :key (gptel-api-key-from-auth-source "api.deepseek.com" "apikey")
    :stream t)
  ;; Use max tokens
  (put 'deepseek-reasoner :request-params '(:max_tokens 65536))
  (put 'deepseek-chat :request-params '(:max_tokens 8192))
  ;; === Moonshot ===
  (gptel-make-openai "Moonshot"
    :host "api.moonshot.ai"   ; or "api.moonshot.cn" for the Chinese site
    :key (gptel-api-key-from-auth-source "api.moonshot.ai" "apikey")
    :stream t
    :models '(kimi-k2-thinking))
  ;; === OpenRouter ====
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :key (gptel-api-key-from-auth-source "openrouter.ai" "apikey")
    :stream t
    :models '(deepseek/deepseek-chat-v3-0324
              deepseek/deepseek-v3.2
              moonshotai/kimi-k2-thinking
              qwen/qwen3-coder:free
              qwen/qwen3-coder-flash
              qwen/qwen3-coder-plus))
  ;; Enable reasoning
  (dolist (provider '(deepseek/deepseek-v3.2))
    (put provider :request-params '(:reasoning (:enabled t))))
  ;; === GitHub Copilot Chat ====
  (gptel-make-gh-copilot "Copilot")
  ;; Enable word-wrap
  (add-hook 'gptel-mode-hook (lambda () (visual-line-mode 1)))
  ;; Highlight LLM response regions
  (add-hook 'gptel-mode-hook #'gptel-highlight-mode)
  ;; Disable Eglot
  (add-hook 'gptel-mode-hook (lambda () (eglot--managed-mode -1)))
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
                             (expand-file-name "data/directive.md"
                                               user-emacs-directory))
                            (string-trim (buffer-string))))
               t)
  (add-to-list 'gptel-directives
               '(debug . "You are a large language model and a debugger. Diagnose issues and suggest fixes.")
               t)
  ;; Preset: chat
  (gptel-make-preset 'chat
    :description "Preset for chat"
    :system 'chat
    :temperature 1.0
    :model 'gemini-flash-latest
    :use-context nil
    :include-reasoning 'ignore
    :use-tools nil)
  ;; Preset: coding
  ;;   MAYBE: Make this the default setting
  (gptel-make-preset 'coding
    :description "Preset for coding"
    :system 'coder
    :model 'deepseek-reasoner
    :temperature 0.1
    :use-context 'user
    :include-reasoning "*gptel-reasoning*"
    ;; TODO: Use :eval or :function to evaluate expressions or functions
    ;; :tools (gptel-tk-get-tools)
    ;; :post (gptel-tk-enable-builtin-tools)
    :use-tools t)
  ;; Enable MCP
  (require 'gptel-integrations))

;; gptel-quick (quick LLM lookups in Emacs)
(use-package gptel-quick
  :bind ("C-c t h" . gptel-quick))
  ;; :config
  ;; ;; Set approximate word count of LLM summary
  ;; (gptel-quick-word-count 12))

;; gptel-agent (agentic LLM use for gptel)
(use-package gptel-agent
  ;; :disabled
  :after gptel
  :config
  ;; Read files from agents directories
  (gptel-agent-update))

;; gptel-agent-tools-introspection (Emacs introspection tools for gptel-agent)
(use-package gptel-agent-tools-introspection
  :defer
  :config
  ;; Allow variable value tool
  (setf (gptel-tool-confirm (gptel-get-tool "variable_value"))
        nil))

(use-package gptel-toolkit
  :disabled
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


;; mcp-server (pure Elisp MCP Server)
;;   See ~/dotfiles/config-ai/MCP.md for configuration instructions
(use-package mcp-server
  :ensure-system-package socat
  :config
  ;; Apply the default security settings
  (setq mcp-server-security-prompt-for-permissions t)
  (setq mcp-server-security-allowed-dangerous-functions nil)
  ;; Add sensitive file patterns
  ;; (dolist (pattern '("~/my-secrets/" "*.key"))
  ;;   (add-to-list 'mcp-server-security-sensitive-file-patterns pattern))
  ;; Add sensitive buffer patterns
  ;; (dolist (pattern '("*my-secure-buffer*" "*top secret*"))
  ;;   (add-to-list 'mcp-server-security-sensitive-buffer-patterns pattern))
  ;; Add functions that require permission
  ;; (dolist (func '(some-secure-command super-secret-command))
  ;;   (add-to-list 'mcp-server-security-dangerous-functions func))
  ;; Enable all tools (default)
  ;; (setq mcp-server-emacs-tools-enabled 'all)
  ;; Enable only specific tools (disable eval-elisp for security)
  ;; (setq mcp-server-emacs-tools-enabled '(get-diagnostics))
  ;; Start MCP server
  (add-hook 'emacs-startup-hook #'mcp-server-start-unix)
  ;; Disable query on exit
  (add-hook 'emacs-startup-hook #'aj8/mcp-server-no-query-on-exit t))

;; elisp-dev-mcp (MCP server for agentic Elisp development)
;;   See ~/dotfiles/config-ai/MCP.md for configuration instructions
(use-package elisp-dev-mcp
  :config
  ;; Allow additional directories
  (setq elisp-dev-mcp-additional-allowed-dirs
        '("~/.emacs.d/"
          "~/git/"
          "~/dotfiles/"
          "~/projects/"))
  ;; Start Emacs server (required for emacsclient)
  (add-hook 'emacs-startup-hook #'server-start)
  ;; Start MCP server
  (add-hook 'emacs-startup-hook #'mcp-server-lib-start))

;;; Buffers

;; buffer-tail-mode (auto-scroll buffers to end)
(use-package buffer-tail-mode
  :demand t
  :config
  ;; Enable auto-scrolling for the *Messages* buffer
  ;;   Note that the *Messages* buffer is created early during startup
  ;;   (before this hook is set), so this hook only applies to newly
  ;;   created *Messages* buffers.  The `with-current-buffer' statement
  ;;   applies the mode to the current *Messages* buffer.
  ;; (add-hook 'messages-buffer-mode-hook
  ;;           (lambda () (buffer-tail-mode 1)))
  (with-current-buffer "*Messages*" (buffer-tail-mode 1)))

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

;; restore-killed (restore killed buffers and files)
(use-package restore-killed
  :demand t
  :diminish restore-killed-mode
  :custom
  ;; Maximum number of killed files to store
  (restore-killed-file-max 20)
  ;; Maximum number of killed buffers to store
  (restore-killed-buffer-max 5)
  ;; Maximum buffer-size to store
  (restore-killed-buffer-max-size 50000)
  :config
  ;; Track killed files and buffers
  (restore-killed-mode 1))

;;; Coding

;; ahk-mode (major-mode for editing AHK (AutoHotkey and AutoHotkey_L))
(use-package ahk-mode
  :mode "\\.ahk$")

;; ansible (minor-mode for editing Ansible (YAML) files)
(use-package ansible
  :defer)
  ;; :mode "\\.yml$"
  ;; :hook (yaml-mode . (lambda () (ansible 1))))

;; csv-mode (major mode for editing comma/char separated values)
(use-package csv-mode
  :mode ("\\.csv$")
  :bind (:map csv-mode-map
              ("C-c M-a" . my/csv-align-visible)
              ("C-M-<left>" . csv-backward-field)
              ("C-M-<right>" . csv-forward-field))
  ;; Enable modeline linenumber for wide files
  :custom
  (line-number-display-limit-width 500)   ; default is 200
  :config
  (defun my/csv-align-visible (&optional arg)
    "Align visible fields."
    (interactive "P")
    (csv-align-fields nil (window-start) (window-end))))

;; lua-mode (major-mode for editing Lua scripts)
(use-package lua-mode
  :mode ("\\.lua$"))

;; markdown-mode (major-mode for editing Markdown files)
(use-package markdown-mode
  :mode "\\.md$"
  ;; :mode ("\\.md$" . markdown-view-mode)
  :bind (:map markdown-mode-map
              ("M-p" . nil)     ; unbind markdown-previous-link
              ("M-n" . nil)         ; unbind markdown-next-link
              ("C-c <left>" . nil)     ; unbind markdown-promote
              ("C-c <right>" . nil)     ; unbind markdown-demote
              ("C-c C-<up>" . markdown-outline-previous-same-level)
              ("C-c C-<down>" . markdown-outline-next-same-level)
              ("C-c <up>" . markdown-outline-previous)
              ("C-c <down>" . markdown-outline-next)
              ("C-c C-M-n" . markdown-move-down)
              ("C-c C-M-p" . markdown-move-up)
              ("C-c C-M-<left>" . markdown-promote)
              ("C-c C-M-<right>" . markdown-demote)))

;; markdown-links (insert Markdown links from various sources)
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

;; flymake-jsonlint (custom Flymake backend for jsonlint)
(use-package flymake-jsonlint
  :ensure nil
  :after flymake
  :ensure-system-package jsonlint
  ;; :hook ((js-json-mode json-ts-mode) . flymake-jsonlint-load)   ; without eglot
  :hook (eglot-managed-mode . flymake-jsonlint-load))   ; with eglot

;; flymake-ruff (a Flymake plugin for python files using Ruff)
(use-package flymake-ruff
  :disabled   ; already included with pylsp
  :ensure-system-package ruff
  ;; :hook (python-base-mode . flymake-ruff-load)   ; without eglot
  :hook (eglot-managed-mode . flymake-ruff-load))   ; with eglot

;; package-lint (a linting library for elisp package authors)
(use-package package-lint
  :defer)

;; ruff-format (Ruff format Python source)
(use-package ruff-format
  :disabled
  :ensure-system-package ruff)
  ;; :hook (python-base-mode . ruff-format-on-save-mode))

;; treesit-auto (automatically use tree-sitter enhanced major modes)
;;   TODO: Add LaTeX: https://github.com/latex-lsp/tree-sitter-latex
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
         ("C-c c M-x" . consult-mode-command)
         ("C-c c h" . consult-history)
         ("C-c c k" . consult-kmacro)
         ("C-c c M" . consult-minor-mode-menu)
         ("C-c c m" . consult-man)
         ("C-c c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)   ; orig. repeat-complex-command
         ("C-x b" . consult-buffer)              ; orig. switch-to-buffer
         ("C-x p b" . consult-project-buffer)    ; orig. project-switch-to-buffer
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
         ("M-s c" . consult-locate)
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
   consult-recent-file consult-xref consult-source-bookmark
   consult-source-file-register consult-source-recent-file
   consult-source-project-recent-file
   :preview-key "C-c M-p")

  ;; TODO: What is this about?
  ;; Completion
  ;; (consult-customize
  ;;  consult-completion-in-region
  ;;  :completion-styles '(basic))   ; disable orderless
  ;;  ;; :require-match t)

  ;; Narrowing key
  (setq consult-narrow-key "<")

  ;; Exclude some dirs from consult-git-grep
  (advice-add 'consult--git-grep-make-builder :around
              #'aj8/consult-git-grep))

;; consult-eglot (query workspace symbol from eglot using consult)
(use-package consult-eglot
  :after (consult eglot))

;; consult-project-extra (consult integration for project.el)
(use-package consult-project-extra
  :bind ("C-c p" . consult-project-extra-find)
  :init
  :config
  ;; Add sources to open Dired and Magit for a project
  ;;   These sources are hidden. After selecting a project from "Known
  ;;   Project", press 'd' to open the project root in Dired or 'm' to open
  ;;   Magit status for that project.
  (add-to-list 'consult-project-extra-sources
               '(:name "Project Dir"
                 :narrow (100 . "Dir")
                 :hidden t
                 :category project
                 :face consult-project-extra-projects
                 :history consult-project-extra--project-history
                 :action (lambda (root) (dired root))
                 :items (lambda () (when-let ((root (consult--project-root)))
                                     (list root)))) t)
  (add-to-list 'consult-project-extra-sources
               '(:name "Project Magit"
                 :narrow (109 . "Magit")
                 :hidden t
                 :category project
                 :face consult-project-extra-projects
                 :history consult-project-extra--project-history
                 :action (lambda (root) (magit-status root))
                 :items (lambda () (when-let ((root (consult--project-root)))
                                     (list root)))) t))

;; corfu (Completion Overlay Region FUnction)
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
  ;; (corfu-auto-prefix 3)          ; min. length of prefix for auto completion.
  ;; (corfu-separator ?\s)          ; orderless field separator
  ;; (corfu-quit-at-boundary t)     ; automatically quit at word boundary
  (corfu-quit-no-match t)        ; automatically quit if there is no match
  ;; (corfu-preview-current nil)    ; disable current candidate preview
  ;; (corfu-preselect nil)          ; disable candidate preselection
  ;; (corfu-on-exact-match 'insert) ; configure handling of exact matches
  ;;   TODO: match not inserted with orderless
  (corfu-scroll-margin 1)        ; use scroll margin
  ;; (global-corfu-minibuffer nil)  ; disable Corfu in the minibuffer
  :init
  ;; Enable Corfu globally
  ;;   (this is useful since dabbrev can be used in all buffers)
  (global-corfu-mode))
  ;; :config
  ;; TODO: Reasearch and enable/remove?
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
  ;;   called. Note that a capf returning an empty completion list (rather
  ;;   than nil) also blocks fallback to subsequent capfs (e.g.,
  ;;   eglot-completion-at-point behaves this way). In such cases, you can
  ;;   add the global values in mode-hooks. E.g.
  ;;
  ;;     (defun my/register-default-capfs ()
  ;;       "Add capf to current mode. This is a workaround for the fact
  ;;     that some modes fill the buffer-local capfs with exclusive
  ;;     completion functions, so that the global capfs don't get called."
  ;;       (add-hook 'completion-at-point-functions #'cape-dabbrev)
  ;;       (add-hook 'completion-at-point-functions #'cape-file)
  ;;       (add-hook 'completion-at-point-functions #'cape-line))
  ;;
  ;;     (add-hook 'emacs-lisp-mode-hook #'my/register-default-capfs)
  ;;
  ;;   The elisp-completion-at-point function is exclusive. To make it
  ;;   non-exclusive you can wrap it with cape-capf-properties:
  ;;
  ;;     (cape-capf-properties #'elisp-completion-at-point :exclusive 'no)
  ;;
  ;;   or more recently, use cape-capf-nonexclusive or
  ;;   cape-wrap-nonexclusive.

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
  ;;   Note: LSP (eglot) handles language-specific completions including
  ;;   keywords, so cape-keyword is not needed. cape-elisp-symbol is also
  ;;   not needed since elisp-mode already has elisp-completion-at-point.
  (defun aj8/prog-mode-capf ()
    "Add dabbrev and dict completions for programming modes.
Inserts cape-dabbrev+dict before `t' in buffer-local capf list."
    (setq-local completion-at-point-functions
                (append (remove t (buffer-local-value
                                   'completion-at-point-functions
                                   (current-buffer)))
                        (list #'cape-dabbrev+dict t))))
  (add-hook 'prog-mode-hook #'aj8/prog-mode-capf)
  ;; (add-hook 'emacs-lisp-mode-hook #'aj8/prog-mode-capf)

  ;; Completion at point function for text-mode
  (defun aj8/text-mode-capf ()
    "Add dabbrev and dict completions for text modes.
Inserts cape-dabbrev+dict before `t' in buffer-local capf list."
    (setq-local completion-at-point-functions
                (append (remove t (buffer-local-value
                                   'completion-at-point-functions
                                   (current-buffer)))
                        (list #'cape-dabbrev+dict t))))
  (add-hook 'text-mode-hook #'aj8/text-mode-capf)

  ;; Completion at point function for LLM chat modes
  (defun aj8/llm-chat-mode-capf ()
    "Add dabbrev and dict completions for LLM chat modes.
Inserts cape-dabbrev+dict before `t' in buffer-local capf list."
    (setq-local completion-at-point-functions
                (append (remove t (buffer-local-value
                                   'completion-at-point-functions
                                   (current-buffer)))
                        (list #'cape-dabbrev+dict t))))
  (add-hook 'agent-shell-mode-hook #'aj8/llm-chat-mode-capf)
  (add-hook 'gptel-mode-hook #'aj8/llm-chat-mode-capf)
  (add-hook 'claude-code-start-hook #'aj8/llm-chat-mode-capf)

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
  ;;   Use prefix matching globally for predictable in-buffer completion
  (orderless-matching-styles '(orderless-literal-prefix))
  ;; Style dispatchers
  (orderless-style-dispatchers
   '(aj8/orderless-dispatch-flex-if-twiddle
     aj8/orderless-dispatch-literal-if-equal
     aj8/orderless-dispatch-prefixes-if-less
     aj8/orderless-dispatch-regexp-if-star
     aj8/orderless-dispatch-without-if-bang))
  :config
  ;; Restore powerfull matching (including regexp) for minibuffer completion
  (defun aj8/minibuffer-enable-orderless-regexp ()
    (setq-local orderless-matching-styles '(orderless-literal orderless-regexp)))
  (add-hook 'minibuffer-setup-hook #'aj8/minibuffer-enable-orderless-regexp))

;; vertico (VERTical Interactive COmpletion)
;;   MAYBE: Set up vertico-grid-mode (with vertico-multiform-mode)
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
         ("M-P" . #'vertico-repeat-previous)
         ("M-N" . #'vertico-repeat-next)
         ("M-/" . aj8/vertico-sort-toggle))
  :custom
  ;; Reduce the popularity bonus (prioritizes recent commands)
  (vertico-sort-history-duplicate 1.0)   ; default is 10
  ;; Decay the popularity bonus faster (prioritizes recent commands)
  (vertico-sort-history-decay 1.0)   ; default is 10
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

;; minibuffer-side-window-mode (side-window display for minibuffer packages)
(use-package minibuffer-side-window-mode
  :after (:any vertico embark which-key)
  :config
  (minibuffer-side-window-mode 1))

;;; Editing

;; combobulate (structured editing and navigation in Emacs with Tree-Sitter)
;;   Note: Missing functionality: barf, slurp, forward/backward sexp (navigate
;;   by parens), ...
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
              ("C-M-<left>" . combobulate-navigate-previous)   ; backward-sexp
              ("C-M-<right>" . combobulate-navigate-next)   ; forward-sexp
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
  (combobulate-key-prefix "C-c o")
  ;; Disable display of syntax tree
  (combobulate-flash-node nil))

;; move-dup (Eclipse-like moving and duplicating lines or rectangles)
(use-package move-dup
  :bind (("C-c <up>" . move-dup-move-lines-up)
         ("C-c <down>" . move-dup-move-lines-down)
         ("C-c C-<up>" . move-dup-duplicate-up)
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

;; unfill (do the opposite of fill-paragraph or fill-region)
(use-package unfill
  :bind (("M-S-q" . 'unfill-paragraph)
         ("C-M-S-q" . 'unfill-region)))

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
  :disabled
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
  :disabled
  :commands vundo
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

;;; Org

;;; Outline

;; hideshow-cycle (cycle code folding with hideshow)
(use-package hideshow-cycle
  :bind (:map hs-minor-mode-map
              ("TAB" . hs-cycle)
              ("<backtab>" . hs-cycle-global))
  :custom
  (hs-cycle-max-depth nil))

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

;; jinx (enchanted spell checker)
(eval `(use-package jinx
         ;; :after vertico
         :ensure-system-package ,(aj8/system-package-name 'enchant)
         :ensure-system-package pkgconf
         :diminish
         :hook (emacs-startup . global-jinx-mode)
         :bind (:map jinx-mode-map
                     ("M-$" . jinx-correct-all)
                     ("C-M-$" . jinx-languages)
                     ("C-," . aj8/jinx-correct-backward)
                     ("C-." . aj8/jinx-correct-forward)
                     ("C-c ," . jinx-previous)
                     ("C-c ." . jinx-next)
                :map jinx-correct-map
                     ("C-," . jinx-previous)
                     ("C-." . jinx-next))
         :custom
         (jinx-languages "en_US")))
         ;; :config
         ;; Multi-column display with Vertico
         ;; (require 'vertico-multiform)
         ;; (when (featurep 'vertico)
         ;;   (add-to-list 'vertico-multiform-categories
         ;;                '(jinx grid (vertico-grid-separator . "  ")))
         ;;   (vertico-multiform-mode 1))))

;;; Terminal

;; eat (Emulate A Terminal, in a region, in a buffer and in Eshell)
(use-package eat
  :config
  (add-hook 'eat-mode-hook
            (lambda ()
              (keymap-set eat-mode-map "M-<prior>" #'scroll-down-command)
                                        ; overwrites scroll-other-window-down
              (keymap-set eat-mode-map "M-<next>" #'scroll-up-command))))
                                        ; overwrites scroll-other-window

;; vterm (fully-featured terminal emulator)
(eval `(use-package vterm
         ;; :ensure-system-package (cmake libvterm-dev)
         :ensure-system-package (cmake ,(aj8/system-package-name 'libvterm))
         :commands vterm
         :config
         ;; Match the default Bash shell prompt
         (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")))

;;; Theme

;; circadian (theme-switching based on daytime)
(use-package circadian
  ;; :disabled  ; 0.3s startup time
  :defer 60
  :after (:any modus-themes ef-themes standard-themes)
  :config
  ;; (cond ((eq aj8/my-os 'macos)   ; macOS
  ;;        (setq circadian-themes '((:sunrise . ef-duo-light)
  ;;                                 (:sunset  . ef-duo-dark))))
  ;;       ((eq aj8/my-os 'wsl)     ; WSL
  ;;        (setq circadian-themes '((:sunrise . standard-light)
  ;;                                 (:sunset  . standard-dark))))
  ;;       ((eq aj8/my-os 'linux)   ; Linux
  ;;        (setq circadian-themes '((:sunrise . modus-operandi)
  ;;                                 (:sunset  . modus-vivendi))))
  ;;       (t (user-error "Unexpected system-name: %s" (system-name))))
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

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
  :disabled
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
  :disabled
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

;;; Version control

;; diff-hl (highlight uncommitted changes using VC)
(use-package diff-hl
  ;; :disabled
  :config
  ;; Integration with Magit
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  ;; Put highlights in the margin in terminal
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  ;; Use diff-hl-mode everywhere
  (global-diff-hl-mode 1))

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
  ;; Hide log margin
  ;; (setf (nth 0 magit-log-margin) nil)
  ;; Use abbreviated ages in log margin
  (setf (nth 1 magit-log-margin) 'age-abbreviated)
  ;; Hide log margin author
  ;; (setf (nth 3 magit-log-margin) nil)
  ;; Reduce log margin author width
  (setf (nth 4 magit-log-margin) 15)
  ;; Clean up process buffers automatically
  (setq aj8/magit-cleanup-buffers t)
  ;; Hook for buffer cleanup
  (add-hook 'magit-mode-hook #'aj8/magit-start-buffer-cleanup-timer))

;; magit-gptcommit (Git commit with help of GPT)
(use-package magit-gptcommit
  ;; :disabled
  :demand t
  :after magit
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept))
  :init
  (require 'llm-gemini)
  (require 'llm-openai)
  ;; Add extended context to help LLM identify locations
  (advice-add 'magit-gptcommit--staged-diff
              :override #'aj8/magit-gptcommit-extended-context)
  ;; Use full file contents (more tokens but complete context)
  ;; (advice-add 'magit-gptcommit--staged-diff
  ;;             :filter-return #'aj8/magit-gptcommit-add-file-context)
  :custom
  ;; Supress warning
  (llm-warn-on-nonfree nil)
  (magit-gptcommit-prompt "INSTRUCTIONS:

You are an expert at writing Git commits. Your job is to summarize all
file changes in a SINGLE commit message.

The commit message should be structured as follows:

{<filename> | <filename>, <filename> | <filename>, ...}: <description>

[<body>]

RULES:

Summary line:
  - MUST describe the most important change with concrete specifics
    about what was added, removed, fixed, or changed - avoid vague
    descriptions that could apply to any change
  - MUST be specific enough to understand the change without reading the
    body
  - MUST be prefixed with filename(s) based on how many files are changed:
    - If one file is changed: use \"filename\"
    - If exactly two files are changed: when space permits, use \"filename, filename\"; otherwise use \"filename, ...\"
    - If three or more files are changed: use \"filename, ...\"
  - Use basename only (not full path)
  - Capitalize the first word in the <description>
  - Do NOT refer to the filename(s) in the <description>
  - Include location context in the <description> when space permits
    (e.g., section, function name, class, or module)
  - Use imperative mood
  - MUST be less than 80 characters wide in total

Body:
  - Optional - only add when it provides significant additional
    information beyond what is stated in the summary line
  - Omit entirely if the summary line is sufficient
  - If included, do NOT repeat information from the summary line
  - Keep it short and concise
  - Avoid generic justifications like \"improves clarity\" or \"ensures
    maintainability\" - focus on specific information about what changed or why
  - Use third-person present (indicative) (e.g., \"Adds feature X\")
  - Wrap all body lines at 72 characters

FILE DIFFS:

\n%s\n")
  :config
  ;; Set up LLM provider: OpenAI
  ;;   Use lazy setup to avoid password prompt during startup
  ;; (setq magit-gptcommit-llm-provider
  ;;       (make-llm-openai :key (auth-info-password
  ;;                              (car (auth-source-search
  ;;                                    :host "api.openai.com"
  ;;                                    :user "apikey")))
  ;;                        :chat-model "gpt-4.1"))
  ;; Set up LLM provider: Gemini
  (setq magit-gptcommit-llm-provider
        (make-llm-gemini :key (auth-info-password
                               (car (auth-source-search
                                     :host "generativelanguage.googleapis.com"
                                     :user "apikey")))
                         :chat-model "gemini-flash-latest"))
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
;;   MAYBE: Add MAYBE
(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode)
  ;; :custom
  ;; ;; Don't require a colon after keyword
  ;; (magit-todos-keyword-suffix ""))
  :config
  ;; Exclude some dirs from search
  (add-to-list 'magit-todos-exclude-globs "**/archive/*"))

;; ztree (text mode directory tree)
;;   MAYBE: Alternative packages: diffed, ediff-directories (built-in)
(use-package ztree
  :defer
  :config
  ;; Use pretty Unicode art
  (setq ztree-draw-unicode-lines t))
  ;; Customize file filter (default is all dot-files)
  ;; (setq-default ztree-diff-filter-list
  ;;               (cons \"^.*\\.pyc\" ztree-diff-filter-list)))

;;; Windows

;; auto-width-mode (automatically resize the width of focused windows)
(use-package auto-width-mode
  :config
  (auto-width-mode 1))

;;; Web

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
