;;
;; INITIALIZATION
;;

;; Use custom-file.el for custom-* code
(setq custom-file "~/.emacs.d/custom-file.el")

;; Load custom file
(load-file custom-file)


;;
;; PACKAGES SETUP
;;

;; Initialize package sources
(require 'package)
(package-initialize)

;; Add melpa to package-archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Add use-package macro
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Is this needed?
(require 'use-package)

;; Always install packages if not present
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;;
;; LOAD PACKAGES
;;

;; auto-package-update
;:   Fails on brain5: tries to delete packages in /usr/share/emacs/site-lisp/elpa
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  ;(setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; spacemacs-theme
;; (use-package spacemacs-theme
;;   :defer t   ;; Fix loading warning
;;   :config
;;   ;; Do not use a different background color for comments
;;   ;(setq spacemacs-theme-comment-bg nil)

;;   ;; Comments should appear in italics.
;;   ;(setq spacemacs-theme-comment-italic t)

;;   ;; Use the spacemacs-dark theme
;;   ;(load-theme 'spacemacs-dark t))

;;   ;; Fix loading warning
;;   :init (load-theme 'spacemacs-dark t))

;; doom-themes
(use-package doom-themes
;  :init (load-theme 'doom-one) t)
;  :init (load-theme 'doom-vibrant) t)
;  :init (load-theme 'doom-snazzy) t)
  :init (load-theme 'doom-Iosvkem) t)

;; company (in-buffer text completion)
(use-package company
  :config
  ;; Provide instant autocompletion (default is 0.5 s)
  ;(setq company-idle-delay 0.0)

  ;; Use company mode everywhere
  (global-company-mode t))

;; ivy (generic completion mechanism)
(use-package ivy
  :config (ivy-mode 1))

;; ivy-rich (add descriptions to ivy/counsel output)
(use-package ivy-rich
  :after ivy
  :config (ivy-rich-mode 1))

;; councel (ivy-enhanced versions of common Emacs commands)
(use-package counsel
  :after ivy
  :config (counsel-mode))

;; swiper (ivy-enhanced isearch)
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; rainbow-delimiters (parentheses coloring)
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
	 (emacs-lisp-mode . rainbow-delimiters-mode)))

;; which-key (displays available keybindings)
(use-package which-key
  :init (which-key-mode))
  ;:config
  ;; Delay (default is 1.0 s)
  ;(setq which-key-idle-delay 0.5))


;;
;; THEME
;;

;; Load theme
;(load-theme 'dichromacy)
;(load-theme 'manoj-dark)
;(load-theme 'misterioso)
;(load-theme 'tango-dark)
;(load-theme 'tsdh-dark)
;(load-theme 'wheatgrass)
;(load-theme 'wombat)


;;
;; CUSTOMIZATION
;;

;; Use Command as Meta in macOS
;(setq mac-command-modifier 'meta)

;; Hide buffer list at startup when loading multiple files
(setq inhibit-startup-buffer-menu t)

;; Highlight current line
(global-hl-line-mode t)

;; Enable line numbers
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		shell-mode-hook
                eshell-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;
;; MODES
;;

;; Use side-by-side view by default with ediff 
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-horizontally)
