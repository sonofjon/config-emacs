;; Load theme
;(load-theme 'dichromacy)
;(load-theme 'manoj-dark)
;(load-theme 'misterioso)
;(load-theme 'tango-dark)
(load-theme 'tsdh-dark)
;(load-theme 'wheatgrass)
;(load-theme 'wombat)

;; Use custom-file.el for custom-* code
(setq custom-file "~/.emacs.d/custom-file.el")

;; Load custom file
(load-file custom-file)

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

;; Use packages

;; auto-package-update
;:   Fails on brain5: tries to delete packages in /usr/share/emacs/site-lisp/elpa
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  ;(setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; company (in-buffer text completion)
(use-package company
  :config
  ;; Provide instant autocompletion
  ;(setq company-idle-delay 0.0)

  ;; Use company mode everywhere
  (global-company-mode t))

;; spacemacs-theme (fails to load!)
(use-package spacemacs-theme
  :config
  ;; Do not use a different background color for comments.
  (setq spacemacs-theme-comment-bg nil)

  ;; Comments should appear in italics.
  (setq spacemacs-theme-comment-italic t)

  ;; Use the `spacemacs-dark` theme.
  (load-theme 'spacemacs-dark))

;; ivy (generic completion mechanism)
(use-package ivy
  :config
  (ivy-mode 1))

;; Use Command as Meta in macOS
;(setq mac-command-modifier 'meta)

;; Highlight current line
(global-hl-line-mode t)
