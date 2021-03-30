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

;; Highlight current line
(global-hl-line-mode t)
