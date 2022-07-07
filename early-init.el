;;; early-init.el -*- lexical-binding: t; -*-

;;; STARTUP

;; Reduce garbage collection during startup
;;   (and reset default values after)
(setq gc-cons-threshold (* 50 1000 1000)
      gc-cons-percentage 0.5)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000
                                            gc-cons-percentage 0.1)))



;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)
;; Add path to local files
;; (add-to-list 'load-path "~/local/share/emacs/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/")


  ;; Set the right directory to store the native compilation cache
  ;; NOTE the method for setting the eln-cache directory depends on the emacs version
  (when (fboundp 'startup-redirect-eln-cache)
    (if (version< emacs-version "29")
        (add-to-list 'native-comp-eln-load-path (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))
      (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))))

  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Remove some unneeded UI elements (the user can turn back on anything they wish)
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Loads a nice blue theme, avoids the white screen flash on startup.
(load-theme 'deeper-blue t)

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(customize-set-variable 'initial-major-mode 'fundamental-mode)
;; Initialize package sources
(require 'package)
(if (version< emacs-version "27")
    (package-initialize))

;; Add package-archives
;;   Emacs 27.x has GNU ELPA as the default
;;   Emacs 28.x adds the non-GNU ELPA to the list by default
(when (version< emacs-version "28")
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))
(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Set package-archives priorities
(customize-set-variable 'package-archive-priorities
                        '(("melpa"  . 4)   ; prefer Melpa packages
                          ("stable" . 3)   ; otherwise, prefer "releases" from Melpa
                          ("nongnu" . 2)   ; otherwise, prefer non-GNU packages
                          ("gnu"    . 1))) ; lastly, use GNU

;; Refresh packages database (in background)
;;   TODO: is this needed
(unless package-archive-contents
  (package-refresh-contents t))

;; Refresh packages database (on first install)
;;   TODO: is this needed
(defun my/package-install-refresh-contents (&rest args)
  "Refresh package database on first install."
  (package-refresh-contents)
  (advice-remove 'package-install #'my/package-install-refresh-contents))

(advice-add 'package-install :before #'my/package-install-refresh-contents)




(defvar rational-load-custom-file t
  "When non-nil, load `custom.el' after `config.el'.

The custom file is found in the `rational-config-path'. It
contains customizations of variables and faces that are made by
the user through the Customization UI, as well as any
customizations made by packages.")

;; Load the early config file if it exists
(let ((early-config-path (expand-file-name "early-config.el" rational-config-path)))
  (when (file-exists-p early-config-path)
    (load early-config-path nil 'nomessage)))
