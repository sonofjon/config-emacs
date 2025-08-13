;;; early-init.el -*- lexical-binding: t; -*-

;;; GENERAL

;; Open up the debugger on error
;; (setq debug-on-error t)

;; Hide buffer list at startup when loading multiple files
(setq inhibit-startup-buffer-menu t)

;;; SYSTEM

;; Store current OS
(defvar aj8/my-os
     (cond
      ((string-match "Microsoft" (shell-command-to-string "cat /proc/version")) 'wsl)
      ((eq system-type 'darwin) 'macos)
      ((eq system-type 'gnu/linux) 'linux)
      (t 'unknown)))

;; Store current Linux OS
(defvar aj8/my-linux-os
     (cond
      ((string-match "fedora" (shell-command-to-string "cat /etc/os-release")) 'fedora)
      ((string-match "ubuntu" (shell-command-to-string "cat /etc/os-release")) 'ubuntu)
      (t 'unknown)))

;; Store current terminal emulator
(defvar aj8/my-terminal-emulator
     (cond
      ((getenv "KONSOLE_DBUS_SESSION") 'konsole)
      ((getenv "GNOME_TERMINAL_SCREEN") 'gnome-terminal)
      ((getenv "WT_PROFILE_ID") 'windows-terminal)
      ((getenv "TERM_PROGRAM")
       (cond
        ((string-equal (getenv "TERM_PROGRAM") "Apple_Terminal") 'apple-terminal)
        ((string-equal (getenv "TERM_PROGRAM") "iTerm.app") 'iterm2)))
      (t 'unknown)))


;;; STARTUP

;; Reduce garbage collection during startup
;;   (and reset default values after)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000
                                            gc-cons-percentage 0.1)))


;;; GUI

;; Disable menu bar
(menu-bar-mode -1)

;; Disable toolbar (graphical Emacs)
(when (display-graphic-p)
  (tool-bar-mode -1))

;; Disable scroll bar
;; (with-eval-after-load "scroll-bar"   ; avoid error on some systems
(when (display-graphic-p)
  (scroll-bar-mode -1))

;; Disable welcome buffer
;; (setq inhibit-startup-screen t)

;; Display *Messages* buffer at startup
;; (setq initial-buffer-choice (lambda () (get-buffer "*Messages*")))

;; Set initial major mode to fundamental-mode
;;   (makes the initial buffer load faster)
;; (customize-set-variable 'initial-major-mode 'fundamental-mode)


;;; PACKAGES

;; Add path to local files
;; (add-to-list 'load-path "~/local/share/emacs/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Prevent stale elisp bytecode from shadowing more up-to-date source files
(setq load-prefer-newer t)

;; Initialize package sources
(require 'package)

;; Natively compile packages during installation
(setq package-native-compile t)

;; Add package-archives
;;   Emacs 28.x has GNU `and' non-GNU ELPA as default
(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Set package-archives priorities
(customize-set-variable 'package-archive-priorities
                        '(("melpa"  . 4)   ; prefer Melpa packages
                          ("stable" . 3)   ; otherwise, prefer "releases" from Melpa
                          ("nongnu" . 2)   ; otherwise, prefer non-GNU packages
                          ("gnu"    . 1))) ; lastly, use GNU

;; Refresh packages database (in background)
;; (unless package-archive-contents
;;   (package-refresh-contents t))

;; Refresh packages database (on first install)
(defun my/package-install-refresh-contents (&rest args)
  "Refresh package database on first install."
  (package-refresh-contents)
  (advice-remove 'package-install #'my/package-install-refresh-contents))

(advice-add 'package-install :before #'my/package-install-refresh-contents)


;;; CUSTOMIZATION

;; Use custom-file.el for custom-* code
(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))

(if (file-exists-p custom-file)
    (load-file custom-file))
