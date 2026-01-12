;;; aj8-keys.el --- My custom keys -*- lexical-binding: t; -*-
;;
;; Author: Andreas Jonsson <ajdev8@gmail.com>
;; Maintainer: Andreas Jonsson <ajdev8@gmail.com>
;; URL: https://github.com/sonofjon/config-emacs.el
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

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
    ;; (define-key input-decode-map "\e[13;2u" (kbd "S-RET"))   ; fails in Konsole, WSL
    (define-key input-decode-map "\e[13;2u" (kbd "M-j"))   ; fails in Konsole, WSL
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

(provide 'aj8-keys)

;;; aj8-keys.el ends here
