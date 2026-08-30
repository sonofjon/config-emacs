;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

;; Ignore directory in project
((nil . ((eval . (add-to-list 'project-vc-ignores "archive/"))))

 ;; Use the real load-path for the Flymake byte-compile check, so
 ;; installed packages and lisp/ are visible when checking init.el
 (emacs-lisp-mode
  . ((eval . (setq-local elisp-flymake-byte-compile-load-path
                         (cons "./" load-path))))))

;; Ignore directory in magit-todos
;; (magit-status-mode . ((eval . (add-to-list 'magit-todos-exclude-globs ""**/archive/**"")))))
