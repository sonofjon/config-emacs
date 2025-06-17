;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

;; Ignore directory in project
((nil . ((eval . (add-to-list 'project-vc-ignores "archive/")))))

;; Ignore directory in magit-todos
;; (magit-status-mode . ((eval . (add-to-list 'magit-todos-exclude-globs ""**/archive/**"")))))
