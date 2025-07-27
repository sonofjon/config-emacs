;;; aj8-gptel.el -*- lexical-binding: t; -*-

;; Tool definitions

(defun aj8/gptel-tool-list-buffers ()
  "List the names of all currently open buffers that are associated with a file."
  (with-temp-message "Running tool: aj8_list_buffers"
    (let ((file-buffers '()))
      (dolist (buffer (buffer-list))
        (when (buffer-file-name buffer)
          (push (buffer-name buffer) file-buffers)))
      (nreverse file-buffers))))

(defun aj8/gptel-tool-list-all-buffers ()
  "List the names of all currently open buffers."
  (with-temp-message "Running tool: aj8_list_all_buffers"
    (mapcar #'buffer-name (buffer-list))))

(defun aj8/gptel-tool-buffer-to-file (buffer-name)
  "Return the file path for a given buffer."
  (with-temp-message "Running tool: aj8_buffer_to_file"
    (let ((buffer (get-buffer buffer-name)))
      (unless (and buffer (buffer-file-name buffer))
        (error "Error: Buffer '%s' not found or not associated with a file." buffer-name))
      (buffer-file-name buffer))))

(defun aj8/gptel-tool-file-to-buffer (file-path)
  "Return the buffer name for a given file path."
  (with-temp-message "Running tool: aj8_file_to_buffer"
    (let ((buffer (find-buffer-visiting file-path)))
      (unless buffer
        (error "Error: No buffer is visiting the file '%s'." file-path))
      (buffer-name buffer))))

(defun aj8/gptel-tool-append-to-buffer (buffer text)
  "Append text to a buffer."
  (with-temp-message "Running tool: aj8_append_to_buffer"
    (let ((buf (get-buffer buffer)))
      (unless buf
        (error "Error: Buffer '%s' not found." buffer))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert text))
      (format "Text successfully appended to buffer %s." buffer))))

(defun aj8/gptel-tool-insert-into-buffer (buffer text line-number)
  "Insert text into a buffer at a specific line number. The text is inserted at the beginning of the specified line."
  (with-temp-message "Running tool: aj8_insert_into_buffer"
    (let ((buf (get-buffer buffer)))
      (unless buf
        (error "Error: Buffer '%s' not found." buffer))
      (with-current-buffer buf
        (goto-line line-number)
        (insert text))
      (format "Text successfully inserted into buffer %s at line %d." buffer line-number))))

(defun aj8/gptel-tool-modify-buffer (buffer content)
  "Completely overwrite the contents of a buffer."
  (with-temp-message "Running tool: aj8_modify_buffer"
    (let ((buf (get-buffer buffer)))
      (unless buf
        (error "Error: Buffer '%s' not found." buffer))
      (with-current-buffer buf
        (erase-buffer)
        (insert content))
      (format "Buffer %s successfully modified." buffer))))

(defun aj8/gptel-tool-edit-buffer (buffer-name old-string new-string)
  "Edit a buffer by replacing a single instance of an exact string."
  (with-temp-message "Running tool: aj8_edit_buffer"
    (let ((buffer (get-buffer buffer-name)))
      (unless buffer
        (error "Error: Buffer '%s' not found." buffer-name))
      (with-current-buffer buffer
        (let ((count (count-matches old-string (point-min) (point-max))))
          (cond
           ((= count 0)
            (error "Error: String '%s' not found in buffer '%s'." old-string buffer-name))
           ((> count 1)
            (error "Error: String '%s' is not unique in buffer '%s'. Found %d occurrences." old-string buffer-name count))
           (t
            (goto-char (point-min))
            (replace-string old-string new-string)
            (format "String in buffer '%s' successfully replaced." buffer-name))))))))

(defun aj8/gptel-tool-apply-buffer-edits (buffer-name buffer-edits)
  "Edit a buffer with a list of edits, applying changes directly without review."
  (with-temp-message "Running tool: aj8_apply_buffer_edits"
    (let ((buffer (get-buffer buffer-name)))
      (unless buffer
        (error "Error: Buffer '%s' not found." buffer-name))
      (with-current-buffer buffer
        (dolist (edit (sort buffer-edits #'(lambda (a b) (> (plist-get a :line-number) (plist-get b :line-number)))))
          (let ((line-number (plist-get edit :line-number))
                (old-string (plist-get edit :old-string))
                (new-string (plist-get edit :new-string)))
            (goto-line line-number)
            (when (search-forward old-string (line-end-position) t)
              (replace-match new-string nil nil))))
        (format "Edits successfully applied to buffer %s." buffer-name)))))

(defun aj8/gptel-tool-apply-buffer-edits-with-review (buffer-name buffer-edits)
  "Edit a buffer with a list of edits and start an Ediff session for review."
  (with-temp-message "Running tool: aj8_apply_buffer_edits_with_review"
    (let* ((original-buffer (get-buffer buffer-name))
           (temp-buffer-name (format "*%s-edits*" buffer-name))
           (temp-buffer (get-buffer-create temp-buffer-name)))
      (unless original-buffer
        (error "Error: Buffer '%s' not found." buffer-name))

      ;; Prepare the edited version in a temporary buffer
      (with-current-buffer temp-buffer
        (erase-buffer)
        (insert-buffer-substring original-buffer)
        (aj8/gptel-tool-apply-buffer-edits temp-buffer-name buffer-edits))

      ;; Start Ediff
      (ediff-buffers original-buffer temp-buffer)
      (format "Ediff session started for %s. Please complete the review." buffer-name))))

(defun aj8/gptel-tool-read-file-section (filepath &optional start end)
  "Read a section of a file."
  (with-temp-message "Running tool: aj8_read_file_section"
    (unless (file-exists-p filepath)
      (error "Error: No such file: %s" filepath))
    (with-temp-buffer
      (insert-file-contents filepath)
      (let* ((start-pos (if start (progn (goto-line start) (point)) (point-min)))
             (end-pos (if end (progn (goto-line end) (line-end-position)) (point-max))))
        (buffer-substring-no-properties start-pos end-pos)))))

(defun aj8/gptel-tool-append-to-file (filepath text)
  "Append text to a file."
  (with-temp-message "Running tool: aj8_append_to_file"
    (let ((buffer (find-file-noselect filepath)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert text)
        (save-buffer))
      (format "Text successfully appended to %s." filepath))))

(defun aj8/gptel-tool-insert-into-file (filepath text line-number)
  "Insert text into a file at a specific line number."
  (with-temp-message "Running tool: aj8_insert_into_file"
    (let ((buffer (find-file-noselect filepath)))
      (with-current-buffer buffer
        (goto-line line-number)
        (insert text)
        (save-buffer))
      (format "Text successfully inserted into %s at line %d." filepath line-number))))

(defun aj8/gptel-tool-edit-file (filename old-string new-string)
  "Edit a file by replacing a single instance of an exact string."
  (with-temp-message "Running tool: aj8_edit_file"
    (let ((buffer (find-file-noselect filename)))
      (with-current-buffer buffer
        (aj8/gptel-tool-edit-buffer (buffer-name) old-string new-string)
        (save-buffer))
      (format "String in file '%s' successfully replaced." filename))))

(defun aj8/gptel-tool-apply-file-edits (file-path file-edits)
  "Edit a file with a list of edits, saving changes directly without review."
  (with-temp-message "Running tool: aj8_apply_file_edits"
    (let ((buffer (find-file-noselect file-path)))
      (with-current-buffer buffer
        (aj8/gptel-tool-apply-buffer-edits (buffer-name) file-edits)
        (save-buffer))
      (format "Edits successfully applied to file %s." file-path))))

(defun aj8/gptel-tool-apply-file-edits-with-review (file-path file-edits)
  "Edit a file with a list of edits and start an Ediff session for review."
  (with-temp-message "Running tool: aj8_apply_file_edits_with_review"
    (let* ((buffer (find-file-noselect file-path))
           (temp-buffer-name (format "*%s-edits*" (buffer-name buffer)))
           (temp-buffer (get-buffer-create temp-buffer-name)))
      (with-current-buffer temp-buffer
        (erase-buffer)
        (insert-buffer-substring buffer)
        (aj8/gptel-tool-apply-buffer-edits temp-buffer-name file-edits))
      (ediff-buffers buffer temp-buffer)
      (format "Ediff session started for %s. Please complete the review." file-path))))

(defun aj8/gptel-tool-read-documentation (symbol)
  "Read the documentation for a given 'symbol'."
  (with-temp-message "Running tool: aj8_read_documentation"
    (let ((doc (documentation (intern-soft symbol))))
      (or doc (format "No documentation found for symbol '%s'." symbol)))))

(defun aj8/gptel-tool-read-function (function)
  "Return the code of the definition of an Emacs Lisp function."
  (with-temp-message "Running tool: aj8_read_function"
    (let ((func-symbol (intern-soft function)))
      (unless (and func-symbol (fboundp func-symbol))
        (error "Error: Function '%s' is not defined." function))
      (let ((func-def (symbol-function func-symbol)))
        (cond
         ((subrp func-def)
          (format "Function '%s' is a built-in primitive (subr); it has no Lisp source code." function))
         ((or (byte-code-function-p func-def)
              (and (listp func-def) (eq (car func-def) 'byte-code)))
          (let* ((found-lib-pair (find-function-library func-symbol))
                 (file-name (cdr found-lib-pair)))
            (if file-name
                (let* ((source-file
                        (let ((base-name (file-name-sans-extension file-name)))
                          ;; Handle cases like ".el.gz" -> ".el"
                          (when (string-suffix-p ".el" base-name)
                            (setq base-name (file-name-sans-extension base-name)))
                          (or (locate-file (concat base-name ".el") load-path)
                              (locate-file (concat base-name ".el.gz") load-path)))))
                  (if source-file
                      (with-temp-buffer
                        (insert-file-contents source-file)
                        (goto-char (point-min))
                        (if (re-search-forward (format "^(def\\(?:un\\|macro\\) %s\\b" (regexp-quote function)) nil t)
                            (save-excursion
                              (goto-char (match-beginning 0))
                              (let ((beg (point)))
                                (forward-sexp 1)
                                (buffer-substring-no-properties beg (point))))
                          (format "Source file for '%s' found at '%s', but the function definition could not be located inside it."
                                  function source-file)))
                    (format "Function '%s' is byte-compiled, and its source code file could not be found." function)))
              (format "Library for function '%s' not found." function))))
         (t
          (prin1-to-string func-def)))))))

(defun aj8/gptel-tool-read-library (library-name)
  "Return the source code of a library or package in Emacs."
  (with-temp-message "Running tool: aj8_read_library"
    (let ((file (find-library-name library-name)))
      (unless file (error "Library '%s' not found." library-name))
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun aj8/gptel-tool-read-info-symbol (symbol-name)
  "Return the contents of the info node for SYMBOL-NAME."
  (with-temp-message "Running tool: aj8_read_info_symbol"
    (let ((info-buffer (info-lookup-symbol (intern symbol-name))))
      (unless info-buffer (error "Cannot find Info node for symbol '%s'." symbol-name))
      (with-current-buffer info-buffer
        (unwind-protect (buffer-string) (kill-buffer info-buffer))))))

(defun aj8/gptel-tool-read-info-node (nodename)
  "Return the contents of a specific NODENAME from the Emacs Lisp manual."
  (with-temp-message "Running tool: aj8_read_info_node"
    (let ((info-buffer (get-buffer-create "*info-node*")))
      (unwind-protect
          (with-current-buffer info-buffer
            (Info-goto-node (format "(emacs-lisp)%s" nodename))
            (buffer-string))
        (kill-buffer info-buffer)))))

(defun aj8/gptel-tool-project-get-root ()
  "Get the root directory of the current project."
  (with-temp-message "Running tool: aj8_project_get_root"
    (let ((project (project-current)))
      (unless project (error "Not inside a project."))
      (project-root project))))

(defun aj8/gptel-tool-project-get-open-buffers ()
  "Return a string listing all open buffers in the current project."
  (with-temp-message "Running tool: aj8_project_get_open_buffers"
    (let ((project (project-current)))
      (unless project (error "Not inside a project."))
      (let ((project-buffers (project-buffers project)))
        (mapconcat (lambda (b)
                     (format "%s: %s" (buffer-name b) (buffer-file-name b)))
                   project-buffers "\n")))))

(defun aj8/gptel-tool-project-find-files-glob (pattern)
  "In the current project, find files whose filenames match the glob PATTERN."
  (with-temp-message "Running tool: aj8_project_find_files_glob"
    (let ((proj (project-current)))
      (unless proj
        (error "No project found in the current context."))
      (let* ((root (project-root proj))
             ;; Get list of non-ignored files from project.el (absolute paths).
             (project-file-list (project-files proj))
             ;; Get list of files matching glob from filesystem (absolute paths).
             (wildcard-file-list
              (let ((default-directory root))
                ;; The 't' argument makes it return absolute paths.
                (file-expand-wildcards pattern t))))
        ;; Return the files present in both lists.
        (seq-intersection project-file-list wildcard-file-list #'string-equal)))))

(defun aj8/gptel-tool-project-search-content (regexp)
  "In the current project, recursively search for content matching the regexp."
  (with-temp-message "Running tool: aj8_project_search_content"
    (let ((project (project-current)))
      (unless project (error "Not inside a project."))
      (let ((command (cond
                      ((executable-find "rg") (list "rg" "--vimgrep" "--" regexp))
                      ((executable-find "git") (list "git" "grep" "-n" "-e" regexp))
                      (t (error "Neither 'rg' nor 'git' found for searching."))))
            (output-buffer (generate-new-buffer "*search-output*")))
        (unwind-protect
            (let ((status (apply #'call-process (car command) nil output-buffer nil (cdr command))))
              (when (not (zerop status))
                (message "Search command exited with status %d" status))
              (with-current-buffer output-buffer
                (buffer-string)))
          (kill-buffer output-buffer))))))

;; Tool Registrations

;; (gptel-make-tool
;;  :function #'aj8/gptel-tool-read-buffer
;;  :name "aj8_read_buffer"
;;  :description "Return the contents of a buffer."
;;  :args (list '(:name "buffer"
;;                       :type string
;;                       :description "The name of the buffer to read."))

(gptel-make-tool
 :function #'aj8/gptel-tool-list-buffers
 :name "aj8_list_buffers"
 :description "List the names of all currently open buffers that are associated with a file."
 :args nil
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-list-all-buffers
 :name "aj8_list_all_buffers"
 :description "List the names of all currently open buffers."
 :args nil
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-buffer-to-file
 :name "aj8_buffer_to_file"
 :description "Return the file path for a given buffer."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-file-to-buffer
 :name "aj8_file_to_buffer"
 :description "Return the buffer name for a given file path."
 :args (list '(:name "file-path"
                     :type string
                     :description "The path to the file."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-append-to-buffer
 :name "aj8_append_to_buffer"
 :description "Append text to a buffer."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the buffer to append text to.")
             '(:name "text"
                     :type string
                     :description "The text to append to the buffer."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-insert-into-buffer
 :name "aj8_insert_into_buffer"
 :description "Insert text into a buffer at a specific line number. The text is inserted at the beginning of the specified line."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the buffer to insert text into.")
             '(:name "text"
                     :type string
                     :description "The text to insert.")
             '(:name "line-number"
                     :type integer
                     :description "The 1-based line number where the text should be inserted."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-modify-buffer
 :name "aj8_modify_buffer"
 :description "Completely overwrite the contents of a buffer."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the buffer to overwrite.")
             '(:name "content"
                     :type string
                     :description "The content to write to the buffer."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-edit-buffer
 :name "aj8_edit_buffer"
 :description "Edit a buffer by replacing a single instance of an exact string."
 :args '((:name "buffer-name"
                :type string
                :description "The name of the buffer to edit.")
         (:name "old-string"
                :type string
                :description "The text to be replaced by 'new-string'.")
         (:name "new-string"
                :type string
                :description "The text to replace 'old-string' with."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-apply-buffer-edits
 :name "aj8_apply_buffer_edits"
 :description "Edit a buffer with a list of edits, applying changes directly without review. Each edit contains a 'line-number', an 'old-string' and a 'new-string'. 'new-string' should replace 'old-string' at the specified line. Edits are applied from the bottom of the buffer to the top to handle line number changes correctly. Note: The 'old-string' must be found entirely on the specified 'line-number'."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to edit.")
             '(:name "buffer-edits"
                     :type array
                     :items (:type object
                                   :properties
                                   (:line-number
                                    (:type integer :description "The 1-based line number where the edit starts.")
                                    :old-string
                                    (:type string :description "The string to be replaced by 'new-string'.")
                                    :new-string
                                    (:type string :description "The string to replace 'old-string'.")))
                     :description "The list of edits to apply to the buffer."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-apply-buffer-edits-with-review
 :name "aj8_apply_buffer_edits_with_review"
 :description "Edit a buffer with a list of edits and start an Ediff session for review. Each edit contains a 'line-number', an 'old-string' and a 'new-string'. 'new-string' should replace 'old-string' at the specified line. Edits are applied from the bottom of the buffer to the top to handle line number changes correctly.  Note: The 'old-string' must be found entirely on the specified 'line-number'.

This action requires manual user review. After calling this tool, you must stop and instruct the user to complete the review in the Ediff session and to notify you when they are finished. Do not proceed with any other tools or actions until you receive confirmation from the user."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to edit.")
             '(:name "buffer-edits"
                     :type array
                     :items (:type object
                                   :properties
                                   (:line-number
                                    (:type integer :description "The 1-based line number where the edit starts.")
                                    :old-string
                                    (:type string :description "The string to be replaced by 'new-string'.")
                                    :new-string
                                    (:type string :description "The string to replace 'old-string'.")))
                     :description "The list of edits to apply to the buffer."))
 :category "buffers")

;; (gptel-make-tool
;;  :function #'aj8/gptel-tool-create-file
;;  :name "aj8_create_file"
;;  :description "Create a new file with the specified content. Overwrites the file if it already exists."
;;  :args '((:name "filepath"
;;                 :type string
;;                 :description "The path of the file to create.")
;;          (:name "content"
;;                 :type string
;;                 :description "The content to write to the new file."))

(gptel-make-tool
 :function #'aj8/gptel-tool-read-file-section
 :name "aj8_read_file_section"
 :description "Read a section of a file. To read the entire file, omit the optional 'start' and 'end' arguments."
 :args (list '( :name "filepath"
                :type string
                :description "The name of the file to read the contents of. ")
             '( :name "start"
                :type integer
                :optional t
                :description "The optional first line to read from.")
             '( :name "end"
                :type integer
                :optional t
                :description "The optional last line to read to."))
 :category "filesystem")

(gptel-make-tool
 :function #'aj8/gptel-tool-append-to-file
 :name "aj8_append_to_file"
 :description "Append text to a file. This tool operates on the buffer visiting the file to avoid losing unsaved changes, and it saves the buffer after the edit."
 :args (list '(:name "filepath"
                     :type string
                     :description "The path of the file to append text to.")
             '(:name "text"
                     :type string
                     :description "The text to append to the file."))
 :category "filesystem")

(gptel-make-tool
 :function #'aj8/gptel-tool-insert-into-file
 :name "aj8_insert_into_file"
 :description "Insert text into a file at a specific line number. The text is inserted at the beginning of the specified line. This tool operates on the buffer visiting the file to avoid losing unsaved changes, and it saves the buffer after the edit."
 :args (list '(:name "filepath"
                     :type string
                     :description "The path of the file to insert text into.")
             '(:name "text"
                     :type string
                     :description "The text to insert.")
             '(:name "line-number"
                     :type integer
                     :description "The 1-based line number where the text should be inserted."))
 :category "filesystem")

(gptel-make-tool
 :function #'aj8/gptel-tool-edit-file
 :name "aj8_edit_file"
 :description "Edit a file by replacing a single instance of an exact string. This tool operates on the buffer visiting the file to avoid losing unsaved changes, and it saves the buffer after the edit."
 :args '((:name "filename"
                :type string
                :description "The path to the file to edit.")
         (:name "old-string"
                :type string
                :description "The text to be replaced by 'new-string'.")
         (:name "new-string"
                :type string
                :description "The text to replace 'old-string' with."))
 :category "filesystem")

(gptel-make-tool
 :function #'aj8/gptel-tool-apply-file-edits
 :name "aj8_apply_file_edits"
 :description "Edit a file with a list of edits, saving changes directly without review. This tool operates on the buffer visiting the file to avoid losing unsaved changes. Each edit contains a 'line-number', an 'old-string' and a 'new-string'. 'new-string' should replace 'old-string' at the specified line. Edits are applied from the bottom of the file to the top to handle line number changes correctly. Note: The 'old-string' must be found entirely on the specified 'line-number'."
 :args (list '(:name "file-path"
                     :type string
                     :description "The path of the file to edit.")
             '(:name "file-edits"
                     :type array
                     :items (:type object
                                   :properties
                                   (:line-number
                                    (:type integer :description "The 1-based line number where the edit starts.")
                                    :old-string
                                    (:type string :description "The string to be replaced by 'new-string'.")
                                    :new-string
                                    (:type string :description "The string to replace 'old-string'.")))
                     :description "The list of edits to apply to the file."))
 :category "filesystem")

(gptel-make-tool
 :function #'aj8/gptel-tool-apply-file-edits-with-review
 :name "aj8_apply_file_edits_with_review"
 :description "Edit a file with a list of edits and start an Ediff session for review. This tool operates on the buffer visiting the file to avoid losing unsaved changes. Each edit contains a 'line-number', an 'old-string' and a 'new-string'. 'new-string' should replace 'old-string' at the specified line. Edits are applied from the bottom of the file to the top to handle line number changes correctly.  Note: The 'old-string' must be found entirely on the specified 'line-number'.

This action requires manual user review. After calling this tool, you must stop and instruct the user to complete the review in the Ediff session and to notify you when they are finished. Do not proceed with any other tools or actions until you receive confirmation from the user."
 :args (list '(:name "file-path"
                     :type string
                     :description "The path of the file to edit.")
             '(:name "file-edits"
                     :type array
                     :items (:type object
                                   :properties
                                   (:line-number
                                    (:type integer :description "The 1-based line number where the edit starts.")
                                    :old-string
                                    (:type string :description "The string to be replaced by 'new-string'.")
                                    :new-string
                                    (:type string :description "The string to replace 'old-string'.")))
                     :description "The list of edits to apply to the file."))
 :category "filesystem")

(gptel-make-tool
 :function #'aj8/gptel-tool-read-documentation
 :name "aj8_read_documentation"
 :description "Read the documentation for a given 'symbol', which can be a function or variable"
 :args (list '(:name "symbol"
                     :type string
                     :description "The name of the function or variable whose documentation is to be read."))
 :category "emacs")

(gptel-make-tool
 :function #'aj8/gptel-tool-read-function
 :name "aj8_read_function"
 :description "Return the code of the definition of an Emacs Lisp function."
 :args (list '(:name "function"
                     :type string
                     :description "The name of the function whose code is to be returned."))
 :category "emacs")

(gptel-make-tool
 :function #'aj8/gptel-tool-read-library
 :name "aj8_read_library"
 :description "Return the source code of a library or package in emacs."
 :args (list '(:name "library-name"
                     :type string
                     :description "The library name."))
 :category "emacs")

(gptel-make-tool
 :function #'aj8/gptel-tool-read-info-symbol
 :name "aj8_read_info_symbol"
 :description "Return the contents of the info node for SYMBOL-NAME as determined by `info-lookup-symbol', specifically for Emacs Lisp symbols."
 :args (list '(:name "symbol-name"
                     :type string
                     :description "The name of the Emacs Lisp symbol to look up."))
 :category "emacs")

(gptel-make-tool
 :function #'aj8/gptel-tool-read-info-node
 :name "aj8_read_info_node"
 :description "Return the contents of a specific NODENAME from the Emacs Lisp manual."
 :args (list '(:name "nodename" :type string :description "The name of the node in the Emacs Lisp manual."))
 :category "emacs")

(gptel-make-tool
 :function #'aj8/gptel-tool-project-get-root
 :name "aj8_project_get_root"
 :description "Get the root directory of the current project."
 :args nil
 :category "project")

(gptel-make-tool
 :function #'aj8/gptel-tool-project-get-open-buffers
 :name "aj8_project_get_open_buffers"
 :description "Return a string listing all open buffers in the current project. Each line contains a buffer name followed by its associated file path."
 :args nil
 :category "project")

;;   (gptel-make-tool
;;    :function #'aj8/gptel-tool-project-find-files
;;    :name "aj8_project_find_files"
;;    :description "In the current project, recursively find files whose filenames contain pattern. This search is case-sensitive and respects .gitignore. It does not find directories."
;;    :args '((:name "pattern"
;;                   :type string
;;                   :description "A pattern to match against the filenames in the project."))
;;    :category "proj

(gptel-make-tool
 :function #'aj8/gptel-tool-project-find-files-glob
 :name "aj8_project_find_files_glob"
 :description "In the current project, find files matching the glob PATTERN. This search respects .gitignore. The pattern is a standard file glob. To search recursively, use the '**/' prefix. For example, a PATTERN of '**/*.el' finds all Emacs Lisp files in the project, while '*.el' finds them only in the root directory."
 :args '((:name "pattern"
                :type string
                :description "A glob pattern to match against the filenames in the project."))
 :category "project")

(gptel-make-tool
 :function #'aj8/gptel-tool-project-search-content
 :name "aj8_project_search_content"
 :description "In the current project, recursively search for content matching the regexp. This search respects .gitignore. It returns a string of matching lines, where each line is prefixed with the file path, line number, and column number."
 :args '((:name "regexp"
                :type string
                :description "A regexp to search for in the project files. The regexp should be compatible with ripgrep or git grep."))
 :category "project")

;; Initialize gptel-tools with all registered tools
;;   Needed primarily for the test suite
(setq gptel-tools (mapcan (lambda (entry) (mapcar #'cdr (cdr entry))) gptel--known-tools))

(provide 'aj8-gptel)
