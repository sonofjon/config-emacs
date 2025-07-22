;; Require ERT, the Emacs Lisp Regression Testing tool
(require 'ert)

;; --- Test Helper Macros ---

(defmacro with-temp-buffer-with-content (buffer-name content &rest body)
  "Create a temporary buffer BUFFER-NAME with CONTENT, execute BODY, and clean up."
  `(let ((test-buf (get-buffer-create ,buffer-name)))
     (unwind-protect
         (with-current-buffer test-buf
           (erase-buffer)
           (insert ,content)
           ,@body)
       (when (buffer-live-p test-buf)
         (kill-buffer test-buf)))))

(defmacro with-temp-file-with-content (file-var content &rest body)
  "Create a temporary file with CONTENT, bind its path to FILE-VAR, execute BODY, and clean up."
  `(let ((,file-var (make-temp-file "ert-test-file-")))
     (unwind-protect
         (progn
           (with-temp-buffer
             (insert ,content)
             (write-file ,file-var))
           ,@body)
       (let ((buffer (get-file-buffer ,file-var)))
         (when buffer
           (kill-buffer buffer)))
       (when (file-exists-p ,file-var)
         (delete-file ,file-var)))))

(defmacro with-temp-project (&rest body)
  "Create a temporary dummy project, execute BODY within it, and clean up."
  `(let* ((proj-dir (expand-file-name "ert-test-project/" temporary-file-directory))
          (default-directory proj-dir))
     (unwind-protect
         (progn
           (make-directory proj-dir t)
           ;; Initialize a git repo to make it a project
           (call-process "git" nil nil nil "init")
           ;; Create dummy files
           (make-directory (expand-file-name "src" proj-dir))
           (with-temp-buffer
             (insert "Project Readme")
             (write-file (expand-file-name "readme.md" proj-dir)))
           (with-temp-buffer
             (insert "(defun hello () (message \"hello\"))")
             (write-file (expand-file-name "src/code.el" proj-dir)))
           (with-temp-buffer
             (insert "some text data")
             (write-file (expand-file-name "data.txt" proj-dir)))
           ;; Add and commit files to make them part of the project
           (call-process "git" nil nil nil "add" ".")
           (call-process "git" nil nil nil
                         "-c" "user.name=Test"
                         "-c" "user.email=test@example.com"
                         "commit" "-m" "Initial commit")
           ,@body)
       (when (file-directory-p proj-dir)
         (delete-directory proj-dir t)))))

;; --- ERT Test Definitions (deftest) ---

;;
;; Category: Buffers
;;

(ert-deftest test-aj8-list-buffers ()
  "Test `aj8/gptel-tool-list-buffers`."
  :tags '(test buffers)
  (with-temp-file-with-content tmp-file "file content"
    (find-file-noselect tmp-file)
    (with-temp-buffer-with-content "*non-file-buffer*" "some content"
      (let ((buffers (aj8/gptel-tool-list-buffers)))
        (should (member (file-name-nondirectory tmp-file) buffers))
        (should-not (member "*non-file-buffer*" buffers))))))

(ert-deftest test-aj8-buffer-and-file-conversion ()
  "Test `aj8/gptel-tool-buffer-to-file` and `aj8/gptel-tool-file-to-buffer`."
  :tags '(test buffers)
  (with-temp-file-with-content test-file "content"
    (let ((buffer (find-file-noselect test-file)))
      (unwind-protect
          (progn
            (should (string-equal (aj8/gptel-tool-buffer-to-file (buffer-name buffer))
                                  (expand-file-name test-file)))
            (should (string-equal (aj8/gptel-tool-file-to-buffer test-file)
                                  (buffer-name buffer)))
            (should-error (aj8/gptel-tool-buffer-to-file "*scratch*") :type 'error)
            (should-error (aj8/gptel-tool-file-to-buffer "/non/existent/file.tmp") :type 'error))
        (kill-buffer buffer)))))

(ert-deftest test-aj8-buffer-modification-tools ()
  "Test `aj8_append_to_buffer`, `aj8_insert_into_buffer`, and `aj8_modify_buffer`."
  :tags '(test buffers)
  (with-temp-buffer-with-content "*test-modify*" "Line 1\nLine 3"
    ;; Append
    (aj8/gptel-tool-append-to-buffer "*test-modify*" "\nLine 4")
    (should (string-equal (buffer-string) "Line 1\nLine 3\nLine 4"))
    ;; Insert
    (aj8/gptel-tool-insert-into-buffer "*test-modify*" "Line 2\n" 2)
    (should (string-equal (buffer-string) "Line 1\nLine 2\nLine 3\nLine 4"))
    ;; Modify
    (aj8/gptel-tool-modify-buffer "*test-modify*" "New Content")
    (should (string-equal (buffer-string) "New Content"))))

(ert-deftest test-aj8-edit-buffer ()
  "Test `aj8/gptel-tool-edit-buffer`."
  :tags '(test buffers)
  (with-temp-buffer-with-content "*test-edit*" "hello world\nhello universe"
    (aj8/gptel-tool-edit-buffer "*test-edit*" "world" "emacs")
    (should (string-equal (buffer-string) "hello emacs\nhello universe"))
    (should-error (aj8/gptel-tool-edit-buffer "*test-edit*" "non-existent" "foo") :type 'error)
    (should-error (aj8/gptel-tool-edit-buffer "*test-edit*" "hello" "hi") :type 'error)))

(ert-deftest test-aj8-apply-buffer-edits ()
  "Test `aj8/gptel-tool-apply-buffer-edits`."
  :tags '(test buffers)
  (with-temp-buffer-with-content "*test-apply-edits*" "Line one.\nLine two.\nLine three."
    (let ((edits '((:line-number 3 :old-string "three" :new-string "THREE")
                   (:line-number 1 :old-string "one" :new-string "ONE"))))
      (aj8/gptel-tool-apply-buffer-edits "*test-apply-edits*" edits)
      (should (string-equal (buffer-string) "Line ONE.\nLine two.\nLine THREE.")))))

(ert-deftest test-aj8-apply-buffer-edits-with-review ()
  "Test `aj8/gptel-tool-apply-buffer-edits-with-review`."
  :tags '(test buffers review)
  (with-temp-buffer-with-content "*test-review*" "Line one.\nLine two."
    (let ((edits '((:line-number 1 :old-string "one" :new-string "ONE")))
          (ediff-called nil))
      ;; Temporarily advise `ediff-buffers` to check if it's called,
      ;; without actually starting the interactive session.
      (cl-letf (((symbol-function 'ediff-buffers) (lambda (b1 b2) (setq ediff-called t))))
        (aj8/gptel-tool-apply-buffer-edits-with-review "*test-review*" edits))
      (should ediff-called)
      ;; Check that the original buffer is unchanged.
      (with-current-buffer "*test-review*"
        (should (string-equal (buffer-string) "Line one.\nLine two."))))))

;;
;; Category: Filesystem
;;

(ert-deftest test-aj8-read-file-section ()
  "Test `aj8/gptel-tool-read-file-section`."
  :tags '(test filesystem)
  (with-temp-file-with-content test-file "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"
    ;; Read whole file
    (should (string-equal (aj8/gptel-tool-read-file-section test-file)
                          "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"))
    ;; Read a section
    (should (string-equal (aj8/gptel-tool-read-file-section test-file 2 4)
                          "Line 2\nLine 3\nLine 4"))
    ;; Read from start
    (should (string-equal (aj8/gptel-tool-read-file-section test-file nil 2)
                          "Line 1\nLine 2"))
    ;; Read to end
    (should (string-equal (aj8/gptel-tool-read-file-section test-file 4)
                          "Line 4\nLine 5"))))

(ert-deftest test-aj8-file-modification ()
  "Test `aj8_append_to_file`, `aj8_insert_into_file`, `aj8_edit_file`."
  :tags '(test filesystem)
  (with-temp-file-with-content test-file "Line 1\nLine 3"
    ;; Append
    (aj8/gptel-tool-append-to-file test-file "\nLine 4")
    (should (string-equal (with-temp-buffer (insert-file-contents test-file) (buffer-string))
                          "Line 1\nLine 3\nLine 4"))
    ;; Insert
    (aj8/gptel-tool-insert-into-file test-file "Line 2\n" 2)
    (should (string-equal (with-temp-buffer (insert-file-contents test-file) (buffer-string))
                          "Line 1\nLine 2\nLine 3\nLine 4"))
    ;; Edit
    (aj8/gptel-tool-edit-file test-file "Line 4" "Line FOUR")
    (should (string-equal (with-temp-buffer (insert-file-contents test-file) (buffer-string))
                          "Line 1\nLine 2\nLine 3\nLine FOUR"))))

(ert-deftest test-aj8-apply-file-edits ()
  "Test `aj8/gptel-tool-apply-file-edits`."
  :tags '(test filesystem)
  (with-temp-file-with-content test-file "Line one.\nLine two.\nLine three."
    (let ((edits '((:line-number 3 :old-string "three" :new-string "THREE")
                   (:line-number 1 :old-string "one" :new-string "ONE"))))
      (aj8/gptel-tool-apply-file-edits test-file edits)
      (should (string-equal (with-temp-buffer (insert-file-contents test-file) (buffer-string))
                            "Line ONE.\nLine two.\nLine THREE.")))))

(ert-deftest test-aj8-apply-file-edits-with-review ()
  "Test `aj8/gptel-tool-apply-file-edits-with-review`."
  :tags '(test filesystem review)
  (with-temp-file-with-content test-file "Line one.\nLine two."
    (let ((edits '((:line-number 1 :old-string "one" :new-string "ONE")))
          (ediff-called nil))
      (cl-letf (((symbol-function 'ediff-buffers) (lambda (b1 b2) (setq ediff-called t))))
        (aj8/gptel-tool-apply-file-edits-with-review test-file edits))
      (should ediff-called)
      ;; Check that original file is unchanged.
      (should (string-equal (with-temp-buffer (insert-file-contents test-file) (buffer-string))
                            "Line one.\nLine two.")))))

;;
;; Category: Emacs
;;

(ert-deftest test-aj8-read-documentation ()
  "Test `aj8/gptel-tool-read-documentation`."
  :tags '(test emacs)
  (should (string-match-p "Return the car of LIST" (aj8/gptel-tool-read-documentation "car")))
  (should (string-match-p "List of directories to search for files to load" (aj8/gptel-tool-read-documentation "load-path")))
  (should (string-match-p "No documentation found" (aj8/gptel-tool-read-documentation "non-existent-symbol-xyz"))))

(ert-deftest test-aj8-read-function-and-library ()
  "Test `aj8_read_function` and `aj8_read_library` tools."
  :tags '(test emacs)
  (unwind-protect
      (progn
        ;; Test read function
        (should (string-match-p "(defun project-current" (aj8/gptel-tool-read-function 'project-current)))
        ;; Test read library
        (should (string-match-p "project.el" (aj8/gptel-tool-read-library "project"))))
    (when (get-buffer "project.el")
      (kill-buffer "project.el"))
    (when (get-buffer "project.el.gz")
      (kill-buffer "project.el.gz"))))


(ert-deftest test-aj8-info-lookup ()
  "Test `aj8_read_info_symbol` and `aj8_read_info_node` tools."
  :tags '(test emacs)
  (should (string-match-p "special form in `Lisp'" (aj8/gptel-tool-read-info-symbol "defun")))
  (should (string-match-p "A function definition has the form" (aj8/gptel-tool-read-info-node "Defining Functions"))))

;;
;; Category: Project
;;

(ert-deftest test-aj8-project-root-and-buffers ()
  "Test `aj8_project_get_root` and `aj8_project_get_open_buffers`."
  :tags '(test project)
  (with-temp-project
    (let ((root (file-truename default-directory)))
      ;; Test get root
      (should (string-match-p (regexp-quote root) (aj8/gptel-tool-project-get-root)))
      ;; Test get open buffers
      (let ((buf (find-file-noselect (expand-file-name "src/code.el"))))
        (unwind-protect
            (let ((open-buffers (aj8/gptel-tool-project-get-open-buffers)))
              (should (string-match-p "code.el" open-buffers))
              (should (string-match-p "src/code.el" open-buffers)))
          (kill-buffer buf))))))

(ert-deftest test-aj8-project-find-and-search ()
  "Test `aj8_project_find_files_glob` and `aj8_project_search_content`."
  :tags '(test project)
  (with-temp-project
    ;; Test find files glob
    (let ((files (aj8/gptel-tool-project-find-files-glob "**/*.el")))
      (should (= 1 (length files)))
      (should (string-match-p "src/code.el" (car files))))
    (let ((files (aj8/gptel-tool-project-find-files-glob "*.txt")))
      (should (= 1 (length files)))
      (should (string-match-p "data.txt" (car files))))
    ;; Test search content
    (when (or (executable-find "rg") (and (executable-find "git") (file-directory-p ".git")))
      (let ((results (aj8/gptel-tool-project-search-content "some text data")))
        (should (string-match-p "data.txt:1:some text data" results))))))

;; --- Test Runner Function ---

(defun aj8/run-all-gptel-tool-tests (&optional selector)
  "Run all ERT tests defined for gptel tools.
The results will be displayed in a new buffer `*ert*`.
With a prefix argument, prompt for a test SELECTOR.
The selector can be a tag like 'buffers or 'project."
  (interactive "P")
  (let ((test-selector (if selector
                           (intern (completing-read "Run tests with selector: " '(test buffers filesystem emacs project review)))
                         'test)))
    (ert-run-tests-interactively `(and (tag ,test-selector) (not (tag excluded))))))
