;;; gptel-test.el --- Tests for Gptel tools

;;
;;; 1. Header & Requirements
;;

;; Require ERT, the Emacs Lisp Regression Testing tool
(require 'ert)

;; Load libraries required for these tests
(require 'gptel)
(require 'aj8-gptel)

;;
;;; 2. Test Helper Macros
;;

(defmacro with-temp-buffer-with-content (buffer-name content &rest body)
  "Execute BODY in a temporary buffer containing initial CONTENT.

The buffer is named BUFFER-NAME. This macro ensures the buffer is killed
after BODY executes, even in case of an error."
  `(let ((test-buf (get-buffer-create ,buffer-name)))
     (unwind-protect
         (with-current-buffer test-buf
           (erase-buffer)
           (insert ,content)
           ,@body)
       (when (buffer-live-p test-buf)
         (kill-buffer test-buf)))))

(defmacro with-temp-file-with-content (file-var content &rest body)
  "Execute BODY with a temporary file containing initial CONTENT.

The file's path is bound to FILE-VAR. This macro ensures both the file
and its associated buffer are deleted after BODY executes."
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
  "Execute BODY with `default-directory' set to a temporary project's root.

This macro creates a directory, initializes a Git repository, and adds
dummy files to simulate a real project. It runs the BODY forms with the
project root as the current working directory, then deletes the
directory."
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

;;
;;; 3. Unit Tests (ert-deftest)
;;

;;; 3.1. Category: Buffers

(ert-deftest test-aj8-list-buffers ()
  "Test `aj8/gptel-tool-list-buffers'.

Ensures the function lists buffers associated with files and excludes
those not associated with any file."
  :tags '(unit buffers)
  (with-temp-file-with-content tmp-file "file content"
    (find-file-noselect tmp-file)
    (with-temp-buffer-with-content "*non-file-buffer*" "some content"
      (let ((buffers (aj8/gptel-tool-list-buffers)))
        (should (member (file-name-nondirectory tmp-file) buffers))
        (should-not (member "*non-file-buffer*" buffers))))))

(ert-deftest test-aj8-buffer-and-file-conversion ()
  "Test buffer-file path conversions.

Verifies that `aj8/gptel-tool-buffer-to-file' and
`aj8/gptel-tool-file-to-buffer' correctly convert between buffer names
and file paths, and that they signal errors for invalid inputs."
  :tags '(unit buffers)
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
  "Test buffer content modification functions.

This test covers `aj8/gptel-tool-append-to-buffer',
`aj8/gptel-tool-insert-into-buffer', and `aj8/gptel-tool-modify-buffer',
ensuring they alter the buffer content as expected."
  :tags '(unit buffers)
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
  "Test `aj8/gptel-tool-edit-buffer'.

Verifies that a single, unique string can be replaced in a buffer.  It
also confirms that an error is signaled if the target string is not
found or is not unique."
  :tags '(unit buffers)
  (with-temp-buffer-with-content "*test-edit*" "hello world\nhello universe"
    (aj8/gptel-tool-edit-buffer "*test-edit*" "world" "emacs")
    (should (string-equal (buffer-string) "hello emacs\nhello universe"))
    (should-error (aj8/gptel-tool-edit-buffer "*test-edit*" "non-existent" "foo") :type 'error)
    (should-error (aj8/gptel-tool-edit-buffer "*test-edit*" "hello" "hi") :type 'error)))

(ert-deftest test-aj8-apply-buffer-edits ()
  "Test `aj8/gptel-tool-apply-buffer-edits'.

Ensures that a list of edits is applied correctly to a buffer."
  :tags '(unit buffers)
  (with-temp-buffer-with-content "*test-apply-edits*" "Line one.\nLine two.\nLine three."
    (let ((edits '((:line-number 3 :old-string "three" :new-string "THREE")
                   (:line-number 1 :old-string "one" :new-string "ONE"))))
      (aj8/gptel-tool-apply-buffer-edits "*test-apply-edits*" edits)
      (should (string-equal (buffer-string) "Line ONE.\nLine two.\nLine THREE.")))))

(ert-deftest test-aj8-apply-buffer-edits-with-review ()
  "Test `aj8/gptel-tool-apply-buffer-edits-with-review'.

Verifies that the function prepares edits and invokes the Ediff review
system, without altering the original buffer."
  :tags '(unit buffers review)
  (with-temp-buffer-with-content "*test-review*" "Line one.\nLine two."
    (let ((edits '((:line-number 1 :old-string "one" :new-string "ONE")))
          (ediff-called nil))
      ;; Temporarily advise `ediff-buffers' to check if it's called,
      ;; without actually starting the interactive session.
      (cl-letf (((symbol-function 'ediff-buffers) (lambda (b1 b2) (setq ediff-called t))))
        (aj8/gptel-tool-apply-buffer-edits-with-review "*test-review*" edits))
      (should ediff-called)
      ;; Check that the original buffer is unchanged.
      (with-current-buffer "*test-review*"
        (should (string-equal (buffer-string) "Line one.\nLine two."))))))

;;; 3.2. Category: Filesystem

(ert-deftest test-aj8-read-file-section ()
  "Test `aj8/gptel-tool-read-file-section'.

Verifies that the function can read a whole file, a section from the
middle, a section from the beginning, and a section to the end."
  :tags '(unit files)
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
  "Test file content modification functions.

This test covers `aj8/gptel-tool-append-to-file',
`aj8/gptel-tool-insert-into-file', and `aj8/gptel-tool-edit-file',
ensuring they alter file content on disk as expected."
  :tags '(unit files)
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
  "Test `aj8/gptel-tool-apply-file-edits'.

Ensures that a list of edits is applied correctly to a file on disk."
  :tags '(unit files)
  (with-temp-file-with-content test-file "Line one.\nLine two.\nLine three."
    (let ((edits '((:line-number 3 :old-string "three" :new-string "THREE")
                   (:line-number 1 :old-string "one" :new-string "ONE"))))
      (aj8/gptel-tool-apply-file-edits test-file edits)
      (should (string-equal (with-temp-buffer (insert-file-contents test-file) (buffer-string))
                            "Line ONE.\nLine two.\nLine THREE.")))))

(ert-deftest test-aj8-apply-file-edits-with-review ()
  "Test `aj8/gptel-tool-apply-file-edits-with-review'.

Verifies that the function prepares file edits and invokes the Ediff
review system, without altering the original file."
  :tags '(unit files review)
  (with-temp-file-with-content test-file "Line one.\nLine two."
    (let ((edits '((:line-number 1 :old-string "one" :new-string "ONE")))
          (ediff-called nil))
      (cl-letf (((symbol-function 'ediff-buffers) (lambda (b1 b2) (setq ediff-called t))))
        (aj8/gptel-tool-apply-file-edits-with-review test-file edits))
      (should ediff-called)
      ;; Check that original file is unchanged.
      (should (string-equal (with-temp-buffer (insert-file-contents test-file) (buffer-string))
                            "Line one.\nLine two.")))))

;;; 3.3. Category: Emacs

(ert-deftest test-aj8-read-documentation ()
  "Test `aj8/gptel-tool-read-documentation'.

Verifies that documentation can be retrieved for both functions and
variables, and that a meaningful message is returned for symbols with no
documentation."
  :tags '(unit emacs)
  (should (string-match-p "Return the car of LIST" (aj8/gptel-tool-read-documentation "car")))
  (should (string-match-p "List of directories to search for files to load" (aj8/gptel-tool-read-documentation "load-path")))
  (should (string-match-p "No documentation found" (aj8/gptel-tool-read-documentation "non-existent-symbol-xyz"))))

(ert-deftest test-aj8-read-function-and-library ()
  "Test `aj8/gptel-tool-read-function' and `aj8/gptel-tool-read-library'.

Ensures that the source code for both a specific function and an entire
library can be retrieved."
  :tags '(unit emacs)
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
  "Test Info manual lookup tools.

Verifies that `aj8/gptel-tool-read-info-symbol' and
`aj8/gptel-tool-read-info-node' can retrieve content from the Emacs Lisp
Info manual."
  :tags '(unit emacs)
  (should (string-match-p "special form in `Lisp'" (aj8/gptel-tool-read-info-symbol "defun")))
  (should (string-match-p "A function definition has the form" (aj8/gptel-tool-read-info-node "Defining Functions"))))

;;; 3.4. Category: Project

(ert-deftest test-aj8-project-root-and-buffers ()
  "Test project root and buffer listing.

Verifies `aj8/gptel-tool-project-get-root' and
`aj8/gptel-tool-project-get-open-buffers' within a temporary project."
  :tags '(unit project)
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
  "Test project file finding and content searching.

Verifies `aj8/gptel-tool-project-find-files-glob' for file searching and
`aj8/gptel-tool-project-search-content' for content searching."
  :tags '(unit project)
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

;;
;;; 4. Integration Tests (ert-deftest)
;;

;;; 4.1. Category: Gptel Tool System Integration

(ert-deftest test-gptel-tool-registration ()
  "Verify that all Gptel tools are registered in `gptel-tools'.

This test checks that a predefined list of essential tool names exists
in the `gptel-tools' alist."
  :tags '(integration tools)
  (let ((expected-tools '("aj8_list_buffers"
                         "aj8_buffer_to_file"
                         "aj8_file_to_buffer"
                         "aj8_append_to_buffer"
                         "aj8_insert_into_buffer"
                         "aj8_modify_buffer"
                         "aj8_edit_buffer"
                         "aj8_apply_buffer_edits"
                         "aj8_apply_buffer_edits_with_review"
                         "aj8_read_file_section"
                         "aj8_append_to_file"
                         "aj8_insert_into_file"
                         "aj8_edit_file"
                         "aj8_apply_file_edits"
                         "aj8_apply_file_edits_with_review"
                         "aj8_read_documentation"
                         "aj8_read_function"
                         "aj8_read_library"
                         "aj8_read_info_symbol"
                         "aj8_read_info_node"
                         "aj8_project_get_root"
                         "aj8_project_get_open_buffers"
                         "aj8_project_find_files_glob"
                         "aj8_project_search_content")))
    (dolist (tool-name expected-tools)
      (should (cl-find-if (lambda (tool) (string-equal (gptel-tool-name tool) tool-name)) gptel-tools)))))

(ert-deftest test-gptel-tool-json-schema-validation ()
  "Validate the structure of each `gptel-tool' definition.

Ensures that every registered tool definition has the required
properties, such as `:function` and `:description`, and that the
argument list `:args` is a valid list."
  :tags '(integration tools)
  (dolist (tool-def gptel-tools)
    (let ((tool-name (gptel-tool-name tool-def)))
      ;; Check that tool has required properties
      (should (gptel-tool-function tool-def))
      (should (gptel-tool-description tool-def))

      ;; Check that args is a list (can be nil)
      (should (listp (gptel-tool-args tool-def))))))

(ert-deftest test-gptel-tool-function-callable ()
  "Verify that tool functions are defined and callable.

This test checks a subset of tools that require no arguments, ensuring
their associated functions can be called without error."
  :tags '(integration tools)
  (let ((no-arg-tools '("aj8_list_buffers"
                        "aj8_project_get_root"
                        "aj8_project_get_open_buffers")))
    ;; Test tools that don't require arguments
    (dolist (tool-name no-arg-tools)
      (let* ((tool-def (cl-find-if (lambda (tool) (string-equal (gptel-tool-name tool) tool-name)) gptel-tools))
             (func (gptel-tool-function tool-def)))
        (should (functionp func))
        (should-not (condition-case nil
                        (funcall func)
                      (error t)))))))

(ert-deftest test-gptel-tool-via-json-call ()
  "Simulate calling Gptel tools via a JSON-like interface.

This test mimics how a Large Language Model (LLM) would call the tools
by invoking the tool's function with arguments directly. It verifies
both a query and a buffer modification tool."
  :tags '(integration tools json)
  (with-temp-buffer-with-content "*test-json-call*" "Hello World\nLine 2"
    ;; Test list buffers tool
    (let* ((tool-def (cl-find "aj8_list_buffers" gptel-tools :key #'gptel-tool-name :test #'string-equal))
           (func (gptel-tool-function tool-def))
           (result (funcall func)))
      (should (listp result))
      (should (member "*test-json-call*" result)))

    ;; Test edit buffer tool with JSON-like parameters
    (let* ((tool-def (cl-find "aj8_edit_buffer" gptel-tools :key #'gptel-tool-name :test #'string-equal))
           (func (gptel-tool-function tool-def))
           (result (funcall func "*test-json-call*" "World" "Gptel")))
      (should (string-match-p "successfully" result))
      (should (string-equal (buffer-string) "Hello Gptel\nLine 2")))))

(ert-deftest test-gptel-tool-error-handling ()
  "Test that Gptel tools handle common errors gracefully.

Verifies that tools produce user-friendly error messages when given
invalid arguments, such as a non-existent buffer name or an invalid file
path."
  :tags '(integration tools errors)
  ;; Test with non-existent buffer
  (let* ((tool-def (cl-find "aj8_edit_buffer" gptel-tools :key #'gptel-tool-name :test #'string-equal))
         (func (gptel-tool-function tool-def)))
    (should (condition-case err
                (funcall func "*non-existent-buffer*" "old" "new")
              (error (string-match-p "Buffer.*not found" (error-message-string err))))))

  ;; Test with invalid file path
  (let* ((tool-def (cl-find "view_buffer" gptel-tools :key #'gptel-tool-name :test #'string-equal))
         (func (gptel-tool-function tool-def)))
    (should (condition-case err
                (funcall func "/non/existent/file.txt")
              (error (string-match-p "No such file" (error-message-string err)))))))

;;; 4.2. Category: Real-world Workflow Simulation

(defun aj8/gptel-tool-test--run-with-mock-llm (tool-name args expected-pattern)
  "Simulate an LLM call to a Gptel tool and check the result.

This helper function looks up TOOL-NAME in `gptel-tools', applies ARGS
to its function, and asserts that the formatted result matches
EXPECTED-PATTERN."
  (let* ((tool-def (cl-find-if (lambda (tool) (string-equal (gptel-tool-name tool) tool-name)) gptel-tools))
         (func (gptel-tool-function tool-def))
         (result (apply func args)))
    (should (string-match-p expected-pattern (format "%s" result)))
    result))

(ert-deftest test-gptel-tools-mock-llm-interaction ()
  "Test Gptel tools by simulating calls from an LLM.

This test uses a helper function to simulate how an LLM might invoke
various tools, checking for expected patterns in their output."
  :tags '(integration tools mock)
  (with-temp-project
    (with-temp-buffer-with-content "*mock-test*" "Original content\nSecond line"
      ;; Test buffer listing
      (aj8/gptel-tool-test--run-with-mock-llm "aj8_list_buffers" nil "mock-test")

      ;; Test buffer editing
      (aj8/gptel-tool-test--run-with-mock-llm "aj8_edit_buffer"
                                        '("*mock-test*" "Original" "Modified")
                                        "successfully")
      (should (string-equal (buffer-string) "Modified content\nSecond line"))

      ;; Test project root
      (aj8/gptel-tool-test--run-with-mock-llm "aj8_project_get_root" nil "ert-test-project")

      ;; Test file search
      (aj8/gptel-tool-test--run-with-mock-llm "aj8_project_find_files_glob"
                                        '("**/*.el")
                                        "code.el"))))

(ert-deftest test-gptel-tool-workflow-simulation ()
  "Simulate a realistic workflow using multiple tools in sequence.

This test models a common task: finding a file in a project, reading its
content, making an edit, and then verifying the change. It ensures that
the tools work together to complete a complex operation."
  :tags '(integration workflow)
  (with-temp-project
    (with-temp-file-with-content test-file "def hello():\n    print('Hello World')\n\ndef goodbye():\n    print('Goodbye')"
      ;; Simulate LLM workflow:
      ;; 1. List project files
      ;; 2. Read a file
      ;; 3. Edit the file
      ;; 4. Verify changes

      ;; Step 1: Find Python files
      (let* ((find-tool (cl-find "aj8_project_find_files_glob" gptel-tools :key #'gptel-tool-name :test #'string-equal))
             (find-func (gptel-tool-function find-tool))
             (py-files (funcall find-func "**/*.py")))
        (should (> (length py-files) 0)))

      ;; Step 2: Read file content
      (let* ((read-tool (cl-find "view_buffer" gptel-tools :key #'gptel-tool-name :test #'string-equal))
             (read-func (gptel-tool-function read-tool))
             (content (funcall read-func test-file)))
        (should (string-match-p "Hello World" content)))

      ;; Step 3: Edit the file
      (let* ((edit-tool (cl-find "aj8_edit_file" gptel-tools :key #'gptel-tool-name :test #'string-equal))
             (edit-func (gptel-tool-function edit-tool))
             (result (funcall edit-func test-file "Hello World" "Hello Gptel")))
        (should (string-match-p "successfully" result)))

      ;; Step 4: Verify changes
      (let* ((read-tool (cl-find "view_buffer" gptel-tools :key #'gptel-tool-name :test #'string-equal))
             (read-func (gptel-tool-function read-tool))
             (new-content (funcall read-func test-file)))
        (should (string-match-p "Hello Gptel" new-content))
        (should-not (string-match-p "Hello World" new-content))))))

(ert-deftest test-gptel-tool-complex-edits ()
  "Test complex, multi-part editing scenarios.

Simulates an LLM performing a refactoring task that requires making
several related edits in a single buffer, using
`aj8/gptel-tool-apply-buffer-edits' to apply them all at once."
  :tags '(integration edits)
  (with-temp-buffer-with-content "*complex-edit-test*"
    "function calculateSum(a, b) {\n    return a + b;\n}\n\nfunction calculateProduct(a, b) {\n    return a * b;\n}\n\nfunction main() {\n    console.log('Starting calculations');\n    let sum = calculateSum(5, 3);\n    let product = calculateProduct(4, 6);\n    console.log('Results:', sum, product);\n}"

    (let* ((edit-tool (cl-find "aj8_apply_buffer_edits" gptel-tools :key #'gptel-tool-name :test #'string-equal))
           (edit-func (gptel-tool-function edit-tool))
           (edits '((:line-number 1 :old-string "calculateSum" :new-string "addNumbers")
                    (:line-number 5 :old-string "calculateProduct" :new-string "multiplyNumbers")
                    (:line-number 9 :old-string "calculateSum" :new-string "addNumbers")
                    (:line-number 10 :old-string "calculateProduct" :new-string "multiplyNumbers"))))

      (funcall edit-func "*complex-edit-test*" edits)

      ;; Verify all changes were applied
      (let ((content (buffer-string)))
        (should (string-match-p "function addNumbers" content))
        (should (string-match-p "function multiplyNumbers" content))
        (should (string-match-p "addNumbers(5, 3)" content))
        (should (string-match-p "multiplyNumbers(4, 6)" content))
        (should-not (string-match-p "calculateSum" content))
        (should-not (string-match-p "calculateProduct" content))))))

;;
;;; 5. Test Runner Functions (interactive)
;;

(defun aj8/gptel-tool-test-run-all ()
  "Run all ERT tests defined for Gptel tools."
  (interactive)
  (ert t))

(defun aj8/gptel-tool-test-run-unit ()
  "Run all Gptel tool unit tests."
  (interactive)
  (ert '(tag unit)))

(defun aj8/gptel-tool-test-run-integration ()
  "Run Gptel tool integration tests."
  (interactive)
  (ert '(tag integration)))

(defun aj8/gptel-tool-test-run-by-tag (tag)
  "Run all Gptel tool tests with a specified TAG."
  (interactive
   (list (completing-read "Select tag: "
                          '("unit" "buffers" "files" "emacs" "project" "review"
                            "integration" "tools" "json" "errors" "mock"
                            "workflow" "edits")
                          nil t)))
  (ert `(tag ,(intern tag))))

(defun aj8/gptel-tool-test-run-by-name ()
  "Run a single Gptel tool test selected by name."
  (interactive)
  (let* ((all-tests (ert-select-tests t t))
         (test-choices
          (mapcar (lambda (test)
                    (let* ((test-name (ert-test-name test))
                           (test-def (get test-name 'ert--test))
                           (tags (when test-def (ert-test-tags test-def)))
                           (doc (when test-def
                                  (ert-test-documentation test-def)))
                           (tag-string (if tags
                                           (format " [%s]"
                                                   (mapconcat #'symbol-name tags ", "))
                                         ""))
                           (doc-preview (if (and doc (> (length doc) 0))
                                            (let ((first-line (car (split-string doc "\n" t))))
                                              (format " - %s" first-line))
                                          "")))
                      (cons (format "%s%s%s" test-name tag-string doc-preview)
                            test-name)))
                  all-tests))
         (selected-display (completing-read "Select test to run: " test-choices nil t))
         (selected-test (cdr (assoc selected-display test-choices))))
    (if selected-test
        (ert selected-test)
      (message "No test selected"))))

;;
;;; 6. Manual Testing & Utility Functions (interactive)
;;

(defun aj8/gptel-tool-run-tool (tool-name)
  "Directly invoke a Gptel tool chosen interactively by its name.

This function prompts for a TOOL-NAME from a list of all registered
gptel tools.  If the tool requires arguments, you will be prompted to
enter a value for each one. The tool is then executed with the provided
arguments.

The return value of the tool is displayed as a message. This is useful
for quick, manual testing and inspection of any tool.

TOOL-NAME is the name of the tool to run (e.g., 'aj8_list_buffers')."
  (interactive
   (let* ((choices (mapcar (lambda (tool)
                             (format "%s [%s]"
                                     (gptel-tool-name tool)
                                     (if (gptel-tool-args tool) "args" "no args")))
                           gptel-tools))
          (selection (completing-read "Select tool: " choices nil t)))
     (list (car (split-string selection " ")))))
  (let* ((tool (cl-find-if (lambda (item) (string-equal (gptel-tool-name item) tool-name)) gptel-tools))
         (func (when tool (gptel-tool-function tool))))
    (if (and func (functionp func))
        (condition-case err
            (let* ((args-spec (gptel-tool-args tool))
                   (result (if args-spec
                               ;; If tool requires args, prompt for them
                               (let ((collected-args '()))
                                 (dolist (arg-def args-spec)
                                   (let* ((arg-name (plist-get arg-def :name))
                                          (arg-type (plist-get arg-def :type))
                                          (prompt (format "Enter value for '%s' (type: %s): " arg-name arg-type)))
                                     (push (read-from-minibuffer prompt) collected-args)))
                                 (apply func (nreverse collected-args)))
                             ;; Otherwise, just call it
                             (funcall func))))
              (message "Tool %s result: %S" tool-name result)
              result)
          (error (message "Error testing tool %s: %s"
                         tool-name (error-message-string err))))
      (message "Tool function not found for %s" tool-name))))

(defun aj8/gptel-tool-validate-definitions ()
  "Validate that all entries in `gptel-tools' are well-formed.

This function checks that each tool is a valid `gptel-tool` struct and
includes the required fields: a `:name`, a callable `:function`, and a
non-empty `:description`.

Returns a list of error messages for tools that fail validation, or nil
if all tools are valid."
  (interactive)
  (let ((errors '()))
    (dolist (tool-struct gptel-tools)
      (let ((tool-name (gptel-tool-name tool-struct)))
        (condition-case err
            (if (gptel-tool-p tool-struct)
                ;; If it is a struct, check its fields
                (let ((func (gptel-tool-function tool-struct))
                      (description (gptel-tool-description tool-struct))
                      (args (gptel-tool-args tool-struct)))

                  ;; Check required properties
                  (unless func
                    (push (format "%s: Missing function" tool-name) errors))
                  (unless description
                    (push (format "%s: Missing description" tool-name) errors))

                  ;; Check function is callable
                  (when func
                    (unless (functionp func)
                      (push (format "%s: function is not a function" tool-name) errors)))

                  ;; Check args structure if present
                  (when args
                    (unless (listp args)
                      (push (format "%s: args should be a list" tool-name) errors))))
              ;; Not a struct
              (push (format "%s: Not a valid gptel-tool struct" tool-name) errors))
          (error (push (format "%s: Error accessing tool properties: %s"
                               tool-name (error-message-string err)) errors)))))

    (if errors
        (progn
          (message "Gptel tool validation errors found:")
          (dolist (error errors)
            (message "  - %s" error))
          errors)
      (message "All Gptel tools validated successfully!")
      nil)))

(defun aj8/gptel-tool-create-scenario ()
  "Create a sandboxed environment for manually testing Gptel tools.

This sets up a temporary directory with several files and opens them in
buffers, simulating a realistic project. It also creates an instructions
buffer with suggested prompts for testing tool-based interactions with
an LLM."
  (interactive)
  (let ((test-dir (expand-file-name "gptel-tool-test/" temporary-file-directory)))
    ;; Create test directory
    (make-directory test-dir t)

    ;; Create test files
    (with-temp-buffer
      (insert "# Test Project\n\nThis is a test project for Gptel tools.\n\n## Files\n- main.py: Python script\n- config.json: Configuration\n- README.md: This file")
      (write-file (expand-file-name "README.md" test-dir)))

    (with-temp-buffer
      (insert "#!/usr/bin/env python3\n\ndef greet(name):\n    \"\"\"Greet someone by name.\"\"\"\n    return f\"Hello, {name}!\"\n\ndef main():\n    print(greet(\"World\"))\n\nif __name__ == \"__main__\":\n    main()")
      (write-file (expand-file-name "main.py" test-dir)))

    (with-temp-buffer
      (insert "{\n  \"app_name\": \"Gptel Test\",\n  \"version\": \"1.0.0\",\n  \"debug\": true\n}")
      (write-file (expand-file-name "config.json" test-dir)))

    ;; Create test buffers
    (with-current-buffer (get-buffer-create "*Gptel Test Buffer*")
      (erase-buffer)
      (insert "This is a test buffer for Gptel tools.\n\nYou can ask the LLM to:\n- Edit this content\n- Add new lines\n- Replace text\n- Apply multiple edits\n\nOriginal timestamp: " (current-time-string)))

    ;; Open files in buffers
    (find-file (expand-file-name "main.py" test-dir))
    (find-file (expand-file-name "config.json" test-dir))

    ;; Display instructions
    (with-current-buffer (get-buffer-create "*Gptel Tool Test Instructions*")
      (erase-buffer)
      (insert "=== Gptel Tool Test Scenario Created ===\n\n")
      (insert "Test files created in: " test-dir "\n\n")
      (insert "Available test buffers:\n")
      (insert "- *Gptel Test Buffer*\n")
      (insert "- main.py\n")
      (insert "- config.json\n\n")
      (insert "=== Suggested Test Prompts ===\n\n")
      (insert "1. \"List all open buffers\"\n")
      (insert "2. \"Show me the content of main.py\"\n")
      (insert "3. \"Change the greeting message in main.py from 'Hello' to 'Hi'\"\n")
      (insert "4. \"Add a new function to main.py that calculates the square of a number\"\n")
      (insert "5. \"Update the version in config.json to 2.0.0\"\n")
      (insert "6. \"Find all Python files in the current project\"\n")
      (insert "7. \"Search for the word 'greet' in all project files\"\n")
      (insert "8. \"Add a comment to the *Gptel Test Buffer*\"\n\n")
      (insert "=== Testing Tips ===\n\n")
      (insert "- Use the 'coding' preset for tool access\n")
      (insert "- Tools should work automatically when enabled\n")
      (insert "- Check that edits are applied correctly\n")
      (insert "- Verify error handling with invalid requests\n")
      (goto-char (point-min)))

    (switch-to-buffer "*Gptel Tool Test Instructions*")
    (message "Gptel tool test scenario created! Check the instructions buffer.")))
