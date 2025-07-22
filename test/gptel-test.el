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

;; --- Test Runner Functions ---

(defun aj8/run-all-gptel-tool-tests (&optional tag)
  "Run all ERT tests defined for gptel tools.
With optional TAG argument, run only tests with that tag.
Without a tag, run all tests with the 'test' tag."
  (interactive
   (list (intern (completing-read "Run tests with tag (default: test): "
                                 '("test" "buffers" "filesystem" "emacs" "project" "review"
                                   "integration" "gptel-tools" "json" "error-handling"
                                   "presets" "mock-llm" "workflow" "simulation" "complex-edits")
                                 nil t nil nil "test"))))
  (ert-run-tests-interactively (if tag
                                  `(tag ,tag)
                                '(tag test))))

(defun aj8/run-gptel-tests-by-tag (tag)
  "Run all GPtel tests with the specified TAG."
  (interactive
   (list (completing-read "Select tag: "
                         '("test" "buffers" "filesystem" "emacs" "project" "review"
                           "integration" "gptel-tools" "json" "error-handling"
                           "presets" "mock-llm" "workflow" "simulation" "complex-edits")
                         nil t)))
  (ert (format "(tag %s)" tag)))

(defun aj8/test-gptel-tool-direct (tool-name)
  "Test a GPtel tool directly by name.
TOOL-NAME is the name of the tool to test (e.g., 'aj8_list_buffers')."
  (interactive
   (list (completing-read "Select tool: "
                         (mapcar #'gptel-tool-name gptel-tools)
                         nil t)))
  (let* ((tool (cl-find-if (lambda (item) (string-equal (gptel-tool-name item) tool-name)) gptel-tools))
         (func (when tool (gptel-tool-function tool))))
    (if (and func (functionp func))
        (condition-case err
            (let ((result (if (gptel-tool-args tool)
                             (message "Tool requires arguments: %S"
                                     (gptel-tool-args tool))
                           (funcall func))))
              (message "Tool %s result: %S" tool-name result)
              result)
          (error (message "Error testing tool %s: %s"
                         tool-name (error-message-string err))))
      (message "Tool function not found for %s" tool-name))))

;;
;; Category: GPtel Tool Integration Tests
;;

(ert-deftest test-gptel-tool-registration ()
  "Test that all GPtel tools are properly registered."
  :tags '(integration gptel-tools)
  (let ((expected-tools '("aj8_list_buffers"
                         "aj8_buffer_to_file"
                         "aj8_file_to_buffer"
                         "aj8_append_to_buffer"
                         "aj8_insert_into_buffer"
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
      (should (assoc tool-name gptel-tools)))))

(ert-deftest test-gptel-tool-json-schema-validation ()
  "Test that GPtel tools have valid JSON schema definitions."
  :tags '(integration gptel-tools)
  (dolist (tool-def gptel-tools)
    (let ((tool-name (gptel-tool-name tool-def)))
      ;; Check that tool has required properties
      (should (gptel-tool-function tool-def))
      (should (gptel-tool-description tool-def))

      ;; Check that args is a list (can be nil)
      (should (listp (gptel-tool-args tool-def))))))

(ert-deftest test-gptel-tool-function-callable ()
  "Test that all GPtel tool functions are callable."
  :tags '(integration gptel-tools)
  (let ((no-arg-tools '("aj8_list_buffers"
                        "aj8_project_get_root"
                        "aj8_project_get_open_buffers")))
    ;; Test tools that don't require arguments
    (dolist (tool-name no-arg-tools)
      (let* ((tool-def (cdr (assoc tool-name gptel-tools)))
             (func (gptel-tool-function tool-def)))
        (should (functionp func))
        (should-not (condition-case nil
                        (funcall func)
                      (error t)))))))

(ert-deftest test-gptel-tool-via-json-call ()
  "Test calling GPtel tools via JSON-like interface (simulating LLM calls)."
  :tags '(integration gptel-tools json)
  (with-temp-buffer-with-content "*test-json-call*" "Hello World\nLine 2"
    ;; Test list buffers tool
    (let* ((tool-def (cdr (assoc "aj8_list_buffers" gptel-tools)))
           (func (gptel-tool-function tool-def))
           (result (funcall func)))
      (should (listp result))
      (should (member "*test-json-call*" result)))

    ;; Test edit buffer tool with JSON-like parameters
    (let* ((tool-def (cdr (assoc "aj8_edit_buffer" gptel-tools)))
           (func (gptel-tool-function tool-def))
           (result (funcall func "*test-json-call*" "World" "GPtel")))
      (should (string-match-p "successfully" result))
      (should (string-equal (buffer-string) "Hello GPtel\nLine 2")))))

(ert-deftest test-gptel-tool-error-handling ()
  "Test that GPtel tools handle errors gracefully."
  :tags '(integration gptel-tools error-handling)
  ;; Test with non-existent buffer
  (let* ((tool-def (cdr (assoc "aj8_edit_buffer" gptel-tools)))
         (func (gptel-tool-function tool-def)))
    (should (condition-case err
                (funcall func "*non-existent-buffer*" "old" "new")
              (error (string-match-p "Buffer.*not found" (error-message-string err))))))

  ;; Test with invalid file path
  (let* ((tool-def (cdr (assoc "aj8_read_file_section" gptel-tools)))
         (func (gptel-tool-function tool-def)))
    (should (condition-case err
                (funcall func "/non/existent/file.txt")
              (error (string-match-p "No such file" (error-message-string err)))))))

(ert-deftest test-gptel-preset-tool-integration ()
  "Test that GPtel presets properly enable/disable tools."
  :tags '(integration gptel-tools presets)
  ;; Check that coding preset has tools enabled
  (let ((coding-preset (cdr (assoc 'coding gptel--presets))))
    (should (plist-get coding-preset :use-tools)))

  ;; Check that chat preset has tools disabled
  (let ((chat-preset (cdr (assoc 'chat gptel--presets))))
    (should-not (plist-get chat-preset :use-tools))))

(defun aj8/test-gptel-tool-with-mock-llm (tool-name args expected-pattern)
  "Helper function to test a GPtel tool by simulating LLM usage.
TOOL-NAME is the name of the tool to test.
ARGS is a list of arguments to pass to the tool.
EXPECTED-PATTERN is a regexp that should match the result."
  (let* ((tool-def (cdr (assoc tool-name gptel-tools)))
         (func (gptel-tool-function tool-def))
         (result (apply func args)))
    (should (string-match-p expected-pattern (format "%s" result)))
    result))

(ert-deftest test-gptel-tools-mock-llm-interaction ()
  "Test GPtel tools as if called by an LLM."
  :tags '(integration gptel-tools mock-llm)
  (with-temp-project
    (with-temp-buffer-with-content "*mock-test*" "Original content\nSecond line"
      ;; Test buffer listing
      (aj8/test-gptel-tool-with-mock-llm "aj8_list_buffers" nil "mock-test")

      ;; Test buffer editing
      (aj8/test-gptel-tool-with-mock-llm "aj8_edit_buffer"
                                        '("*mock-test*" "Original" "Modified")
                                        "successfully")
      (should (string-equal (buffer-string) "Modified content\nSecond line"))

      ;; Test project root
      (aj8/test-gptel-tool-with-mock-llm "aj8_project_get_root" nil "ert-test-project")

      ;; Test file search
      (aj8/test-gptel-tool-with-mock-llm "aj8_project_find_files_glob"
                                        '("**/*.el")
                                        "code.el"))))

;; --- Interactive Test Functions ---

(defun aj8/test-gptel-tools-interactively ()
  "Interactively test GPtel tools by simulating LLM requests.
This function creates a temporary GPtel buffer and demonstrates
how tools would be called by an actual LLM."
  (interactive)
  (let ((test-buffer (get-buffer-create "*GPtel Tool Test*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (insert "=== GPtel Tool Integration Test ===\n\n")

      ;; Test 1: List available tools
      (insert "1. Available tools:\n")
      (dolist (tool-struct gptel-tools)
        (insert (format "   - %s: %s\n"
                        (gptel-tool-name tool-struct)
                        (gptel-tool-description tool-struct))))
      (insert "\n")

      ;; Test 2: Test a simple tool
      (insert "2. Testing aj8_list_buffers:\n")
      (let* ((tool-struct (cl-find-if (lambda (tool) (string-equal (gptel-tool-name tool) "aj8_list_buffers")) gptel-tools))
             (func (when tool-struct (gptel-tool-function tool-struct)))
             (result (if func (funcall func) "TOOL NOT FOUND")))
        (insert (format "   Result: %s\n\n" result)))

      ;; Test 3: Test documentation tool
      (insert "3. Testing aj8_read_documentation for 'car':\n")
      (let* ((tool-struct (cl-find-if (lambda (tool) (string-equal (gptel-tool-name tool) "aj8_read_documentation")) gptel-tools))
             (func (when tool-struct (gptel-tool-function tool-struct)))
             (result (if func (funcall func "car") "TOOL NOT FOUND")))
        (insert (format "   Result: %s\n\n" (substring (format "%s" result) 0 (min 200 (length (format "%s" result)))))))

      (insert "=== Test Complete ===\n")
      (goto-char (point-min)))
    (switch-to-buffer test-buffer)))

(defun aj8/validate-gptel-tool-schemas ()
  "Validate that all GPtel tool schemas are properly formatted.
Returns a list of any validation errors found."
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
          (message "GPtel tool validation errors found:")
          (dolist (error errors)
            (message "  - %s" error))
          errors)
      (message "All GPtel tools validated successfully!")
      nil)))

;; --- Enhanced Test Runner ---

(defun aj8/run-gptel-integration-tests (&optional selector)
  "Run GPtel tool integration tests.
With a prefix argument, prompt for a test SELECTOR."
  (interactive "P")
  (let ((test-selector-str (if selector
                               (completing-read "Run tests with selector: "
                                                '("integration" "gptel-tools" "json" "error-handling" "presets" "mock-llm"))
                             "integration")))
    (ert `(tag ,(intern test-selector-str)))))
;;
;; Category: Real-world LLM Interaction Simulation
;;

(ert-deftest test-gptel-tool-workflow-simulation ()
  "Simulate a realistic workflow where an LLM uses multiple tools in sequence."
  :tags '(integration workflow simulation)
  (with-temp-project
    (with-temp-file-with-content test-file "def hello():\n    print('Hello World')\n\ndef goodbye():\n    print('Goodbye')"
      ;; Simulate LLM workflow:
      ;; 1. List project files
      ;; 2. Read a file
      ;; 3. Edit the file
      ;; 4. Verify changes

      ;; Step 1: Find Python files
      (let* ((find-tool (cdr (assoc "aj8_project_find_files_glob" gptel-tools)))
             (find-func (gptel-tool-function find-tool))
             (py-files (funcall find-func "**/*.py")))
        (should (> (length py-files) 0)))

      ;; Step 2: Read file content
      (let* ((read-tool (cdr (assoc "aj8_read_file_section" gptel-tools)))
             (read-func (gptel-tool-function read-tool))
             (content (funcall read-func test-file)))
        (should (string-match-p "Hello World" content)))

      ;; Step 3: Edit the file
      (let* ((edit-tool (cdr (assoc "aj8_edit_file" gptel-tools)))
             (edit-func (gptel-tool-function edit-tool))
             (result (funcall edit-func test-file "Hello World" "Hello GPtel")))
        (should (string-match-p "successfully" result)))

      ;; Step 4: Verify changes
      (let* ((read-tool (cdr (assoc "aj8_read_file_section" gptel-tools)))
             (read-func (gptel-tool-function read-tool))
             (new-content (funcall read-func test-file)))
        (should (string-match-p "Hello GPtel" new-content))
        (should-not (string-match-p "Hello World" new-content))))))

(ert-deftest test-gptel-tool-complex-edits ()
  "Test complex editing scenarios that an LLM might perform."
  :tags '(integration complex-edits)
  (with-temp-buffer-with-content "*complex-edit-test*"
    "function calculateSum(a, b) {\n    return a + b;\n}\n\nfunction calculateProduct(a, b) {\n    return a * b;\n}\n\nfunction main() {\n    console.log('Starting calculations');\n    let sum = calculateSum(5, 3);\n    let product = calculateProduct(4, 6);\n    console.log('Results:', sum, product);\n}"

    ;; Simulate LLM making multiple related edits
    (let* ((edit-tool (cdr (assoc "aj8_apply_buffer_edits" gptel-tools)))
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

(defun aj8/benchmark-gptel-tools ()
  "Benchmark GPtel tool performance to ensure they're fast enough for LLM use."
  (interactive)
  (let ((results '())
        (iterations 10))

    ;; Benchmark list buffers
    (condition-case err
        (let ((start-time (current-time)))
          (dotimes (_ iterations)
            (let* ((tool-def (cl-find-if (lambda (tool) (string-equal (gptel-tool-name tool) "aj8_list_buffers")) gptel-tools))
                   (func (gptel-tool-function tool-def)))
              (when (functionp func)
                (funcall func))))
          (let ((elapsed (float-time (time-subtract (current-time) start-time))))
            (push (cons "aj8_list_buffers" (/ elapsed iterations)) results)))
      (error (push (cons "aj8_list_buffers" (format "ERROR: %s" (error-message-string err))) results)))

    ;; Benchmark documentation lookup
    (condition-case err
        (let ((start-time (current-time)))
          (dotimes (_ iterations)
            (let* ((tool-def (cl-find-if (lambda (tool) (string-equal (gptel-tool-name tool) "aj8_read_documentation")) gptel-tools))
                   (func (gptel-tool-function tool-def)))
              (when (functionp func)
                (funcall func "car"))))
          (let ((elapsed (float-time (time-subtract (current-time) start-time))))
            (push (cons "aj8_read_documentation" (/ elapsed iterations)) results)))
      (error (push (cons "aj8_read_documentation" (format "ERROR: %s" (error-message-string err))) results)))

    ;; Display results
    (with-current-buffer (get-buffer-create "*GPtel Tool Benchmark*")
      (erase-buffer)
      (insert "=== GPtel Tool Performance Benchmark ===\n\n")
      (insert (format "Iterations per tool: %d\n\n" iterations))
      (dolist (result (reverse results))
        (let ((name (car result))
              (value (cdr result)))
          (if (numberp value)
              (insert (format "%-30s: %.4f seconds\n" name value))
            (insert (format "%-30s: %s\n" name value)))))
      (insert "\n=== Benchmark Complete ===\n")
      (goto-char (point-min)))
    (switch-to-buffer "*GPtel Tool Benchmark*")))

;; --- Manual Testing Helper ---

(defun aj8/create-gptel-tool-test-scenario ()
  "Create a test scenario for manually testing GPtel tools with a real LLM.
This sets up buffers and files that you can reference when testing with GPtel."
  (interactive)
  (let ((test-dir (expand-file-name "gptel-tool-test/" temporary-file-directory)))
    ;; Create test directory
    (make-directory test-dir t)

    ;; Create test files
    (with-temp-buffer
      (insert "# Test Project\n\nThis is a test project for GPtel tools.\n\n## Files\n- main.py: Python script\n- config.json: Configuration\n- README.md: This file")
      (write-file (expand-file-name "README.md" test-dir)))

    (with-temp-buffer
      (insert "#!/usr/bin/env python3\n\ndef greet(name):\n    \"\"\"Greet someone by name.\"\"\"\n    return f\"Hello, {name}!\"\n\ndef main():\n    print(greet(\"World\"))\n\nif __name__ == \"__main__\":\n    main()")
      (write-file (expand-file-name "main.py" test-dir)))

    (with-temp-buffer
      (insert "{\n  \"app_name\": \"GPtel Test\",\n  \"version\": \"1.0.0\",\n  \"debug\": true\n}")
      (write-file (expand-file-name "config.json" test-dir)))

    ;; Create test buffers
    (with-current-buffer (get-buffer-create "*GPtel Test Buffer*")
      (erase-buffer)
      (insert "This is a test buffer for GPtel tools.\n\nYou can ask the LLM to:\n- Edit this content\n- Add new lines\n- Replace text\n- Apply multiple edits\n\nOriginal timestamp: " (current-time-string)))

    ;; Open files in buffers
    (find-file (expand-file-name "main.py" test-dir))
    (find-file (expand-file-name "config.json" test-dir))

    ;; Display instructions
    (with-current-buffer (get-buffer-create "*GPtel Tool Test Instructions*")
      (erase-buffer)
      (insert "=== GPtel Tool Test Scenario Created ===\n\n")
      (insert "Test files created in: " test-dir "\n\n")
      (insert "Available test buffers:\n")
      (insert "- *GPtel Test Buffer*\n")
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
      (insert "8. \"Add a comment to the *GPtel Test Buffer*\"\n\n")
      (insert "=== Testing Tips ===\n\n")
      (insert "- Use the 'coding' preset for tool access\n")
      (insert "- Tools should work automatically when enabled\n")
      (insert "- Check that edits are applied correctly\n")
      (insert "- Verify error handling with invalid requests\n")
      (goto-char (point-min)))

    (switch-to-buffer "*GPtel Tool Test Instructions*")
    (message "GPtel tool test scenario created! Check the instructions buffer.")))
