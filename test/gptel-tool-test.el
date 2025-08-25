;;; gptel-test.el --- Tests for Gptel tools -*- lexical-binding: t; -*-

(message "Running gptel tool tests")

;;
;;;; 1. Requirements
;;

;; Require ERT, the Emacs Lisp Regression Testing tool
(require 'ert)

;; Common CL utilities (cl-defun, cl-letf, cl-every, etc.)
(require 'cl-lib)

;; Load libraries required for these tests
(require 'gptel)
(require 'aj8-gptel)

;; Ensure straight quotes in error messages that we match against
(setq text-quoting-style 'straight)

;;
;;;; 2. Test Helpers
;;

;;; 2.1 Macros

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

;;; 2.2 Functions

(cl-defun aj8--assert-tool-error (result &key tool-name
                                         details-str details-regex
                                         details-predicate
                                         details-nonempty)
  "Assert that RESULT contains a properly formatted Gptel tool error message.

This function validates that RESULT starts with the expected \"tool: TOOL-NAME:\"
format and optionally validates additional error content using the provided
validation parameters.

RESULT should be a string containing the tool's error output.

Required keyword parameters:
- TOOL-NAME: The name of the tool that generated the error.

Optional keyword parameters:
- DETAILS-STR: If provided, check that this substring appears in the error.
- DETAILS-REGEX: If provided, check that this regexp matches the error.
- DETAILS-PREDICATE: If provided, call this function with RESULT; it should
  return non-nil if the error is valid.
- DETAILS-NONEMPTY: If non-nil, require that the error contains
  non-empty details after \"Error:\" in the message."
  (unless tool-name
    (error "tool-name is required"))
  (let ((header (format "tool: %s:" tool-name)))
    ;; Basic header prefix check
    (should (string-prefix-p header result))

    ;; If no details requested, done
    (unless (or details-str details-regex details-predicate details-nonempty)
      (cl-return-from aj8--assert-tool-error t))

    ;; details-predicate takes precedence
    (when details-predicate
      (should (funcall details-predicate result))
      (cl-return-from aj8--assert-tool-error t))

    ;; details-str as substring
    (when details-str
      (should (string-match-p (regexp-quote details-str) result))
      (cl-return-from aj8--assert-tool-error t))

    ;; details-regex
    (when details-regex
      (should (string-match-p details-regex result))
      (cl-return-from aj8--assert-tool-error t))

    ;; expect non-empty details
    (when details-nonempty
      (let ((after (substring result (min (length result) (or (string-match-p "Error" result) 0)))))
        (should (> (length (string-trim after)) 0))))))

;;
;;;; 3. Unit Tests (ert-deftest)
;;

;;; 3.1. Category: Buffers

(ert-deftest test-aj8-open-file-in-buffer ()
  "Test `aj8/gptel-tool-open-file-in-buffer'.
Verifies that the function opens a file into a buffer."
  :tags '(unit buffers)
  (with-temp-file-with-content
   test-file "test content"
   ;; Assert no buffer exists before open
   (should (null (get-file-buffer test-file)))
   (aj8/gptel-tool-open-file-in-buffer test-file)
   ;; Assert the file's buffer is live after opening
   (should (buffer-live-p (get-file-buffer test-file))))

  ;; Assert non-existent file errors
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    (let* ((tmp (make-temp-file "aj8-nonexistent-file-")))
      (delete-file tmp)
      ;; Assert an error is signaled for opening a missing file
      (should-error (aj8/gptel-tool-open-file-in-buffer tmp) :type 'error)))

  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let* ((tmp (make-temp-file "aj8-nonexistent-file-")))
      (delete-file tmp)
      (let ((result (aj8/gptel-tool-open-file-in-buffer tmp)))
        ;; Assert returned error message matches expected format
        (should (string-equal
                 (format "tool: aj8_open_file_in_buffer: Error: No such file: %s" tmp)
                 result)))))

  ;; Assert directory path errors
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    (let ((dir (make-temp-file "aj8-temp-dir-" t)))
      (unwind-protect
          (should-error (aj8/gptel-tool-open-file-in-buffer dir) :type 'error)
        (when (file-directory-p dir)
          (delete-directory dir t)))))

  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((dir (make-temp-file "aj8-temp-dir-" t)))
      (unwind-protect
          (let ((result (aj8/gptel-tool-open-file-in-buffer dir)))
            ;; Assert returned message for directory path error
            (should (string-equal
                     (format "tool: aj8_open_file_in_buffer: Error: '%s' is a directory." dir)
                     result)))
        (when (file-directory-p dir)
          (delete-directory dir t))))))

(ert-deftest test-aj8-read-buffer-lines ()
  "Test `aj8/gptel-tool-read-buffer-lines' and
`aj8/gptel-tool-read-buffer-lines-count'.
Verifies that the functions can read a whole buffer, a section from the
middle, a section from the beginning, and a section to the end. It also
verifies that an error is signaled for a non-existent buffer, and for
requests larger than `aj8/gptel-tool-max-lines'."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-read-buffer*" "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"
   ;; Read whole buffer
   ;; (should (string-equal (aj8/gptel-tool-read-buffer-lines "*test-read-buffer*")
   ;;                       "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"))
   ;; Reading the full buffer returns all lines
   ;; Assert reading the full buffer returns all lines
   (should (string-equal (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" 1 5)
                         "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"))
   ;; Read a section
   ;; (should (string-equal (aj8/gptel-tool-read-buffer-lines "*test-read-buffer*" 2 4)
   ;;                       "Line 2\nLine 3\nLine 4"))
   ;; Reading a middle section returns the expected lines
   ;; Assert reading a middle section returns expected lines
   (should (string-equal (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" 2 3)
                         "Line 2\nLine 3\nLine 4"))
   ;; Read from start
   ;; (should (string-equal (aj8/gptel-tool-read-buffer-lines "*test-read-buffer*" nil 2)
   ;;                       "Line 1\nLine 2"))
   ;; Reading from the start returns the first N lines
   ;; Assert reading from the start returns the first N lines
   (should (string-equal (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" 1 2)
                         "Line 1\nLine 2"))
   ;; Read to end
   ;; (should (string-equal (aj8/gptel-tool-read-buffer-lines "*test-read-buffer*" 4)
   ;;                       "Line 4\nLine 5"))
   ;; Reading to the end returns the last lines
   ;; Assert reading to the end returns the last lines
   (should (string-equal (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" 4 2)
                         "Line 4\nLine 5"))

   ;; Assert non-existent buffer errors
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert error signaled for non-existent buffer (re-signal mode)
     (should-error (aj8/gptel-tool-read-buffer-lines-count "*non-existent-buffer*") :type 'error))

   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-read-buffer-lines-count "*non-existent-buffer*")))
       (should (string-equal
                "tool: aj8_read_buffer_lines_count: Error: Buffer '*non-existent-buffer*' not found."
                result))))
   ;; Test handling of max number of lines
   (let* ((n (1+ aj8/gptel-tool-max-lines))
          (content "")
          (first-n ""))
     (dotimes (i n)
       (setq content (concat content (format "Line %d\n" (1+ i))))
       (when (< i aj8/gptel-tool-max-lines)
         (setq first-n (concat first-n (format "Line %d\n" (1+ i))))))
     (setq content (replace-regexp-in-string "\n\\'" "" content))
     (setq first-n (replace-regexp-in-string "\n\\'" "" first-n))
     (with-temp-buffer-with-content
      "*test-read-buffer-max*" content
      ;; Use signal mode for all error assertions in this block
      (let ((aj8/gptel-tool-return-error nil))
        ;; Assert error when total-lines > max
        ;; (should-error (aj8/gptel-tool-read-buffer-lines "*test-read-buffer-max*") :type 'error)
        ;; Assert error when requested length > max
        ;; (should-error (aj8/gptel-tool-read-buffer-lines "*test-read-buffer-max*" 1 n) :type 'error)
        ;; Assert error when START < 1
        ;; (should-error (aj8/gptel-tool-read-buffer-lines "*test-read-buffer-max*" 0 2) :type 'error)
        ;; Assert error when START > total-lines
        ;; (should-error (aj8/gptel-tool-read-buffer-lines "*test-read-buffer-max*" (1+ (count-lines (point-min) (point-max)))) :type 'error)
        ;; Assert error when COUNT > MAX
        (should-error (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer-max*" 1 n) :type 'error)
        ;; Assert error when START < 1
        (should-error (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer-max*" 0 2) :type 'error)
        ;; Assert error when START > total-lines
        (should-error (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer-max*" (1+ (count-lines (point-min) (point-max))) 1) :type 'error))

      ;; Success cases (use default return-error mode)
      ;; Range API: explicit request for first MAX lines should succeed
      (should (string-equal (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer-max*" 1 aj8/gptel-tool-max-lines) first-n))
      ;; Default COUNT (nil) is treated as MAX and should return first N lines
      (should (string-equal (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer-max*") first-n))
      ;; Requesting COUNT == MAX should return first MAX lines
      (should (string-equal (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer-max*" 1 aj8/gptel-tool-max-lines) first-n))))))

(ert-deftest test-aj8-list-buffers ()
  "Test buffer listing tools.

This test ensures that `aj8/gptel-tool-list-buffers' lists only
file-associated buffers, while `aj8/gptel-tool-list-all-buffers' lists
all buffers, including non-file-backed ones.

Formatting assertions verify both modes (with and without line counts),
ensuring file-backed entries show names with paths and non-file entries
show names only when applicable.

It also verifies that PATH is absolute for files outside the current
project and relative to the project root for files inside it."
  :tags '(unit buffers)
  (with-temp-file-with-content
   tmp-file "file content"
   (find-file-noselect tmp-file)
   (with-temp-buffer-with-content
    "*non-file-buffer*" "some content"
    ;; 1. Test `aj8/gptel-tool-list-buffers' (file-backed only)
    (let ((buffers (split-string (aj8/gptel-tool-list-buffers) "\n" t)))
      ;; File-backed buffer should be listed as "NAME: PATH"
      (let* ((name (buffer-name (get-file-buffer tmp-file)))
             (path tmp-file)
             (expected (format "%s: %s" name path)))
        (should (member expected buffers)))
      ;; Non-file buffer should not be listed
      (should-not (member "*non-file-buffer*" buffers)))
    ;; 1.1. Test include-counts
    (let ((buffers (split-string (aj8/gptel-tool-list-buffers t) "\n" t)))
      ;; All lines should be listed as "NAME: PATH (N lines)"
      ;; Assert all lines formatted as "NAME: PATH (N lines)"
      (should (cl-every (lambda (s) (string-match-p "^[^:]+: .+ ([0-9]+ lines)$" s)) buffers))
      ;; Exact file-backed line: "NAME: PATH (N lines)"
      (let* ((name (buffer-name (get-file-buffer tmp-file)))
             (path tmp-file)
             (expected (format "%s: %s (%d lines)" name path 1)))
        (should (member expected buffers)))
      ;; Non-file buffer should not be listed
      (should-not (member "*non-file-buffer*" buffers)))
    ;; 2. Test `aj8/gptel-tool-list-all-buffers' (all)
    (let ((buffers (split-string (aj8/gptel-tool-list-all-buffers) "\n" t)))
      ;; Non-file buffer should be listed as "NAME"
      (should (member "*non-file-buffer*" buffers))
      ;; File-backed buffer should be listed as "NAME: PATH"
      (let* ((name (buffer-name (get-file-buffer tmp-file)))
             (path tmp-file)
             (expected (format "%s: %s" name path)))
        (should (member expected buffers))))
    ;; 2.1 Test include-counts
    (let ((buffers (split-string (aj8/gptel-tool-list-all-buffers t) "\n" t)))
      ;; All lines should be listed as either "NAME (N lines)" or "NAME: PATH (N lines)"
      ;; Assert include-counts formatting for all lines
      (should (cl-every (lambda (s) (string-match-p "^[^:]+\\(: .+\\)? ([0-9]+ lines)$" s)) buffers))
      ;; Exact non-file line: "NAME (N lines)"
      (let* ((name "*non-file-buffer*")
             (expected (format "%s (%d lines)" name 1)))
        (should (member expected buffers)))
      ;; Exact file-backed line: "NAME: PATH (N lines)"
      (let* ((name (buffer-name (get-file-buffer tmp-file)))
             (path tmp-file)
             (expected (format "%s: %s (%d lines)" name path 1)))
        (should (member expected buffers)))))))

(ert-deftest test-aj8-buffer-and-file-conversion ()
  "Test buffer-file path conversions.
Verifies that `aj8/gptel-tool-buffer-to-file' and
`aj8/gptel-tool-file-to-buffer' correctly convert between buffer names
and file paths, and that they signal errors for invalid inputs."
  :tags '(unit buffers)
  (with-temp-file-with-content
   test-file "content"
   (let ((buffer (find-file-noselect test-file)))
     (unwind-protect
         (progn
           ;; Assert buffer name converts to the expected file path
           (should (string-equal (aj8/gptel-tool-buffer-to-file (buffer-name buffer))
                                 (expand-file-name test-file)))
           ;; Assert file path converts back to the expected buffer name
           (should (string-equal (aj8/gptel-tool-file-to-buffer test-file)
                                 (buffer-name buffer)))

           ;; Assert errors
           ;; Mode 1: tool re-signals the error
           (let ((aj8/gptel-tool-return-error nil))
             ;; Assert error when converting a non-file buffer to a file (re-signal)
             (should-error (aj8/gptel-tool-buffer-to-file "*scratch*") :type 'error))

           ;; Mode 2: tool returns the error as a string
           (let ((aj8/gptel-tool-return-error t))
             (let ((result (aj8/gptel-tool-buffer-to-file "*scratch*")))
               (should (string-equal
                        "tool: aj8_buffer_to_file: Error: Buffer '*scratch*' not found or not associated with a file."
                        result))))

           ;; Mode 1: tool re-signals the error
           (let ((aj8/gptel-tool-return-error nil))
             ;; Assert error when converting a non-existent file to a buffer (re-signal)
             (should-error (aj8/gptel-tool-file-to-buffer "/non/existent/file.tmp") :type 'error))

           ;; Mode 2: tool returns the error as a string
           (let ((aj8/gptel-tool-return-error t))
             (let ((result (aj8/gptel-tool-file-to-buffer "/non/existent/file.tmp")))
               (should (string-equal
                        (format "tool: aj8_file_to_buffer: Error: No buffer is visiting the file '%s'." "/non/existent/file.tmp")
                        result)))))

       (kill-buffer buffer)))))

(ert-deftest test-aj8-append-to-buffer ()
  "Test `aj8/gptel-tool-append-to-buffer'.
Ensures that content can be appended to a buffer correctly."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-append*" "Line 1\nLine 3"
   ;; Append
   (aj8/gptel-tool-append-to-buffer "*test-append*" "\nLine 4")
   ;; Assert appended content appears at buffer end
   (should (string-equal (buffer-string) "Line 1\nLine 3\nLine 4"))

   ;; Assert non-existent buffer errors
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert append signals error for missing buffer (re-signal)
     (should-error (aj8/gptel-tool-append-to-buffer "*nope*" "text") :type 'error))

   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-append-to-buffer "*nope*" "text")))
       (should (string-equal
                "tool: aj8_append_to_buffer: Error: Buffer '*nope*' not found."
                result))))))

(ert-deftest test-aj8-insert-in-buffer ()
  "Test `aj8/gptel-tool-insert-in-buffer'.
Ensures that content can be inserted at a specific position in a buffer."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-insert*" "Line 1\nLine 3"
   ;; Insert
   (aj8/gptel-tool-insert-in-buffer "*test-insert*" "Line 2\n" 2)
   ;; Assert inserted content appears at the requested position
   (should (string-equal (buffer-string) "Line 1\nLine 2\nLine 3"))

   ;; Assert non-existent buffer errors
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert insert signals error for missing buffer (re-signal)
     (should-error (aj8/gptel-tool-insert-in-buffer "*nope*" "text" 1) :type 'error))

   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-insert-in-buffer "*nope*" "text" 1)))
       (should (string-equal
                "tool: aj8_insert_in_buffer: Error: Buffer '*nope*' not found."
                result))))))

(ert-deftest test-aj8-replace-buffer ()
  "Test `aj8/gptel-tool-replace-buffer'.
Ensures that a buffer's entire content can be replaced correctly."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-replace*" "Line 1\nLine 3"
   ;; Replace
   (aj8/gptel-tool-replace-buffer "*test-replace*" "New Content")
   ;; Assert buffer replaced with new content
   (should (string-equal (buffer-string) "New Content"))

   ;; Assert non-existent buffer errors
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert replace signals error for missing buffer (re-signal)
     (should-error (aj8/gptel-tool-replace-buffer "*nope*" "content") :type 'error))

   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-replace-buffer "*nope*" "content")))
       (should (string-equal
                "tool: aj8_replace_buffer: Error: Buffer '*nope*' not found."
                result))))))

(ert-deftest test-aj8-edit-buffer-string ()
  "Test `aj8/gptel-tool-edit-buffer-string'.
Verifies that a single, unique string can be replaced in a buffer.  It
also confirms that an error is signaled if the target string is not
found or is not unique."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-edit*" "hello world\nhello universe"
   (aj8/gptel-tool-edit-buffer-string "*test-edit*" "world" "emacs")
   ;; Single-string replacement succeeded
   (should (string-equal (buffer-string) "hello emacs\nhello universe"))

   ;; Assert errors
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert edit-string signals error when target string not found (re-signal)
     (should-error (aj8/gptel-tool-edit-buffer-string "*test-edit*" "non-existent" "foo") :type 'error))

   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-edit-buffer-string "*test-edit*" "non-existent" "foo")))
       (should (string-equal
                "tool: aj8_edit_buffer_string: Error: String 'non-existent' not found in buffer '*test-edit*'."
                result))))

   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert edit-string signals error when target string is not unique (re-signal)
     (should-error (aj8/gptel-tool-edit-buffer-string "*test-edit*" "hello" "hi") :type 'error))

   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-edit-buffer-string "*test-edit*" "hello" "hi")))
       (should (string-equal
                "tool: aj8_edit_buffer_string: Error: String 'hello' is not unique in buffer '*test-edit*'. Found 2 occurrences."
                result))))

   ;; Verify that a multi-line :old-string is accepted
   (aj8/gptel-tool-edit-buffer-string "*test-edit*" "emacs\nhello" "EMACS\nHI")
   (should (string-equal (buffer-string) "hello EMACS\nHI universe"))))

(ert-deftest test-aj8-edit-buffer-line ()
  "Test `aj8/gptel-tool-replace-buffer-line'.
Ensures that a specific single line in a buffer is replaced correctly."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-edit-line*" "Line A
Line B
Line C"
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert invalid line numbers re-signal errors
     (should-error (aj8/gptel-tool-replace-buffer-line "*test-edit-line*" 0 "X") :type 'error)
     (should-error (aj8/gptel-tool-replace-buffer-line "*test-edit-line*" 10 "X") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-replace-buffer-line "*test-edit-line*" 0 "X")))
       (should (string-equal
                "tool: aj8_replace_buffer_line: Error: START-LINE must be >= 1"
                result)))
     (let ((result (aj8/gptel-tool-replace-buffer-line "*test-edit-line*" 10 "X")))
       (should (string-equal
                "tool: aj8_replace_buffer_line: Error: END-LINE exceeds buffer length (3)."
                result))))
   ;; Success case
   (aj8/gptel-tool-replace-buffer-line "*test-edit-line*" 2 "X")
   ;; Line replacement succeeded
   (should (string-equal (buffer-string) "Line A
X
Line C")))

  ;; Assert non-existent buffer errors
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    ;; Assert editing a missing buffer re-signals an error
    (should-error (aj8/gptel-tool-replace-buffer-line "*non-existent-buffer*" 1 "X") :type 'error))

  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((result (aj8/gptel-tool-replace-buffer-line "*non-existent-buffer*" 1 "X")))
      (should (string-equal
               "tool: aj8_replace_buffer_line: Error: Buffer '*non-existent-buffer*' not found."
               result)))))

(ert-deftest test-aj8-edit-buffer-lines ()
  "Test `aj8/gptel-tool-replace-buffer-lines'.
Ensures that a contiguous range of lines is replaced correctly in a buffer."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-edit-buffer-lines*" "Line A
Line B
Line C
Line D"
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert various invalid line ranges re-signal errors
     (should-error (aj8/gptel-tool-replace-buffer-lines "*test-edit-buffer-lines*" 0 1 "X") :type 'error)
     (should-error (aj8/gptel-tool-replace-buffer-lines "*test-edit-buffer-lines*" 3 2 "X") :type 'error)
     (should-error (aj8/gptel-tool-replace-buffer-lines "*test-edit-buffer-lines*" 2 5 "X") :type 'error))

   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-replace-buffer-lines "*test-edit-buffer-lines*" 0 1 "X")))
       (should (string-equal
                "tool: aj8_replace_buffer_lines: Error: START-LINE must be >= 1"
                result)))
     (let ((result (aj8/gptel-tool-replace-buffer-lines "*test-edit-buffer-lines*" 3 2 "X")))
       (should (string-equal
                "tool: aj8_replace_buffer_lines: Error: END-LINE must be >= START-LINE"
                result)))
     (let ((result (aj8/gptel-tool-replace-buffer-lines "*test-edit-buffer-lines*" 2 5 "X")))
       (should (string-equal
                "tool: aj8_replace_buffer_lines: Error: END-LINE exceeds buffer length (4)."
                result))))

   ;; Success case
   (aj8/gptel-tool-replace-buffer-lines "*test-edit-buffer-lines*" 2 3 "X\nY")
   ;; LIne range replacement succeeded
   (should (string-equal (buffer-string) "Line A\nX\nY\nLine D"))

   ;; Assert non-existent buffer errors
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert editing a missing buffer line range re-signals an error
     (should-error (aj8/gptel-tool-replace-buffer-lines "*non-existent-buffer*" 1 1 "X") :type 'error))

   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-replace-buffer-lines "*non-existent-buffer*" 1 1 "X")))
       (should (string-equal
                "tool: aj8_replace_buffer_lines: Error: Buffer '*non-existent-buffer*' not found."
                result))))))

(ert-deftest test-aj8-delete-buffer-string ()
  "Test `aj8/gptel-tool-delete-buffer-string'.
Verifies that a single, unique string can be deleted from a buffer. It
also confirms that an error is signaled if the target string is not
found or is not unique."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-delete*" "hello world\nhello universe"
   (aj8/gptel-tool-delete-buffer-string "*test-delete*" "world")
   ;; Assert the target string was removed
   (should (string-equal (buffer-string) "hello \nhello universe"))

   ;; Assert errors
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert deleting a missing substring re-signals an error
     (should-error (aj8/gptel-tool-delete-buffer-string "*test-delete*" "non-existent") :type 'error))

   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-delete-buffer-string "*test-delete*" "non-existent")))
       ;; Assert returned error message when substring not found
       (should (string-equal
                "tool: aj8_delete_buffer_string: Error: String 'non-existent' not found in buffer '*test-delete*'."
                result))))

   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert deleting a non-unique substring re-signals an error
     (should-error (aj8/gptel-tool-delete-buffer-string "*test-delete*" "hello") :type 'error))

   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-delete-buffer-string "*test-delete*" "hello")))
       ;; Assert returned message for non-unique deletion
       (should (string-equal
                "tool: aj8_delete_buffer_string: Error: String 'hello' is not unique in buffer '*test-delete*'. Found 2 occurrences."
                result)))))

  ;; Verify that a multi-line :old-string is accepted for deletion
  (with-temp-buffer-with-content
   "*test-delete-ml*" "A\nB\nC"
   (aj8/gptel-tool-delete-buffer-string "*test-delete-ml*" "B\n")
   ;; Assert multi-line deletion works
   (should (string-equal (buffer-string) "A\nC"))))

(ert-deftest test-aj8-delete-buffer-line ()
  "Test `aj8/gptel-tool-delete-buffer-line'.
Ensures that a specific single line in a buffer is deleted correctly."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-delete-line*" "Line A\nLine B\nLine C"
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert invalid line numbers re-signal errors
     (should-error (aj8/gptel-tool-delete-buffer-line "*test-delete-line*" 0) :type 'error)
     (should-error (aj8/gptel-tool-delete-buffer-line "*test-delete-line*" 10) :type 'error)
     ;; Assert deleting a line in a missing buffer re-signals an error
     (should-error (aj8/gptel-tool-delete-buffer-line "*non-existent-buffer*" 2) :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-delete-buffer-line "*test-delete-line*" 0)))
       ;; Assert returned message for invalid start-line
       (should (string-equal
                "tool: aj8_delete_buffer_line: Error: START-LINE must be >= 1"
                result)))
     (let ((result (aj8/gptel-tool-delete-buffer-line "*test-delete-line*" 10)))
       ;; Assert returned message for end-line exceeding buffer
       (should (string-equal
                "tool: aj8_delete_buffer_line: Error: END-LINE exceeds buffer length (3)."
                result)))
     ;; Assert returned message for missing buffer
     (let ((result (aj8/gptel-tool-delete-buffer-line "*non-existent-buffer*" 2)))
       (should (string-equal
                "tool: aj8_delete_buffer_line: Error: Buffer '*non-existent-buffer*' not found."
                result))))
   ;; Success case
   (aj8/gptel-tool-delete-buffer-line "*test-delete-line*" 2)
   (should (string-equal (buffer-string) "Line A\n\nLine C"))))

(ert-deftest test-aj8-delete-buffer-lines ()
  "Test `aj8/gptel-tool-delete-buffer-lines'.
Ensures that a contiguous range of lines is deleted correctly in a buffer."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-delete-buffer-lines*" "Line A\nLine B\nLine C\nLine D"
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert invalid line ranges re-signal errors
     (should-error (aj8/gptel-tool-delete-buffer-lines "*test-delete-buffer-lines*" 0 1) :type 'error)
     (should-error (aj8/gptel-tool-delete-buffer-lines "*test-delete-buffer-lines*" 3 2) :type 'error)
     (should-error (aj8/gptel-tool-delete-buffer-lines "*test-delete-buffer-lines*" 2 5) :type 'error)
     ;; Assert deleting a line range in missing buffer re-signals error
     (should-error (aj8/gptel-tool-delete-buffer-lines "*non-existent-buffer*" 2 3) :type 'error))

   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-delete-buffer-lines "*test-delete-buffer-lines*" 0 1)))
       ;; Assert returned message for invalid start-line
       (should (string-equal
                "tool: aj8_delete_buffer_lines: Error: START-LINE must be >= 1"
                result)))
     (let ((result (aj8/gptel-tool-delete-buffer-lines "*test-delete-buffer-lines*" 3 2)))
       ;; Assert returned message for end-line < start-line
       (should (string-equal
                "tool: aj8_delete_buffer_lines: Error: END-LINE must be >= START-LINE"
                result)))
     (let ((result (aj8/gptel-tool-delete-buffer-lines "*test-delete-buffer-lines*" 2 5)))
       ;; Assert returned message for end-line exceeding buffer
       (should (string-equal
                "tool: aj8_delete_buffer_lines: Error: END-LINE exceeds buffer length (4)."
                result)))
     ;; Assert returned message for missing buffer
     (let ((result (aj8/gptel-tool-delete-buffer-lines "*non-existent-buffer*" 2 3)))
       (should (string-equal
                "tool: aj8_delete_buffer_lines: Error: Buffer '*non-existent-buffer*' not found."
                result))))

   ;; Success case
   (aj8/gptel-tool-delete-buffer-lines "*test-delete-buffer-lines*" 2 3)
   (should (string-equal (buffer-string) "Line A\n\nLine D"))))

(ert-deftest test-aj8-apply-buffer-string-edits ()
  "Test `aj8/gptel-tool-apply-buffer-string-edits'.
Ensures that a list of substring edits is applied correctly to a buffer."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-apply-edits*" "Line one.\nLine two.\nLine three."
   (let ((edits '((:line-number 3 :old-string "three" :new-string "THREE")
                  (:line-number 1 :old-string "one" :new-string "ONE"))))
     (aj8/gptel-tool-apply-buffer-string-edits "*test-apply-edits*" edits)
     ;; Assert batched substring edits applied successfully
     (should (string-equal (buffer-string) "Line ONE.\nLine two.\nLine THREE."))
     ;; Verify that a multi-line :old-string is rejected for batched string edits
     (let ((edits2 '((:line-number 2 :old-string "two\nextra" :new-string "TWO"))))

       ;; Mode 1: tool re-signals the error
       (let ((aj8/gptel-tool-return-error nil))
         (should-error (aj8/gptel-tool-apply-buffer-string-edits "*test-apply-edits*" edits2) :type 'error))

       ;; Mode 2: tool returns the error as a string
       (let ((aj8/gptel-tool-return-error t))
         (let ((result (aj8/gptel-tool-apply-buffer-string-edits "*test-apply-edits*" edits2)))
           ;; Assert returned message when applying invalid string edits
           (should (string-equal
                    "tool: aj8_apply_buffer_string_edits: Error applying edits to buffer '*test-apply-edits*': 1 (out of 1) failed.\n - line 2: old-string contains newline (old-string: \"two\nextra\")"
                    result))))))))

(ert-deftest test-aj8-apply-buffer-line-edits ()
  "Test `aj8/gptel-tool-apply-buffer-line-edits'.
Ensures that a list of full-line edits is applied correctly to a buffer."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-apply-edits*" "Line one.\nLine two.\nLine three."
   (let ((edits '((:line-number 3 :old-string "Line three." :new-string "Line THREE.")
                  (:line-number 1 :old-string "Line one." :new-string "Line ONE."))))
     (aj8/gptel-tool-apply-buffer-line-edits "*test-apply-edits*" edits)
     (should (string-equal (buffer-string) "Line ONE.\nLine two.\nLine THREE."))
     ;; Verify that a multi-line :old-string is rejected for batched line edits
     (let ((edits2 '((:line-number 2 :old-string "Line two.\nextra" :new-string "Line TWO."))))
       ;; Assert errors
       ;; Mode 1: tool re-signals the error
       (let ((aj8/gptel-tool-return-error nil))
         (should-error (aj8/gptel-tool-apply-buffer-line-edits "*test-apply-edits*" edits2) :type 'error))

       ;; Mode 2: tool returns the error as a string
       (let ((aj8/gptel-tool-return-error t))
         (let ((result (aj8/gptel-tool-apply-buffer-line-edits "*test-apply-edits*" edits2)))
           (should (string-equal
                    "tool: aj8_apply_buffer_line_edits: Error applying edits to buffer '*test-apply-edits*': 1 (out of 1) failed.\n - line 2: old-string contains newline (old-string: \"Line two.\nextra\")"
                    result))))

       ;; Assert non-existent buffer errors
       ;; Mode 1: tool re-signals the error
       (let ((aj8/gptel-tool-return-error nil))
         (should-error
          (aj8/gptel-tool-apply-buffer-line-edits "*non-existent*" '((:line-number 1 :old-string "x" :new-string "y")))
          :type 'error))

       ;; Mode 2: tool returns the error as a string
       (let ((aj8/gptel-tool-return-error t))
         (let ((result
                (aj8/gptel-tool-apply-buffer-line-edits "*non-existent*" '((:line-number 1 :old-string "x" :new-string "y")))))
           (should (string-equal
                    "tool: aj8_apply_buffer_line_edits: Error: Buffer '*non-existent*' not found."
                    result))))))))

(ert-deftest test-aj8-apply-buffer-string-edits-with-review ()
  "Test `aj8/gptel-tool-apply-buffer-string-edits-with-review'.
Verifies that the function prepares substring edits and invokes the Ediff
review system, without altering the original buffer."
  :tags '(unit buffers review)
  (with-temp-buffer-with-content
   "*test-review*" "Line one.\nLine two."
   (let ((edits '((:line-number 1 :old-string "one" :new-string "ONE")))
         (ediff-called nil))
     ;; Temporarily advise `ediff-buffers' to check if it's called,
     ;; without actually starting the interactive session.
     (cl-letf (((symbol-function 'ediff-buffers) (lambda (b1 b2) (setq ediff-called t))))
       (aj8/gptel-tool-apply-buffer-string-edits-with-review "*test-review*" edits))
     ;; Assert the ediff review function was called
     (should ediff-called)
     ;; Check that the original buffer is unchanged.
     (with-current-buffer "*test-review*"
       (should (string-equal (buffer-string) "Line one.\nLine two."))))
   ;; Verify that a multi-line :old-string is rejected for batched string edits
   (let ((edits2 '((:line-number 2 :old-string "two\nextra" :new-string "TWO"))))
     ;; Mode 1: tool re-signals the error
     (let ((aj8/gptel-tool-return-error nil))
       (should-error (aj8/gptel-tool-apply-buffer-string-edits-with-review "*test-review*" edits2) :type 'error))
     ;; Mode 2: tool returns the error as a string
     (let ((aj8/gptel-tool-return-error t))
       (let ((result (aj8/gptel-tool-apply-buffer-string-edits-with-review "*test-review*" edits2)))
         ;; Assert header and that the specific diagnostic text appears; allow
         ;; temporary review buffer naming to differ.
         (should (string-equal
                  "tool: aj8_apply_buffer_string_edits_with_review: Error applying edits to buffer '**test-review*-edits*': 1 (out of 1) failed.\n - line 2: old-string contains newline (old-string: \"two\nextra\")\nNote: No review was started and no changes were applied to buffer '*test-review*'. Any details above refer only to the temporary review buffer."
                  result))))

     ;; Assert non-existent buffer errors
     ;; Mode 1: tool re-signals the error
     (let ((aj8/gptel-tool-return-error nil))
       (should-error (aj8/gptel-tool-apply-buffer-string-edits-with-review "*non-existent*" '((:line-number 1 :old-string "x" :new-string "y"))) :type 'error))
     ;; Mode 2: tool returns the error as a string
     (let ((aj8/gptel-tool-return-error t))
       (let ((result (aj8/gptel-tool-apply-buffer-string-edits-with-review "*non-existent*" '((:line-number 1 :old-string "x" :new-string "y")))))
         (should (string-equal
                  "tool: aj8_apply_buffer_string_edits_with_review: Error: Buffer '*non-existent*' not found."
                  result)))))))

(ert-deftest test-aj8-apply-buffer-line-edits-with-review ()
  "Test `aj8/gptel-tool-apply-buffer-line-edits-with-review'.
Verifies that the function prepares full-line edits and invokes the Ediff
review system, without altering the original buffer."
  :tags '(unit buffers review)
  (with-temp-buffer-with-content
   "*test-review*" "Line one.\nLine two."
   (let ((edits '((:line-number 1 :old-string "Line one." :new-string "Line ONE.")))
         (ediff-called nil))
     ;; Temporarily advise `ediff-buffers' to check if it's called,
     ;; without actually starting the interactive session.
     (cl-letf (((symbol-function 'ediff-buffers) (lambda (b1 b2) (setq ediff-called t))))
       (aj8/gptel-tool-apply-buffer-line-edits-with-review "*test-review*" edits))
     ;; Assert the ediff review function was called
     (should ediff-called)
     ;; Check that the original buffer is unchanged.
     (with-current-buffer "*test-review*"
       (should (string-equal (buffer-string) "Line one.\nLine two."))))
   ;; Verify that a multi-line :old-string is rejected for batched line edits
   (let ((edits2 '((:line-number 2 :old-string "Line two.\nextra" :new-string "Line TWO."))))
     ;; Mode 1: tool re-signals the error
     (let ((aj8/gptel-tool-return-error nil))
       (should-error (aj8/gptel-tool-apply-buffer-line-edits-with-review "*test-review*" edits2) :type 'error))
     ;; Mode 2: tool returns the error as a string
     (let ((aj8/gptel-tool-return-error t))
       (let ((result (aj8/gptel-tool-apply-buffer-line-edits-with-review "*test-review*" edits2)))
         (should (string-equal
                  "tool: aj8_apply_buffer_line_edits_with_review: Error applying edits to buffer '**test-review*-edits*': 1 (out of 1) failed.\n - line 2: old-string contains newline (old-string: \"Line two.\nextra\")\nNote: No review was started and no changes were applied to buffer '*test-review*'. Any details above refer only to the temporary review buffer."
                  result)))))

   ;; Assert non-existent buffer errors
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     (should-error (aj8/gptel-tool-apply-buffer-line-edits-with-review "*non-existent*" '((:line-number 1 :old-string "x" :new-string "y"))) :type 'error))

   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-apply-buffer-line-edits-with-review "*non-existent*" '((:line-number 1 :old-string "x" :new-string "y")))))
       (should (string-equal
                "tool: aj8_apply_buffer_line_edits_with_review: Error: Buffer '*non-existent*' not found."
                result))))))

;;; 3.3. Category: Emacs

(ert-deftest test-aj8-read-documentation ()
  "Test `aj8/gptel-tool-read-documentation'.
Verifies that documentation can be retrieved for both functions and
variables, and that a meaningful message is returned for symbols with no
documentation."
  :tags '(unit emacs)
  ;; Assert function documentation contains expected phrase
  (should (string-match-p "Return the car of LIST" (aj8/gptel-tool-read-documentation "car")))
  ;; Assert variable documentation contains expected phrase
  (should (string-match-p "List of directories to search for files to load" (aj8/gptel-tool-read-documentation "load-path")))
  ;; Assert missing symbol returns a 'no documentation' message
  (should (string-match-p "No documentation found" (aj8/gptel-tool-read-documentation "non-existent-symbol-xyz"))))

(ert-deftest test-aj8-read-function ()
  "Test `aj8/gptel-tool-read-function'.
Ensures that the source code for a specific function can be retrieved."
  :tags '(unit emacs)
  (unwind-protect
      (progn
        ;; Assert function source contains expected fragment or valid function representation
        (let ((result (aj8/gptel-tool-read-function "aj8/gptel-tool-read-documentation")))
          (should (or (string-match-p "(defun aj8/gptel-tool-read-documentation" result)
                      (string-match-p "#\\[" result))))
        ;; Error cases: undefined function
        ;; Mode 1: tool re-signals the error
        (let ((aj8/gptel-tool-return-error nil))
          ;; Assert missing function raises an error when re-signaling is enabled
          (should-error (aj8/gptel-tool-read-function "non-existent-function-xyz") :type 'error))
        ;; Mode 2: tool returns the error as a string
        (let ((aj8/gptel-tool-return-error t))
          (let ((result (aj8/gptel-tool-read-function "non-existent-function-xyz")))
            ;; Assert returned error message for missing function
            (should (string-equal
                     "tool: aj8_read_function: Error: Function 'non-existent-function-xyz' is not defined."
                     result)))))
    (when (get-buffer "project.el")
      (kill-buffer "project.el"))
    (when (get-buffer "project.el.gz")
      (kill-buffer "project.el.gz"))))

(ert-deftest test-aj8-read-library ()
  "Test `aj8/gptel-tool-read-library'.
Ensures that the source code for an entire library can be retrieved."
  :tags '(unit emacs)
  (unwind-protect
      (progn
        ;; Assert library source contains expected filename
        (should (string-match-p "project.el" (aj8/gptel-tool-read-library "project")))
        ;; Error cases: missing library
        ;; Mode 1: tool re-signals the error
        (let ((aj8/gptel-tool-return-error nil))
          ;; Assert missing library raises an error when re-signaling is enabled
          (should-error (aj8/gptel-tool-read-library "non-existent-library-xyz") :type 'error))
        ;; Mode 2: tool returns the error as a string
        (let ((aj8/gptel-tool-return-error t))
          (let ((result (aj8/gptel-tool-read-library "non-existent-library-xyz")))
            ;; Assert returned error message for missing library
            (should (string-equal
                     "tool: aj8_read_library: Library 'non-existent-library-xyz' not found."
                     result)))))
    (when (get-buffer "project.el")
      (kill-buffer "project.el"))
    (when (get-buffer "project.el.gz")
      (kill-buffer "project.el.gz"))))

(ert-deftest test-aj8-read-info-symbol ()
  "Test `aj8/gptel-tool-read-info-symbol'.
Verifies that Info manual content can be retrieved by symbol lookup."
  :tags '(unit emacs)
  ;; Assert Info lookup by symbol returns expected text
  (should (string-match-p "special form" (aj8/gptel-tool-read-info-symbol "defun")))

  ;; Error cases: unknown symbol for info lookup
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    (should-error (aj8/gptel-tool-read-info-symbol "non-existent-symbol-xyz") :type 'error))
  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((result (aj8/gptel-tool-read-info-symbol "non-existent-symbol-xyz")))
      (should (string-equal
               "tool: aj8_read_info_symbol: Not documented as a symbol: non-existent-symbol-xyz"
               result)))))

(ert-deftest test-aj8-read-info-node ()
  "Test `aj8/gptel-tool-read-info-node'.
Verifies that Info manual content can be retrieved by node lookup."
  :tags '(unit emacs)
  ;; Assert Info lookup by node returns expected text
  (should (string-match-p "defining a function" (aj8/gptel-tool-read-info-node "Defining Functions")))

  ;; Error cases: invalid node for info manual
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    (should-error (aj8/gptel-tool-read-info-node "Bogus Node 123") :type 'error))
  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((result (aj8/gptel-tool-read-info-node "Bogus Node 123")))
      (should (string-equal
               "tool: aj8_read_info_node: No such node or anchor: Bogus Node 123"
               result)))))

;;; 3.4. Category: Project

(ert-deftest test-aj8-project-get-root ()
  "Test `aj8/gptel-tool-project-get-root'.
Verifies that the project root can be determined correctly."
  :tags '(unit project)
  (with-temp-project
   (let ((root default-directory))
     ;; 1) Project root should match default-directory
     ;; Assert returned project root corresponds to the temporary project directory
     (should (string-equal (file-name-as-directory (aj8/gptel-tool-project-get-root))
                           (file-name-as-directory root)))))
  ;; Error cases: outside of any project
  ;; Mode 1: tool re-signals the error
  (let* ((tmpdir (make-temp-file "aj8-non-project" t)))
    (unwind-protect
        (let ((default-directory tmpdir))
          (let ((aj8/gptel-tool-return-error nil))
            (should-error (aj8/gptel-tool-project-get-root) :type 'error))
          ;; Mode 2: tool returns the error as a string
          (let ((aj8/gptel-tool-return-error t))
            (let ((res1 (aj8/gptel-tool-project-get-root)))
              (should (string-equal "tool: aj8_project_get_root: Not inside a project." res1)))))
      (when (file-directory-p tmpdir)
        (delete-directory tmpdir t)))))

(ert-deftest test-aj8-project-list-files ()
  "Test `aj8/gptel-tool-project-list-files'.
Verifies that project buffer listing works correctly with and without line counts.
The project buffer listing without counts includes project-relative paths.
The project buffer listing with counts includes entries that contain:
* the base name of the file
* the (nondirectory) filename
* a trailing \": N lines\" count"
  :tags '(unit project)
  (with-temp-project
   (let ((root default-directory))
     (let ((buf (find-file-noselect (expand-file-name "src/code.el"))))
       (with-current-buffer buf
         (let ((lines (split-string (aj8/gptel-tool-project-list-files t) "\n" t))
               (fname (file-name-nondirectory (buffer-file-name buf)))
               (rel (file-relative-name (buffer-file-name buf) root)))
           ;; 2) Without counts: the project-relative path should appear
           ;;    in the listing
           (should (string-match-p "src/code.el" (aj8/gptel-tool-project-list-files)))
           ;; Exact no-counts "NAME: PATH" entry exists
           (let* ((no-counts-lines (split-string (aj8/gptel-tool-project-list-files) "\n" t))
                  (expected (format "%s: %s" fname rel)))
             (should (member expected no-counts-lines)))
           ;; 3) With counts:
           ;; 3a) Exact line: "NAME: PATH (N lines).
           (should (member (format "%s: %s (%d lines)" fname rel 1) lines))
           ;; 4) Assert that the exact number of lines is reported.
           ;; All include-counts entries end with "(N lines)"
           (should (cl-every (lambda (s) (string-match-p "^[^:]+: .+ ([0-9]+ lines)$" s)) lines))
           (let ((expected (format "%s: %s (%d lines)" fname rel 1)))
             (should (member expected lines)))))
       (kill-buffer buf))))
  ;; Error cases: outside of any project
  ;; Mode 1: tool re-signals the error
  (let* ((tmpdir (make-temp-file "aj8-non-project" t)))
    (unwind-protect
        (let ((default-directory tmpdir))
          (let ((aj8/gptel-tool-return-error nil))
            (should-error (aj8/gptel-tool-project-list-files) :type 'error))
          ;; Mode 2: tool returns the error as a string
          (let ((aj8/gptel-tool-return-error t))
            (let ((res2 (aj8/gptel-tool-project-list-files)))
              (should (string-equal "tool: aj8_project_list_files: Not inside a project." res2)))))
      (when (file-directory-p tmpdir)
        (delete-directory tmpdir t)))))

(ert-deftest test-aj8-project-find-files-glob ()
  "Test `aj8/gptel-tool-project-find-files-glob'.
Verifies that project file finding with glob patterns works correctly."
  :tags '(unit project)
  (with-temp-project
   ;; Test find files glob
   (let* ((files-str (aj8/gptel-tool-project-find-files-glob "**/*.el"))
          (files (split-string files-str "\n" t)))
     ;; Assert the glob found the expected single file
     (should (= 1 (length files)))
     (should (string-match-p "src/code.el" (car files))))
   (let* ((files-str (aj8/gptel-tool-project-find-files-glob "*.txt"))
          (files (split-string files-str "\n" t)))
     ;; Assert the glob found the expected single text file
     (should (= 1 (length files)))
     (should (string-match-p "data.txt" (car files)))))
  ;; Error cases: outside of any project
  ;; Mode 1: tool re-signals the error
  (let* ((tmpdir (make-temp-file "aj8-non-project" t)))
    (unwind-protect
        (let ((default-directory tmpdir))
          (let ((aj8/gptel-tool-return-error nil))
            (should-error (aj8/gptel-tool-project-find-files-glob "**/*.el") :type 'error))
          ;; Mode 2: tool returns the error as a string
          (let ((aj8/gptel-tool-return-error t))
            (let ((res1 (aj8/gptel-tool-project-find-files-glob "**/*.el")))
              (should (string-equal "tool: aj8_project_find_files_glob: No project found in the current context." res1)))))
      (when (file-directory-p tmpdir)
        (delete-directory tmpdir t)))))

(ert-deftest test-aj8-project-search-regexp ()
  "Test `aj8/gptel-tool-project-search-regexp'.
Verifies that project content searching with regular expressions works correctly."
  :tags '(unit project)
  (with-temp-project
   ;; Test search content
   (when (or (executable-find "rg") (and (executable-find "git") (file-directory-p ".git")))
     ;; Assert exact output PATH:LINE:TEXT
     (let ((results (aj8/gptel-tool-project-search-regexp "some text data")))
       (should (string-equal results "data.txt:1:some text data")))
     ;; Assert exact output PATH:LINE:COLUMN:TEXT with include-columns
     (let ((results-with-col (aj8/gptel-tool-project-search-regexp "some text data" t)))
       (should (string-equal results-with-col "data.txt:1:1:some text data")))
     ;; No-match path should return an informative message
     (let* ((regexp "NO_MATCH_REGEX_12345")
            (nores (aj8/gptel-tool-project-search-regexp regexp)))
       (should (string-equal nores (format "No matches found for regexp: %s" regexp))))
     ;; Error path: invalid regexp should cause backend to return status > 1 and signal an error
     (should-error (aj8/gptel-tool-project-search-regexp "[") :type 'error)
     ;; Mode 2: tool returns the error as a string
     (let ((aj8/gptel-tool-return-error t))
       (let ((res (aj8/gptel-tool-project-search-regexp "[")))
         (should (string-match-p "^tool: aj8_project_search_content: Search command .* failed with status .* for regexp: \\[" res)))))
   (let ((res (aj8/gptel-tool-project-search-regexp "[")))
     ;; Mode 1: tool re-signals the error
     (let* ((tmpdir (make-temp-file "aj8-non-project" t)))
       (unwind-protect
           (let ((default-directory tmpdir))
             (let ((aj8/gptel-tool-return-error nil))
               (should-error (aj8/gptel-tool-project-search-regexp "x") :type 'error))
             ;; Mode 2: tool returns the error as a string
             (let ((aj8/gptel-tool-return-error t))
               (let ((res2 (aj8/gptel-tool-project-search-regexp "x")))
                 (should (string-equal "tool: aj8_project_search_content: Not inside a project." res2)))))
         (when (file-directory-p tmpdir)
           (delete-directory tmpdir t)))))))

;;
;;;; 4. Integration Tests (ert-deftest)
;;

;;; 4.1. Category: Tool Definition and Invocation
;;
;; This section tests the fundamental integrity of the tool system. It
;; ensures that all tools are correctly defined, registered, and can be
;; invoked through their function pointers. It also verifies that the tool
;; definitions meet the structural requirements and that error handling for
;; invalid arguments is working as expected.

(ert-deftest test-gptel-tools-registration ()
  "Verify that all Gptel tools are registered in `gptel-tools'.
This test checks that a predefined list of essential tool names exists
in the `gptel-tools' alist."
  :tags '(integration tools)
  (let ((expected-tools '("aj8_buffer_search_content"
                          "aj8_open_file_in_buffer"
                          ;; "aj8_read_buffer"
                          ;; "aj8_read_buffer_lines"
                          "aj8_read_buffer_lines_count"
                          "aj8_list_buffers"
                          "aj8_list_all_buffers"
                          "aj8_buffer_to_file"
                          "aj8_file_to_buffer"
                          "aj8_append_to_buffer"
                          "aj8_insert_in_buffer"
                          "aj8_replace_buffer"
                          "aj8_edit_buffer_string"
                          "aj8_replace_buffer_line"
                          "aj8_replace_buffer_lines"
                          "aj8_delete_buffer_string"
                          "aj8_delete_buffer_line"
                          "aj8_delete_buffer_lines"
                          "aj8_apply_buffer_string_edits"
                          "aj8_apply_buffer_string_edits_with_review"
                          "aj8_apply_buffer_line_edits"
                          "aj8_apply_buffer_line_edits_with_review"
                          ;; "aj8_create_file"
                          "aj8_read_documentation"
                          "aj8_read_function"
                          "aj8_read_library"
                          "aj8_read_info_symbol"
                          "aj8_read_info_node"
                          "aj8_project_get_root"
                          "aj8_project_list_files"
                          ;; "aj8_project_find_files"
                          "aj8_project_find_files_glob"
                          "aj8_project_search_regexp")))
    (dolist (tool-name expected-tools)
      ;; Assert the tool is registered in `gptel-tools'
      (should (cl-find-if (lambda (tool) (string-equal (gptel-tool-name tool) tool-name)) gptel-tools)))))

;; TODO: duplication?
(ert-deftest test-gptel-tools-json-schema-validation ()
  "Validate the structure of each `gptel-tool' definition.
Ensures that every registered tool definition has the required
properties, such as `:function` and `:description`, and that the
argument list `:args` is a valid list."
  :tags '(integration tools)
  (dolist (tool-def gptel-tools)
    (let ((tool-name (gptel-tool-name tool-def)))
      ;; Assert tool has a function field
      (should (gptel-tool-function tool-def))
      ;; Assert tool has a description
      (should (gptel-tool-description tool-def))

      ;; Assert args field is a list (or nil)
      (should (listp (gptel-tool-args tool-def))))))

(ert-deftest test-gptel-tools-function-callable ()
  "Verify that tool functions are defined and callable.

This test checks a subset of tools that require no arguments, ensuring
their associated functions can be called without error."
  :tags '(integration tools)
  (let ((no-arg-tools '("aj8_list_buffers"
                        "aj8_list_all_buffers"
                        "aj8_project_get_root"
                        "aj8_project_list_files")))
    ;; Test tools that don't require arguments
    (dolist (tool-name no-arg-tools)
      (let* ((tool-def (cl-find-if (lambda (tool) (string-equal (gptel-tool-name tool) tool-name)) gptel-tools))
             (func (gptel-tool-function tool-def)))
        ;; The tool function should be a callable function
        (should (functionp func))
        ;; Calling the function should not raise an error
        (should-not (condition-case nil
                        (progn (funcall func) nil)
                      (error t)))))))

(ert-deftest test-gptel-tools-via-json-call ()
  "Simulate calling Gptel tools via a JSON-like interface.
This test mimics how a Large Language Model (LLM) would call the tools
by invoking the tool's function with arguments directly. It verifies
both a query and a buffer modification tool."
  :tags '(integration tools json)
  (with-temp-buffer-with-content
   "*test-json-call*" "Hello World\nLine 2"
   ;; Test list all buffers tool
   (let* ((tool-def (cl-find "aj8_list_all_buffers" gptel-tools :key #'gptel-tool-name :test #'string-equal))
          (func (gptel-tool-function tool-def))
          (result (funcall func)))
     ;; Assert the tool returned a list
     (should (listp result))
     ;; Assert our test buffer is included in the result
     (should (member "*test-json-call*" result)))

   ;; Test edit buffer tool with JSON-like parameters
   (let* ((tool-def (cl-find "aj8_edit_buffer_string" gptel-tools :key #'gptel-tool-name :test #'string-equal))
          (func (gptel-tool-function tool-def))
          (result (funcall func "*test-json-call*" "World" "Gptel")))
     ;; Assert edit returned a success message
     (should (string-match-p "successfully" result))
     ;; Assert buffer content reflects the edit
     (with-current-buffer (get-buffer "*test-json-call*")
       (should (string-equal (buffer-string) "Hello Gptel\nLine 2"))))))

(ert-deftest test-gptel-tools-error-handling ()
  "Test that Gptel tools handle common errors gracefully.
Verifies that tools produce user-friendly error messages when given
invalid arguments, such as a non-existent buffer name or an invalid file
path."
  :tags '(integration tools errors)
  ;; Test with non-existent buffer
  (let* ((tool-def (cl-find "aj8_edit_buffer_string" gptel-tools :key #'gptel-tool-name :test #'string-equal))
         (func (gptel-tool-function tool-def)))
    ;; Calling edit on a missing buffer signals a helpful error
    ;; Assert an informative error message is produced
    (should (condition-case err
                (funcall func "*non-existent-buffer*" "old" "new")
              (error (string-match-p "Buffer.*not found" (error-message-string err)))))))

;;; 4.2. Category: LLM Tool Mock Simulation
;;
;; This section simulates the end-to-end process of an LLM calling a
;; tool. It uses mock LLM responses containing tool call requests to test if
;; `gptel' correctly parses these requests, invokes the appropriate tool
;; with the right arguments, and that the tools produce the expected side
;; effects (e.g., modifying a buffer or file).


;; (defun aj8/gptel-tool-test--run-with-mock-llm (tool-name args expected-pattern)
;;   "Simulate an LLM call to a Gptel tool and check the result.

;; This helper function looks up TOOL-NAME in `gptel-tools', applies ARGS
;; to its function, and asserts that the formatted result matches
;; EXPECTED-PATTERN."
;;   (let* ((tool-def (cl-find-if (lambda (tool) (string-equal (gptel-tool-name tool) tool-name)) gptel-tools))
;;          (func (gptel-tool-function tool-def))
;;          (result (apply func args)))
;;     (should (string-match-p expected-pattern (format "%s" result)))
;;     result))

(defun test-gptel-tools--mock-response (response function)
  "Mock `gptel-request' to return RESPONSE, and run FUNCTION.
The response is processed by `gptel--streaming-done-callback'."
  (let ((gptel-buffer (get-buffer-create "*gptel*")))
    (with-current-buffer gptel-buffer
      (let ((gptel-streaming nil))
        (cl-letf (((symbol-function 'gptel-request)
                   (lambda (&rest _)
                     (funcall gptel--streaming-callback response)
                     (funcall gptel--streaming-done-callback))))
          (funcall function))))))

(ert-deftest test-gptel-tools-llm-mock-buffers ()
  "Test buffer tools by simulating calls from an LLM."
  :tags '(integration tools mock buffers)
  (with-temp-buffer-with-content
   "*mock-test*" "Original content"
   (let ((gptel-buffer (get-buffer-create "*gptel*")))
     (with-current-buffer gptel-buffer
       ;; Test a read-only tool
       (erase-buffer)
       (let ((mock-response "{\"tool_calls\": [{\"name\": \"aj8_list_all_buffers\", \"arguments\": {}}]}"))
         (test-gptel-tools--mock-response mock-response (lambda () (gptel-send "dummy query")))
         ;; Assert the mock buffer name appears in the buffer
         (should (string-match-p "mock-test" (buffer-string))))
       ;; Test a modifying tool
       (erase-buffer)
       (let ((mock-response "{\"tool_calls\": [{\"name\": \"aj8_edit_buffer_string\", \"arguments\": {\"buffer-name\": \"*mock-test*\", \"old-string\": \"Original\", \"new-string\": \"Modified\"}}]}"))
         (test-gptel-tools--mock-response mock-response (lambda () (gptel-send "dummy query")))
         ;; Check that the target buffer was modified
         (with-current-buffer "*mock-test*"
           ;; Assert the target buffer was modified
           (should (string-equal (buffer-string) "Modified content")))
         ;; Check that the gptel buffer contains the tool result
         (with-current-buffer gptel-buffer
           ;; Gptel buffer should record the tool result message
           (should (string-match-p "Tool `aj8_edit_buffer_string` returned: String replaced successfully." (buffer-string)))))))))

(ert-deftest test-gptel-tools-llm-mock-project ()
  "Test project tools by simulating calls from an LLM."
  :tags '(integration tools mock project)
  (with-temp-project
   (let ((gptel-buffer (get-buffer-create "*gptel*")))
     (with-current-buffer gptel-buffer
       (erase-buffer)
       (let ((mock-response "{\"tool_calls\": [{\"name\": \"aj8_project_get_root\", \"arguments\": {}}]}"))
         (test-gptel-tools--mock-response mock-response (lambda () (gptel-send "dummy query")))
         ;; Assert mock project-get-root included project dir name
         (should (string-match-p "ert-test-project" (buffer-string))))

       (erase-buffer)
       (let ((mock-response "{\"tool_calls\": [{\"name\": \"aj8_project_find_files_glob\", \"arguments\": {\"pattern\": \"**/*.el\"}}]}"))
         (test-gptel-tools--mock-response mock-response (lambda () (gptel-send "dummy query")))
         ;; Assert mock project-find-files listed the project file
         (should (string-match-p "src/code.el" (buffer-string))))))))

(ert-deftest test-gptel-tools-llm-mock-emacs ()
  "Test Emacs introspection tools by simulating calls from an LLM."
  :tags '(integration tools mock emacs)
  (let ((gptel-buffer (get-buffer-create "*gptel*")))
    (with-current-buffer gptel-buffer
      (erase-buffer)
      ;; Test read_documentation
      (let ((mock-response "{\"tool_calls\": [{\"name\": \"aj8_read_documentation\", \"arguments\": {\"symbol-name\": \"car\"}}]}"))
        (test-gptel-tools--mock-response mock-response (lambda () (gptel-send "dummy query")))
        ;; Assert mock documentation read included expected phrase
        (should (string-match-p "Return the car of LIST" (buffer-string))))
      (erase-buffer)
      ;; Test read_function
      (let ((mock-response "{\"tool_calls\": [{\"name\": \"aj8_read_function\", \"arguments\": {\"function-name\": \"gptel-send\"}}]}"))
        (test-gptel-tools--mock-response mock-response (lambda () (gptel-send "dummy query")))
        ;; Assert mock function read included defun for gptel-send
        (should (string-match-p "(defun gptel-send" (buffer-string)))))))

;;; 4.3. Category: User Workflow Simulation
;;
;; This section tests common sequences of tool calls that emulate a user's
;; workflow for a specific task. Unlike the LLM simulations, these tests
;; call the tool functions directly to verify that combinations of tools
;; work together correctly to achieve a larger goal, such as refactoring
;; code in a buffer or managing project files.

(ert-deftest test-gptel-tools-workflow-buffers ()
  "Simulate a workflow using buffer tools."
  :tags '(integration workflow buffers)
  ;; Test on a buffer not visiting a file
  (with-temp-buffer-with-content
   "*test-buffer*" "initial content"
   (let ((buffer-name "*test-buffer*"))
     ;; list-all-buffers, list-buffers
     ;; Assert buffer appears in the 'all' listing
     (should (member buffer-name (split-string (aj8/gptel-tool-list-all-buffers) "\n" t)))
     ;; Assert buffer not in file-backed-only listing
     (should-not (member buffer-name (split-string (aj8/gptel-tool-list-buffers) "\n" t)))
     ;; Assert converting a non-file buffer to a file errors
     (should-error (aj8/gptel-tool-buffer-to-file buffer-name))

     ;; append-to-buffer
     ;; Append text and verify
     (aj8/gptel-tool-append-to-buffer buffer-name "\nAppended")
     (should (string-equal (with-current-buffer buffer-name (buffer-string)) "initial content\nAppended"))

     ;; insert-in-buffer
     ;; Insert at start and verify ordering
     (aj8/gptel-tool-insert-in-buffer buffer-name "Prepended\n" 1)
     (should (string-equal (with-current-buffer buffer-name (buffer-string)) "Prepended\ninitial content\nAppended"))

     ;; edit-buffer
     ;; Replace substring and verify content
     (aj8/gptel-tool-edit-buffer-string buffer-name "initial" "original")
     (should (string-equal (with-current-buffer buffer-name (buffer-string)) "Prepended\noriginal content\nAppended"))

     ;; modify-buffer
     ;; Replace entire buffer and verify
     (aj8/gptel-tool-modify-buffer buffer-name "new content")
     (should (string-equal (with-current-buffer buffer-name (buffer-string)) "new content"))

     ;; read-buffer-lines-count
     ;; Reading the buffer returns the new content
     (should (string-equal (aj8/gptel-tool-read-buffer-lines-count buffer-name) "new content"))))

  ;; Test on a buffer visiting a file
  (with-temp-file-with-content
   test-file "file content"
   (let ((buffer (find-file-noselect test-file)))
     (unwind-protect
         (progn
           (should (member (buffer-name buffer) (aj8/gptel-tool-list-buffers)))
           (should (string-equal (aj8/gptel-tool-file-to-buffer test-file) (buffer-name buffer)))
           (should (string-equal (aj8/gptel-tool-buffer-to-file (buffer-name buffer)) (expand-file-name test-file))))
       (kill-buffer buffer)))))

(ert-deftest test-gptel-tools-workflow-project ()
  "Simulate a workflow using project tools."
  :tags '(integration workflow project)
  (with-temp-project
   ;; get-root
   (let ((root (aj8/gptel-tool-project-get-root)))
     (should (string-match-p "ert-test-project" root)))

   ;; find-files-glob
   (let* ((files-str (aj8/gptel-tool-project-find-files-glob "**/*.el"))
          (files (split-string files-str "\n" t)))
     (should (= 1 (length files)))
     (should (string-match-p "src/code.el" (car files))))

   ;; search-content
   (when (or (executable-find "rg") (and (executable-find "git") (file-directory-p ".git")))
     (let ((results (aj8/gptel-tool-project-search-regexp "hello")))
       (should (string-match-p "src/code.el:1:.*hello" results))))

   ;; get-open-buffers
   (let ((buf (find-file-noselect (expand-file-name "src/code.el"))))
     (unwind-protect
         (let ((open-buffers (aj8/gptel-tool-project-list-files)))
           (should (string-match-p "code.el" open-buffers))
           (should (string-match-p "src/code.el" open-buffers)))
       (kill-buffer buf)))))

(ert-deftest test-gptel-tools-workflow-emacs ()
  "Simulate a workflow using Emacs introspection tools."
  :tags '(integration workflow emacs)
  ;; read-documentation
  ;; Documentation lookup returns expected snippet
  (should (string-match-p "car of LIST"
                          (aj8/gptel-tool-read-documentation "car")))

  ;; read-function
  ;; Function source retrieval returns expected fragments
  (should (string-match-p "(defun project-current"
                          (aj8/gptel-tool-read-function "project-current")))
  (should (string-match-p "built-in primitive"
                          (aj8/gptel-tool-read-function "car")))

  ;; read-library
  ;; Library source retrieval includes expected file name
  (should (string-match-p "subr.el"
                          (aj8/gptel-tool-read-library "subr")))

  ;; read-info-symbol
  ;; Info symbol lookup returns expected phrase
  (should (string-match-p "special form"
                          (aj8/gptel-tool-read-info-symbol "defun")))

  ;; read-info-node
  ;; Info node lookup returns expected phrase
  (should (string-match-p "function definition"
                          (aj8/gptel-tool-read-info-node "Defining Functions"))))

(ert-deftest test-gptel-tools-multi-buffer-string-edits ()
  "Test complex, multi-part string editing scenarios.
Simulates an LLM performing a refactoring task that requires making
several related substring edits in a single buffer, using
`aj8/gptel-tool-apply-buffer-string-edits' to apply them all at once."
  :tags '(integration workflow buffer)
  (with-temp-buffer-with-content
   "*complex-edit-test*"
   "function calculateSum(a, b) {\n    return a + b;\n}\n\nfunction calculateProduct(a, b) {\n    return a * b;\n}\n\nfunction main() {\n    console.log('Starting calculations');\n    let sum = calculateSum(5, 3);\n    let product = calculateProduct(4, 6);\n    console.log('Results:', sum, product);\n}"

   (let* ((edit-tool (cl-find "aj8_apply_buffer_string_edits" gptel-tools :key #'gptel-tool-name :test #'string-equal))
          (edit-func (gptel-tool-function edit-tool))
          (edits '((:line-number 1 :old-string "calculateSum" :new-string "addNumbers")
                   (:line-number 5 :old-string "calculateProduct" :new-string "multiplyNumbers")
                   (:line-number 11 :old-string "calculateSum" :new-string "addNumbers")
                   (:line-number 12 :old-string "calculateProduct" :new-string "multiplyNumbers"))))

     (funcall edit-func "*complex-edit-test*" edits)

     ;; Verify all changes were applied
     (let ((content (buffer-string)))
       ;; Assert new function names appear
       (should (string-match-p "function addNumbers" content))
       (should (string-match-p "function multiplyNumbers" content))
       ;; Assert calls updated to new names
       (should (string-match-p "addNumbers(5, 3)" content))
       (should (string-match-p "multiplyNumbers(4, 6)" content))
       ;; Assert old names were removed
       (should-not (string-match-p "calculateSum" content))
       (should-not (string-match-p "calculateProduct" content))))))

(ert-deftest test-gptel-tools-multi-buffer-line-edits ()
  "Test complex, multi-part line editing scenarios.
Simulates an LLM performing a refactoring task that requires making
several related full-line edits in a single buffer, using
`aj8/gptel-tool-apply-buffer-line-edits' to apply them all at once."
  :tags '(integration workflow buffer)
  (with-temp-buffer-with-content
   "*complex-edit-test*"
   "function calculateSum(a, b) {\n    return a + b;\n}\n\nfunction calculateProduct(a, b) {\n    return a * b;\n}\n\nfunction main() {\n    console.log('Starting calculations');\n    let sum = calculateSum(5, 3);\n    let product = calculateProduct(4, 6);\n    console.log('Results:', sum, product);\n}"

   (let* ((edit-tool (cl-find "aj8_apply_buffer_line_edits" gptel-tools :key #'gptel-tool-name :test #'string-equal))
          (edit-func (gptel-tool-function edit-tool))
          (edits '((:line-number 1 :old-string "function calculateSum(a, b) {" :new-string "function addNumbers(a, b) {")
                   (:line-number 5 :old-string "function calculateProduct(a, b) {" :new-string "function multiplyNumbers(a, b) {")
                   (:line-number 11 :old-string "    let sum = calculateSum(5, 3);" :new-string "    let sum = addNumbers(5, 3);")
                   (:line-number 12 :old-string "    let product = calculateProduct(4, 6);" :new-string "    let product = multiplyNumbers(4, 6);"))))

     (funcall edit-func "*complex-edit-test*" edits)

     (let ((content (buffer-string)))
       ;; Assert new function names expected after line edits
       (should (string-match-p "function addNumbers" content))
       (should (string-match-p "function multiplyNumbers" content))
       ;; Assert updated call sites
       (should (string-match-p "addNumbers(5, 3)" content))
       (should (string-match-p "multiplyNumbers(4, 6)" content))
       ;; Assert original names no longer appear
       (should-not (string-match-p "calculateSum" content))
       (should-not (string-match-p "calculateProduct" content))))))

;;
;;;; 5. Test Runner Functions (interactive)
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
                          '("unit" "buffers" "emacs" "project" "review"
                            "integration" "tools" "json" "errors" "mock"
                            "workflow")
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
;;;; 6. Manual Testing & Utility Functions (interactive)
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

(provide 'gptel-test)
