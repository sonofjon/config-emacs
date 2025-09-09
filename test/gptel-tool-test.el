;;; gptel-test.el --- Tests for GPTel tools -*- lexical-binding: t; -*-

(message "Running gptel tool tests")

;;;; 1. Requirements

;; ERT, the Emacs Lisp Regression Testing tool
(require 'ert)

;; Common CL utilities (cl-defun, cl-letf, cl-every, etc.)
(require 'cl-lib)

;; Libraries required for these tests
(require 'gptel)
(require 'aj8-gptel)
(require 'aj8-lisp)

;; Ensure straight quotes in error messages that we match against
(setq text-quoting-style 'straight)

;;;; 2. Test Helpers

;;; 2.1 Macros

(defmacro with-temp-buffer-with-content (buffer-name content &rest body)
  "Execute BODY in a temporary buffer containing initial CONTENT.
The buffer is named BUFFER-NAME.  This macro ensures the buffer is
killed after BODY executes, even in case of an error."
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
The file's path is bound to FILE-VAR.  This macro ensures both the file
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
dummy files to simulate a real project.  It runs the BODY forms with the
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

(cl-defun aj8--assert-tool-error (result &key tool-name details-str
                                         details-regex details-predicate
                                         details-nonempty)
  "Assert that RESULT contains a properly formatted GPTel tool error message.

This function validates that RESULT starts with the expected \"tool:
TOOL-NAME:\" format and optionally validates additional error content
using the provided validation parameters.

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
    ;; Assert basic header prefix check
    (should (string-prefix-p header result))

    ;; If no details are requested, return
    (unless (or details-str details-regex details-predicate details-nonempty)
      (cl-return-from aj8--assert-tool-error t))

    ;; Assert details-predicate
    (when details-predicate
      (should (funcall details-predicate result))
      (cl-return-from aj8--assert-tool-error t))

    ;; Assert details-str
    (when details-str
      (should (string-match-p (regexp-quote details-str) result))
      (cl-return-from aj8--assert-tool-error t))

    ;; Assert details-regex
    (when details-regex
      (should (string-match-p details-regex result))
      (cl-return-from aj8--assert-tool-error t))

    ;; Assert non-empty details
    (when details-nonempty
      (let ((after (substring result (min (length result) (or (string-match-p "Error" result) 0)))))
        (should (> (length (string-trim after)) 0))))))

;;;; 3. Unit Tests (ert-deftest)

;;; 3.1. Category: Buffers

(ert-deftest test-aj8-open-file-in-buffer ()
  "Test `aj8/gptel-tool-open-file-in-buffer'."
  :tags '(unit buffers)

  ;; === SUCCESS CASES ===

  ;; Test basic file opening functionality:
  (with-temp-file-with-content
   test-file "test content"
   ;; Assert that no buffer exists before opening the file
   (should (null (get-file-buffer test-file)))
   (aj8/gptel-tool-open-file-in-buffer test-file)
   ;; Assert that a file buffer is created and alive after opening
   (should (buffer-live-p (get-file-buffer test-file))))

  ;; === ERROR CASES ===

  ;; Test error handling for non-existent files:
  (let* ((tmp (make-temp-file "aj8-nonexistent-file-")))
    (unwind-protect
        (progn
          (delete-file tmp)
          ;; Mode 1: tool re-signals the error
          (let ((aj8/gptel-tool-return-error nil))
            ;; Assert that an error is signaled when opening missing file
            (should-error (aj8/gptel-tool-open-file-in-buffer tmp) :type 'error))
          ;; Mode 2: tool returns the error as a string
          (let ((aj8/gptel-tool-return-error t))
            (let ((result (aj8/gptel-tool-open-file-in-buffer tmp)))
              ;; Assert that the error message contains expected file path
              (should (string-equal
                       (format "tool: aj8_open_file_in_buffer: Error: No such file: %s" tmp)
                       result)))))
      (when (file-exists-p tmp)
        (delete-file tmp))))

  ;; Test error handling for directory paths:
  (let ((dir (make-temp-file "aj8-temp-dir-" t)))
    (unwind-protect
        (progn
          ;; Mode 1: tool re-signals the error
          (let ((aj8/gptel-tool-return-error nil))
            ;; Assert that an error is signaled when opening directory as file
            (should-error (aj8/gptel-tool-open-file-in-buffer dir) :type 'error))
          ;; Mode 2: tool returns the error as a string
          (let ((aj8/gptel-tool-return-error t))
            (let ((result (aj8/gptel-tool-open-file-in-buffer dir)))
              ;; Assert that the error message indicates directory is not a file
              (should (string-equal
                       (format "tool: aj8_open_file_in_buffer: Error: '%s' is a directory." dir)
                       result)))))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest test-aj8-buffer-search-regexp ()
  "Test `aj8/gptel-tool-buffer-search-regexp'."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-buffer-search*" "line 1\ntest content here\nline 3"

   ;; === SUCCESS CASES ===

   ;; Test search results:
   (let ((result (aj8/gptel-tool-buffer-search-regexp "*test-buffer-search*" "content")))
     ;; Assert that search without columns returns results in LINE:TEXT format
     (should (string-equal result "2:test content here")))

   ;; Test search results with columns:
   (let ((result-with-col (aj8/gptel-tool-buffer-search-regexp "*test-buffer-search*" "content" t)))
     ;; Assert that search with columns returns results in LINE:COLUMN:TEXT format
     (should (string-equal result-with-col "2:5:test content here")))

   ;; Test no match case:
   (let* ((regexp "NO_MATCH_REGEX_12345")
          (nores (aj8/gptel-tool-buffer-search-regexp "*test-buffer-search*" regexp)))
     ;; Assert that no-match case returns the expected message
     (should (string-equal nores (format "No matches found for regexp: %s" regexp))))

   ;; === ERROR CASES ===

   ;; Test error handling for non-existent buffer:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled when searching missing buffer
     (should-error (aj8/gptel-tool-buffer-search-regexp "*non-existent-buffer*" "test") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-buffer-search-regexp "*non-existent-buffer*" "test")))
       ;; Assert that the error message matches expected format
       (should (string-equal
                "tool: aj8_buffer_search_regexp: Error: Buffer '*non-existent-buffer*' does not exist"
                result))))

   ;; Test invalid regexp error handling:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled for invalid regexp
     (should-error (aj8/gptel-tool-buffer-search-regexp "*test-buffer-search*" "[invalid") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-buffer-search-regexp "*test-buffer-search*" "[invalid")))
       ;; Assert that the returned error message matches expected format
       (should (string-equal
                "tool: aj8_buffer_search_regexp: Error: Invalid regexp: \"Unmatched [ or [^\""
                result))))))

;; (ert-deftest test-aj8-read-buffer-lines ()
;;   "Test `aj8/gptel-tool-read-buffer-lines'."
;;   :tags '(unit buffers)
;;   (with-temp-buffer-with-content
;;    "*test-read-buffer*" "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"

;;    ;; Assert reading the full buffer returns all lines
;;    (should (string-equal (aj8/gptel-tool-read-buffer-lines "*test-read-buffer*")
;;                          "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"))

;;    ;; Assert reading a middle section returns the expected lines
;;    (should (string-equal (aj8/gptel-tool-read-buffer-lines "*test-read-buffer*" 2 4)
;;                          "Line 2\nLine 3\nLine 4"))

;;    ;; Assert reading from the start returns the first N lines
;;    (should (string-equal (aj8/gptel-tool-read-buffer-lines "*test-read-buffer*" nil 2)
;;                          "Line 1\nLine 2"))

;;    ;; Assert reading to the end returns the last lines
;;    (should (string-equal (aj8/gptel-tool-read-buffer-lines "*test-read-buffer*" 4)
;;                          "Line 4\nLine 5"))

;;    ;; Test handling of max number of lines:
;;    (let* ((n (1+ aj8/gptel-tool-max-lines))
;;           (content "")
;;           (first-n ""))
;;      (dotimes (i n)
;;        (setq content (concat content (format "Line %d\n" (1+ i))))
;;        (when (< i aj8/gptel-tool-max-lines)
;;          (setq first-n (concat first-n (format "Line %d\n" (1+ i))))))
;;      (setq content (replace-regexp-in-string "\n\\'" "" content))
;;      (setq first-n (replace-regexp-in-string "\n\\'" "" first-n))

;;      ;; Test current buffer update with max+1 lines content:
;;      (erase-buffer)
;;      (insert content)

;;      ;; Test error validation line limits:
;;      ;; Mode 1: tool re-signals the error
;;      (let ((aj8/gptel-tool-return-error nil))
;;        ;; Assert error when total number of lines > MAX
;;        (should-error (aj8/gptel-tool-read-buffer-lines "*test-read-buffer*") :type 'error)
;;        ;; Assert error when requested length > MAX
;;        (should-error (aj8/gptel-tool-read-buffer-lines "*test-read-buffer*" 1 n) :type 'error)
;;        ;; Assert error when START < 1
;;        (should-error (aj8/gptel-tool-read-buffer-lines "*test-read-buffer*" 0 2) :type 'error)
;;        ;; Assert error when START > total number of lines
;;        (should-error (aj8/gptel-tool-read-buffer-lines "*test-read-buffer*" (1+ (count-lines (point-min) (point-max)))) :type 'error)))))

(ert-deftest test-aj8-read-buffer-lines-count ()
  "Test `aj8/gptel-tool-read-buffer-lines-count'."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-read-buffer*" "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"

   ;; === SUCCESS CASES ===

   ;; Assert that reading the full buffer returns all the lines
   (should (string-equal (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" 1 5)
                         "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"))
   ;; Assert that reading a middle section returns the expected lines
   (should (string-equal (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" 2 3)
                         "Line 2\nLine 3\nLine 4"))
   ;; Assert that reading from the start returns the first N lines
   (should (string-equal (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" 1 2)
                         "Line 1\nLine 2"))
   ;; Assert that reading to the end returns the last lines
   (should (string-equal (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" 4 2)
                         "Line 4\nLine 5"))

   ;; === ERROR CASES ===

   ;; Test non-existent buffer errors:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled for non-existent buffer
     (should-error (aj8/gptel-tool-read-buffer-lines-count "*non-existent-buffer*") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-read-buffer-lines-count "*non-existent-buffer*")))
       ;; Assert that the returned error message matches expected format
       (should (string-equal
                "tool: aj8_read_buffer_lines_count: Error: Buffer '*non-existent-buffer*' not found."
                result))))

   ;; Test handling of max number of lines:
   (let* ((n (1+ aj8/gptel-tool-max-lines))
          (content "")
          (first-n ""))
     (dotimes (i n)
       (setq content (concat content (format "Line %d\n" (1+ i))))
       (when (< i aj8/gptel-tool-max-lines)
         (setq first-n (concat first-n (format "Line %d\n" (1+ i))))))
     (setq content (replace-regexp-in-string "\n\\'" "" content))
     (setq first-n (replace-regexp-in-string "\n\\'" "" first-n))

     ;; Test current buffer update with max+1 lines content:
     (erase-buffer)
     (insert content)

     ;; Test error validation line limits:
     ;; Mode 1: tool re-signals the error
     (let ((aj8/gptel-tool-return-error nil))
       ;; Assert that an error is signaled when COUNT > MAX
       (should-error (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" 1 n) :type 'error)
       ;; Assert that an error is signaled when START < 1
       (should-error (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" 0 2) :type 'error)
       ;; Assert that an error is signaled when START > total number of lines
       (should-error (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" (1+ (count-lines (point-min) (point-max))) 1) :type 'error))
     ;; Mode 2: tool returns the error as a string for line validation errors
     (let ((aj8/gptel-tool-return-error t))
       ;; Assert that a COUNT > MAX error is signaled in return-string mode
       (let ((result (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" 1 n)))
         (should (string-equal
                  (format "tool: aj8_read_buffer_lines_count: Error: Requested COUNT (%d) exceeds maximum allowed (%d)." n aj8/gptel-tool-max-lines)
                  result)))
       ;; Assert that a START < 1 error is signaled in return-string mode
       (let ((result (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" 0 2)))
         (should (string-equal
                  "tool: aj8_read_buffer_lines_count: Error: START-LINE must be >= 1"
                  result)))
       ;; Assert that a START > total number of lines error is signaled in return-string mode
       (let ((result (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" (1+ (count-lines (point-min) (point-max))) 1)))
         (should (string-equal
                  (format "tool: aj8_read_buffer_lines_count: Error: START-LINE (%d) exceeds buffer length (%d)."
                          (1+ (count-lines (point-min) (point-max)))
                          (count-lines (point-min) (point-max)))
                  result))))

     ;; Test line range reading:
     ;; Assert that an explicit request for first MAX lines should succeed:
     (should (string-equal (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" 1 aj8/gptel-tool-max-lines) first-n))
     ;; Assert that the default COUNT (nil) is treated as MAX and should return first N lines
     (should (string-equal (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*") first-n))
     ;; Assert that requesting COUNT == MAX should return first MAX lines
     (should (string-equal (aj8/gptel-tool-read-buffer-lines-count "*test-read-buffer*" 1 aj8/gptel-tool-max-lines) first-n)))))

(ert-deftest test-aj8-list-buffers ()
  "Test `aj8/gptel-tool-list-buffers'."
  :tags '(unit buffers)
  (with-temp-file-with-content
   tmp-file "file content"
   (find-file-noselect tmp-file)
   (with-temp-buffer-with-content
    "*non-file-buffer*" "some content"

    ;; === SUCCESS CASES ===

    ;; Test file buffer listing:
    (let ((buffers (split-string (aj8/gptel-tool-list-buffers) "\n" t)))
      ;; Assert that the file-backed buffers appear in the "NAME: PATH" format
      (let* ((name (buffer-name (get-file-buffer tmp-file)))
             (path tmp-file)
             (expected (format "%s: %s" name path)))
        (should (member expected buffers)))
      ;; Assert that the non-file buffer is excluded from listing
      (should-not (member "*non-file-buffer*" buffers)))
    ;; Test include-counts option:
    (let ((buffers (split-string (aj8/gptel-tool-list-buffers t) "\n" t)))
      ;; Assert that all entries use the "NAME: PATH (N lines)" format
      (should (cl-every (lambda (s) (string-match-p "^[^:]+: .+ ([0-9]+ lines)$" s)) buffers))
      ;; Assert that the file-backed buffer shows the correct line count
      (let* ((name (buffer-name (get-file-buffer tmp-file)))
             (path tmp-file)
             (expected (format "%s: %s (%d lines)" name path 1)))
        (should (member expected buffers)))
      ;; Assert that the non-file buffer remains excluded with counts
      (should-not (member "*non-file-buffer*" buffers))))))

(ert-deftest test-aj8-list-all-buffers ()
  "Test `aj8/gptel-tool-list-all-buffers'."
  :tags '(unit buffers)
  (with-temp-file-with-content
   tmp-file "file content"
   (find-file-noselect tmp-file)
   (with-temp-buffer-with-content
    "*non-file-buffer*" "some content"

    ;; === SUCCESS CASES ===

    ;; Test file buffer listing:
    (let ((buffers (split-string (aj8/gptel-tool-list-all-buffers) "\n" t)))
      ;; Assert that the non-file buffer appears in the all-buffers listing
      (should (member "*non-file-buffer*" buffers))
      ;; Assert that the file-backed buffer appears in the "NAME: PATH" format
      (let* ((name (buffer-name (get-file-buffer tmp-file)))
             (path tmp-file)
             (expected (format "%s: %s" name path)))
        (should (member expected buffers))))
    ;; Test include-counts option:
    (let ((buffers (split-string (aj8/gptel-tool-list-all-buffers t) "\n" t)))
      ;; Assert that all entries use the proper count format
      (should (cl-every (lambda (s) (string-match-p "^.+\\(: .+\\)? ([0-9]+ lines)$" s)) buffers))
      ;; Assert that the non-file buffer shows the "NAME (N lines)" format
      (let* ((name "*non-file-buffer*")
             (expected (format "%s (%d lines)" name 1)))
        (should (member expected buffers)))
      ;; Assert that the file-backed buffer shows the "NAME: PATH (N lines)" format
      (let* ((name (buffer-name (get-file-buffer tmp-file)))
             (path tmp-file)
             (expected (format "%s: %s (%d lines)" name path 1)))
        (should (member expected buffers)))))))

(ert-deftest test-aj8-buffer-to-file ()
  "Test `aj8/gptel-tool-buffer-to-file'."
  :tags '(unit buffers)
  (with-temp-file-with-content
   test-file "content"

   ;; === SUCCESS CASES ===

   ;; Test buffer name to file path conversion:
   (let ((buffer (find-file-noselect test-file)))
     ;; Assert that the buffer name converts to the expected file path
     (should (string-equal (aj8/gptel-tool-buffer-to-file (buffer-name buffer))
                           (expand-file-name test-file)))

     ;; === ERROR CASES ===

     ;; Test error handling for non-file buffers:
     ;; Mode 1: tool re-signals the error
     (let ((aj8/gptel-tool-return-error nil))
       ;; Assert that an error is signaled when converting non-file buffer
       (should-error (aj8/gptel-tool-buffer-to-file "*scratch*") :type 'error))
     ;; Mode 2: tool returns the error as a string
     (let ((aj8/gptel-tool-return-error t))
       (let ((result (aj8/gptel-tool-buffer-to-file "*scratch*")))
         ;; Assert that the error message describes buffer not associated with file
         (should (string-equal
                  "tool: aj8_buffer_to_file: Error: Buffer '*scratch*' not found or not associated with a file."
                  result)))))))

(ert-deftest test-aj8-file-to-buffer ()
  "Test `aj8/gptel-tool-file-to-buffer'."
  :tags '(unit buffers)
  (with-temp-file-with-content
   test-file "content"

   ;; === SUCCESS CASES ===

   ;; Test file path to buffer name conversion:
   (let ((buffer (find-file-noselect test-file)))
     ;; Assert that the file path converts to the expected buffer name
     (should (string-equal (aj8/gptel-tool-file-to-buffer test-file)
                           (buffer-name buffer)))

     ;; === ERROR CASES ===

     ;; Test error handling for non-existent files:
     ;; Mode 1: tool re-signals the error
     (let ((aj8/gptel-tool-return-error nil))
       ;; Assert that an error is signaled when converting non-existent file
       (should-error (aj8/gptel-tool-file-to-buffer "/non/existent/file.tmp") :type 'error))
     ;; Mode 2: tool returns the error as a string
     (let ((aj8/gptel-tool-return-error t))
       (let ((result (aj8/gptel-tool-file-to-buffer "/non/existent/file.tmp")))
         ;; Assert that the error message describes no buffer visiting file
         (should (string-equal
                  (format "tool: aj8_file_to_buffer: Error: No buffer is visiting the file '%s'." "/non/existent/file.tmp")
                  result)))))))

(ert-deftest test-aj8-append-to-buffer ()
  "Test `aj8/gptel-tool-append-to-buffer'."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-append*" "Line 1\nLine 3"

   ;; === SUCCESS CASES ===

   ;; Test basic append functionality:
   (aj8/gptel-tool-append-to-buffer "*test-append*" "\nLine 4")
   ;; Assert that the appended content appears at the end of the buffer
   (should (string-equal (buffer-string) "Line 1\nLine 3\nLine 4"))

   ;; === ERROR CASES ===

   ;; Test error handling for non-existent buffers:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled when appending to missing buffer
     (should-error (aj8/gptel-tool-append-to-buffer "*nope*" "text") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-append-to-buffer "*nope*" "text")))
       ;; Assert that the error message describes buffer not found
       (should (string-equal
                "tool: aj8_append_to_buffer: Error: Buffer '*nope*' not found."
                result))))))

(ert-deftest test-aj8-insert-in-buffer ()
  "Test `aj8/gptel-tool-insert-in-buffer'."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-insert*" "Line 1\nLine 3"

   ;; === SUCCESS CASES ===

   ;; Test basic insertion functionality:
   (aj8/gptel-tool-insert-in-buffer "*test-insert*" "Line 2\n" 2)
   ;; Assert that the inserted content appears at the specified position
   (should (string-equal (buffer-string) "Line 1\nLine 2\nLine 3"))

   ;; Test line number validation errors:
   ;; Mode 1: tool re-signals the error for invalid line numbers
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled for line number 0
     (should-error (aj8/gptel-tool-insert-in-buffer "*test-insert*" "X" 0) :type 'error)
     ;; Assert that an error is signaled for line number beyond buffer
     (should-error (aj8/gptel-tool-insert-in-buffer "*test-insert*" "Y" 999) :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     ;; Assert that the error message is correct for line number 0
     (let ((result (aj8/gptel-tool-insert-in-buffer "*test-insert*" "X" 0)))
       (should (string-equal
                "tool: aj8_insert_in_buffer: Error: LINE-NUMBER must be >= 1"
                result)))
     ;; Assert that the error message is correct for line number beyond buffer
     (let ((result (aj8/gptel-tool-insert-in-buffer "*test-insert*" "Y" 999)))
       (should (string-equal
                (format "tool: aj8_insert_in_buffer: Error: LINE-NUMBER (999) exceeds buffer length (%d)."
                        (with-current-buffer (get-buffer "*test-insert*")
                          (count-lines (point-min) (point-max))))
                result))))

   ;; === ERROR CASES ===

   ;; Test error handling for non-existent buffers:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled when inserting into missing buffer
     (should-error (aj8/gptel-tool-insert-in-buffer "*nope*" "text" 1) :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-insert-in-buffer "*nope*" "text" 1)))
       ;; Assert that the error message describes buffer not found
       (should (string-equal
                "tool: aj8_insert_in_buffer: Error: Buffer '*nope*' not found."
                result))))))

(ert-deftest test-aj8-replace-buffer ()
  "Test `aj8/gptel-tool-replace-buffer'."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-replace*" "Line 1\nLine 3"

   ;; === SUCCESS CASES ===

   ;; Test basic replacement functionality:
   (aj8/gptel-tool-replace-buffer "*test-replace*" "New Content")
   ;; Assert that the buffer content is replaced entirely
   (should (string-equal (buffer-string) "New Content"))

   ;; === ERROR CASES ===

   ;; Test error handling for non-existent buffers:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled when replacing missing buffer
     (should-error (aj8/gptel-tool-replace-buffer "*nope*" "content") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-replace-buffer "*nope*" "content")))
       ;; Assert that the error message describes buffer not found
       (should (string-equal
                "tool: aj8_replace_buffer: Error: Buffer '*nope*' not found."
                result))))))

(ert-deftest test-aj8-edit-buffer-string ()
  "Test `aj8/gptel-tool-edit-buffer-string'."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-edit*" "hello world\nhello universe"

   ;; === SUCCESS CASES ===

   ;; Test basic string replacement functionality:
   (aj8/gptel-tool-edit-buffer-string "*test-edit*" "world" "emacs")
   ;; Assert that string replacement occurs for a unique match
   (should (string-equal (buffer-string) "hello emacs\nhello universe"))

   ;; Test error handling for missing strings:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled when target string not found
     (should-error (aj8/gptel-tool-edit-buffer-string "*test-edit*" "non-existent" "foo") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-edit-buffer-string "*test-edit*" "non-existent" "foo")))
       ;; Assert that the error message describes string not found
       (should (string-equal
                "tool: aj8_edit_buffer_string: Error: String 'non-existent' not found in buffer '*test-edit*'."
                result))))

   ;; Test error handling for ambiguous strings:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled when target string appears multiple times
     (should-error (aj8/gptel-tool-edit-buffer-string "*test-edit*" "hello" "hi") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-edit-buffer-string "*test-edit*" "hello" "hi")))
       ;; Assert that the error message describes string not unique
       (should (string-equal
                "tool: aj8_edit_buffer_string: Error: String 'hello' is not unique in buffer '*test-edit*'. Found 2 occurrences."
                result))))

   ;; Test error handling for non-existent buffers:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled when editing missing buffer
     (should-error (aj8/gptel-tool-edit-buffer-string "*non-existent-buffer*" "text" "replacement") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-edit-buffer-string "*non-existent-buffer*" "text" "replacement")))
       ;; Assert that the error message describes buffer not found
       (should (string-equal
                "tool: aj8_edit_buffer_string: Error: Buffer '*non-existent-buffer*' not found."
                result))))

   ;; Test multi-line string replacement:
   (aj8/gptel-tool-edit-buffer-string "*test-edit*" "emacs\nhello" "EMACS\nHI")
   ;; Assert that multi-line replacement works correctly
   (should (string-equal (buffer-string) "hello EMACS\nHI universe"))))

(ert-deftest test-aj8-edit-buffer-line ()
  "Test `aj8/gptel-tool-replace-buffer-line'."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-edit-line*" "Line A\nLine B\nLine C"

   ;; === ERROR CASES ===

   ;; Test error handling for invalid line numbers:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled for line number 0
     (should-error (aj8/gptel-tool-replace-buffer-line "*test-edit-line*" 0 "X") :type 'error)
     ;; Assert that an error is signaled for line number beyond buffer
     (should-error (aj8/gptel-tool-replace-buffer-line "*test-edit-line*" 10 "X") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-replace-buffer-line "*test-edit-line*" 0 "X")))
       ;; Assert that the error message is correct for line number 0
       (should (string-equal
                "tool: aj8_replace_buffer_line: Error: START-LINE must be >= 1"
                result)))
     (let ((result (aj8/gptel-tool-replace-buffer-line "*test-edit-line*" 10 "X")))
       ;; Assert that the error message is correct for line number beyond buffer
       (should (string-equal
                "tool: aj8_replace_buffer_line: Error: END-LINE exceeds buffer length (3)."
                result))))

   ;; === SUCCESS CASES ===

   ;; Test line replacement:
   (aj8/gptel-tool-replace-buffer-line "*test-edit-line*" 2 "X")
   ;; Assert that the specified line is replaced with new content
   (should (string-equal (buffer-string) "Line A\nX\nLine C")))

  ;; Test error handling for non-existent buffers:
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    ;; Assert that an error is signaled when editing missing buffer
    (should-error (aj8/gptel-tool-replace-buffer-line "*non-existent-buffer*" 1 "X") :type 'error))
  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((result (aj8/gptel-tool-replace-buffer-line "*non-existent-buffer*" 1 "X")))
      ;; Assert that the error message describes buffer not found
      (should (string-equal
               "tool: aj8_replace_buffer_line: Error: Buffer '*non-existent-buffer*' not found."
               result)))))

(ert-deftest test-aj8-edit-buffer-lines ()
  "Test `aj8/gptel-tool-replace-buffer-lines'."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-edit-buffer-lines*" "Line A\nLine B\nLine C\nLine D"

   ;; === ERROR CASES ===

   ;; Test error handling for invalid line ranges:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled for start line 0
     (should-error (aj8/gptel-tool-replace-buffer-lines "*test-edit-buffer-lines*" 0 1 "X") :type 'error)
     ;; Assert that an error is signaled when end line before start line
     (should-error (aj8/gptel-tool-replace-buffer-lines "*test-edit-buffer-lines*" 3 2 "X") :type 'error)
     ;; Assert that an error is signaled when end line beyond buffer
     (should-error (aj8/gptel-tool-replace-buffer-lines "*test-edit-buffer-lines*" 2 5 "X") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-replace-buffer-lines "*test-edit-buffer-lines*" 0 1 "X")))
       ;; Assert that the error message is correct for start line 0
       (should (string-equal
                "tool: aj8_replace_buffer_lines: Error: START-LINE must be >= 1"
                result)))
     (let ((result (aj8/gptel-tool-replace-buffer-lines "*test-edit-buffer-lines*" 3 2 "X")))
       ;; Assert that the error message is correct for end line before start line
       (should (string-equal
                "tool: aj8_replace_buffer_lines: Error: END-LINE must be >= START-LINE"
                result)))
     (let ((result (aj8/gptel-tool-replace-buffer-lines "*test-edit-buffer-lines*" 2 5 "X")))
       ;; Assert that the error message is correct for end line beyond buffer
       (should (string-equal
                "tool: aj8_replace_buffer_lines: Error: END-LINE exceeds buffer length (4)."
                result))))

   ;; === SUCCESS CASES ===

   ;; Test range replacement:
   (aj8/gptel-tool-replace-buffer-lines "*test-edit-buffer-lines*" 2 3 "X\nY")
   ;; Assert that the specified line range is replaced with new content
   (should (string-equal (buffer-string) "Line A\nX\nY\nLine D"))

   ;; Test error handling for non-existent buffers:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled when editing missing buffer line range
     (should-error (aj8/gptel-tool-replace-buffer-lines "*non-existent-buffer*" 1 1 "X") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-replace-buffer-lines "*non-existent-buffer*" 1 1 "X")))
       ;; Assert that the error message describes buffer not found
       (should (string-equal
                "tool: aj8_replace_buffer_lines: Error: Buffer '*non-existent-buffer*' not found."
                result))))))

(ert-deftest test-aj8-delete-buffer-string ()
  "Test `aj8/gptel-tool-delete-buffer-string'."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-delete*" "hello world\nhello universe"

   ;; === SUCCESS CASES ===

   ;; Test basic string deletion:
   (aj8/gptel-tool-delete-buffer-string "*test-delete*" "world")
   ;; Assert that the target string is removed from the buffer
   (should (string-equal (buffer-string) "hello \nhello universe"))

   ;; Test error handling for missing strings:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled when deleting non-existent string
     (should-error (aj8/gptel-tool-delete-buffer-string "*test-delete*" "non-existent") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-delete-buffer-string "*test-delete*" "non-existent")))
       ;; Assert that the error message describes string not found
       (should (string-equal
                "tool: aj8_delete_buffer_string: Error: String 'non-existent' not found in buffer '*test-delete*'."
                result))))

   ;; Test error handling for ambiguous strings:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled when deleting non-unique string
     (should-error (aj8/gptel-tool-delete-buffer-string "*test-delete*" "hello") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-delete-buffer-string "*test-delete*" "hello")))
       ;; Assert that the error message describes string not unique
       (should (string-equal
                "tool: aj8_delete_buffer_string: Error: String 'hello' is not unique in buffer '*test-delete*'. Found 2 occurrences."
                result)))))

  ;; Test error handling for non-existent buffers:
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    ;; Assert that an error is signaled when deleting from missing buffer
    (should-error (aj8/gptel-tool-delete-buffer-string "*non-existent-buffer*" "text") :type 'error))
  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((result (aj8/gptel-tool-delete-buffer-string "*non-existent-buffer*" "text")))
      ;; Assert that the error message describes buffer not found
      (should (string-equal
               "tool: aj8_delete_buffer_string: Error: Buffer '*non-existent-buffer*' not found."
               result))))

  ;; Test multi-line string deletion:
  (with-temp-buffer-with-content
   "*test-delete-ml*" "A\nB\nC"
   (aj8/gptel-tool-delete-buffer-string "*test-delete-ml*" "B\n")
   ;; Assert that multi-line deletion works correctly
   (should (string-equal (buffer-string) "A\nC"))))

(ert-deftest test-aj8-delete-buffer-line ()
  "Test `aj8/gptel-tool-delete-buffer-line'."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-delete-line*" "Line A\nLine B\nLine C"

   ;; === ERROR CASES ===

   ;; Test error handling for invalid line numbers:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled for line number 0
     (should-error (aj8/gptel-tool-delete-buffer-line "*test-delete-line*" 0) :type 'error)
     ;; Assert that an error is signaled for line number beyond buffer
     (should-error (aj8/gptel-tool-delete-buffer-line "*test-delete-line*" 10) :type 'error)
     ;; Assert that an error is signaled when deleting from missing buffer
     (should-error (aj8/gptel-tool-delete-buffer-line "*non-existent-buffer*" 2) :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-delete-buffer-line "*test-delete-line*" 0)))
       ;; Assert that the error message is correct for line number 0
       (should (string-equal
                "tool: aj8_delete_buffer_line: Error: START-LINE must be >= 1"
                result)))
     (let ((result (aj8/gptel-tool-delete-buffer-line "*test-delete-line*" 10)))
       ;; Assert that the error message is correct for line number beyond buffer
       (should (string-equal
                "tool: aj8_delete_buffer_line: Error: END-LINE exceeds buffer length (3)."
                result)))
     ;; Assert that the error message is correct for missing buffer
     (let ((result (aj8/gptel-tool-delete-buffer-line "*non-existent-buffer*" 2)))
       (should (string-equal
                "tool: aj8_delete_buffer_line: Error: Buffer '*non-existent-buffer*' not found."
                result))))

   ;; === SUCCESS CASES ===

   ;; Test line deletion:
   (aj8/gptel-tool-delete-buffer-line "*test-delete-line*" 2)
   ;; Assert that the specified line is deleted from the buffer
   (should (string-equal (buffer-string) "Line A\n\nLine C"))))

(ert-deftest test-aj8-delete-buffer-lines ()
  "Test `aj8/gptel-tool-delete-buffer-lines'."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-delete-buffer-lines*" "Line A\nLine B\nLine C\nLine D"

   ;; === ERROR CASES ===

   ;; Test error handling for invalid line ranges:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled for invalid line ranges
     (should-error (aj8/gptel-tool-delete-buffer-lines "*test-delete-buffer-lines*" 0 1) :type 'error)
     (should-error (aj8/gptel-tool-delete-buffer-lines "*test-delete-buffer-lines*" 3 2) :type 'error)
     (should-error (aj8/gptel-tool-delete-buffer-lines "*test-delete-buffer-lines*" 2 5) :type 'error)
     ;; Assert that an error is signaled when deleting from missing buffer
     (should-error (aj8/gptel-tool-delete-buffer-lines "*non-existent-buffer*" 2 3) :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-delete-buffer-lines "*test-delete-buffer-lines*" 0 1)))
       ;; Assert that the returned message is correct for invalid start-line
       (should (string-equal
                "tool: aj8_delete_buffer_lines: Error: START-LINE must be >= 1"
                result)))
     (let ((result (aj8/gptel-tool-delete-buffer-lines "*test-delete-buffer-lines*" 3 2)))
       ;; Assert that the returned message is correct for end-line < start-line
       (should (string-equal
                "tool: aj8_delete_buffer_lines: Error: END-LINE must be >= START-LINE"
                result)))
     (let ((result (aj8/gptel-tool-delete-buffer-lines "*test-delete-buffer-lines*" 2 5)))
       ;; Assert that the returned message is correct for end-line exceeding buffer
       (should (string-equal
                "tool: aj8_delete_buffer_lines: Error: END-LINE exceeds buffer length (4)."
                result)))
     ;; Assert that the returned message is correct for missing buffer
     (let ((result (aj8/gptel-tool-delete-buffer-lines "*non-existent-buffer*" 2 3)))
       (should (string-equal
                "tool: aj8_delete_buffer_lines: Error: Buffer '*non-existent-buffer*' not found."
                result))))

   ;; === SUCCESS CASES ===

   ;; Test line range deletion:
   (aj8/gptel-tool-delete-buffer-lines "*test-delete-buffer-lines*" 2 3)
   (should (string-equal (buffer-string) "Line A\n\nLine D"))))

(ert-deftest test-aj8-apply-buffer-string-edits ()
  "Test `aj8/gptel-tool-apply-buffer-string-edits'."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-apply-edits*" "Line one.\nLine two.\nLine three."

   ;; === SUCCESS CASES ===

   ;; Test basic batch edit functionality:
   (let ((edits '((:line-number 3 :old-string "three" :new-string "THREE")
                  (:line-number 1 :old-string "one" :new-string "ONE"))))
     (aj8/gptel-tool-apply-buffer-string-edits "*test-apply-edits*" edits)
     ;; Assert that the batched string edits are applied successfully
     (should (string-equal (buffer-string) "Line ONE.\nLine two.\nLine THREE.")))

   ;; Test edge cases:
   (erase-buffer)
   (insert "Line one.\nLine two.\nLine three.")

   ;; Assert that empty edits succeed and do nothing
   (aj8/gptel-tool-apply-buffer-string-edits "*test-apply-edits*" '())
   ;; Assert that the buffer remains unchanged
   (should (string-equal (buffer-string) "Line one.\nLine two.\nLine three."))

   ;; Assert that nil edits succeed and do nothing
   (aj8/gptel-tool-apply-buffer-string-edits "*test-apply-edits*" nil)
   ;; Assert that the buffer remains unchanged
   (should (string-equal (buffer-string) "Line one.\nLine two.\nLine three.")))

  ;; === ERROR CASES ===

  ;; Test multi-line old-string rejection:
  (with-temp-buffer-with-content
   "*test-apply-edits*" "Line one.\nLine two.\nLine three."
   (let ((edits2 '((:line-number 2 :old-string "two\nextra" :new-string "TWO"))))
     ;; Mode 1: tool re-signals the error
     (let ((aj8/gptel-tool-return-error nil))
       ;; Assert that an error is signaled for a multi-line old-string
       (should-error (aj8/gptel-tool-apply-buffer-string-edits "*test-apply-edits*" edits2) :type 'error))
     ;; Mode 2: tool returns the error as a string
     (let ((aj8/gptel-tool-return-error t))
       (let ((result (aj8/gptel-tool-apply-buffer-string-edits "*test-apply-edits*" edits2)))
         ;; Assert that the returned message describes the multi-line error
         (should (string-equal
                  "tool: aj8_apply_buffer_string_edits: Error: Could not apply edits to buffer '*test-apply-edits*': 1 (out of 1) failed.\n - line 2: old-string contains newline (old-string: \"two\nextra\")"
                  result))))))

  ;; Test non-existent buffer errors:
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    ;; Assert that an error is signaled for a non-existent buffer
    (should-error
     (aj8/gptel-tool-apply-buffer-string-edits "*non-existent*" '((:line-number 1 :old-string "x" :new-string "y")))
     :type 'error))
  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((result
           (aj8/gptel-tool-apply-buffer-string-edits "*non-existent*" '((:line-number 1 :old-string "x" :new-string "y")))))
      ;; Assert that the error message describes the missing buffer
      (should (string-equal
               "tool: aj8_apply_buffer_string_edits: Error: Buffer '*non-existent*' not found."
               result)))))

(ert-deftest test-aj8-apply-buffer-line-edits ()
  "Test `aj8/gptel-tool-apply-buffer-line-edits'."
  :tags '(unit buffers)
  (with-temp-buffer-with-content
   "*test-apply-edits*" "Line one.\nLine two.\nLine three."

   ;; === SUCCESS CASES ===

   ;; Test basic batch line edit functionality:
   (let ((edits '((:line-number 3 :old-string "Line three." :new-string "Line THREE.")
                  (:line-number 1 :old-string "Line one." :new-string "Line ONE."))))
     (aj8/gptel-tool-apply-buffer-line-edits "*test-apply-edits*" edits)
     ;; Assert that the batched line edits are applied successfully
     (should (string-equal (buffer-string) "Line ONE.\nLine two.\nLine THREE.")))

   ;; Test edge cases with empty and nil edits:
   (erase-buffer)
   (insert "Line one.\nLine two.\nLine three.")
   ;; Assert that empty edits succeed and do nothing
   (aj8/gptel-tool-apply-buffer-line-edits "*test-apply-edits*" '())
   ;; Assert that the buffer remains unchanged
   (should (string-equal (buffer-string) "Line one.\nLine two.\nLine three."))

   ;; Assert that nil edits succeed and do nothing
   (aj8/gptel-tool-apply-buffer-line-edits "*test-apply-edits*" nil)
   ;; Assert that the buffer remains unchanged
   (should (string-equal (buffer-string) "Line one.\nLine two.\nLine three.")))

  ;; === ERROR CASES ===

  ;; Test multi-line old-string rejection:
  (with-temp-buffer-with-content
   "*test-apply-edits*" "Line one.\nLine two.\nLine three."
   (let ((edits '((:line-number 2 :old-string "Line two.\nextra" :new-string "Line TWO."))))
     ;; Mode 1: tool re-signals the error
     (let ((aj8/gptel-tool-return-error nil))
       ;; Assert that an error is signaled for a multi-line old-string
       (should-error (aj8/gptel-tool-apply-buffer-line-edits "*test-apply-edits*" edits) :type 'error))
     ;; Mode 2: tool returns the error as a string
     (let ((aj8/gptel-tool-return-error t))
       (let ((result (aj8/gptel-tool-apply-buffer-line-edits "*test-apply-edits*" edits)))
         ;; Assert that the error message describes the multi-line error
         (should (string-equal
                  "tool: aj8_apply_buffer_line_edits: Error: Could not apply edits to buffer '*test-apply-edits*': 1 (out of 1) failed.\n - line 2: old-string contains newline (old-string: \"Line two.\nextra\")"
                  result))))))

  ;; Test non-existent buffer errors:
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    ;; Assert that an error is signaled for a non-existent buffer
    (should-error (aj8/gptel-tool-apply-buffer-line-edits "*non-existent*" '((:line-number 1 :old-string "x" :new-string "y"))) :type 'error))
  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((result (aj8/gptel-tool-apply-buffer-line-edits "*non-existent*" '((:line-number 1 :old-string "x" :new-string "y")))))
      ;; Assert that the error message describes the missing buffer
      (should (string-equal
               "tool: aj8_apply_buffer_line_edits: Error: Buffer '*non-existent*' not found."
               result))))

  ;; Clean up any Ediff buffers created during testing
  (aj8/ediff-cleanup-buffers))

(ert-deftest test-aj8-apply-buffer-line-edits-with-review ()
  "Test `aj8/gptel-tool-apply-buffer-line-edits-with-review'."
  :tags '(unit buffers review)
  (with-temp-buffer-with-content
   "*test-review*" "Line one.\nLine two."

   ;; === SUCCESS CASES ===

   ;; Test basic review functionality:
   (let ((edits '((:line-number 1 :old-string "Line one." :new-string "Line ONE.")))
         (ediff-called nil))
     ;; Temporarily advise `ediff-buffers' to check if it's called,
     ;; without actually starting the interactive session.
     (cl-letf (((symbol-function 'ediff-buffers) (lambda (b1 b2 &optional startup-hooks) (setq ediff-called t))))
       (aj8/gptel-tool-apply-buffer-line-edits-with-review "*test-review*" edits))
     ;; Assert that the ediff review function was called
     (should ediff-called)
     ;; Assert that the original buffer is unchanged after review setup
     (with-current-buffer "*test-review*"
       (should (string-equal (buffer-string) "Line one.\nLine two."))))

   ;; === ERROR CASES ===

   ;; Test multi-line old-string rejection:
   (let ((edits2 '((:line-number 2 :old-string "Line two.\nextra" :new-string "Line TWO."))))
     ;; Mode 1: tool re-signals the error
     (let ((aj8/gptel-tool-return-error nil))
       ;; Assert that an error is signaled for a multi-line old-string
       (should-error (aj8/gptel-tool-apply-buffer-line-edits-with-review "*test-review*" edits2) :type 'error))
     ;; Mode 2: tool returns the error as a string
     (let ((aj8/gptel-tool-return-error t))
       (let ((result (aj8/gptel-tool-apply-buffer-line-edits-with-review "*test-review*" edits2)))
         ;; Assert that the returned message describes the multi-line error
         (should (string-equal
                  "tool: aj8_apply_buffer_line_edits_with_review: Error: Could not apply edits to buffer '*test-review-edits*': 1 (out of 1) failed.\n - line 2: old-string contains newline (old-string: \"Line two.\nextra\")\nNote: No review was started and no changes were applied to buffer '*test-review*'. Any details above refer only to the temporary review buffer."
                  result)))))

   ;; Test non-existent buffer errors:
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled for a non-existent buffer
     (should-error (aj8/gptel-tool-apply-buffer-line-edits-with-review "*non-existent*" '((:line-number 1 :old-string "x" :new-string "y"))) :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-apply-buffer-line-edits-with-review "*non-existent*" '((:line-number 1 :old-string "x" :new-string "y")))))
       ;; Assert that the error message describes the missing buffer
       (should (string-equal
                "tool: aj8_apply_buffer_line_edits_with_review: Error: Buffer '*non-existent*' not found."
                result))))

   ;; Test edge cases:
   (with-temp-buffer-with-content
    "*test-empty-edits*" "Line one.\nLine two.\nLine three."
    ;; Assert that empty edits succeed and do nothing
    (aj8/gptel-tool-apply-buffer-line-edits-with-review "*test-empty-edits*" '())
    ;; Assert that the buffer remains unchanged
    (should (string-equal (buffer-string) "Line one.\nLine two.\nLine three."))

    ;; Assert that nil edits succeed and do nothing
    (aj8/gptel-tool-apply-buffer-line-edits-with-review "*test-empty-edits*" nil)
    ;; Assert that the buffer remains unchanged
    (should (string-equal (buffer-string) "Line one.\nLine two.\nLine three."))))

  ;; Clean up any Ediff buffers created during testing
  (aj8/ediff-cleanup-buffers))

;;; 3.2. Category: Emacs

(ert-deftest test-aj8-read-documentation ()
  "Test `aj8/gptel-tool-read-documentation'."
  :tags '(unit emacs)

  ;; === SUCCESS CASES ===

  ;; Test documentation lookup:
  ;; Assert that function documentation contains expected phrase
  (should (string-match-p "Return the car of LIST" (aj8/gptel-tool-read-documentation "car")))
  ;; Assert that variable documentation contains expected phrase
  (should (string-match-p "List of directories to search for files to load" (aj8/gptel-tool-read-documentation "load-path")))
  ;; Assert that a missing symbol returns a 'no documentation' message
  (should (string-match-p "No documentation found" (aj8/gptel-tool-read-documentation "non-existent-symbol-xyz"))))

(ert-deftest test-aj8-read-function ()
  "Test `aj8/gptel-tool-read-function'."
  :tags '(unit emacs)
  (unwind-protect
      (progn

        ;; === SUCCESS CASES ===

        ;; Test function source retrieval with a built-in Emacs function:
        (let ((result (aj8/gptel-tool-read-function "find-function-noselect")))
          ;; Assert that function source contains expected content
          (should (string-match-p "(defun find-function-noselect" result)))

        ;; Test with a non-byte-compiled function:
        (require 'aj8-lisp)
        (let ((result (aj8/gptel-tool-read-function "aj8/system-package-name")))
          ;; Assert that function source contains expected content
          (should (string-match-p "(defun aj8/system-package-name" result)))

        ;; Test built-in primitive function:
        (let ((result (aj8/gptel-tool-read-function "car")))
          ;; Assert that a built-in primitive returns expected message
          (should (string-match-p "built-in function" result)))

        ;; === ERROR CASES ===

        ;; Test missing function errors:
        ;; Mode 1: tool re-signals the error
        (let ((aj8/gptel-tool-return-error nil))
          ;; Assert that a missing function raises an error
          (should-error (aj8/gptel-tool-read-function "non-existent-function-xyz") :type 'error))
        ;; Mode 2: tool returns the error as a string
        (let ((aj8/gptel-tool-return-error t))
          (let ((result (aj8/gptel-tool-read-function "non-existent-function-xyz")))
            ;; Assert that the returned error message is correct for missing function
            (should (string-equal
                     "tool: aj8_read_function: Error: Symbol's function definition is void: non-existent-function-xyz"
                     result)))))
    (when (get-buffer "loaddefs.el")
      (kill-buffer "loaddefs.el"))
    (when (get-buffer "loaddefs.el.gz")
      (kill-buffer "loaddefs.el.gz"))
    ;; (when (get-buffer "aj8-lisp.el")
    ;;   (kill-buffer "aj8-lisp.el"))
    ))

(ert-deftest test-aj8-load-library ()
  "Test `aj8/gptel-tool-load-library'."
  :tags '(unit emacs)
  (unwind-protect
      (progn

        ;; === SUCCESS CASES ===

        ;; Test library loading without counts:
        (let ((result (aj8/gptel-tool-load-library "project")))
          ;; Assert that the library is loaded and the buffer is created
          (should (string-match-p "Library 'project' loaded into buffer 'project.el'\\." result))
          ;; Assert that the buffer exists and is alive
          (should (buffer-live-p (get-buffer "project.el"))))

        ;; Test library loading with counts:
        (when (get-buffer "project.el")
          (kill-buffer "project.el"))
        (let ((result (aj8/gptel-tool-load-library "project" t)))
          ;; Assert that the library is loaded with line count information
          (should (string-match-p "Library 'project' loaded into buffer 'project.el' ([0-9]+ lines)\\." result))
          ;; Assert that the buffer exists and is alive
          (should (buffer-live-p (get-buffer "project.el"))))

        ;; === ERROR CASES ===

        ;; Test missing library errors:
        ;; Mode 1: tool re-signals the error
        (let ((aj8/gptel-tool-return-error nil))
          ;; Assert that a missing library raises an error
          (should-error (aj8/gptel-tool-load-library "non-existent-library-xyz") :type 'error))
        ;; Mode 2: tool returns the error as a string
        (let ((aj8/gptel-tool-return-error t))
          (let ((result (aj8/gptel-tool-load-library "non-existent-library-xyz")))
            ;; Assert that the returned error message is correct for missing library
            (should (string-equal
                     "tool: aj8_load_library: Error: Can't find library: non-existent-library-xyz"
                     result)))))
    (when (get-buffer "project.el")
      (kill-buffer "project.el"))
    (when (get-buffer "project.el.gz")
      (kill-buffer "project.el.gz"))))

;; (ert-deftest test-aj8-read-library ()
;;   "Test `aj8/gptel-tool-read-library'."
;;   :tags '(unit emacs)
;;   (unwind-protect
;;       (progn
;;         ;; Test library source reading:
;;         ;; Assert library source contains expected filename
;;         (should (string-match-p "project.el" (aj8/gptel-tool-read-library "project")))

;;         ;; Test missing library errors:
;;         ;; Mode 1: tool re-signals the error
;;         (let ((aj8/gptel-tool-return-error nil))
;;           ;; Assert missing library raises an error
;;           (should-error (aj8/gptel-tool-read-library "non-existent-library-xyz") :type 'error))
;;         ;; Mode 2: tool returns the error as a string
;;         (let ((aj8/gptel-tool-return-error t))
;;           (let ((result (aj8/gptel-tool-read-library "non-existent-library-xyz")))
;;             ;; Assert returned error message for missing library
;;             (should (string-equal
;;                      "tool: aj8_read_library: Library 'non-existent-library-xyz' not found."
;;                      result)))))
;;     (when (get-buffer "project.el")
;;       (kill-buffer "project.el"))
;;     (when (get-buffer "project.el.gz")
;;       (kill-buffer "project.el.gz"))))

(ert-deftest test-aj8-read-info-symbol ()
  "Test `aj8/gptel-tool-read-info-symbol'."
  :tags '(unit emacs)

  ;; === SUCCESS CASES ===

  ;; Test Info symbol lookup:
  ;; Assert that Info lookup by symbol returns expected text
  (should (string-match-p "special form" (aj8/gptel-tool-read-info-symbol "defun")))

  ;; === ERROR CASES ===

  ;; Test non-existent symbol errors:
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    ;; Assert that an error is signaled when looking up non-existent symbol
    (should-error (aj8/gptel-tool-read-info-symbol "non-existent-symbol-xyz") :type 'error))
  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((result (aj8/gptel-tool-read-info-symbol "non-existent-symbol-xyz")))
      ;; Assert that the error message indicates symbol is not documented
      (should (string-equal
               "tool: aj8_read_info_symbol: Error: Not documented as a symbol: non-existent-symbol-xyz"
               result)))))

(ert-deftest test-aj8-read-info-node ()
  "Test `aj8/gptel-tool-read-info-node'."
  :tags '(unit emacs)

  ;; === SUCCESS CASES ===

  ;; Test Info lookup by node:
  ;; Assert that Info lookup by node returns expected text
  (should (string-match-p "defining a function" (aj8/gptel-tool-read-info-node "Defining Functions")))

  ;; === ERROR CASES ===

  ;; Test non-existent node errors:
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    ;; Assert that an error is signaled when looking up non-existent node
    (should-error (aj8/gptel-tool-read-info-node "Bogus Node 123") :type 'error))
  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((result (aj8/gptel-tool-read-info-node "Bogus Node 123")))
      ;; Assert that the error message indicates node does not exist
      (should (string-equal
               "tool: aj8_read_info_node: No such node or anchor: Bogus Node 123"
               result)))))

(ert-deftest test-aj8-eval-buffer ()
  "Test `aj8/gptel-tool-eval-buffer'."
  :tags '(unit emacs)
  (with-temp-buffer-with-content
   "*test-eval-buffer*" "(setq test-eval-result 42)\n(+ 1 2 3)"

   ;; === SUCCESS CASES ===

   ;; Test basic buffer evaluation functionality:
   (let ((result (aj8/gptel-tool-eval-buffer "*test-eval-buffer*")))
     ;; Assert that the evaluation returns a success message
     (should (string-equal result "Successfully evaluated all code in buffer *test-eval-buffer*."))
     ;; Assert that the side effects occurred (variable was set)
     (should (eq test-eval-result 42)))

  ;; === ERROR CASES ===

  ;; Test error handling for non-existent buffer:
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    ;; Assert that an error is signaled when buffer not found
    (should-error (aj8/gptel-tool-eval-buffer "*non-existent-buffer*") :type 'error))
  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((result (aj8/gptel-tool-eval-buffer "*non-existent-buffer*")))
      ;; Assert that the error message describes buffer not found
      (should (string-match "Buffer '\\*non-existent-buffer\\*' not found" result))))

  ;; Test error handling for syntax errors:
  (with-temp-buffer-with-content
   "*test-eval-syntax-error*" "(defun broken-syntax (x\n  ;; missing closing paren"
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled for malformed code
     (should-error (aj8/gptel-tool-eval-buffer "*test-eval-syntax-error*") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-eval-buffer "*test-eval-syntax-error*")))
       ;; Assert that the error message indicates evaluation failure
       (should (string-match "tool: aj8_eval_buffer:" result)))))))

(ert-deftest test-aj8-eval-function ()
  "Test `aj8/gptel-tool-eval-function'."
  :tags '(unit emacs)
  (with-temp-buffer-with-content
   "*test-eval-function*" "(defun test-func-one (x) (* x 2))\n\n(defun test-func-two (y) (+ y 10))\n\n(setq some-var 5)"

   ;; === SUCCESS CASES ===

   ;; Test basic function evaluation functionality:
   (let ((result (aj8/gptel-tool-eval-function "test-func-one" "*test-eval-function*")))
     ;; Assert that the function evaluation returns a success message
     (should (string-equal result "Successfully evaluated function test-func-one from buffer *test-eval-function*."))
     ;; Assert that the function is now defined and callable
     (should (eq (test-func-one 5) 10)))

   ;; Test evaluating second function:
   (let ((result (aj8/gptel-tool-eval-function "test-func-two" "*test-eval-function*")))
     ;; Assert that the function evaluation returns a success message
     (should (string-equal result "Successfully evaluated function test-func-two from buffer *test-eval-function*."))
     ;; Assert that the function is now defined and callable
     (should (eq (test-func-two 5) 15))))

  ;; === ERROR CASES ===

  ;; Test error handling for non-existent buffer:
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    ;; Assert that an error is signaled when buffer not found
    (should-error (aj8/gptel-tool-eval-function "some-func" "*non-existent-buffer*") :type 'error))
  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((result (aj8/gptel-tool-eval-function "some-func" "*non-existent-buffer*")))
      ;; Assert that the error message describes buffer not found
      (should (string-match "Buffer '\\*non-existent-buffer\\*' not found" result))))

  ;; Test error handling for function not found:
  (with-temp-buffer-with-content
   "*test-eval-no-func*" "(setq some-var 42)\n(+ 1 2 3)"
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled when function not found
     (should-error (aj8/gptel-tool-eval-function "non-existent-func" "*test-eval-no-func*") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-eval-function "non-existent-func" "*test-eval-no-func*")))
       ;; Assert that the error message describes function not found
       (should (string-match "Function 'non-existent-func' not found" result)))))

  ;; Test error handling for malformed function:
  (with-temp-buffer-with-content
   "*test-eval-bad-func*" "(defun broken-func (x\n  ;; missing closing paren and body"
   ;; Mode 1: tool re-signals the error
   (let ((aj8/gptel-tool-return-error nil))
     ;; Assert that an error is signaled for malformed function
     (should-error (aj8/gptel-tool-eval-function "broken-func" "*test-eval-bad-func*") :type 'error))
   ;; Mode 2: tool returns the error as a string
   (let ((aj8/gptel-tool-return-error t))
     (let ((result (aj8/gptel-tool-eval-function "broken-func" "*test-eval-bad-func*")))
       ;; Assert that the error message indicates evaluation failure
       (should (string-match "tool: aj8_eval_function:" result))))))

(ert-deftest test-aj8-eval-expression ()
  "Test `aj8/gptel-tool-eval-expression'."
  :tags '(unit emacs)

  ;; === SUCCESS CASES ===

  ;; Test basic expression evaluation:
  (let ((result (aj8/gptel-tool-eval-expression "(+ 1 2 3)")))
    ;; Assert that the arithmetic expression evaluates correctly
    (should (string-equal result "Expression result: 6")))

  ;; Test string expression:
  (let ((result (aj8/gptel-tool-eval-expression "(concat \"hello\" \" \" \"world\")")))
    ;; Assert that string concatenation works
    (should (string-equal result "Expression result: \"hello world\"")))

  ;; Test list expression:
  (let ((result (aj8/gptel-tool-eval-expression "(list 1 2 3)")))
    ;; Assert that list creation works
    (should (string-equal result "Expression result: (1 2 3)")))

  ;; Test boolean expressions:
  (let ((result (aj8/gptel-tool-eval-expression "(> 5 3)")))
    ;; Assert that the result is boolean true
    (should (string-equal result "Expression result: t")))
  (let ((result (aj8/gptel-tool-eval-expression "(< 5 3)")))
    ;; Assert that the result is boolean false (nil)
    (should (string-equal result "Expression result: nil")))

  ;; Test variable assignment and retrieval:
  (aj8/gptel-tool-eval-expression "(setq test-eval-var 123)")
  (let ((result (aj8/gptel-tool-eval-expression "test-eval-var")))
    ;; Assert that the variable was set and can be retrieved
    (should (string-equal result "Expression result: 123")))

  ;; Test function call:
  (aj8/gptel-tool-eval-expression "(defun test-eval-func (x) (* x x))")
  (let ((result (aj8/gptel-tool-eval-expression "(test-eval-func 4)")))
    ;; Assert that the function call works
    (should (string-equal result "Expression result: 16")))

  ;; === ERROR CASES ===

  ;; Test error handling for syntax errors:
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    ;; Assert that an error is signaled for malformed expression
    (should-error (aj8/gptel-tool-eval-expression "(+ 1 2") :type 'error))
  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((result (aj8/gptel-tool-eval-expression "(+ 1 2")))
      ;; Assert that the error message indicates syntax error
      (should (string-match "tool: aj8_eval_expression:" result))))

  ;; Test error handling for runtime errors:
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    ;; Assert that an error is signaled for undefined function
    (should-error (aj8/gptel-tool-eval-expression "(undefined-function-xyz 1 2)") :type 'error))
  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((result (aj8/gptel-tool-eval-expression "(undefined-function-xyz 1 2)")))
      ;; Assert that the error message indicates runtime error
      (should (string-match "tool: aj8_eval_expression:" result))))

  ;; Test error handling for division by zero:
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    ;; Assert that an error is signaled for division by zero
    (should-error (aj8/gptel-tool-eval-expression "(/ 1 0)") :type 'error))
  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((result (aj8/gptel-tool-eval-expression "(/ 1 0)")))
      ;; Assert that the error message indicates arithmetic error
      (should (string-match "tool: aj8_eval_expression:" result)))))

;;; 3.3. Category: Project

(ert-deftest test-aj8-project-get-root ()
  "Test `aj8/gptel-tool-project-get-root'."
  :tags '(unit project)
  (with-temp-project

   ;; === SUCCESS CASES ===

   ;; Test project root detection:
   (let ((root default-directory))
     ;; Assert that the returned project root corresponds to the temporary project directory
     (should (string-equal (file-name-as-directory (aj8/gptel-tool-project-get-root))
                           (file-name-as-directory root)))))

  ;; === ERROR CASES ===

  ;; Test non-project directory errors:
  (let* ((tmpdir (make-temp-file "aj8-non-project" t)))
    (unwind-protect
        (let ((default-directory tmpdir))
          ;; Mode 1: tool re-signals the error
          (let ((aj8/gptel-tool-return-error nil))
            ;; Assert that an error is signaled when not inside a project
            (should-error (aj8/gptel-tool-project-get-root) :type 'error))
          ;; Mode 2: tool returns the error as a string
          (let ((aj8/gptel-tool-return-error t))
            (let ((res1 (aj8/gptel-tool-project-get-root)))
              ;; Assert that the error message indicates not inside a project
              (should (string-equal "tool: aj8_project_get_root: Error: Not inside a project." res1)))))
      (when (file-directory-p tmpdir)
        (delete-directory tmpdir t)))))

(ert-deftest test-aj8-project-list-files ()
  "Test `aj8/gptel-tool-project-list-files'."
  :tags '(unit project)
  (with-temp-project

   ;; === SUCCESS CASES ===

   ;; Test project file listing:
   (let ((root default-directory))
     (let ((buf (find-file-noselect (expand-file-name "src/code.el"))))
       (unwind-protect
           (with-current-buffer buf
             (let ((lines (split-string (aj8/gptel-tool-project-list-files t) "\n" t))
                   (fname (file-name-nondirectory (buffer-file-name buf)))
                   (rel (file-relative-name (buffer-file-name buf) root)))
               ;; Assert that the file appears in the project listing output
               (should (string-match-p "src/code.el" (aj8/gptel-tool-project-list-files)))
               ;; Assert that exact no-counts line "NAME: PATH" entry exists
               (let* ((no-counts-lines (split-string (aj8/gptel-tool-project-list-files) "\n" t))
                      (expected (format "%s: %s" fname rel)))
                 (should (member expected no-counts-lines)))
               ;; Assert that exact counts line: "NAME: PATH (N lines) exists
               (should (member (format "%s: %s (%d lines)" fname rel 1) lines))
               ;; Assert that all include-counts entries end with "(N lines)"
               (should (cl-every (lambda (s) (string-match-p "^[^:]+: .+ ([0-9]+ lines)$" s)) lines))
               (let ((expected (format "%s: %s (%d lines)" fname rel 1)))
                 ;; Assert that the exact line count format is present in the output
                 (should (member expected lines)))))
         (when (buffer-live-p buf)
           (kill-buffer buf)))))

   ;; === ERROR CASES ===

   ;; Test non-project directory errors:
   (let* ((tmpdir (make-temp-file "aj8-non-project" t)))
     (unwind-protect
         (let ((default-directory tmpdir))
           ;; Mode 1: tool re-signals the error
           (let ((aj8/gptel-tool-return-error nil))
             ;; Assert that an error is signaled when not inside a project
             (should-error (aj8/gptel-tool-project-list-files) :type 'error))
           ;; Mode 2: tool returns the error as a string
           (let ((aj8/gptel-tool-return-error t))
             (let ((res2 (aj8/gptel-tool-project-list-files)))
               ;; Assert that the error message indicates not inside a project
               (should (string-equal "tool: aj8_project_list_files: Error: Not inside a project." res2)))))
       (when (file-directory-p tmpdir)
         (delete-directory tmpdir t))))))

(ert-deftest test-aj8-project-find-files-glob ()
  "Test `aj8/gptel-tool-project-find-files-glob'."
  :tags '(unit project)
  (with-temp-project

   ;; === SUCCESS CASES ===

   ;; Test find files glob functionality:
   (let* ((files-str (aj8/gptel-tool-project-find-files-glob "**/*.el"))
          (files (split-string files-str "\n" t)))
     ;; Assert that the glob found the expected single file
     (should (= 1 (length files)))
     ;; Assert that the glob result contains the expected Elisp file
     (should (string-match-p "src/code.el" (car files))))
   (let* ((files-str (aj8/gptel-tool-project-find-files-glob "*.txt"))
          (files (split-string files-str "\n" t)))
     ;; Assert that the glob found the expected single text file
     (should (= 1 (length files)))
     ;; Assert that the glob result contains the expected text file
     (should (string-match-p "data.txt" (car files)))))

  ;; === ERROR CASES ===

  ;; Test non-project directory errors:
  (let* ((tmpdir (make-temp-file "aj8-non-project" t)))
    (unwind-protect
        (let ((default-directory tmpdir))
          ;; Mode 1: tool re-signals the error
          (let ((aj8/gptel-tool-return-error nil))
            ;; Assert that an error is signaled when not inside a project
            (should-error (aj8/gptel-tool-project-find-files-glob "**/*.el") :type 'error))
          ;; Mode 2: tool returns the error as a string
          (let ((aj8/gptel-tool-return-error t))
            (let ((res1 (aj8/gptel-tool-project-find-files-glob "**/*.el")))
              ;; Assert that the error message indicates no project found
              (should (string-equal "tool: aj8_project_find_files_glob: Error: No project found in the current context." res1)))))
      (when (file-directory-p tmpdir)
        (delete-directory tmpdir t)))))

(ert-deftest test-aj8-project-search-regexp ()
  "Test `aj8/gptel-tool-project-search-regexp'."
  :tags '(unit project)
  (with-temp-project

   ;; === SUCCESS CASES ===

   ;; Test search content functionality:
   (when (or (executable-find "rg") (and (executable-find "git") (file-directory-p ".git")))
     (let ((results (aj8/gptel-tool-project-search-regexp "some text data")))
       ;; Assert that the exact output is PATH:LINE:TEXT
       (should (string-equal results "data.txt:1:some text data")))
     (let ((results-with-col (aj8/gptel-tool-project-search-regexp "some text data" t)))
       ;; Assert that the exact output is PATH:LINE:COLUMN:TEXT with include-columns
       (should (string-equal results-with-col "data.txt:1:1:some text data")))
     (let* ((regexp "NO_MATCH_REGEX_12345")
            (nores (aj8/gptel-tool-project-search-regexp regexp)))
       ;; Assert that a no-match path should return an informative message
       (should (string-equal nores (format "No matches found for regexp: %s" regexp))))

     ;; === ERROR CASES ===

     ;; Test invalid regexp errors:
     ;; Mode 1: tool re-signals the error
     (let ((aj8/gptel-tool-return-error nil))
       ;; Assert that an error is signaled for invalid regexp
       (should-error (aj8/gptel-tool-project-search-regexp "[") :type 'error))
     ;; Mode 2: tool returns the error as a string
     (let ((aj8/gptel-tool-return-error t))
       (let ((res (aj8/gptel-tool-project-search-regexp "[")))
         ;; Assert that the error message describes regexp failure
         (should (string-match-p "^tool: aj8_project_search_regexp: Error: Search command .* failed with status .* for regexp: \\[" res))))

     ;; Test non-project directory errors:
     (let* ((tmpdir (make-temp-file "aj8-non-project" t)))
       (unwind-protect
           (let ((default-directory tmpdir))
             ;; Mode 1: tool re-signals the error
             (let ((aj8/gptel-tool-return-error nil))
               ;; Assert that an error is signaled when not inside a project
               (should-error (aj8/gptel-tool-project-search-regexp "x") :type 'error))
             ;; Mode 2: tool returns the error as a string
             (let ((aj8/gptel-tool-return-error t))
               (let ((res2 (aj8/gptel-tool-project-search-regexp "x")))
                 ;; Assert that the error message indicates not inside a project
                 (should (string-equal "tool: aj8_project_search_regexp: Error: Not inside a project." res2)))))
         (when (file-directory-p tmpdir)
           (delete-directory tmpdir t)))))))

;;; 3.4. Category: Test

(ert-deftest test-aj8-ert-run-unit ()
  "Test `aj8/gptel-tool-ert-run-unit'."
  :tags '(integration test)

  ;; === SUCCESS CASES ===

  (let ((result (aj8/gptel-tool-ert-run-unit)))
    ;; Assert that the function returns a status string and does not error
    (should (stringp result))))

(ert-deftest test-aj8-ert-run-by-name ()
  "Test `aj8/gptel-tool-ert-run-by-name'."
  :tags '(unit test)

  ;; === SUCCESS CASES ===

  ;; Test basic test running functionality:
  (let ((success (aj8/gptel-tool-ert-run-by-name "test-aj8-open-file-in-buffer")))
    ;; Assert that the success message matches expected format
    (should (string-prefix-p
             "Ran 1 test, 1 passed, 0 failed"
             success)))

  ;; === ERROR CASES ===

  ;; Test unknown test errors:
  ;; Mode 1: tool re-signals the error
  (let ((aj8/gptel-tool-return-error nil))
    ;; Assert that an error is signaled for an unknown test name
    (should-error (aj8/gptel-tool-ert-run-by-name "NON_EXISTENT_TEST") :type 'error))
  ;; Mode 2: tool returns the error as a string
  (let ((aj8/gptel-tool-return-error t))
    (let ((res (aj8/gptel-tool-ert-run-by-name "NON_EXISTENT_TEST")))
      ;; Assert that the formatted error string matches expected error message
      (should (string-equal
               "tool: aj8_ert_run_by_name: Error: No ERT test found named NON_EXISTENT_TEST"
               res)))))

(ert-deftest test-aj8-ert-list-unit-tests ()
  "Test `aj8/gptel-tool-ert-list-unit-tests'."
  :tags '(unit test)

  ;; === SUCCESS CASES ===

  ;; Test basic test listing functionality:
  (let* ((result (aj8/gptel-tool-ert-list-unit-tests))
         (lines (split-string result "\n" t)))
    ;; Assert that the list includes this test as a full line
    (should (member "test-aj8-ert-list-unit-tests" lines)))
  ;; Test missing test errors:
  (cl-letf (((symbol-function 'ert-select-tests) (lambda (&rest _) nil)))
    (let ((result (aj8/gptel-tool-ert-list-unit-tests)))
      ;; Assert that the exact no-tests message appears when no unit tests are loaded
      (should (string-equal "No loaded ERT unit tests found." result)))))

;;;; 4. Integration Tests (ert-deftest)

;;; 4.1. Category: Tool Definition and Invocation
;;
;; This section tests the fundamental integrity of the tool system.  It
;; ensures that all tools are correctly defined, registered, and can be
;; invoked through their function pointers.  It also verifies that the tool
;; definitions meet the structural requirements and that error handling for
;; invalid arguments is working as expected.

(ert-deftest test-gptel-tools-registration ()
  "Verify that all GPTel tools are registered in `gptel-tools'.
This test checks that a predefined list of essential tool names exists
in the `gptel-tools' alist."
  :tags '(integration tools)
  (let ((expected-tools '("aj8_buffer_search_regexp"
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
                          "aj8_load_library"
                          ;; "aj8_read_library"
                          "aj8_read_info_symbol"
                          "aj8_read_info_node"
                          "aj8_eval_buffer"
                          "aj8_eval_function"
                          "aj8_eval_expression"
                          "aj8_project_get_root"
                          "aj8_project_list_files"
                          ;; "aj8_project_find_files"
                          "aj8_project_find_files_glob"
                          "aj8_project_search_regexp"
                          "aj8_ert_run_unit"
                          "aj8_ert_run_by_name"
                          "aj8_ert_list_unit_tests")))
    (dolist (tool-name expected-tools)
      ;; Assert that the tool is registered in `gptel-tools'
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
      ;; Assert that tool has a function field
      (should (gptel-tool-function tool-def))
      ;; Assert that tool has a description
      (should (gptel-tool-description tool-def))

      ;; Assert that the args field is a list (or nil)
      (should (listp (gptel-tool-args tool-def))))))

(ert-deftest test-gptel-tools-function-callable ()
  "Verify that tool functions are defined and callable.

This test checks a subset of tools that require no arguments, ensuring
their associated functions can be called without error."
  :tags '(integration tools)
  (let ((no-arg-tools '("aj8_list_buffers"
                        "aj8_list_all_buffers"
                        "aj8_project_get_root"
                        "aj8_project_list_files"
                        "aj8_ert_run_unit"
                        "aj8_ert_list_unit_tests")))
    ;; Test tools that don't require arguments
    (dolist (tool-name no-arg-tools)
      (let* ((tool-def (cl-find-if (lambda (tool) (string-equal (gptel-tool-name tool) tool-name)) gptel-tools))
             (func (gptel-tool-function tool-def)))
        ;; Assert that the tool function should be a callable function
        (should (functionp func))
        ;; Assert that calling the function should not raise an error
        (should-not (condition-case nil
                        (progn (funcall func) nil)
                      (error t)))))))

(ert-deftest test-gptel-tools-via-json-call ()
  "Simulate calling GPTel tools via a JSON-like interface.
This test mimics how a Large Language Model (LLM) would call the tools
by invoking the tool's function with arguments directly.  It verifies
both a query and a buffer modification tool."
  :tags '(integration tools json)
  (with-temp-buffer-with-content
   "*test-json-call*" "Hello World\nLine 2"
   ;; Test list all buffers tool
   (let* ((tool-def (cl-find "aj8_list_all_buffers" gptel-tools :key #'gptel-tool-name :test #'string-equal))
          (func (gptel-tool-function tool-def))
          (result (funcall func)))
     ;; Assert that the tool returned a list
     (should (listp result))
     ;; Assert that our test buffer is included in the result
     (should (member "*test-json-call*" result)))

   ;; Test edit buffer tool with JSON-like parameters
   (let* ((tool-def (cl-find "aj8_edit_buffer_string" gptel-tools :key #'gptel-tool-name :test #'string-equal))
          (func (gptel-tool-function tool-def))
          (result (funcall func "*test-json-call*" "World" "GPTel")))
     ;; Assert that edit returned a success message
     (should (string-match-p "successfully" result))
     ;; Assert that the buffer content reflects the edit
     (with-current-buffer (get-buffer "*test-json-call*")
       (should (string-equal (buffer-string) "Hello GPTel\nLine 2"))))))

(ert-deftest test-gptel-tools-error-handling ()
  "Test that GPTel tools handle common errors gracefully.
Verifies that tools produce user-friendly error messages when given
invalid arguments, such as a non-existent buffer name or an invalid file
path."
  :tags '(integration tools errors)
  ;; Test with non-existent buffer
  (let* ((tool-def (cl-find "aj8_edit_buffer_string" gptel-tools :key #'gptel-tool-name :test #'string-equal))
         (func (gptel-tool-function tool-def)))
    ;; Calling edit on a missing buffer signals a helpful error
    ;; Assert that an informative error message is produced
    (should (condition-case err
                (funcall func "*non-existent-buffer*" "old" "new")
              (error (string-match-p "Buffer.*not found" (error-message-string err)))))))

;;; 4.2. Category: LLM Tool Mock Simulation
;;
;; This section simulates the end-to-end process of an LLM calling a tool.
;; It uses mock LLM responses containing tool call requests to test if
;; `gptel' correctly parses these requests, invokes the appropriate tool
;; with the right arguments, and that the tools produce the expected side
;; effects (e.g., modifying a buffer or file).


;; (defun aj8/gptel-tool-test--run-with-mock-llm (tool-name args expected-pattern)
;;   "Simulate an LLM call to a GPTel tool and check the result.

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
       ;; Test a read-only tool:
       (erase-buffer)
       (let ((mock-response "{\"tool_calls\": [{\"name\": \"aj8_list_all_buffers\", \"arguments\": {}}]}"))
         (test-gptel-tools--mock-response mock-response (lambda () (gptel-send "dummy query")))
         ;; Assert that the mock buffer name appears in the buffer
         (should (string-match-p "mock-test" (buffer-string))))
       ;; Test a modifying tool:
       (erase-buffer)
       (let ((mock-response "{\"tool_calls\": [{\"name\": \"aj8_edit_buffer_string\", \"arguments\": {\"buffer-name\": \"*mock-test*\", \"old-string\": \"Original\", \"new-string\": \"Modified\"}}]}"))
         (test-gptel-tools--mock-response mock-response (lambda () (gptel-send "dummy query")))
         ;; Assert that the target buffer was modified
         (with-current-buffer "*mock-test*"
           ;; Assert that the target buffer was modified
           (should (string-equal (buffer-string) "Modified content")))
         ;; Check that the gptel buffer contains the tool result
         (with-current-buffer gptel-buffer
           ;; Assert that GPTel buffer should record the tool result message
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
         ;; Assert that mock project-get-root included project dir name
         (should (string-match-p "ert-test-project" (buffer-string))))

       (erase-buffer)
       (let ((mock-response "{\"tool_calls\": [{\"name\": \"aj8_project_find_files_glob\", \"arguments\": {\"pattern\": \"**/*.el\"}}]}"))
         (test-gptel-tools--mock-response mock-response (lambda () (gptel-send "dummy query")))
         ;; Assert that mock project-find-files listed the project file
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
        ;; Assert that mock documentation read included expected phrase
        (should (string-match-p "Return the car of LIST" (buffer-string))))
      (erase-buffer)
      ;; Test read_function
      (let ((mock-response "{\"tool_calls\": [{\"name\": \"aj8_read_function\", \"arguments\": {\"function-name\": \"gptel-send\"}}]}"))
        (test-gptel-tools--mock-response mock-response (lambda () (gptel-send "dummy query")))
        ;; Assert that mock function read included defun for gptel-send
        (should (string-match-p "(defun gptel-send" (buffer-string)))))))

;;; 4.3. Category: User Workflow Simulation
;;
;; This section tests common sequences of tool calls that emulate a user's
;; workflow for a specific task.  Unlike the LLM simulations, these tests
;; call the tool functions directly to verify that combinations of tools
;; work together correctly to achieve a larger goal, such as refactoring
;; code in a buffer or managing project files.

(ert-deftest test-gptel-tools-workflow-buffers ()
  "Simulate a workflow using buffer tools."
  :tags '(integration workflow buffers)
  ;; Test on a buffer not visiting a file:
  (with-temp-buffer-with-content
   "*test-buffer*" "initial content"
   (let ((buffer-name "*test-buffer*"))
     ;; Test buffer listing functionality:
     ;; Assert that the buffer appears in the 'all' listing
     (should (member buffer-name (split-string (aj8/gptel-tool-list-all-buffers) "\n" t)))
     ;; Assert that the buffer is not in the file-backed-only listing
     (should-not (member buffer-name (split-string (aj8/gptel-tool-list-buffers) "\n" t)))
     ;; Assert that converting a non-file buffer to a file errors
     (should-error (aj8/gptel-tool-buffer-to-file buffer-name))

     ;; Test append-to-buffer functionality:
     ;; Assert that we can append text and verify
     (aj8/gptel-tool-append-to-buffer buffer-name "\nAppended")
     (should (string-equal (with-current-buffer buffer-name (buffer-string)) "initial content\nAppended"))

     ;; Test insert-in-buffer functionality:
     ;; Assert that we can insert at the start and verify ordering
     (aj8/gptel-tool-insert-in-buffer buffer-name "Prepended\n" 1)
     (should (string-equal (with-current-buffer buffer-name (buffer-string)) "Prepended\ninitial content\nAppended"))

     ;; Test edit-buffer functionality:
     ;; Assert that we can replace a substring and verify content
     (aj8/gptel-tool-edit-buffer-string buffer-name "initial" "original")
     (should (string-equal (with-current-buffer buffer-name (buffer-string)) "Prepended\noriginal content\nAppended"))

     ;; Test modify-buffer functionality:
     ;; Assert that we can replace the entire buffer and verify
     (aj8/gptel-tool-modify-buffer buffer-name "new content")
     (should (string-equal (with-current-buffer buffer-name (buffer-string)) "new content"))

     ;; Test read-buffer-lines-count functionality:
     ;; Assert that reading the buffer returns the new content
     (should (string-equal (aj8/gptel-tool-read-buffer-lines-count buffer-name) "new content"))))

  ;; Test buffer visiting a file:
  (with-temp-file-with-content
   test-file "file content"
   (let ((buffer (find-file-noselect test-file)))
     (unwind-protect
         (progn
           (should (member (buffer-name buffer) (aj8/gptel-tool-list-buffers)))
           (should (string-equal (aj8/gptel-tool-file-to-buffer test-file) (buffer-name buffer)))
           (should (string-equal (aj8/gptel-tool-buffer-to-file (buffer-name buffer)) (expand-file-name test-file))))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))))))

(ert-deftest test-gptel-tools-workflow-project ()
  "Simulate a workflow using project tools."
  :tags '(integration workflow project)
  (with-temp-project
   ;; Test get-root functionality:
   (let ((root (aj8/gptel-tool-project-get-root)))
     (should (string-match-p "ert-test-project" root)))

   ;; Test find-files-glob functionality:
   (let* ((files-str (aj8/gptel-tool-project-find-files-glob "**/*.el"))
          (files (split-string files-str "\n" t)))
     (should (= 1 (length files)))
     (should (string-match-p "src/code.el" (car files))))

   ;; Test search-content functionality:
   (when (or (executable-find "rg") (and (executable-find "git") (file-directory-p ".git")))
     (let ((results (aj8/gptel-tool-project-search-regexp "hello")))
       (should (string-match-p "src/code.el:1:.*hello" results))))

   ;; Test get-open-buffers functionality:
   (let ((buf (find-file-noselect (expand-file-name "src/code.el"))))
     (unwind-protect
         (let ((open-buffers (aj8/gptel-tool-project-list-files)))
           (should (string-match-p "code.el" open-buffers))
           (should (string-match-p "src/code.el" open-buffers)))
       (when (buffer-live-p buf)
         (kill-buffer buf))))))

(ert-deftest test-gptel-tools-workflow-emacs ()
  "Simulate a workflow using Emacs introspection tools."
  :tags '(integration workflow emacs)
  ;; Test documentation lookup:
  ;; Assert that documentation lookup returns expected snippet
  (should (string-match-p "car of LIST"
                          (aj8/gptel-tool-read-documentation "car")))

  ;; Test function source retrieval:
  ;; Assert that function source retrieval returns expected fragments
  (should (string-match-p "(defun project-current"
                          (aj8/gptel-tool-read-function "project-current")))
  (should (string-match-p "built-in function"
                          (aj8/gptel-tool-read-function "car")))

  ;; Test library loading:
  ;; Assert that library loading returns expected confirmation message
  (should (string-match-p "Library 'subr' loaded into buffer 'subr.el'"
                          (aj8/gptel-tool-load-library "subr")))

  ;; Test info symbol lookup:
  ;; Assert that info symbol lookup returns expected phrase
  (should (string-match-p "special form"
                          (aj8/gptel-tool-read-info-symbol "defun")))

  ;; Test info node lookup:
  ;; Assert that info node lookup returns expected phrase
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

     ;; Test complex batch string edits:
     (funcall edit-func "*complex-edit-test*" edits)

     ;; Verify all changes were applied
     (let ((content (buffer-string)))
       ;; Assert that the new function names appear
       (should (string-match-p "function addNumbers" content))
       (should (string-match-p "function multiplyNumbers" content))
       ;; Assert that the calls are updated to the new names
       (should (string-match-p "addNumbers(5, 3)" content))
       (should (string-match-p "multiplyNumbers(4, 6)" content))
       ;; Assert that the old names were removed
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

     ;; Test complex batch line edits:
     (funcall edit-func "*complex-edit-test*" edits)

     (let ((content (buffer-string)))
       ;; Assert that the new function names are expected after line edits
       (should (string-match-p "function addNumbers" content))
       (should (string-match-p "function multiplyNumbers" content))
       ;; Assert that updated call sites
       (should (string-match-p "addNumbers(5, 3)" content))
       (should (string-match-p "multiplyNumbers(4, 6)" content))
       ;; Assert that the original names no longer appear
       (should-not (string-match-p "calculateSum" content))
       (should-not (string-match-p "calculateProduct" content))))))

;;;; 5. Test Runner Functions (interactive)

(defun aj8/gptel-tool-test-run-all ()
  "Run all ERT tests defined for GPTel tools."
  (interactive)
  (ert t))

(defun aj8/gptel-tool-test-run-unit ()
  "Run all GPTel tool unit tests."
  (interactive)
  (ert '(tag unit)))

(defun aj8/gptel-tool-test-run-integration ()
  "Run GPTel tool integration tests."
  (interactive)
  (ert '(tag integration)))

(defun aj8/gptel-tool-test-run-by-tag (tag)
  "Run all GPTel tool tests with a specified TAG."
  (interactive
   (list (completing-read "Select tag: "
                          '("unit" "buffers" "emacs" "project" "review"
                            "integration" "tools" "json" "errors" "mock"
                            "workflow")
                          nil t)))
  (ert `(tag ,(intern tag))))

(defun aj8/gptel-tool-test-run-by-name ()
  "Run a single GPTel tool test selected by name."
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

;;;; 6. Manual Testing & Utility Functions (interactive)

(defun aj8/gptel-tool-run-tool (tool-name)
  "Directly invoke a GPTel tool chosen interactively by its name.

This function prompts for a TOOL-NAME from a list of all registered
gptel tools.  If the tool requires arguments, you will be prompted to
enter a value for each one.  The tool is then executed with the provided
arguments.

The return value of the tool is displayed as a message.  This is useful
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
          (message "GPTel tool validation errors found:")
          (dolist (error errors)
            (message "  - %s" error))
          errors)
      (message "All GPTel tools validated successfully!")
      nil)))

(defun aj8/gptel-tool-create-scenario ()
  "Create a sandboxed environment for manually testing GPTel tools.
This sets up a temporary directory with several files and opens them in
buffers, simulating a realistic project.  It also creates an
instructions buffer with suggested prompts for testing tool-based
interactions with an LLM."
  (interactive)
  (let ((test-dir (expand-file-name "gptel-tool-test/" temporary-file-directory)))
    ;; Create test directory
    (make-directory test-dir t)

    ;; Create test files
    (with-temp-buffer
      (insert "# Test Project\n\nThis is a test project for GPTel tools.\n\n## Files\n- main.py: Python script\n- config.json: Configuration\n- README.md: This file")
      (write-file (expand-file-name "README.md" test-dir)))

    (with-temp-buffer
      (insert "#!/usr/bin/env python3\n\ndef greet(name):\n    \"\"\"Greet someone by name.\"\"\"\n    return f\"Hello, {name}!\"\n\ndef main():\n    print(greet(\"World\"))\n\nif __name__ == \"__main__\":\n    main()")
      (write-file (expand-file-name "main.py" test-dir)))

    (with-temp-buffer
      (insert "{\n  \"app_name\": \"GPTel Test\",\n  \"version\": \"1.0.0\",\n  \"debug\": true\n}")
      (write-file (expand-file-name "config.json" test-dir)))

    ;; Create test buffers
    (with-current-buffer (get-buffer-create "*GPTel Test Buffer*")
      (erase-buffer)
      (insert "This is a test buffer for GPTel tools.\n\nYou can ask the LLM to:\n- Edit this content\n- Add new lines\n- Replace text\n- Apply multiple edits\n\nOriginal timestamp: " (current-time-string)))

    ;; Open files in buffers
    (find-file (expand-file-name "main.py" test-dir))
    (find-file (expand-file-name "config.json" test-dir))

    ;; Display instructions
    (with-current-buffer (get-buffer-create "*GPTel Tool Test Instructions*")
      (erase-buffer)
      (insert "=== GPTel Tool Test Scenario Created ===\n\n")
      (insert "Test files created in: " test-dir "\n\n")
      (insert "Available test buffers:\n")
      (insert "- *GPTel Test Buffer*\n")
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
      (insert "8. \"Add a comment to the *GPTel Test Buffer*\"\n\n")
      (insert "=== Testing Tips ===\n\n")
      (insert "- Use the 'coding' preset for tool access\n")
      (insert "- Tools should work automatically when enabled\n")
      (insert "- Check that edits are applied correctly\n")
      (insert "- Verify error handling with invalid requests\n")
      (goto-char (point-min)))

    (switch-to-buffer "*GPTel Tool Test Instructions*")
    (message "GPTel tool test scenario created! Check the instructions buffer.")))

(provide 'gptel-tool-test)
