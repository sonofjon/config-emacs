;;; aj8-gptel.el --- Tools for gptel -*- lexical-binding: t; -*-

;;; Helpers

(defun aj8/gptel-tool--truncate-for-display (obj)
  "Return a truncated, display-safe copy of OBJ for minibuffer messages.

OBJ is the source object to convert; it may be nil, a string, a list, a
vector, or a list or vector of property lists (plists).  The conversion
rules are:

- nil: returned as nil.
- string: if the string contains a newline, return only the text up to
  the first newline followed by the suffix \"...(+N more)\" where N is
  the number of remaining lines; otherwise return the original string.
- list: recursively process each element and return a new list.
- list of plists: return a list whose first element is the processed
  first plist and, if there are more elements, a second element that is
  the string \"...(+N more)\" where N is the number of remaining plists.
- vector: recursively process each element and return a new vector.
- vector of plists: see \"list of plists\" above; the same truncation is
  applied, but the result is a vector, not a list.

The original OBJ is not mutated; the result is a fresh structure intended
for use with `prin1-to-string' for concise minibuffer display."
  (cond
   ((null obj) nil)
   ;; Strings
   ((stringp obj)
    (let ((lines (split-string obj "\n")))
      (if (> (length lines) 1)
          (concat (car lines) (format "...(+%d more)" (1- (length lines))))
        obj)))
   ;; Lists of plists
   ((and (listp obj)
         ;; Each element is a list whose car is a keyword
         (cl-every (lambda (e) (and (listp e) (keywordp (car e)))) obj))
    (let ((len (length obj)))
      (if (= len 0)
          '()
        (let ((first (aj8/gptel-tool--truncate-for-display (car obj)))
              (rest-count (1- len)))
          (if (> rest-count 0)
              (list first (format "...(+%d more)" rest-count))
            (list first))))))
   ;; Lists
   ((listp obj)
    (mapcar #'aj8/gptel-tool--truncate-for-display obj))
   ;; Vectors of plists
   ((and (vectorp obj)
         ;; Each element is a list whose car is a keyword
         (cl-every (lambda (e) (and (listp e) (keywordp (car e)))) obj))
    (let ((len (length obj)))
      (if (= len 0)
          (vector)
        (let ((first (aj8/gptel-tool--truncate-for-display (aref obj 0)))
              (rest-count (1- len)))
          (if (> rest-count 0)
              (vector first (format "...(+%d more)" rest-count))
            (vector first))))))
   ;; Vectors
   ((vectorp obj)
    (let* ((len (length obj))
           (out (make-vector len nil)))
      (dotimes (i len)
        (aset out i (aj8/gptel-tool--truncate-for-display (aref obj i))))
      out))
   (t obj)))

(defun aj8/gptel-tool--log-to-buffer (tool-name args result &optional error-p)
  "Append to `*gptel-tool-log*' recording TOOL-NAME, ARGS and RESULT.
If ERROR-P is non-nil record it as an error entry.  The record is
machine-readable (prin1) and timestamped."
  (let ((buf (get-buffer-create "*gptel-tool-log*"))
        (ts (format-time-string "%Y-%m-%d %T")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "%s | %s | args=%s | %s=%s\n"
                      ts
                      tool-name
                      (prin1-to-string args)
                      (if error-p "error" "result")
                      (prin1-to-string (or result "<nil>"))))
      (force-window-update (get-buffer-window buf)))))

(defvar aj8/gptel-tool-return-error t
  "When non-nil, tools return errors to the caller.
If non-nil, tool error handlers return the same human-readable error
text that is messaged in the minibuffer, instead of signaling an Emacs
error.  When nil, errors are re-signaled after being messaged and
logged.")

(defun aj8/gptel-tool--report-and-return-or-signal (tool-name args err)
  "Message and log ERR for TOOL-NAME with ARGS, then return or re-signal.

ERR is the error object received by a condition-case handler.

This builds the exact minibuffer message string for ERR, messages it,
and logs it.  If `aj8/gptel-tool-return-error' is non-nil, it returns
that string; otherwise it re-signals the original error."
  (let ((msg (format "%s: %s" tool-name (error-message-string err))))
    (message "%s" msg)
    (aj8/gptel-tool--log-to-buffer tool-name args (error-message-string err) t)
    (if aj8/gptel-tool-return-error
        msg
      (signal (car err) (cdr err)))))

(defmacro aj8/gptel-tool--with-tool (tool-name args &rest body)
  "Run BODY for TOOL-NAME and message/log the action.

TOOL-NAME is display name of the tool.  ARGS is a property list of
keyword/value pairs describing the parameters passed to the tool
function.

If ARGS is nil the minibuffer will show only the tool name (no argument
summary is displayed).

The macro binds local variables `tool-name' and `args' and then:
- Messages the running tool name and a display-safe summary of ARGS in the
  minibuffer (using `aj8/gptel-tool--truncate-for-display').
- Executes BODY and on success logs the full ARGS and the RESULT to the
  `*gptel-tool-log*' buffer and returns RESULT.
- On error it delegates to
  `aj8/gptel-tool--report-and-return-or-signal', which messages/logs and
  returns or re-signals depending on `aj8/gptel-tool-return-error'."
  `(let ((tool-name ,tool-name)
         (args ,args))
     (message "%s%s"
              tool-name
              (if args (concat " " (prin1-to-string (aj8/gptel-tool--truncate-for-display args))) ""))
     (condition-case err
         (let ((result (progn ,@body)))
           ;; (message "%s: Success" tool-name)
           (aj8/gptel-tool--log-to-buffer tool-name args result)
           result)
       (error (aj8/gptel-tool--report-and-return-or-signal tool-name args err)))))

;;; Tool definitions

;; Buffers

(defun aj8/gptel-tool-buffer-search-regexp (buffer-name regexp &optional include-columns)
  "Search for content matching REGEXP in BUFFER-NAME.
Return a newline-separated string of matching lines.  Each match is
formatted as \"LINE:TEXT\" or, if INCLUDE-COLUMNS is non-nil,
\"LINE:COLUMN:TEXT\" where LINE is the 1-based line number, COLUMN is
the 0-based column number, and TEXT is the full text of the matching
line."
  (aj8/gptel-tool--with-tool
   "tool: aj8_buffer_search_content"
   (list :buffer-name buffer-name :regexp regexp :include-columns include-columns)
   (let ((buf (get-buffer buffer-name)))
     (unless buf
       (error "Error: Buffer '%s' not found." buffer-name))
     (with-current-buffer buf
       (save-excursion
         (goto-char (point-min))
         (let ((results '()))
           (condition-case err
               (while (re-search-forward regexp nil t)
                 (let* ((match-pos (match-beginning 0))
                        (line (line-number-at-pos match-pos))
                        (col (save-excursion (goto-char match-pos) (current-column)))
                        (line-str (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                   (push (if include-columns
                             (format "%d:%d:%s" line col line-str)
                           (format "%d:%s" line line-str))
                         results)))
             (error (error "Invalid regexp: %s" regexp)))
           (if results
               (mapconcat #'identity (nreverse results) "\n")
             (format "No matches found for regexp: %s" regexp))))))))

(defun aj8/gptel-tool-open-file-in-buffer (file-path)
  "Open FILE-PATH into a visiting buffer."
  (aj8/gptel-tool--with-tool
   "tool: aj8_open_file_in_buffer"
   (list :file-path file-path)
   (unless (file-exists-p file-path)
     (error "Error: No such file: %s" file-path))
   (when (file-directory-p file-path)
     (error "Error: '%s' is a directory." file-path))
   (let ((buf (find-file-noselect file-path)))
     (format "File '%s' opened in buffer '%s'." file-path (buffer-name buf)))))

;; (defun aj8/gptel-tool-read-buffer (buffer-name)
;;   "Return the contents of BUFFER."
;;   (aj8/gptel-tool--with-tool
;;   "tool: my_read_buffer"
;;   (list :buffer buffer)
;;    (unless (buffer-live-p (get-buffer buffer))
;;      (error "Error: buffer %s is not live." buffer))
;;    (with-current-buffer buffer
;;      (buffer-substring-no-properties (point-min) (point-max)))))

(defvar aj8/gptel-tool-max-lines 100
  "Default maximum number of lines any read tool will return.")

;; (defun aj8/gptel-tool-read-buffer-lines (buffer-name &optional start-line end-line)
;;   "Read lines from BUFFER-NAME between START-LINE and END-LINE.
;; When START-LINE is nil it defaults to 1.  When END-LINE is nil it
;; defaults to the end of the buffer.  If the length of the requested range
;; exceeds `aj8/gptel-tool-max-lines' an error is signaled.  If START-LINE
;; or END-LINE fall outside the buffer bounds they are silently truncated
;; to the valid range (1..buffer length)."
;;   (aj8/gptel-tool--with-tool
;;    "tool: aj8_read_buffer_lines"
;;    (list :buffer-name buffer-name :start-line start-line :end-line end-line)
;;    (let ((buffer (get-buffer buffer-name)))
;;      (unless buffer
;;        (error "Error: Buffer '%s' not found." buffer-name))
;;      (with-current-buffer buffer
;;        (save-excursion
;;          (let* ((total-lines (count-lines (point-min) (point-max)))
;;                 (requested-start (or start-line 1))
;;                 (requested-end   (or end-line total-lines)))
;;            (when (< requested-end requested-start)
;;              (error "Error: END-LINE must be >= START-LINE"))
;;            (let* ((start-line (max 1 requested-start))
;;                   (end-line   (min total-lines requested-end))
;;                   (requested (1+ (- end-line start-line))))
;;              (when (> requested aj8/gptel-tool-max-lines)
;;                (error "Error: requested range length (%d) exceeds maximum allowed (%d)."
;;                       requested aj8/gptel-tool-max-lines))
;;              (goto-line start-line)
;;              (let ((start-pos (point)))
;;                (goto-line end-line)
;;                (let ((end-pos (line-end-position)))
;;                  (buffer-substring-no-properties start-pos end-pos))))))))))

(defun aj8/gptel-tool-read-buffer-lines-count (buffer-name &optional start-line count)
  "Read COUNT lines from BUFFER-NAME starting at line START-LINE.
When START-LINE is nil it defaults to 1.  When COUNT is nil it defaults
to `aj8/gptel-tool-max-lines'.  If COUNT is greater than
`aj8/gptel-tool-max-lines' an error is signaled.  If the requested range
extends past the end of the buffer, the function returns only the
available lines."
  (aj8/gptel-tool--with-tool
   "tool: aj8_read_buffer_lines_count"
   (list :buffer-name buffer-name :start-line start-line :count count)
   (let ((buffer (get-buffer buffer-name)))
     (unless buffer
       (error "Error: Buffer '%s' not found." buffer-name))
     (with-current-buffer buffer
       (save-excursion
         (let* ((total-lines (count-lines (point-min) (point-max)))
                (requested-start (or start-line 1))
                (requested-count (or count aj8/gptel-tool-max-lines)))
           (when (< requested-count 1)
             (error "Error: COUNT must be >= 1"))
           (when (> requested-count aj8/gptel-tool-max-lines)
             (error "Error: requested COUNT (%d) exceeds maximum allowed (%d)."
                    requested-count aj8/gptel-tool-max-lines))
           (when (< requested-start 1)
             (error "Error: START-LINE must be >= 1"))
           (when (> requested-start total-lines)
             (error "Error: START-LINE (%d) exceeds buffer length (%d)."
                    requested-start total-lines))
           (let* ((start-line requested-start)
                  (end-line (min total-lines (+ start-line (1- requested-count)))))
             (goto-line start-line)
             (let ((start-pos (point)))
               (goto-line end-line)
               (let ((end-pos (line-end-position)))
                 (buffer-substring-no-properties start-pos end-pos))))))))))

(defun aj8/gptel-tool-list-buffers (&optional include-counts)
  "Return a newline-separated string of open file-backed buffers.
Each line is of the form \"NAME: PATH\", where NAME is the buffer name
and PATH is the file path relative to the current project root.  When
the file is outside the current project, PATH is the absolute file path.
If INCLUDE-COUNTS is non-nil, append the number of lines as \" (N
lines)\"."
  (aj8/gptel-tool--with-tool
   "tool: aj8_list_buffers"
   (list :include-counts include-counts)
   (let ((lines '())
         (proj (project-current)))
     (dolist (buffer (buffer-list))
       (when (buffer-file-name buffer)
         (with-current-buffer buffer
           (let* ((buf-name (buffer-name buffer))
                  (file (buffer-file-name buffer))
                  (path (if (and file
                                 proj
                                 (file-in-directory-p file (project-root proj)))
                            (file-relative-name file (project-root proj))
                          file)))
             (if include-counts
                 (push (format "%s: %s (%d lines)" buf-name path (count-lines (point-min) (point-max))) lines)
               (push (format "%s: %s" buf-name path) lines))))))
     (mapconcat #'identity (nreverse lines) "\n"))))

(defun aj8/gptel-tool-list-all-buffers (&optional include-counts)
  "Return a newline-separated string of all open buffers.
Each line is either \"NAME: PATH\" for file-backed buffers or just
\"NAME\" for non-file buffers.  NAME is the buffer name and PATH is the
file path relative to the current project root.  When the file is
outside the current project, PATH is the absolute file path.  If
INCLUDE-COUNTS is non-nil, append the number of lines as \" (N lines)\"."
  (aj8/gptel-tool--with-tool
   "tool: aj8_list_all_buffers"
   (list :include-counts include-counts)
   (let ((lines '())
         (proj (project-current)))
     (dolist (buffer (buffer-list))
       (with-current-buffer buffer
         (let* ((buf-name (buffer-name buffer))
                (file (buffer-file-name buffer))
                (path (when file (if (and
                                      proj
                                      (file-in-directory-p file (project-root proj)))
                                     (file-relative-name file (project-root proj))
                                   file))))
           (if file
               (if include-counts
                   (push (format "%s: %s (%d lines)" buf-name path (count-lines (point-min) (point-max))) lines)
                 (push (format "%s: %s" buf-name path) lines))
             (if include-counts
                 (push (format "%s (%d lines)" buf-name (count-lines (point-min) (point-max))) lines)
               (push buf-name lines))))))
     (mapconcat #'identity (nreverse lines) "\n"))))

(defun aj8/gptel-tool-buffer-to-file (buffer-name)
  "Return the file path for BUFFER-NAME."
  (aj8/gptel-tool--with-tool
   "tool: aj8_buffer_to_file"
   (list :buffer-name buffer-name)
   (let ((buffer (get-buffer buffer-name)))
     (unless (and buffer (buffer-file-name buffer))
       (error "Error: Buffer '%s' not found or not associated with a file." buffer-name))
     (buffer-file-name buffer))))

(defun aj8/gptel-tool-file-to-buffer (file-path)
  "Return the buffer name for FILE-PATH."
  (aj8/gptel-tool--with-tool
   "tool: aj8_file_to_buffer"
   (list :file-path file-path)
   (let ((buffer (find-buffer-visiting file-path)))
     (unless buffer
       (error "Error: No buffer is visiting the file '%s'." file-path))
     (buffer-name buffer))))

(defun aj8/gptel-tool-append-to-buffer (buffer-name text)
  "Append TEXT to BUFFER-NAME."
  (aj8/gptel-tool--with-tool
   "tool: aj8_append_to_buffer"
   (list :buffer-name buffer-name :text text)
   (let ((buf (get-buffer buffer-name)))
     (unless buf
       (error "Error: Buffer '%s' not found." buffer-name))
     (with-current-buffer buf
       (save-excursion
         (goto-char (point-max))
         (insert text)))
     (format "Text successfully appended to buffer %s." buffer-name))))

(defun aj8/gptel-tool-insert-in-buffer (buffer-name text line-number)
  "Insert TEXT in BUFFER-NAME at LINE-NUMBER.
The text is inserted at the beginning of the specified line."
  (aj8/gptel-tool--with-tool
   "tool: aj8_insert_in_buffer"
   (list :buffer-name buffer-name :text text :line-number line-number)
   (let ((buf (get-buffer buffer-name)))
     (unless buf
       (error "Error: Buffer '%s' not found." buffer-name))
     (with-current-buffer buf
       (save-excursion
         (let ((total-lines (count-lines (point-min) (point-max))))
           (when (< line-number 1)
             (error "Error: LINE-NUMBER must be >= 1"))
           (when (> line-number total-lines)
             (error "Error: LINE-NUMBER (%d) exceeds buffer length (%d)."
                    line-number total-lines))
           (goto-line line-number)
           (insert text))))
     (format "Text successfully inserted into buffer %s at line %d." buffer-name line-number))))

(defun aj8/gptel-tool-replace-buffer (buffer-name content)
  "Overwrite BUFFER-NAME with CONTENT."
  (aj8/gptel-tool--with-tool
   "tool: aj8_replace_buffer"
   (list :buffer-name buffer-name :content content)
   (let ((buf (get-buffer buffer-name)))
     (unless buf
       (error "Error: Buffer '%s' not found." buffer-name))
     (with-current-buffer buf
       (erase-buffer)
       (insert content))
     (format "Buffer %s successfully modified." buffer-name))))

(defun aj8/gptel-tool-edit-buffer-string (buffer-name old-string new-string)
  "Replace a single instance of OLD-STRING with NEW-STRING in BUFFER-NAME."
  (aj8/gptel-tool--with-tool
   "tool: aj8_edit_buffer_string"
   (list :buffer-name buffer-name :old-string old-string :new-string new-string)
   (let ((buffer (get-buffer buffer-name)))
     (unless buffer
       (error "Error: Buffer '%s' not found." buffer-name))
     (with-current-buffer buffer
       (save-excursion
         (let ((count (count-matches (regexp-quote old-string)
                                     (point-min) (point-max))))
           (cond
            ((= count 0)
             (error "Error: String '%s' not found in buffer '%s'." old-string buffer-name))
            ((> count 1)
             (error "Error: String '%s' is not unique in buffer '%s'. Found %d occurrences." old-string buffer-name count))
            (t
             (goto-char (point-min))
             (replace-string old-string new-string)
             (format "String in buffer '%s' successfully replaced." buffer-name)))))))))

(defun aj8/gptel-tool-replace-buffer-line (buffer-name line-number content)
  "Replace line LINE-NUMBER in file BUFFER-NAME with CONTENT.
This wrapper function delegates replacement to
`aj8/gptel-tool-replace-buffer-lines' with START-LINE and END-LINE both
equal to LINE-NUMBER."
  (aj8/gptel-tool--with-tool
   "tool: aj8_replace_buffer_line"
   (list :buffer-name buffer-name :line-number line-number :content content)
   (let ((result (aj8/gptel-tool-replace-buffer-lines buffer-name line-number line-number content)))
     ;; If the result is an error string (when aj8/gptel-tool-return-error
     ;; is t), replace the tool name to match this wrapper function
     (if (and aj8/gptel-tool-return-error (string-match "^tool: aj8_replace_buffer_lines: " result))
         (replace-regexp-in-string "^tool: aj8_replace_buffer_lines: " "tool: aj8_replace_buffer_line: " result)
       ;; If not an error, return success message
       (format "Line %d in buffer '%s' successfully replaced." line-number buffer-name)))))

(defun aj8/gptel-tool-replace-buffer-lines (buffer-name start-line end-line content)
  "Replace lines START-LINE through END-LINE in BUFFER-NAME with CONTENT."
  (aj8/gptel-tool--with-tool
   "tool: aj8_replace_buffer_lines"
   (list :buffer-name buffer-name :start-line start-line :end-line end-line :content content)
   (let ((buf (get-buffer buffer-name)))
     (unless buf
       (error "Error: Buffer '%s' not found." buffer-name))
     (with-current-buffer buf
       (save-excursion
         (let ((total-lines (count-lines (point-min) (point-max))))
           (when (< start-line 1)
             (error "Error: START-LINE must be >= 1"))
           (when (< end-line start-line)
             (error "Error: END-LINE must be >= START-LINE"))
           (when (> end-line total-lines)
             (error "Error: END-LINE exceeds buffer length (%d)." total-lines))
           (goto-line start-line)
           (let ((beg (point)))
             (goto-line end-line)
             (end-of-line)
             (delete-region beg (point))
             (insert content)))))
     (format "Line range %d-%d in buffer '%s' successfully replaced."
             start-line end-line buffer-name))))

(defun aj8/gptel-tool-delete-buffer-string (buffer-name old-string)
  "Delete a single instance of OLD-STRING in BUFFER-NAME.
This wrapper function delegates deletion to
`aj8/gptel-tool-edit-buffer-string' by setting NEW-STRING to an empty
string."
  (aj8/gptel-tool--with-tool
   "tool: aj8_delete_buffer_string"
   (list :buffer-name buffer-name :old-string old-string)
   (let ((result (aj8/gptel-tool-edit-buffer-string buffer-name old-string "")))
     ;; If the result is an error string (when aj8/gptel-tool-return-error
     ;; is t), replace the tool name to match this wrapper function
     (if (and aj8/gptel-tool-return-error (string-match "^tool: aj8_edit_buffer_string: " result))
         (replace-regexp-in-string "^tool: aj8_edit_buffer_string: " "tool: aj8_delete_buffer_string: " result)
       ;; If not an error, return success message
       (format "String in buffer '%s' successfully deleted." buffer-name)))))

(defun aj8/gptel-tool-delete-buffer-line (buffer-name line-number)
  "Delete line LINE-NUMBER in BUFFER-NAME.
This wrapper function delegates deletion to
`aj8/gptel-tool-replace-buffer-line' by setting CONTENT to an empty
string."
  (aj8/gptel-tool--with-tool
   "tool: aj8_delete_buffer_line"
   (list :buffer-name buffer-name :line-number line-number)
   (let ((result (aj8/gptel-tool-replace-buffer-line buffer-name line-number "")))
     ;; If the result is an error string (when aj8/gptel-tool-return-error
     ;; is t), replace the tool name to match this wrapper function
     (if (and aj8/gptel-tool-return-error (string-match "^tool: aj8_replace_buffer_line: " result))
         (replace-regexp-in-string "^tool: aj8_replace_buffer_line: " "tool: aj8_delete_buffer_line: " result)
       ;; If not an error, return success message
       (format "Line %d in buffer '%s' successfully deleted." line-number buffer-name)))))

(defun aj8/gptel-tool-delete-buffer-lines (buffer-name start-line end-line)
  "Delete lines START-LINE through END-LINE in BUFFER-NAME.
This wrapper function delegates deletion to
`aj8/gptel-tool-replace-buffer-lines' by setting CONTENT to an empty
string."
  (aj8/gptel-tool--with-tool
   "tool: aj8_delete_buffer_lines"
   (list :buffer-name buffer-name :start-line start-line :end-line end-line)
   (let ((result (aj8/gptel-tool-replace-buffer-lines buffer-name start-line end-line "")))
     ;; If the result is an error string (when aj8/gptel-tool-return-error
     ;; is t), replace the tool name to match this wrapper function
     (if (and aj8/gptel-tool-return-error (string-match "^tool: aj8_replace_buffer_lines: " result))
         (replace-regexp-in-string "^tool: aj8_replace_buffer_lines: " "tool: aj8_delete_buffer_lines: " result)
       ;; If not an error, return success message
       (format "Line range %d-%d in buffer '%s' successfully deleted."
               start-line end-line buffer-name)))))

(defun aj8/--apply-buffer-edits (buffer-name buffer-edits edit-type)
  "Apply a list of edits to BUFFER-NAME.

BUFFER-EDITS is a list of property lists describing edits.  Each edit is
a plist with the following keys:
- :line-number (integer) -- The line to edit.
- :old-string (string) -- The text to replace. Must not contain newline
                          characters.
- :new-string (string) -- The replacement text to insert.

EDIT-TYPE can be 'line or 'string.  For 'line edits the :old-string is
compared against the entire line; when equal the entire line is replaced
with :new-string.  For 'string edits the function searches from the
beginning of the specified line to the end of the line for the first
occurrence of :old-string and replaces that occurrence with :new-string.

Edits are applied in descending order of :line-number to avoid shifting
subsequent line numbers."
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Error: Buffer '%s' not found." buffer-name))
    (with-current-buffer buffer
      (let* ((edits (if (vectorp buffer-edits) (append buffer-edits nil) buffer-edits))
             (sorted-edits (sort edits #'(lambda (a b)
                                           (> (plist-get a :line-number)
                                              (plist-get b :line-number)))))
             (total 0)
             (applied 0)
             (failures nil))
        (dolist (edit sorted-edits)
          (setq total (1+ total))
          (let ((line-number (plist-get edit :line-number))
                (old-string (plist-get edit :old-string))
                (new-string (plist-get edit :new-string))
                (success nil))
            ;; Validate that old-string is single-line.
            (if (string-match-p "\n" old-string)
                (push (list line-number
                            "old-string contains newline"
                            old-string)
                      failures)
              (save-excursion
                (goto-line line-number)
                (cond
                 ((eq edit-type 'line)
                  (let ((line-start (point)))
                    (when (string-equal (buffer-substring-no-properties line-start (line-end-position)) old-string)
                      (delete-region line-start (line-end-position))
                      (insert new-string)
                      (setq success t))))
                 ((eq edit-type 'string)
                  (when (search-forward old-string (line-end-position) t)
                    (replace-match new-string nil nil)
                    (setq success t))))
                (when success
                  (setq applied (1+ applied)))
                (unless success
                  (push (list line-number
                              (if (eq edit-type 'line)
                                  "entire line did not equal old-string"
                                "old-string not found on the line")
                              old-string)
                        failures))))))
        (when failures
          (let ((failed (length failures))
                (details (mapconcat
                          (lambda (f)
                            (format " - line %d: %s (old-string: %S)"
                                    (nth 0 f) (nth 1 f) (nth 2 f)))
                          (nreverse failures)
                          "\n")))
            (error "Error applying edits to buffer '%s': %d (out of %d) failed.\n%s"
                   buffer-name failed total details)))))))

(defun aj8/--review-buffer-edits (buffer-name buffer-edits edit-type)
  "Review a list of buffer edits in Ediff.

Creates a temporary buffer containing the original buffer's content with
the proposed edits applied, then launches an Ediff session to visually
compare the original buffer against the edited version.

BUFFER-EDITS is a list of property lists with the same shape as
described for `aj8/--apply-buffer-edits': each edit should contain
:line-number, :old-string, and :new-string.

EDIT-TYPE can be 'line or 'string, as described in
`aj8/--apply-buffer-edits'."
  (let ((original-buffer (get-buffer buffer-name)))
    (unless original-buffer
      (error "Error: Buffer '%s' not found." buffer-name))

    (let* ((temp-buffer-name (format "*%s-edits*"
                                     (string-trim buffer-name "*" "*")))
           (temp-buffer (get-buffer-create temp-buffer-name)))
      (unwind-protect
          (progn
            ;; Prepare the edited version in a temporary buffer
            (with-current-buffer temp-buffer
              (erase-buffer)
              (insert-buffer-substring original-buffer)
              (condition-case err
                  (aj8/--apply-buffer-edits temp-buffer-name buffer-edits edit-type)
                (error
                 (error "%s\nNote: No review was started and no changes were applied to buffer '%s'. Any details above refer only to the temporary review buffer."
                        (error-message-string err) buffer-name))))

            ;; Start Ediff
            (ediff-buffers original-buffer temp-buffer))
        ;; Cleanup: kill the temporary buffer
        (when (buffer-live-p temp-buffer)
          (kill-buffer temp-buffer))))))

(defun aj8/gptel-tool-apply-buffer-string-edits (buffer-name buffer-edits)
  "Edit BUFFER-NAME with a list of string edits.
BUFFER-EDITS is a list of property lists where each edit must contain
the keys :line-number (integer), :old-string (string), and
:new-string (string).  The function searches from the start of the
specified line to the end of the line for the first occurrence of
:old-string and replaces it with :new-string.  Edits are applied in
descending order of :line-number."
  (aj8/gptel-tool--with-tool
   "tool: aj8_apply_buffer_string_edits"
   (list :buffer-name buffer-name :buffer-edits buffer-edits)
   (aj8/--apply-buffer-edits buffer-name buffer-edits 'string)
   (format "String edits successfully applied to buffer %s." buffer-name)))

(defun aj8/gptel-tool-apply-buffer-string-edits-with-review (buffer-name buffer-edits)
  "Edit BUFFER-NAME with a list of string edits and review with Ediff.
BUFFER-EDITS is a list of property lists where each edit must contain
the keys :line-number (integer), :old-string (string), and
:new-string (string).  This function prepares a temporary buffer with
the string edits applied and launches `ediff-buffers' to let the user
review the changes interactively.  Edits are applied to the temporary
buffer only; the original buffer is not modified by this command."
  (aj8/gptel-tool--with-tool
   "tool: aj8_apply_buffer_string_edits_with_review"
   (list :buffer-name buffer-name :buffer-edits buffer-edits)
   (aj8/--review-buffer-edits buffer-name buffer-edits 'string)
   (format "Ediff session started for %s. Please complete the review." buffer-name)))

(defun aj8/gptel-tool-apply-buffer-line-edits (buffer-name buffer-edits)
  "Edit BUFFER-NAME with a list of line edits.
BUFFER-EDITS is a list of property lists where each edit must contain
the keys :line-number (integer), :old-string (string), and
:new-string (string).  The :old-string is compared against the entire
line; when it matches, the line is replaced with :new-string.  Edits are
applied in descending order of :line-number."
  (aj8/gptel-tool--with-tool
   "tool: aj8_apply_buffer_line_edits"
   (list :buffer-name buffer-name :buffer-edits buffer-edits)
   (aj8/--apply-buffer-edits buffer-name buffer-edits 'line)
   (format "Line edits successfully applied to buffer %s." buffer-name)))

(defun aj8/gptel-tool-apply-buffer-line-edits-with-review (buffer-name buffer-edits)
  "Edit BUFFER-NAME with a list of line edits and review with Ediff.
BUFFER-EDITS is a list of property lists where each edit must contain
the keys :line-number (integer), :old-string (string), and
:new-string (string).  This function prepares a temporary buffer with
the line edits applied and launches `ediff-buffers' to let the user
review the changes interactively.  Edits are applied to the temporary
buffer only; the original buffer is not modified by this command."
  (aj8/gptel-tool--with-tool
   "tool: aj8_apply_buffer_line_edits_with_review"
   (list :buffer-name buffer-name :buffer-edits buffer-edits)
   (aj8/--review-buffer-edits buffer-name buffer-edits 'line)
   (format "Ediff session started for %s. Please complete the review." buffer-name)))

;; Files

;; (defun aj8/gptel-tool-create-file (file-path content)
;;   "Create a new file at FILE-PATH with CONTENT."
;;   (with-temp-message "Running tool: aj8_create_file"
;;     (let ((full-path (expand-file-name file-path)))
;;       (with-temp-buffer
;;         (insert content)
;;         (write-file full-path t)) ; The 't' arg prevents confirmation prompts
;;       (format "Successfully created file: %s" full-path))))

;; Emacs

(defun aj8/gptel-tool-read-documentation (symbol-name)
  "Read the documentation for SYMBOL-NAME."
  (aj8/gptel-tool--with-tool
   "tool: aj8_read_documentation"
   (list :symbol-name symbol-name)
   (let* ((sym (intern-soft symbol-name))
          (doc (if (fboundp sym)
                   ;; Functions
                   (documentation sym)
                 ;; Variables
                 (documentation-property sym 'variable-documentation))))
     (or doc (format "No documentation found for symbol '%s'." symbol-name)))))

(defun aj8/gptel-tool-read-function (function-name)
  "Return the definition of FUNCTION-NAME."
  (aj8/gptel-tool--with-tool
   "tool: aj8_read_function"
   (list :function-name function-name)
   (let ((func-symbol (intern-soft function-name)))
     (unless (and func-symbol (fboundp func-symbol))
       (error "Error: Function '%s' is not defined." function-name))
     (let ((func-def (symbol-function func-symbol)))
       (cond
        ;; Built-in primitive functions (C functions)
        ((subrp func-def)
         (format "Function '%s' is a built-in primitive (subr); it has no Lisp source code." function-name))
        ;; All other functions
        (t
         (condition-case err
             (let ((location (find-function-noselect func-symbol t)))
               (if (cdr location)
                   ;; Found source location
                   (with-current-buffer (car location)
                     (save-excursion
                       (goto-char (cdr location))
                       (let ((beg (point)))
                         (forward-sexp 1)
                         (buffer-substring-no-properties beg (point)))))
                 ;; No source location found
                 (format "Function '%s' source could not be located." function-name)))
           (error (format "Error finding function '%s': %s" function-name (error-message-string err))))))))))

(defun aj8/gptel-tool-read-library (library-name)
  "Return the source code of LIBRARY-NAME."
  (aj8/gptel-tool--with-tool
   "tool: aj8_read_library"
   (list :library-name library-name)
   (let ((file (condition-case nil
                   (find-library-name library-name)
                 (error nil))))
     (unless file (error "Library '%s' not found." library-name))
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string)))))

(defun aj8/gptel-tool-read-info-symbol (symbol-name)
  "Return the contents of the Info node for SYMBOL-NAME.

SYMBOL-NAME should be the name of an Emacs Lisp function, macro, or
variable (e.g., \"defun\", \"let\", \"buffer-string\").  The function
uses `info-lookup-symbol` to find where the symbol is documented and
returns the content of that Info page."
  (aj8/gptel-tool--with-tool
   "tool: aj8_read_info_symbol"
   (list :symbol-name symbol-name)
   ;; Track existing Info buffers before lookup to enable targeted cleanup
   (let ((info-buffer-names-before
          (mapcar #'buffer-name
                  (cl-remove-if-not
                   (lambda (buf)
                     (string-match-p "\\*info\\*\\(<[0-9]+>\\)?$" (buffer-name buf)))
                   (buffer-list)))))
     (unwind-protect
         (with-temp-buffer
           (emacs-lisp-mode)
           (let ((result (info-lookup-symbol (intern symbol-name))))
             (unless result (error "Cannot find Info node for symbol '%s'." symbol-name))
             (let ((info-buffer (get-buffer "*info*")))
               (unless info-buffer (error "Not documented as a symbol: %s" symbol-name))
               (with-current-buffer info-buffer
                 (buffer-string)))))
       ;; Cleanup: kill any new Info buffers that were created
       (dolist (buffer (buffer-list))
         (when (and (string-match-p "\\*info\\*\\(<[0-9]+>\\)?$" (buffer-name buffer))
                    (not (member (buffer-name buffer) info-buffer-names-before)))
           (kill-buffer buffer)))))))

(defun aj8/gptel-tool-read-info-node (node-name)
  "Return the contents of the Info node NODE-NAME.

NODE-NAME should be the name of a section in the Elisp manual, such as
\"Control Structures\", \"Variables\", or \"Functions\".  The function
navigates to (elisp)NODE-NAME and returns the content from that Info
page."
  (aj8/gptel-tool--with-tool
   "tool: aj8_read_info_node"
   (list :node-name node-name)
   ;; Track existing Info buffers  before lookup to enable targeted cleanup
   (let ((info-buffer-names-before
          (mapcar #'buffer-name
                  (cl-remove-if-not
                   (lambda (buf)
                     (string-match-p "\\*info\\*\\(<[0-9]+>\\)?$" (buffer-name buf)))
                   (buffer-list)))))
     (unwind-protect
         (with-current-buffer info-buffer
           (Info-goto-node (format "(elisp)%s" node-name))
           (buffer-string))
       (kill-buffer info-buffer)
       ;; Cleanup: kill any new Info buffers that were created
       (dolist (buffer (buffer-list))
         (when (and (string-match-p "\\*info\\*\\(<[0-9]+>\\)?$" (buffer-name buffer))
                    (not (member (buffer-name buffer) info-buffer-names-before)))
           (kill-buffer buffer)))))))

;; Project

(defun aj8/gptel-tool-project-get-root ()
  "Get the root directory of the current project."
  (aj8/gptel-tool--with-tool
   "tool: aj8_project_get_root" nil
   (let ((project (project-current)))
     (unless project (error "Not inside a project."))
     (project-root project))))

(defun aj8/gptel-tool-project-list-files (&optional include-counts)
  "Return a newline-separated string listing all files in the current project.

Each line is of the form \"NAME: PATH\".  If INCLUDE-COUNTS is non-nil,
append the number of lines as \"NAME: PATH (N lines)\".

NAME is the base name of the file and PATH is the path relative to the
project root."
  (aj8/gptel-tool--with-tool
   "tool: aj8_project_list_files"
   (list :include-counts include-counts)
   (let ((project (project-current)))
     (unless project (error "Not inside a project."))
     (let* ((root (project-root project))
            (project-file-list (project-files project)))
       (mapconcat (lambda (f)
                    (let* ((rel (file-relative-name f root))
                           (name (file-name-nondirectory f)))
                      (if include-counts
                          (let ((nlines (with-temp-buffer
                                          (insert-file-contents f)
                                          (count-lines (point-min) (point-max)))))
                            (format "%s: %s (%d lines)" name rel nlines))
                        (format "%s: %s" name rel))))
                  project-file-list "\n")))))

;; (defun aj8/gptel-tool-project-find-files (pattern)
;;   "In the current project, find files whose filenames contain PATTERN.
;; This function respects .gitignore.  It does not return directories."
;;   (with-temp-message "Running tool: my_project_find_files"
;;     (let ((proj (project-current)))
;;       (if (not proj)
;;           (error "No project found in the current context.")
;;         (let ((all-files (project-files proj)))
;;           (seq-filter (lambda (file) (string-search pattern (file-name-nondirectory file))) all-files))))))

(defun aj8/gptel-tool-project-find-files-glob (pattern &optional include-counts)
  "In the current project, find files whose filenames match the glob PATTERN.
Returns a newline-separated string where each line is \"NAME: PATH\".
NAME is the file's base name and PATH is the path relative to the
project root.  If INCLUDE-COUNTS is non-nil append the number of lines
as \" (N lines)\".  This function respects .gitignore."
  (aj8/gptel-tool--with-tool
   "tool: aj8_project_find_files_glob"
   (list :pattern pattern :include-counts include-counts)
   (let ((proj (project-current)))
     (unless proj
       (error "No project found in the current context."))
     (let* ((root (project-root proj))
            ;; Get list of non-ignored files from project.el (absolute paths)
            (project-file-list (project-files proj))
            ;; Get list of files matching glob from filesystem (absolute paths)
            (wildcard-file-list
             (let ((default-directory root))
               ;; The 't' argument makes it return absolute paths
               (file-expand-wildcards pattern t)))
            ;; Return the files present in both lists
            (matched (seq-intersection project-file-list wildcard-file-list #'string-equal)))
       ;; Return as newline-separated relative paths, with optional line counts
       (mapconcat (lambda (f)
                    (let* ((rel (file-relative-name f root))
                           (name (file-name-nondirectory f)))
                      (if include-counts
                          (let ((nlines (with-temp-buffer
                                          (insert-file-contents f)
                                          (count-lines (point-min) (point-max)))))
                            (format "%s: %s (%d lines)" name rel nlines))
                        (format "%s: %s" name rel))))
                  matched "\n")))))

(defun aj8/gptel-tool-project-search-regexp (regexp &optional include-columns)
  "In the current project, recursively search for content matching REGEXP.
Results are newline-separated strings of matching lines, each specifying
PATH:LINE:TEXT, or if INCLUDE-COLUMNS is non-nil, PATH:LINE:COLUMN:TEXT.
Both line and column numbers are 1-based.  This search respects
.gitignore."
  (aj8/gptel-tool--with-tool
   "tool: aj8_project_search_regexp"
   (list :regexp regexp :include-columns include-columns)
   (let ((project (project-current)))
     (unless project (error "Not inside a project."))
     (let ((command (cond
                     ((executable-find "rg")
                      (let ((base (list "rg" "--no-heading" "--line-number" "--hidden" "--glob" "!.git/**"))
                            (flags (if include-columns
                                       (list "--column" "--vimgrep")
                                     (list "--no-column"))))
                        (append base flags (list "--regexp" regexp))))
                     ((executable-find "git")
                      (let ((base (list "git" "grep" "--full-name" "--perl-regexp" "--line-number"))
                            (flags (and include-columns (list "--column"))))
                        (append base flags (list "-e" regexp))))
                     (t (error "Neither 'rg' nor 'git' found for searching."))))
           (output-buffer (generate-new-buffer "*search-output*")))
       (unwind-protect
           (let* ((default-directory (project-root project))
                  (status (apply #'call-process (car command) nil
                                 output-buffer nil (cdr command))))
             (cond
              ((zerop status)
               (with-current-buffer output-buffer
                 (string-trim-right (buffer-string))))
              ((= status 1)
               (format "No matches found for regexp: %s" regexp))
              (t
               (error "Search command '%s' failed with status %d for regexp: %s"
                      (car command) status regexp))))
         (kill-buffer output-buffer))))))

;;; Tool Registrations

;; Buffers

(gptel-make-tool
 :function #'aj8/gptel-tool-buffer-search-regexp
 :name "aj8_buffer_search_content"
 :description "Search BUFFER for content matching REGEXP. This returns a newline-separated string of matching lines. Each line is formatted as LINE:TEXT or, if INCLUDE-COLUMNS is non-nil, LINE:COLUMN:TEXT where LINE is 1-based and COLUMN is 0-based."
 :args '((:name "buffer-name"
                :type string
                :description "The name of the buffer to search.")
         (:name "regexp"
                :type string
                :description "The regexp to search for in the buffer.")
         (:name "include-columns"
                :type boolean
                :optional t
                :description "If non-nil, include 0-based column numbers in the result."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-open-file-in-buffer
 :name "aj8_open_file_in_buffer"
 :description "Open a file into a visiting buffer."
 :args (list '(:name "file-path"
                     :type string
                     :description "Path to the file to open."))
 :category "buffers")

;; (gptel-make-tool
;;  :function #'aj8/gptel-tool-read-buffer
;;  :name "aj8_read_buffer"
;;  :description "Return the contents of a buffer."
;;  :args (list '(:name "buffer-name"
;;                       :type string
;;                       :description "The name of the buffer to read."))

;; (gptel-make-tool
;;  :function #'aj8/gptel-tool-read-buffer-lines
;;  :name "aj8_read_buffer_lines"
;;  :description (format "Read lines from a buffer; max lines per call: %d. Use chunking for larger ranges. START-LINE and END-LINE are optional 1-based line numbers; if START-LINE is nil, read from the beginning of the buffer. If END-LINE is nil, read to the end of the buffer." aj8/gptel-tool-max-lines)
;;  :args (list '( :name "buffer-name"
;;                 :type string
;;                 :description "The name of the buffer to read the contents of.")
;;              '( :name "start-line"
;;                 :type integer
;;                 :optional t
;;                 :description "The first line to read from.")
;;              '( :name "end-line"
;;                 :type integer
;;                 :optional t
;;                 :description "The last line to read to."))
;;  :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-read-buffer-lines-count
 :name "aj8_read_buffer_lines_count"
 :description (format "Read lines from a buffer; max lines per call: %d. START and COUNT are optional and default to 1 and %d, respectively." aj8/gptel-tool-max-lines aj8/gptel-tool-max-lines)
 :args (list '( :name "buffer-name"
                :type string
                :description "The name of the buffer to read from.")
             '( :name "start-line"
                :type integer
                :optional t
                :description "The 1-based line number to start reading from (default is 1).")
             (list :name "count"
                   :type 'integer
                   :optional t
                   :description (format "The number of lines to read (must be <= %d)."
                                        aj8/gptel-tool-max-lines)))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-list-buffers
 :name "aj8_list_buffers"
 :description "Return a newline-separated string listing all currently open buffers that are associated with a file. Each line is of the form \"NAME: PATH\" where NAME is the buffer name and PATH is the file path relative to the current project root when the file is inside the current project; otherwise PATH is the absolute file path. If the optional argument INCLUDE-COUNTS is non-nil, append the number of lines as \" (N lines)\"."
 :args '((:name "include-counts"
                :type boolean
                :optional t
                :description "If non-nil, append the number of lines to each entry as ' (N lines)'."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-list-all-buffers
 :name "aj8_list_all_buffers"
 :description "Return a newline-separated string listing all currently open buffers. Each line is either \"NAME: PATH\" for file-backed buffers or just \"NAME\" for non-file buffers. NAME is the buffer name and PATH is the file path relative to the current project root when the file is inside the current project; otherwise PATH is the absolute file path. If the optional argument INCLUDE-COUNTS is non-nil, append the number of lines as \" (N lines)\"."
 :args '((:name "include-counts"
                :type boolean
                :optional t
                :description "If non-nil, append the number of lines to each entry as ' (N lines)'."))
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
 :description "Append text to a buffer (at the end of the buffer)."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to append text to.")
             '(:name "text"
                     :type string
                     :description "The text to append to the buffer."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-insert-in-buffer
 :name "aj8_insert_in_buffer"
 :description "Insert text in a buffer at a specific line number. The text is inserted at the beginning of the specified line."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to insert text into.")
             '(:name "text"
                     :type string
                     :description "The text to insert in the buffer.")
             '(:name "line-number"
                     :type integer
                     :description "The 1-based line number where the text should be inserted."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-replace-buffer
 :name "aj8_replace_buffer"
 :description "Completely overwrite the contents of a buffer."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to overwrite.")
             '(:name "content"
                     :type string
                     :description "The content to write to the buffer."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-edit-buffer-string
 :name "aj8_edit_buffer_string"
 :description "Edit a buffer by replacing a single instance of an exact string: The tool replaces a single instance of OLD-STRING with NEW-STRING in BUFFER. OLD-STRING is treated literally and may contain newline characters; it must occur exactly once. NEW-STRING may contain newline characters and will be inserted as-is."
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
 :function #'aj8/gptel-tool-replace-buffer-line
 :name "aj8_replace_buffer_line"
 :description "Replace a single line in a buffer with new content. The new content may contain newline characters."
 :args '((:name "buffer-name" :type string :description "The name of the buffer to modify.")
         (:name "line-number" :type integer :description "The 1-based line number of the line to replace.")
         (:name "content" :type string :description "The new content."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-replace-buffer-lines
 :name "aj8_replace_buffer_lines"
 :description "Replace a range of lines in a buffer with new content. The new content may contain newline characters. To replace a single line set 'start-line==end-line'. Line numbers are 1-based."
 :args (list '(:name "buffer-name" :type string
                     :description "The name of the buffer to modify.")
             '(:name "start-line" :type integer
                     :description "The first line of the range to replace.")
             '(:name "end-line" :type integer
                     :description "The last line of the range to replace.")
             '(:name "content" :type string
                     :description "The new content."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-delete-buffer-string
 :name "aj8_delete_buffer_string"
 :description "Delete a single instance of an exact string from a buffer. OLD-STRING is treated literally and may contain newline characters; it must occur exactly once."
 :args '((:name "buffer-name"
                :type string
                :description "The name of the buffer to edit.")
         (:name "old-string"
                :type string
                :description "The text to delete."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-delete-buffer-line
 :name "aj8_delete_buffer_line"
 :description "Delete a single line in a buffer."
 :args '((:name "buffer-name" :type string :description "The name of the buffer to modify.")
         (:name "line-number" :type integer :description "The 1-based line number of the line to delete."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-delete-buffer-lines
 :name "aj8_delete_buffer_lines"
 :description "Delete a range of lines in a buffer. To delete a single line set 'start-line==end-line', Line numbers are 1-based."
 :args (list '(:name "buffer-name" :type string
                     :description "The name of the buffer to modify.")
             '(:name "start-line" :type integer
                     :description "The first line of the range to delete.")
             '(:name "end-line" :type integer
                     :description "The last line of the range to delete."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-apply-buffer-string-edits
 :name "aj8_apply_buffer_string_edits"
 :description "Edit a buffer with a list of string edits, applying changes directly without review. Each edit contains a 'line-number', an 'old-string' and a 'new-string'. The 'old-string' must be found entirely on the specified line (it must not contain newline characters). The 'new-string' may contain newline characters and will be inserted as-is. Edits are applied from the bottom of the buffer to the top to handle line number changes correctly."
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
 :function #'aj8/gptel-tool-apply-buffer-string-edits-with-review
 :name "aj8_apply_buffer_string_edits_with_review"
 :description "Edit a buffer with a list of string edits and start an Ediff session for review. Each edit contains a 'line-number', an 'old-string' and a 'new-string'. The 'old-string' must be found entirely on the specified line (it must not contain newline characters). The 'new-string' may contain newline characters and will be inserted as-is. Edits are applied from the bottom of the buffer to the top to handle line number changes correctly.

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

(gptel-make-tool
 :function #'aj8/gptel-tool-apply-buffer-line-edits
 :name "aj8_apply_buffer_line_edits"
 :description "Edit a buffer with a list of edits, applying changes directly without review. Each edit targets a specific line and must contain a 'line-number' and an 'old-string' that must exactly match that line's content (old-string must not contain newline characters). The 'new-string' will replace the line and may contain newline characters; it will be inserted as-is. Edits are applied from the bottom of the buffer to the top to handle line number changes correctly."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to edit.")
             '(:name "buffer-edits"
                     :type array
                     :items (:type object
                                   :properties
                                   (:line-number
                                    (:type integer :description "The 1-based line number of the line to modify.")
                                    :old-string
                                    (:type string :description "The entire original content of the line to be replaced.")
                                    :new-string
                                    (:type string :description "The new content.")))
                     :description "The list of edits to apply to the buffer."))
 :category "buffers")

(gptel-make-tool
 :function #'aj8/gptel-tool-apply-buffer-line-edits-with-review
 :name "aj8_apply_buffer_line_edits_with_review"
 :description "Edit a buffer with a list of edits and start an Ediff session for review. Each edit targets a specific line and must contain a 'line-number' and an 'old-string' that must exactly match that line's content (old-string must not contain newline characters). The 'new-string' will replace the matched text and may contain newline characters; it will be inserted as-is. Edits are applied from the bottom of the buffer to the top to handle line number changes correctly.

This action requires manual user review. After calling this tool, you must stop and instruct the user to complete the review in the Ediff session and to notify you when they are finished. Do not proceed with any other tools or actions until you receive confirmation from the user."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to edit.")
             '(:name "buffer-edits"
                     :type array
                     :items (:type object
                                   :properties
                                   (:line-number
                                    (:type integer :description "The 1-based line number of the line to modify.")
                                    :old-string
                                    (:type string :description "The entire original content of the line to be replaced.")
                                    :new-string
                                    (:type string :description "The new content.")))
                     :description "The list of edits to apply to the buffer."))
 :category "buffers")

;; Files

;; (gptel-make-tool
;;  :function #'aj8/gptel-tool-create-file
;;  :name "aj8_create_file"
;;  :description "Create a new file with the specified content. Overwrites the file if it already exists."
;;  :args '((:name "file-path"
;;                 :type string
;;                 :description "The path of the file to create.")
;;          (:name "content"
;;                 :type string
;;                 :description "The content to write to the new file."))

;; Emacs

(gptel-make-tool
 :function #'aj8/gptel-tool-read-documentation
 :name "aj8_read_documentation"
 :description "Return the documentation for a given Emacs Lisp symbol. The symbol can be either a function or a variable"
 :args (list '(:name "symbol-name"
                     :type string
                     :description "The name of the symbol to return the documentation for."))
 :category "emacs")

(gptel-make-tool
 :function #'aj8/gptel-tool-read-function
 :name "aj8_read_function"
 :description "Return the code of a given Emacs Lisp function."
 :args (list '(:name "function-name"
                     :type string
                     :description "The name of the function to return the code for."))
 :category "emacs")

(gptel-make-tool
 :function #'aj8/gptel-tool-read-library
 :name "aj8_read_library"
 :description "Return the source code for an Emacs library or package."
 :args (list '(:name "library-name"
                     :type string
                     :description "The name of the library or package to return source code for."))
 :category "emacs")

(gptel-make-tool
 :function #'aj8/gptel-tool-read-info-symbol
 :name "aj8_read_info_symbol"
 :description "Return the contents of the info node where a given Emacs Lisp symbol is documented."
 :args (list '(:name "symbol-name"
                     :type string
                     :description "The name of the symbol to look up."))
 :category "emacs")

(gptel-make-tool
 :function #'aj8/gptel-tool-read-info-node
 :name "aj8_read_info_node"
 :description "Return the contents of a specific info node from the Emacs Lisp manual."
 :args (list '(:name "node-name" :type string :description "The name of the node in the Emacs Lisp manual."))
 :category "emacs")

;; Project

(gptel-make-tool
 :function #'aj8/gptel-tool-project-get-root
 :name "aj8_project_get_root"
 :description "Get the root directory of the current project."
 :args nil
 :category "project")

(gptel-make-tool
 :function #'aj8/gptel-tool-project-list-files
 :name "aj8_project_list_files"
 :description "Return a newline-separated string listing all files in the current project. Each line contains a file base name followed by its path relative to the current project root. if the optional argument INCLUDE-COUNTS is non-nil, append the line count as \" (N lines).\""
 :args '((:name "include-counts"
                :type boolean
                :optional t
                :description "If non-nil, append the number of lines to each entry as ' (N lines)'."))
 :category "project")

;;   (gptel-make-tool
;;    :function #'aj8/gptel-tool-project-find-files
;;    :name "aj8_project_find_files"
;;    :description "In the current project, recursively find files whose filenames contain pattern. This search is case-sensitive. It does not find directories."
;;    :args '((:name "pattern"
;;                   :type string
;;                   :description "A pattern to match against the filenames in the project."))
;;    :category "proj

(gptel-make-tool
 :function #'aj8/gptel-tool-project-find-files-glob
 :name "aj8_project_find_files_glob"
 :description "In the current project, find files matching a glob pattern. To search recursively, use the '**/' prefix. For example, a PATTERN of '**/*.el' finds all Emacs Lisp files in the project, while '*.el' finds them only in the root directory. The result is a newline-separated string where each line is \"NAME: PATH\", where NAME is the file's base name and PATH is the path relative to the current project root.  If the optional argument INCLUDE-COUNTS is non-nil append the number of lines as \" (N lines)\"."
 :args '((:name "pattern"
                :type string
                :description "A glob pattern to match against the filenames in the project.")
         (:name "include-counts"
                :type boolean
                :optional t
                :description "If non-nil, append the number of lines to each entry as ' (N lines)'."))
 :category "project")

(gptel-make-tool
 :function #'aj8/gptel-tool-project-search-regexp
 :name "aj8_project_search_regexp"
 :description "In the current project, recursively search for content matching REGEXP. The tool returns a newline-separated string of matching lines. Each line includes: PATH:LINE:TEXT, where PATH is the file path relative to the current project root, LINE is the 1-based line number of the match, and TEXT is the matched line text. If the optional argument INCLUDE-COLUMNS is non-nil, the tool returns PATH:LINE:COLUMN:TEXT, where COLUMN is the 1-based column number of the match."
 :args '((:name "regexp"
                :type string
                :description "A regexp to search for in the project files. The regexp should be compatible with ripgrep or git grep.")
         (:name "include-columns"
                :type boolean
                :optional t
                :description "If non-nil, include 1-based column numbers in the result."))
 :category "project")

;;; Initialization
;;   Initializes gptel-tools with all the registered tools.  This is only
;;   needed for the test suite, i.e.  when `gptel-test' is loaded (so that
;;   tests that inspect `gptel-tools' will pass).
(when (featurep 'gptel-test)
  (setq gptel-tools
        (mapcan (lambda (entry)
                  (mapcar #'cdr (cdr entry)))
                gptel--known-tools)))

(provide 'aj8-gptel)
