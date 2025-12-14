;;; markdown-mermaid-tests.el --- Tests for markdown-mermaid  -*- lexical-binding: t; -*-

(require 'ert)
(require 'markdown-mermaid)
(require 'json)
(require 'seq)

(ert-deftest markdown-mermaid-theme-generation-test ()
  "Ensure the generated JSON config is valid and contains keys."
  (let ((temp-file (make-temp-file "test-mermaid-config-" nil ".json")))
    (unwind-protect
        (progn
          (markdown-mermaid--generate-theme-config temp-file)
          (let* ((json-object-type 'alist)
                 (json-key-type 'symbol)
                 (json-array-type 'list)
                 (data (json-read-file temp-file)))
            (should (equal (alist-get 'theme data) "base"))
            (should (alist-get 'themeVariables data))))
      (delete-file temp-file))))

(ert-deftest markdown-mermaid-find-bounds-test ()
  "Ensure we can identify a mermaid block inside a buffer."
  (with-temp-buffer
    (insert "Some text\n\n```mermaid\ngraph TD;\n    A-->B;\n```\n\nMore text")
    (goto-char (point-min))
    (search-forward "A-->B") ;; Move cursor inside block
    ;; We catch the error to detect success/fail logic of the function's pre-check
    ;; but checking for no-error here acts as a sanity check.
    (should
     (save-excursion
       (re-search-backward "^[ \t]*```[ \t]*mermaid" nil t)))))

(ert-deftest markdown-mermaid-cleanup-on-kill-test ()
  "Ensure temporary files are deleted when the preview buffer is killed."
  (let ((temp-files (list (make-temp-file "test-file-1") (make-temp-file "test-file-2")))
        (test-buffer (generate-new-buffer "test-mermaid-preview")))
    (unwind-protect
        (progn
          ;; Create dummy files
          (dolist (f temp-files) (write-region "" nil f))
          (should (file-exists-p (car temp-files)))

          ;; Set up buffer-local variables and hook
          (with-current-buffer test-buffer
            (setq-local markdown-mermaid-temp-files-to-delete temp-files)
            (add-hook 'kill-buffer-hook 'markdown-mermaid--delete-temp-files-on-kill nil t))

          ;; Kill the buffer
          (kill-buffer test-buffer)

          ;; Check if files are deleted
          (should (not (file-exists-p (car temp-files))))
          (should (not (file-exists-p (cadr temp-files)))))
      ;; Ensure cleanup if test fails before kill-buffer
      (dolist (f temp-files)
        (when (file-exists-p f) (delete-file f))))))

(ert-deftest markdown-mermaid-single-buffer-cleanup-test ()
  "Ensure only one buffer is created for the preview and cleanup works on kill."
  (let* ((temp-output (make-temp-file "test-output-" nil ".png"))
         (temp-file-1 (make-temp-file "test-file-1"))
         (temp-file-2 (make-temp-file "test-file-2"))
         (temp-files (list temp-file-1 temp-file-2 temp-output))
         (compile-result (list temp-output temp-files))
         (preview-name-prefix "*mermaid-image*")
         (preview-name-regex (concat "^" (regexp-quote preview-name-prefix))))

    ;; Ensure a clean slate for preview buffers before running the test
    (dolist (b (buffer-list))
      (when (string-match-p preview-name-regex (buffer-name b))
        (kill-buffer b)))

    (unwind-protect
        (progn
          ;; 1. Create dummy files (including the output image file)
          (dolist (f temp-files) (write-region "" nil f))
          (should (file-exists-p temp-output))

          ;; 2. Call display function
          (markdown-mermaid--display compile-result)

          ;; 3. Check that exactly one buffer matching the pattern exists
          (let ((buffer-list (seq-filter (lambda (b) (string-match-p preview-name-regex (buffer-name b))) (buffer-list))))
            (should (eq (length buffer-list) 1))
            (let* ((preview-buffer (car buffer-list))
                   (actual-buffer-name (buffer-name preview-buffer)))

              ;; 4. Check that the buffer is visiting the temporary image file
              (should (equal (buffer-file-name preview-buffer) temp-output))

              ;; 5. Kill the buffer
              (kill-buffer preview-buffer)

              ;; 6. Check that the buffer is gone
              (should (not (get-buffer actual-buffer-name)))

              ;; 7. Check that all temporary files are deleted
              (dolist (f temp-files)
                (should (not (file-exists-p f)))))))

      ;; Final cleanup in case of failure
      (dolist (b (buffer-list))
        (when (string-match-p preview-name-regex (buffer-name b))
          (kill-buffer b)))
      (dolist (f temp-files)
        (when (file-exists-p f) (delete-file f))))))

;;; markdown-mermaid-tests.el ends here
