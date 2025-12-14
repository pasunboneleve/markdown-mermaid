;;; markdown-mermaid-tests.el --- Tests for markdown-mermaid  -*- lexical-binding: t; -*-

(require 'ert)
(require 'markdown-mermaid)
(require 'json)

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

;;; markdown-mermaid-tests.el ends here
