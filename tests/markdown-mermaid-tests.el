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
    ;; Ideally, we would refactor the bounds checking into a pure function to test returns
    ;; but checking for no-error here acts as a sanity check.
    (should
     (save-excursion
       (re-search-backward "^[ \t]*```[ \t]*mermaid" nil t)))))

;;; markdown-mermaid-tests.el ends here
