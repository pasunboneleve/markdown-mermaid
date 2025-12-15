;;; markdown-mermaid.el --- Preview Mermaid code blocks in Markdown  -*- lexical-binding: t; -*-

;; Author: Daniel Vianna and contributors
;; Maintainer: Daniel Vianna <dmlvianna@gmail.com>
;; Package-Requires: ((emacs "26.1") (markdown-mode "2.3"))
;; Keywords: markdown, tools, mermaid, diagrams
;; URL: https://github.com/pasunboneleve/markdown-mermaid
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.3.0

;;; Commentary:

;; This package extracts mermaid code blocks from markdown buffers, compiles
;; them using the mermaid-cli (mmdc), and displays the resulting image in
;; a new buffer.
;;
;; It automatically attempts to match the generated diagram colors to your
;; current Emacs theme variables.
;;
;; Requirements:
;;   npm install -g @mermaid-js/mermaid-cli

;;; Code:

(require 'markdown-mode)
(require 'json)
(require 'color)

(defgroup markdown-mermaid nil
  "Preview Mermaid diagrams within Markdown buffers."
  :group 'markdown
  :prefix "markdown-mermaid-")

;;; Configuration

(defcustom markdown-mermaid-mmdc-path (executable-find "mmdc")
  "Path to the mermaid-cli executable (mmdc).
Defaults to looking up `mmdc' in your system path."
  :type '(choice (file :tag "Path to executable")
                 (const :tag "Not found" nil))
  :group 'markdown-mermaid)

(defvar-local markdown-mermaid-temp-files-to-delete nil
  "List of temporary files created for this specific mermaid preview buffer.")

;;; Internal Functions

(defun markdown-mermaid--get-theme-colors ()
  "Return an alist of color strings based on the current Emacs theme."
  (let* ((fg (face-attribute 'default :foreground nil 'default))
         (bg (face-attribute 'default :background nil 'default))
         (secondary (face-attribute 'font-lock-type-face :foreground nil 'default))
         (lines (face-attribute 'font-lock-constant-face :foreground nil 'default)))

    ;; Fallbacks
    (when (or (not (stringp fg)) (string-empty-p fg)) (setq fg "#000000"))
    (when (or (not (stringp bg)) (string-empty-p bg)) (setq bg "#ffffff"))
    (when (or (not (stringp secondary)) (string-empty-p secondary)) (setq secondary "#333333"))
    (when (or (not (stringp lines)) (string-empty-p lines)) (setq lines fg))

    `((fg . ,fg)
      (bg . ,bg)
      (secondary . ,secondary)
      (lines . ,lines))))

(defun markdown-mermaid--generate-theme-config (file-path)
  "Create a Mermaid JSON config file at FILE-PATH based on current Emacs theme."
  (let* ((colors (markdown-mermaid--get-theme-colors))
         (bg (alist-get 'bg colors))
         (fg (alist-get 'fg colors))
         (secondary (alist-get 'secondary colors))
         (lines (alist-get 'lines colors))
         (json-str (json-encode
                    `((theme . "base")
                      (themeVariables . (
                                         (primaryColor . ,bg)
                                         (primaryTextColor . ,fg)
                                         (primaryBorderColor . ,secondary)
                                         (lineColor . ,lines)
                                         (secondaryColor . ,bg)
                                         (tertiaryColor . ,bg)))))))
    (with-temp-file file-path
      (insert json-str))))

(defun markdown-mermaid--delete-temp-files-on-kill ()
  "Delete temporary files associated with the current buffer."
  (when markdown-mermaid-temp-files-to-delete
    (dolist (file markdown-mermaid-temp-files-to-delete)
      (when (file-exists-p file)
        (delete-file file)))))


;;; Main Command

(defun markdown-mermaid--compile (mermaid-code)
  "Compile the MERMAID-CODE and return a list: (OUTPUT-PATH TEMP-FILES-LIST)."
  (let ((temp-input (make-temp-file "mermaid-block-" nil ".mmd"))
        (temp-output (make-temp-file "mermaid-block-" nil ".png"))
        (temp-config (make-temp-file "mermaid-config-" nil ".json"))
        (screen-dimensions
         (alist-get 'geometry (car (display-monitor-attributes-list))))
        (temp-files nil))

    ;; Register for cleanup
    (setq temp-files (list temp-input temp-output temp-config))

    (markdown-mermaid--generate-theme-config temp-config)

    (with-temp-file temp-input
      (insert mermaid-code))

    (message "Compiling Mermaid block...")
    (call-process markdown-mermaid-mmdc-path
                  nil
                  "*mermaid-error*"
                  nil
                  "-i" temp-input
                  "-o" temp-output
                  "-c" temp-config
                  "-b" "transparent"
                  "--width" (number-to-string (nth 2 screen-dimensions))
                  "--height" (number-to-string (nth 3 screen-dimensions)))

    (if (file-exists-p temp-output)
        (list temp-output temp-files)
      (progn
        ;; If compilation fails, clean up input/config files immediately
        (dolist (file (list temp-input temp-config))
          (when (file-exists-p file) (delete-file file)))
        nil))))

(defun markdown-mermaid--display (compile-result)
  "Display the image based on COMPILE-RESULT (list of path and temp files)."
  (let ((image-path (car compile-result))
        (temp-files (cadr compile-result)))
    (if image-path
        (progn
          (message "Preview generated.")

          ;; Load the image file into a buffer. find-file-noselect returns the buffer object.
          (let ((image-buffer (find-file-noselect image-path)))

            (with-current-buffer image-buffer
              ;; 1. Ensure the buffer is named consistently for previews.
              ;; This buffer is now visiting the temporary file.
              (rename-buffer "*mermaid-image*" t)

              ;; 2. Set up cleanup variables/hook in the buffer holding the image.
              (setq-local markdown-mermaid-temp-files-to-delete temp-files)
              (add-hook 'kill-buffer-hook
                        #'markdown-mermaid--delete-temp-files-on-kill nil t)

              ;; 3. Display the buffer.
              (switch-to-buffer-other-window image-buffer))))
      (switch-to-buffer-other-window "*mermaid-error*")
      (message "Compilation failed. Check *mermaid-error* buffer."))))

(defun markdown-mermaid--compile-and-display ()
  "Compile the Mermaid block and display it in a buffer."
  (let* ((start nil)
         (end nil)
         (mermaid-code nil))
    ;; Find bounds
    (save-excursion
      (let ((case-fold-search t))
        (if (re-search-backward "^[ \t]*```[ \t]*mermaid" nil t)
            (progn
              (forward-line 1)
              (setq start (point))
              (if (re-search-forward "^[ \t]*```" nil t)
                  (setq end (match-beginning 0))
                (error "Found start of block, but not the end")))
          (error "Cursor is not inside a ```mermaid block"))))

    (setq mermaid-code (buffer-substring-no-properties start end))
    (markdown-mermaid--display (markdown-mermaid--compile mermaid-code))))

;;;###autoload
(defun markdown-mermaid-preview ()
  "Compile and preview the Mermaid block under cursor."
  (interactive)
  (unless markdown-mermaid-mmdc-path
    (user-error "Mermaid CLI (mmdc) not found.  Please run 'npm install -g @mermaid-js/mermaid-cli' or set 'markdown-mermaid-mmdc-path' manually"))

  (markdown-mermaid--compile-and-display))

(provide 'markdown-mermaid)
;;; markdown-mermaid.el ends here
