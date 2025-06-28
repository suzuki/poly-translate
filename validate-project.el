;;; validate-project.el --- Project-wide Emacs Lisp validation -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Norio Suzuki <norio.suzuki@gmail.com>

;;; Commentary:

;; Batch validation script for the entire poly-translate project.
;; Can be run from command line or within Emacs.

;;; Code:

(require 'verify-elisp)

(defun validate-project-batch ()
  "Validate all elisp files in the project and report results."
  (let ((project-root default-directory)
        (files-to-check '("poly-translate.el"
                          "poly-translate-core.el"
                          "poly-translate-backend.el"
                          "poly-translate-ui.el"
                          "backends/poly-translate-deepl.el"
                          "backends/poly-translate-google.el"
                          "backends/poly-translate-llm.el"
                          "verify-elisp.el"
                          "mcp-server.el"))
        (all-results '())
        (failed-files '()))
    
    (message "Starting project validation...")
    (message "Project root: %s" project-root)
    
    (dolist (file files-to-check)
      (let ((full-path (expand-file-name file project-root)))
        (if (file-exists-p full-path)
            (progn
              (message "Validating: %s" file)
              (let ((results (verify-elisp-file full-path)))
                (push (cons file results) all-results)
                (unless (plist-get results :overall-success)
                  (push file failed-files))))
          (message "Warning: File not found: %s" file))))
    
    ;; Generate summary report
    (let ((total-files (length files-to-check))
          (checked-files (length all-results))
          (failed-count (length failed-files)))
      
      (message "\n=== VALIDATION SUMMARY ===")
      (message "Total files: %d" total-files)
      (message "Checked files: %d" checked-files)
      (message "Passed: %d" (- checked-files failed-count))
      (message "Failed: %d" failed-count)
      
      (when failed-files
        (message "\nFailed files:")
        (dolist (file failed-files)
          (message "  - %s" file)))
      
      (if (zerop failed-count)
          (message "\n✓ All files passed validation!")
        (message "\n✗ %d files failed validation" failed-count))
      
      ;; Exit with appropriate code for CI
      (when noninteractive
        (kill-emacs (if (zerop failed-count) 0 1)))
      
      all-results)))

;; For batch execution
(when noninteractive
  (validate-project-batch))

(provide 'validate-project)
;;; validate-project.el ends here