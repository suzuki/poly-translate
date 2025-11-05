;;; validate-without-compile.el --- Project validation without byte compilation -*- lexical-binding: t; -*-

(require 'verify-elisp)

;; Override comprehensive validation to skip byte compilation
(defun verify-elisp-comprehensive-no-compile (code &optional filename context)
  "Comprehensive verification without byte compilation."
  (let ((results '())
        (filename (or filename "snippet"))
        (start-time (current-time)))
    
    ;; Stage 1: Syntax check
    (let ((syntax-result (verify-elisp-check-syntax code)))
      (push syntax-result results)
      
      ;; Only continue if syntax is valid
      (when (verify-elisp-result-success syntax-result)
        
        ;; Stage 2: Symbol existence check
        (push (verify-elisp-check-symbols code) results)
        
        ;; Stage 3: Dependency check
        (push (verify-elisp-check-dependencies code) results)
        
        ;; Skip byte compilation
        
        ;; Stage 5: Safe evaluation (for short, simple code)
        (when (< (length code) 500)
          (push (verify-elisp-safe-eval code) results))))
    
    ;; Return comprehensive result
    (let ((all-passed (cl-every #'verify-elisp-result-success results))
          (end-time (current-time)))
      `(:overall-success ,all-passed
        :filename ,filename
        :context ,context
        :start-time ,start-time
        :end-time ,end-time
        :duration ,(float-time (time-subtract end-time start-time))
        :results ,(nreverse results)))))

(defun validate-project-no-compile ()
  "Validate all elisp files without byte compilation."
  (let ((project-root default-directory)
        (files-to-check '("poly-translate.el"
                          "poly-translate-core.el"
                          "poly-translate-backend.el" 
                          "poly-translate-ui.el"
                          "backends/poly-translate-deepl.el"
                          "backends/poly-translate-google.el"
                          "backends/poly-translate-llm.el"))
        (all-results '())
        (failed-files '()))
    
    (message "Starting project validation (without byte compilation)...")
    
    (dolist (file files-to-check)
      (let ((full-path (expand-file-name file project-root)))
        (if (file-exists-p full-path)
            (progn
              (message "Validating: %s" file)
              (let ((results (verify-elisp-comprehensive-no-compile
                              (with-temp-buffer
                                (insert-file-contents full-path)
                                (buffer-string))
                              file "file")))
                (push (cons file results) all-results)
                (unless (plist-get results :overall-success)
                  (push file failed-files))))
          (message "Warning: File not found: %s" file))))
    
    ;; Generate summary
    (let ((total-files (length files-to-check))
          (checked-files (length all-results))
          (failed-count (length failed-files)))
      
      (message "\n=== VALIDATION SUMMARY (NO BYTE COMPILE) ===")
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
      
      all-results)))

;; Run validation
(validate-project-no-compile)