;;; simple-test.el --- Simple validation test -*- lexical-binding: t; -*-

(require 'verify-elisp)

;; Test basic functionality without byte compilation
(setq verify-elisp-strict-warnings nil)

;; Override the comprehensive function to skip byte compilation
(defun verify-elisp-comprehensive-simple (code &optional filename context)
  "Simplified comprehensive verification without byte compilation."
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
        
        ;; Skip byte compilation for now
        
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

;; Test with poly-translate.el
(let ((results (verify-elisp-comprehensive-simple 
                (with-temp-buffer
                  (insert-file-contents "poly-translate.el")
                  (buffer-string))
                "poly-translate.el")))
  (message "=== SIMPLE TEST RESULTS ===")
  (message "Overall: %s" (plist-get results :overall-success))
  (dolist (result (plist-get results :results))
    (message "%s: %s - %s" 
             (verify-elisp-result-stage result)
             (if (verify-elisp-result-success result) "PASS" "FAIL")
             (verify-elisp-result-message result))))