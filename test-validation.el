;; Quick test of validation system

(require 'verify-elisp)

(setq verify-elisp-strict-warnings nil)

(let ((results (verify-elisp-file "poly-translate.el")))
  (message "=== VALIDATION RESULTS ===")
  (message "Overall: %s" (plist-get results :overall-success))
  (dolist (result (plist-get results :results))
    (message "%s: %s - %s" 
             (verify-elisp-result-stage result)
             (if (verify-elisp-result-success result) "PASS" "FAIL")
             (verify-elisp-result-message result))))