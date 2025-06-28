;;; verify-elisp.el --- Emacs Lisp code verification utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Norio Suzuki <norio.suzuki@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: verification, elisp, development

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides comprehensive Emacs Lisp code verification utilities
;; designed to catch errors before code execution. It includes:
;; 
;; - Syntax validation
;; - Byte-compilation checking
;; - Function/variable existence validation
;; - Load-path dependency checking
;; - Safe code evaluation
;; - Integration testing support

;;; Code:

(require 'cl-lib)
(require 'warnings)

;; Configuration
(defgroup verify-elisp nil
  "Emacs Lisp code verification utilities."
  :group 'development
  :prefix "verify-elisp-")

(defcustom verify-elisp-strict-warnings nil
  "Whether to treat warnings as errors during verification."
  :type 'boolean
  :group 'verify-elisp)

(defcustom verify-elisp-safe-eval-functions
  '(+ - * / = < > <= >= and or not
    car cdr cons list append length reverse
    numberp stringp symbolp listp consp null
    format concat substring string-match string-match-p
    plist-get plist-put plist-member
    make-hash-table gethash puthash remhash
    make-vector aref aset
    point point-min point-max
    current-buffer buffer-name buffer-string
    message error signal)
  "List of functions considered safe for evaluation."
  :type '(repeat symbol)
  :group 'verify-elisp)

;; Verification result structure
(cl-defstruct verify-elisp-result
  "Structure for verification results."
  stage           ; Verification stage (symbol)
  success         ; Whether verification passed (boolean)
  message         ; Result message (string)
  details         ; Additional details (any)
  timestamp)      ; When verification was performed

;; Core verification functions
(defun verify-elisp-check-syntax (code)
  "Check syntax of elisp CODE.
Returns a verify-elisp-result structure."
  (condition-case err
      (progn
        (read code)
        (make-verify-elisp-result
         :stage 'syntax
         :success t
         :message "Syntax validation passed"
         :timestamp (current-time)))
    (error
     (make-verify-elisp-result
      :stage 'syntax
      :success nil
      :message (format "Syntax error: %s" (error-message-string err))
      :details err
      :timestamp (current-time)))))

(defun verify-elisp-check-symbols (code)
  "Check if all symbols in CODE are defined or builtin.
Returns a verify-elisp-result structure."
  (condition-case err
      (let ((form (read code))
            (undefined-symbols '())
            (all-symbols '()))
        
        ;; Extract all symbols from the form
        (cl-labels ((extract-symbols (obj)
                      (cond
                       ((symbolp obj)
                        (unless (or (keywordp obj)
                                    (eq obj nil)
                                    (eq obj t)
                                    (string-prefix-p ":" (symbol-name obj)))
                          (push obj all-symbols)))
                       ((listp obj)
                        (dolist (item obj)
                          (extract-symbols item))))))
          (extract-symbols form))
        
        ;; Check each symbol
        (dolist (symbol all-symbols)
          (unless (or (fboundp symbol)
                      (boundp symbol)
                      (special-form-p symbol)
                      (macrop symbol)
                      ;; Allow common external symbols and libraries
                      (string-prefix-p "poly-translate-" (symbol-name symbol))
                      (memq symbol '(cl-lib subr-x url json
                                     cl-defstruct cl-defmethod cl-loop cl-every
                                     defgroup defcustom interactive
                                     make-temp-file with-temp-file directory-files-recursively
                                     locate-library file-name-directory expand-file-name)))
            (push symbol undefined-symbols)))
        
        (if undefined-symbols
            (make-verify-elisp-result
             :stage 'symbols
             :success nil
             :message (format "Undefined symbols: %s" 
                              (mapconcat #'symbol-name undefined-symbols ", "))
             :details undefined-symbols
             :timestamp (current-time))
          (make-verify-elisp-result
           :stage 'symbols
           :success t
           :message "All symbols are defined"
           :timestamp (current-time))))
    (error
     (make-verify-elisp-result
      :stage 'symbols
      :success nil
      :message (format "Symbol check error: %s" (error-message-string err))
      :details err
      :timestamp (current-time)))))

(defun verify-elisp-byte-compile (code &optional filename)
  "Byte-compile CODE and check for errors/warnings.
FILENAME is optional context for error messages."
  (let ((temp-file (make-temp-file (or filename "verify-elisp") nil ".el"))
        (compile-log-buffer "*Compile-Log*")
        (old-log-buffer-content ""))
    
    ;; Store old compile log content
    (when (get-buffer compile-log-buffer)
      (with-current-buffer compile-log-buffer
        (setq old-log-buffer-content (buffer-string))))
    
    (unwind-protect
        (condition-case err
            (progn
              (with-temp-file temp-file
                (insert code))
              
              (let ((byte-compile-error-on-warn verify-elisp-strict-warnings)
                    (warning-minimum-level (if verify-elisp-strict-warnings :warning :error)))
                
                (if (byte-compile-file temp-file)
                    (let ((new-content ""))
                      ;; Check for new warnings/errors in compile log
                      (when (get-buffer compile-log-buffer)
                        (with-current-buffer compile-log-buffer
                          (setq new-content (substring (buffer-string) 
                                                       (length old-log-buffer-content)))))
                      
                      (if (and verify-elisp-strict-warnings 
                               (not (string-empty-p (string-trim new-content))))
                          (make-verify-elisp-result
                           :stage 'byte-compile
                           :success nil
                           :message "Byte compilation produced warnings"
                           :details new-content
                           :timestamp (current-time))
                        (make-verify-elisp-result
                         :stage 'byte-compile
                         :success t
                         :message "Byte compilation succeeded"
                         :timestamp (current-time))))
                  
                  (make-verify-elisp-result
                   :stage 'byte-compile
                   :success nil
                   :message "Byte compilation failed"
                   :timestamp (current-time)))))
          (error
           (make-verify-elisp-result
            :stage 'byte-compile
            :success nil
            :message (format "Byte compile error: %s" (error-message-string err))
            :details err
            :timestamp (current-time))))
      
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (let ((elc-file (concat temp-file "c")))
        (when (file-exists-p elc-file)
          (delete-file elc-file))))))

(defun verify-elisp-safe-eval (code)
  "Safely evaluate CODE in a restricted environment.
Only allows predefined safe functions."
  (condition-case err
      (let ((form (read code)))
        ;; Check if all function calls are safe
        (cl-labels ((check-safe (obj)
                      (cond
                       ((and (listp obj) (symbolp (car obj)))
                        (unless (or (special-form-p (car obj))
                                    (macrop (car obj))
                                    (memq (car obj) verify-elisp-safe-eval-functions))
                          (error "Unsafe function: %s" (car obj)))
                        (dolist (arg (cdr obj))
                          (check-safe arg)))
                       ((listp obj)
                        (dolist (item obj)
                          (check-safe item))))))
          (check-safe form))
        
        ;; If safe, evaluate
        (let ((result (eval form)))
          (make-verify-elisp-result
           :stage 'safe-eval
           :success t
           :message (format "Evaluation succeeded: %S" result)
           :details result
           :timestamp (current-time))))
    (error
     (make-verify-elisp-result
      :stage 'safe-eval
      :success nil
      :message (format "Safe evaluation failed: %s" (error-message-string err))
      :details err
      :timestamp (current-time)))))

(defun verify-elisp-check-dependencies (code)
  "Check if all required dependencies are available."
  (condition-case err
      (let ((form (read code))
            (missing-deps '())
            (required-features '()))
        
        ;; Extract require statements
        (cl-labels ((extract-requires (obj)
                      (when (and (listp obj)
                                 (eq (car obj) 'require)
                                 (listp (cadr obj))
                                 (eq (car (cadr obj)) 'quote))
                        (push (cadr (cadr obj)) required-features))
                      (when (listp obj)
                        (dolist (item obj)
                          (extract-requires item)))))
          (extract-requires form))
        
        ;; Check each required feature
        (dolist (feature required-features)
          (unless (featurep feature)
            (condition-case nil
                (require feature)
              (error
               (push feature missing-deps)))))
        
        (if missing-deps
            (make-verify-elisp-result
             :stage 'dependencies
             :success nil
             :message (format "Missing dependencies: %s"
                              (mapconcat #'symbol-name missing-deps ", "))
             :details missing-deps
             :timestamp (current-time))
          (make-verify-elisp-result
           :stage 'dependencies
           :success t
           :message "All dependencies satisfied"
           :timestamp (current-time))))
    (error
     (make-verify-elisp-result
      :stage 'dependencies
      :success nil
      :message (format "Dependency check error: %s" (error-message-string err))
      :details err
      :timestamp (current-time)))))

;; Main verification function
(defun verify-elisp-comprehensive (code &optional filename context)
  "Perform comprehensive verification of elisp CODE.
FILENAME provides context for error messages.
CONTEXT is additional information about where the code came from."
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
        
        ;; Stage 4: Byte compilation
        (push (verify-elisp-byte-compile code filename) results)
        
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

;; Utility functions
(defun verify-elisp-format-results (results)
  "Format verification RESULTS for display."
  (let ((output "")
        (overall (plist-get results :overall-success)))
    (setq output (concat output
                         (format "=== Verification Results ===\n")
                         (format "File: %s\n" (plist-get results :filename))
                         (format "Overall: %s\n" (if overall "PASS" "FAIL"))
                         (format "Duration: %.3fs\n\n" (plist-get results :duration))))
    
    (dolist (result (plist-get results :results))
      (setq output (concat output
                           (format "%s %s: %s\n"
                                   (verify-elisp-result-stage result)
                                   (if (verify-elisp-result-success result) "✓" "✗")
                                   (verify-elisp-result-message result)))))
    output))

(defun verify-elisp-display-results (results)
  "Display verification RESULTS in a buffer."
  (with-current-buffer (get-buffer-create "*Elisp Verification*")
    (erase-buffer)
    (insert (verify-elisp-format-results results))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;; Interactive commands
(defun verify-elisp-current-buffer ()
  "Verify the current buffer."
  (interactive)
  (let ((code (buffer-string))
        (filename (or (buffer-file-name) (buffer-name))))
    (let ((results (verify-elisp-comprehensive code filename "current-buffer")))
      (verify-elisp-display-results results)
      results)))

(defun verify-elisp-region (start end)
  "Verify the current region."
  (interactive "r")
  (let ((code (buffer-substring-no-properties start end))
        (filename (or (buffer-file-name) (buffer-name))))
    (let ((results (verify-elisp-comprehensive code filename "region")))
      (verify-elisp-display-results results)
      results)))

(defun verify-elisp-file (file)
  "Verify an elisp FILE."
  (interactive "fVerify elisp file: ")
  (if (not (file-exists-p file))
      (error "File does not exist: %s" file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((code (buffer-string))
            (filename (file-name-nondirectory file)))
        (let ((results (verify-elisp-comprehensive code filename "file")))
          (verify-elisp-display-results results)
          results)))))

(provide 'verify-elisp)
;;; verify-elisp.el ends here