;;; mcp-server.el --- MCP Server for Emacs Lisp validation -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Norio Suzuki <norio.suzuki@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: mcp, validation, elisp

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

;; MCP Server for validating Emacs Lisp code in real-time.
;; This server provides tools for Claude Code to validate elisp before
;; suggesting changes, reducing the chance of runtime errors.

;;; Code:

(require 'cl-lib)

;; Check if mcp-server-lib is available
(defvar poly-translate-mcp-available-p nil
  "Whether MCP server lib is available.")

(condition-case nil
    (progn
      (require 'mcp-server-lib)
      (setq poly-translate-mcp-available-p t))
  (error
   (setq poly-translate-mcp-available-p nil)))

;; Validation framework
(defun poly-translate-validate-syntax (code)
  "Validate syntax of ELISP CODE.
Returns (success . result) where success is t/nil and result is the message."
  (condition-case err
      (progn
        (read code)
        (cons t "Syntax OK"))
    (error
     (cons nil (format "Syntax error: %s" (error-message-string err))))))

(defun poly-translate-validate-byte-compile (code filename)
  "Validate CODE by byte-compiling to temporary file.
FILENAME is used for the temporary file name."
  (let ((temp-file (make-temp-file filename nil ".el")))
    (unwind-protect
        (condition-case err
            (progn
              (with-temp-file temp-file
                (insert code))
              (let ((byte-compile-error-on-warn nil)
                    (warning-minimum-level :error))
                (if (byte-compile-file temp-file)
                    (cons t "Byte compilation OK")
                  (cons nil "Byte compilation failed"))))
          (error
           (cons nil (format "Byte compile error: %s" (error-message-string err)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (let ((elc-file (concat temp-file "c")))
        (when (file-exists-p elc-file)
          (delete-file elc-file))))))

(defun poly-translate-safe-eval (code)
  "Safely evaluate CODE in a restricted environment.
Returns (success . result) where success is t/nil and result is the value/error."
  ;; Create a minimal environment for evaluation
  (let ((inhibit-message t))
    (condition-case err
        (let ((result (eval (read code))))
          (cons t (format "%S" result)))
      (error
       (cons nil (format "Eval error: %s" (error-message-string err)))))))

(defun poly-translate-validate-full (code &optional filename)
  "Perform full validation of CODE with optional FILENAME context.
Returns a plist with validation results."
  (let ((filename (or filename "test"))
        results)
    
    ;; Stage 1: Syntax validation
    (let ((syntax-result (poly-translate-validate-syntax code)))
      (push (list :stage "syntax" 
                  :success (car syntax-result)
                  :message (cdr syntax-result)) results))
    
    ;; Stage 2: Byte compilation (only if syntax is OK)
    (when (car (poly-translate-validate-syntax code))
      (let ((compile-result (poly-translate-validate-byte-compile code filename)))
        (push (list :stage "byte-compile"
                    :success (car compile-result)
                    :message (cdr compile-result)) results)))
    
    ;; Stage 3: Safe evaluation for simple expressions
    (when (and (car (poly-translate-validate-syntax code))
               (< (length code) 1000)) ; Only eval short code
      (let ((eval-result (poly-translate-safe-eval code)))
        (push (list :stage "eval"
                    :success (car eval-result)
                    :message (cdr eval-result)) results)))
    
    (nreverse results)))

;; MCP Tools Registration
(when (and poly-translate-mcp-available-p
           (fboundp 'mcp-server-lib-register-tool))
  
  ;; Tool: Validate Elisp Syntax
  (mcp-server-lib-register-tool #'poly-translate-mcp-validate-syntax
    :id "validate-elisp-syntax"
    :description "Validate Emacs Lisp code syntax")
  
  ;; Tool: Full Elisp Validation
  (mcp-server-lib-register-tool #'poly-translate-mcp-validate-full
    :id "validate-elisp-full"
    :description "Perform full validation of Emacs Lisp code")
  
  ;; Tool: Eval Elisp in current context
  (mcp-server-lib-register-tool #'poly-translate-mcp-eval-elisp
    :id "eval-elisp"
    :description "Evaluate Emacs Lisp code in current Emacs context"))

;; MCP Tool implementations
(defun poly-translate-mcp-validate-syntax (code)
  "MCP tool to validate elisp syntax."
  (if (fboundp 'mcp-server-lib-with-error-handling)
      (mcp-server-lib-with-error-handling
        (let ((result (poly-translate-validate-syntax code)))
          (format "%s: %s" 
                  (if (car result) "SUCCESS" "FAILED")
                  (cdr result))))
    (let ((result (poly-translate-validate-syntax code)))
      (format "%s: %s" 
              (if (car result) "SUCCESS" "FAILED")
              (cdr result)))))

(defun poly-translate-mcp-validate-full (code &optional filename)
  "MCP tool to perform full elisp validation."
  (if (fboundp 'mcp-server-lib-with-error-handling)
      (mcp-server-lib-with-error-handling
        (let ((results (poly-translate-validate-full code filename))
              (output ""))
          (dolist (result results)
            (setq output 
                  (concat output
                          (format "%s %s: %s\n"
                                  (plist-get result :stage)
                                  (if (plist-get result :success) "✓" "✗")
                                  (plist-get result :message)))))
          output))
    (let ((results (poly-translate-validate-full code filename))
          (output ""))
      (dolist (result results)
        (setq output 
              (concat output
                      (format "%s %s: %s\n"
                              (plist-get result :stage)
                              (if (plist-get result :success) "✓" "✗")
                              (plist-get result :message)))))
      output)))

(defun poly-translate-mcp-eval-elisp (code)
  "MCP tool to evaluate elisp code."
  (if (fboundp 'mcp-server-lib-with-error-handling)
      (mcp-server-lib-with-error-handling
        (condition-case err
            (let ((result (eval (read code))))
              (format "SUCCESS: %S" result))
          (error 
           (format "ERROR: %s" (error-message-string err)))))
    (condition-case err
        (let ((result (eval (read code))))
          (format "SUCCESS: %S" result))
      (error 
       (format "ERROR: %s" (error-message-string err))))))

;; Utility functions for batch validation
(defun poly-translate-validate-file (file)
  "Validate an entire elisp FILE."
  (interactive "fValidate elisp file: ")
  (if (not (file-exists-p file))
      (message "File does not exist: %s" file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((code (buffer-string))
            (filename (file-name-nondirectory file)))
        (let ((results (poly-translate-validate-full code filename)))
          (with-current-buffer (get-buffer-create "*Elisp Validation*")
            (erase-buffer)
            (insert (format "Validation Results for: %s\n" file))
            (insert "=" (make-string 50 ?=) "\n\n")
            (dolist (result results)
              (insert (format "%s %s: %s\n"
                              (plist-get result :stage)
                              (if (plist-get result :success) "✓" "✗")
                              (plist-get result :message))))
            (display-buffer (current-buffer))))))))

(defun poly-translate-validate-project ()
  "Validate all elisp files in the current project."
  (interactive)
  (let ((files (directory-files-recursively default-directory "\\.el$")))
    (dolist (file files)
      (when (not (string-match-p "\\.elc$\\|#.*#$\\|~$" file))
        (poly-translate-validate-file file)))))

;; Server startup
(defun poly-translate-start-mcp-server ()
  "Start the MCP server for elisp validation."
  (interactive)
  (if (and poly-translate-mcp-available-p
           (fboundp 'mcp-server-lib-start))
      (progn
        (mcp-server-lib-start :stdio)
        (message "Poly-translate MCP server started"))
    (error "mcp-server-lib is not available. Please install it first.")))

(provide 'mcp-server)
;;; mcp-server.el ends here