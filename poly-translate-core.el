;;; poly-translate-core.el --- Core functionality for poly-translate -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Norio Suzuki <norio.suzuki@gmail.com>

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

;; Core functionality for poly-translate, including engine registration
;; and management.

;;; Code:

(require 'cl-lib)

;; External variable declarations
(defvar poly-translate-language-codes)

;; Engine structure
(cl-defstruct poly-translate-engine
  "Structure representing a translation engine."
  name          ; Engine name (string)
  backend       ; Backend symbol (e.g., 'google, 'deepl, 'llm-openai)
  input-lang    ; Input language code (string, "auto" for auto-detect)
  output-lang   ; Output language code (string)
  config)       ; Backend-specific configuration (plist)

;; Global engine registry
(defvar poly-translate-engines (make-hash-table :test 'equal)
  "Hash table storing registered translation engines.")

(defvar poly-translate-backends (make-hash-table :test 'eq)
  "Hash table storing available backends.")

;; Engine management functions
(defun poly-translate-register-engine (spec)
  "Register a translation engine from SPEC.
SPEC should be a plist with the following keys:
  :name - Engine name (required)
  :backend - Backend symbol (required)
  :input-lang - Input language code (default: \"auto\")
  :output-lang - Output language code (required)
  Additional keys are stored in the config field."
  (let* ((name (plist-get spec :name))
         (backend (plist-get spec :backend))
         (input-lang (or (plist-get spec :input-lang) "auto"))
         (output-lang (plist-get spec :output-lang))
         (config (cl-loop for (key value) on spec by #'cddr
                          unless (memq key '(:name :backend :input-lang :output-lang))
                          collect key and collect value)))
    ;; Validate required fields
    (unless name
      (error "Engine name is required"))
    (unless backend
      (error "Backend is required"))
    (unless output-lang
      (error "Output language is required"))
    (when (string= output-lang "auto")
      (error "Output language cannot be \"auto\""))

    ;; Create and register engine
    (let ((engine (make-poly-translate-engine
                   :name name
                   :backend backend
                   :input-lang input-lang
                   :output-lang output-lang
                   :config config)))
      (puthash name engine poly-translate-engines)
      (message "Registered translation engine: %s" name)
      engine)))

(defun poly-translate-unregister-engine (name)
  "Unregister the translation engine with NAME."
  (remhash name poly-translate-engines))

(defun poly-translate-get-engine (name)
  "Get the translation engine with NAME.
Returns nil if no such engine exists."
  (gethash name poly-translate-engines))

(defun poly-translate-list-engines ()
  "Return a list of all registered engine names."
  (let (engines)
    (maphash (lambda (name _engine)
               (push name engines))
             poly-translate-engines)
    (sort engines #'string<)))

(defun poly-translate-list-engines-for-backend (backend)
  "Return a list of engine names using BACKEND."
  (let (engines)
    (maphash (lambda (name engine)
               (when (eq (poly-translate-engine-backend engine) backend)
                 (push name engines)))
             poly-translate-engines)
    (sort engines #'string<)))

;; Backend registration
(defun poly-translate-register-backend (backend-symbol functions)
  "Register a backend with BACKEND-SYMBOL and FUNCTIONS.
FUNCTIONS should be a plist with at least :translate function."
  (unless (plist-get functions :translate)
    (error "Backend must provide :translate function"))
  (puthash backend-symbol functions poly-translate-backends))

(defun poly-translate-get-backend (backend-symbol)
  "Get the backend functions for BACKEND-SYMBOL."
  (gethash backend-symbol poly-translate-backends))

;; Translation execution
(defun poly-translate-with-engine (engine-name text callback &optional error-callback)
  "Translate TEXT using engine with ENGINE-NAME.
CALLBACK is called with the translated text on success.
ERROR-CALLBACK is called with error message on failure."
  (let ((engine (poly-translate-get-engine engine-name)))
    (unless engine
      (error "No such engine: %s" engine-name))

    (let* ((backend-symbol (poly-translate-engine-backend engine))
           (backend (poly-translate-get-backend backend-symbol)))
      (unless backend
        (error "Backend not available: %s" backend-symbol))

      (let ((translate-fn (plist-get backend :translate)))
        (funcall translate-fn
                 backend-symbol
                 text
                 (poly-translate-engine-input-lang engine)
                 (poly-translate-engine-output-lang engine)
                 (poly-translate-engine-config engine)
                 callback
                 (or error-callback
                     (lambda (err)
                       (message "Translation error: %s" err))))))))

;; Language detection support
(defun poly-translate-detect-language (text callback &optional error-callback)
  "Detect the language of TEXT.
CALLBACK is called with the detected language code.
ERROR-CALLBACK is called with error message on failure."
  ;; This is a placeholder - backends can implement their own detection
  ;; For now, we'll use a simple heuristic
  (ignore error-callback)  ; Suppress unused argument warning
  (let ((lang (cond
               ;; Japanese detection
               ((string-match-p "[あ-ん]\\|[ア-ン]\\|[一-龯]" text) "ja")
               ;; Chinese detection (simplified)
               ((string-match-p "[一-龯]" text) "zh")
               ;; Korean detection
               ((string-match-p "[가-힣]" text) "ko")
               ;; Arabic detection
               ((string-match-p "[؀-ۿ]" text) "ar")
               ;; Default to English
               (t "en"))))
    (funcall callback lang)))

;; Utility functions
(defun poly-translate-language-name (code)
  "Get the display name for language CODE."
  (or (cdr (assoc code poly-translate-language-codes))
      code))

(defun poly-translate-format-engine-info (engine)
  "Format ENGINE information for display."
  (format "%s (%s: %s → %s)"
          (poly-translate-engine-name engine)
          (poly-translate-engine-backend engine)
          (poly-translate-language-name (poly-translate-engine-input-lang engine))
          (poly-translate-language-name (poly-translate-engine-output-lang engine))))

(provide 'poly-translate-core)
;;; poly-translate-core.el ends here
