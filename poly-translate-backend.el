;;; poly-translate-backend.el --- Backend interface for poly-translate -*- lexical-binding: t; -*-

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

;; This file defines the backend interface for poly-translate.
;; All translation backends should implement the functions defined here.

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'json)

;; Forward declarations
(declare-function poly-translate-detect-language "poly-translate-core" (text callback &optional error-callback))

;; Backend interface definition
(cl-defgeneric poly-translate-backend-translate (backend text from-lang to-lang config callback error-callback)
  "Translate TEXT from FROM-LANG to TO-LANG using BACKEND.
CONFIG is backend-specific configuration.
CALLBACK is called with translated text on success.
ERROR-CALLBACK is called with error message on failure.")

(cl-defgeneric poly-translate-backend-detect-language (_backend text callback error-callback)
  "Detect language of TEXT using BACKEND.
CALLBACK is called with detected language code.
ERROR-CALLBACK is called with error message on failure."
  ;; Default implementation - can be overridden by specific backends
  (poly-translate-detect-language text callback error-callback))

(cl-defgeneric poly-translate-backend-validate-config (backend config)
  "Validate CONFIG for BACKEND.
Should signal an error if configuration is invalid."
  ;; Default implementation - backends should override
  (ignore backend config)  ; Suppress unused argument warnings
  t)

;; Common utility functions for backends
(defun poly-translate-backend-url-retrieve (url callback &optional error-callback method data headers)
  "Retrieve URL asynchronously with CALLBACK.
ERROR-CALLBACK is called on error.
METHOD defaults to \"GET\".
DATA is the request body for POST requests.
HEADERS is an alist of additional headers."
  (let ((url-request-method (or method "GET"))
        (url-request-extra-headers headers)
        (url-request-data (when data
                            (encode-coding-string data 'utf-8))))
    (url-retrieve
     url
     (lambda (status)
       (condition-case err
         (if (plist-get status :error)
             (funcall (or error-callback #'ignore)
                      (format "Network error: %s" (plist-get status :error)))
           (goto-char (point-min))
           ;; Check HTTP status code
           (let ((status-line (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position))))
             (when (bound-and-true-p poly-translate-debug)
               (message "HTTP response status: %s" status-line))
             (if (re-search-forward "\r?\n\r?\n" nil t)
                 (progn
                   ;; Ensure proper UTF-8 decoding
                   (set-buffer-multibyte t)
                   (let ((response (decode-coding-string
                                    (buffer-substring-no-properties (point) (point-max))
                                    'utf-8)))
                     ;; Check for HTTP error status
                     (if (string-match "HTTP/[0-9.]+ \\([45][0-9][0-9]\\)" status-line)
                         (funcall (or error-callback #'ignore)
                                  (format "HTTP error %s: %s"
                                          (match-string 1 status-line) response))
                       (funcall callback response))))
               (funcall (or error-callback #'ignore) "Invalid HTTP response format"))))
         (error
          (funcall (or error-callback #'ignore)
                   (format "Error in response processing: %s" err)))))
     nil t t)))

(defun poly-translate-backend-json-read-from-string (string)
  "Parse JSON STRING safely."
  (condition-case err
      (let ((json-object-type 'alist)
            (json-array-type 'vector)
            (json-key-type 'symbol))
        (json-read-from-string string))
    (error
     (signal 'poly-translate-json-error
             (list (format "JSON parse error: %s" err) string)))))

(defun poly-translate-backend-encode-url-params (params)
  "Encode PARAMS alist for URL query string."
  (mapconcat
   (lambda (param)
     (format "%s=%s"
             (url-hexify-string (car param))
             (url-hexify-string (if (stringp (cdr param))
                                    (cdr param)
                                  (format "%s" (cdr param))))))
   params
   "&"))

;; Error types
(define-error 'poly-translate-error "Translation error")
(define-error 'poly-translate-network-error "Network error" 'poly-translate-error)
(define-error 'poly-translate-json-error "JSON parsing error" 'poly-translate-error)
(define-error 'poly-translate-api-error "API error" 'poly-translate-error)
(define-error 'poly-translate-config-error "Configuration error" 'poly-translate-error)

;; Rate limiting support
(defvar poly-translate-backend-rate-limits (make-hash-table :test 'eq)
  "Hash table for tracking rate limits per backend.")

(cl-defstruct poly-translate-rate-limit
  "Rate limit information for a backend."
  requests      ; Number of requests allowed
  period        ; Time period in seconds
  timestamps)   ; List of recent request timestamps

(defun poly-translate-backend-check-rate-limit (backend requests-per-period period)
  "Check if BACKEND is within rate limits.
REQUESTS-PER-PERIOD is the number of requests allowed per PERIOD seconds.
Returns t if request is allowed, nil otherwise."
  (let* ((now (float-time))
         (rate-limit (or (gethash backend poly-translate-backend-rate-limits)
                         (puthash backend
                                  (make-poly-translate-rate-limit
                                   :requests requests-per-period
                                   :period period
                                   :timestamps nil)
                                  poly-translate-backend-rate-limits)))
         (cutoff (- now period))
         (recent-timestamps (cl-remove-if
                             (lambda (ts) (< ts cutoff))
                             (poly-translate-rate-limit-timestamps rate-limit))))
    (setf (poly-translate-rate-limit-timestamps rate-limit) recent-timestamps)
    (if (< (length recent-timestamps) requests-per-period)
        (progn
          (push now (poly-translate-rate-limit-timestamps rate-limit))
          t)
      nil)))

;; Cache support
(defvar poly-translate-backend-cache (make-hash-table :test 'equal)
  "Translation cache.")

(defcustom poly-translate-backend-cache-ttl 3600
  "Cache time-to-live in seconds."
  :type 'integer
  :group 'poly-translate)

(cl-defstruct poly-translate-cache-entry
  "Cache entry for translations."
  translation
  timestamp)

(defun poly-translate-backend-cache-key (backend text from-lang to-lang)
  "Generate cache key for BACKEND translation of TEXT from FROM-LANG to TO-LANG."
  (format "%s:%s:%s:%s" backend from-lang to-lang text))

(defun poly-translate-backend-cache-get (backend text from-lang to-lang)
  "Get cached translation for TEXT.
Returns translation string or nil if not cached or expired."
  (let* ((key (poly-translate-backend-cache-key backend text from-lang to-lang))
         (entry (gethash key poly-translate-backend-cache)))
    (when entry
      (let ((age (- (float-time) (poly-translate-cache-entry-timestamp entry))))
        (if (< age poly-translate-backend-cache-ttl)
            (poly-translate-cache-entry-translation entry)
          (remhash key poly-translate-backend-cache)
          nil)))))

(defun poly-translate-backend-cache-put (backend text from-lang to-lang translation)
  "Cache TRANSLATION of TEXT."
  (let ((key (poly-translate-backend-cache-key backend text from-lang to-lang)))
    (puthash key
             (make-poly-translate-cache-entry
              :translation translation
              :timestamp (float-time))
             poly-translate-backend-cache)))

(defun poly-translate-backend-clear-cache ()
  "Clear all cached translations."
  (interactive)
  (clrhash poly-translate-backend-cache)
  (message "Translation cache cleared"))

(provide 'poly-translate-backend)
;;; poly-translate-backend.el ends here
