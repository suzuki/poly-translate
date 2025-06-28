;;; poly-translate-google.el --- Google Translate backend for poly-translate -*- lexical-binding: t; -*-

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

;; Google Translate API backend for poly-translate.
;; Requires a Google Cloud Translation API key.

;;; Code:

(require 'poly-translate-backend)
(require 'url)
(require 'json)

;; Constants
(defconst poly-translate-google-api-url "https://translation.googleapis.com/language/translate/v2"
  "Google Translate API endpoint.")

(defconst poly-translate-google-detect-url "https://translation.googleapis.com/language/translate/v2/detect"
  "Google Translate language detection endpoint.")

;; Backend implementation
(cl-defmethod poly-translate-backend-translate ((backend (eql google)) text from-lang to-lang config callback error-callback)
  "Translate TEXT using Google Translate API."
  (let ((api-key-raw (plist-get config :api-key))
        api-key)
    
    ;; Handle function or string API key
    (setq api-key (if (functionp api-key-raw)
                      (funcall api-key-raw)
                    api-key-raw))
    
    (unless api-key
      (funcall error-callback "Google Translate API key not configured")
      (return-from poly-translate-backend-translate))

    ;; Check cache first
    (let ((cached (poly-translate-backend-cache-get backend text from-lang to-lang)))
      (when cached
        (funcall callback cached)
        (return-from poly-translate-backend-translate)))

    ;; Check rate limit
    (unless (poly-translate-backend-check-rate-limit backend 100 3600) ; 100 requests per hour
      (funcall error-callback "Rate limit exceeded for Google Translate")
      (return-from poly-translate-backend-translate))

    (let* ((params `(("key" . ,api-key)
                     ("q" . ,text)
                     ("target" . ,to-lang)
                     ,@(unless (string= from-lang "auto")
                         `(("source" . ,from-lang)))))
           (url (format "%s?%s"
                        poly-translate-google-api-url
                        (poly-translate-backend-encode-url-params params))))

      (poly-translate-backend-url-retrieve
       url
       (lambda (response)
         (condition-case err
             (progn
               ;; Debug: Log the raw response
               (when (bound-and-true-p poly-translate-debug)
                 (message "Google raw response: %s" response))
               (let* ((json-data (poly-translate-backend-json-read-from-string response))
                      (error-info (cdr (assoc 'error json-data)))
                      (data (cdr (assoc 'data json-data))))
                 ;; Debug: Log the parsed data
                 (when (bound-and-true-p poly-translate-debug)
                   (message "Google parsed data: %S" json-data))
                 (if error-info
                     (let ((error-message (cdr (assoc 'message error-info)))
                           (error-code (cdr (assoc 'code error-info))))
                       (funcall error-callback (format "Google API error %s: %s" error-code error-message)))
                   (let ((translations (cdr (assoc 'translations data))))
                     (if translations
                         (let ((translation (cdr (assoc 'translatedText (aref translations 0)))))
                           (poly-translate-backend-cache-put backend text from-lang to-lang translation)
                           (funcall callback translation))
                       (funcall error-callback "No translation found in response"))))))
           (error
            (funcall error-callback (format "Error parsing response: %s" err)))))
       (lambda (err)
         (funcall error-callback (format "Google Translate API error: %s" err)))))))

(cl-defmethod poly-translate-backend-detect-language ((backend (eql google)) text callback error-callback)
  "Detect language using Google Translate API."
  ;; For now, use the simple built-in detection
  ;; Could be enhanced to use Google's detection API
  (poly-translate-detect-language text callback error-callback))

(cl-defmethod poly-translate-backend-validate-config ((backend (eql google)) config)
  "Validate Google Translate configuration."
  (unless (plist-get config :api-key)
    (signal 'poly-translate-config-error
            '("Google Translate backend requires :api-key in configuration")))
  t)

;; Register the backend
(poly-translate-register-backend 'google
                                  '(:translate poly-translate-backend-translate
                                    :detect-language poly-translate-backend-detect-language
                                    :validate-config poly-translate-backend-validate-config))

;; Convenience functions for registration
;;;###autoload
(defun poly-translate-register-google-engine (name input-lang output-lang api-key)
  "Register a Google Translate engine.
NAME is the engine name.
INPUT-LANG is the source language (\"auto\" for auto-detection).
OUTPUT-LANG is the target language.
API-KEY is the Google Cloud Translation API key."
  (poly-translate-register-engine
   `(:name ,name
     :backend google
     :input-lang ,input-lang
     :output-lang ,output-lang
     :api-key ,api-key)))

;; Example configurations
(defun poly-translate-google-setup-examples ()
  "Set up example Google Translate engines."
  (interactive)
  (let ((api-key (read-string "Google Translate API key: ")))
    (poly-translate-register-google-engine "Google Japanese to English" "ja" "en" api-key)
    (poly-translate-register-google-engine "Google English to Japanese" "en" "ja" api-key)
    (poly-translate-register-google-engine "Google Auto to English" "auto" "en" api-key)
    (poly-translate-register-google-engine "Google Auto to Japanese" "auto" "ja" api-key)
    (message "Google Translate engines registered")))

(provide 'poly-translate-google)
;;; poly-translate-google.el ends here
