;;; poly-translate-deepl.el --- DeepL API backend for poly-translate -*- lexical-binding: t; -*-

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

;; DeepL API backend for poly-translate.
;; Requires a DeepL API key (free or pro).

;;; Code:

(require 'poly-translate-backend)
(require 'url)
(require 'json)

;; Constants
(defconst poly-translate-deepl-api-url-free "https://api-free.deepl.com/v2/translate"
  "DeepL API endpoint for free accounts.")

(defconst poly-translate-deepl-api-url-pro "https://api.deepl.com/v2/translate"
  "DeepL API endpoint for pro accounts.")

(defconst poly-translate-deepl-usage-url-free "https://api-free.deepl.com/v2/usage"
  "DeepL usage endpoint for free accounts.")

(defconst poly-translate-deepl-usage-url-pro "https://api.deepl.com/v2/usage"
  "DeepL usage endpoint for pro accounts.")

;; Language code mapping (DeepL uses specific codes)
(defconst poly-translate-deepl-language-map
  '(("auto" . nil)  ; DeepL auto-detects by default
    ("en" . "EN")
    ("ja" . "JA")
    ("zh" . "ZH")
    ("ko" . "KO")
    ("es" . "ES")
    ("fr" . "FR")
    ("de" . "DE")
    ("it" . "IT")
    ("pt" . "PT")
    ("ru" . "RU")
    ("pl" . "PL")
    ("nl" . "NL")
    ("sv" . "SV")
    ("da" . "DA")
    ("fi" . "FI")
    ("no" . "NB")
    ("et" . "ET")
    ("lv" . "LV")
    ("lt" . "LT")
    ("cs" . "CS")
    ("sk" . "SK")
    ("sl" . "SL")
    ("hu" . "HU")
    ("ro" . "RO")
    ("bg" . "BG")
    ("el" . "EL")
    ("tr" . "TR")
    ("uk" . "UK")
    ("ar" . "AR")
    ("hi" . "HI"))
  "Mapping of standard language codes to DeepL language codes.")

;; Helper functions
(defun poly-translate-deepl-get-language-code (lang)
  "Convert standard language code LANG to DeepL format."
  (or (cdr (assoc lang poly-translate-deepl-language-map))
      (upcase lang)))

(defun poly-translate-deepl-get-api-url (config)
  "Get API URL based on CONFIG (free or pro account)."
  (if (plist-get config :pro)
      poly-translate-deepl-api-url-pro
    poly-translate-deepl-api-url-free))

;; Backend implementation
(cl-defmethod poly-translate-backend-translate ((backend (eql deepl)) text from-lang to-lang config callback error-callback)
  "Translate TEXT using DeepL API."
  (let ((api-key-raw (plist-get config :api-key))
        api-key)
    
    ;; Handle function or string API key
    (setq api-key (if (functionp api-key-raw)
                      (funcall api-key-raw)
                    api-key-raw))
    
    (when poly-translate-debug
      (message "DeepL API key debug:")
      (message "  Raw: %s (type: %s)" api-key-raw (type-of api-key-raw))
      (message "  Final: %s (type: %s)" 
               (if api-key (concat (substring api-key 0 8) "...") "nil")
               (type-of api-key)))
    
    (unless api-key
      (funcall error-callback "DeepL API key not configured")
      (return-from poly-translate-backend-translate))

    ;; Check cache first
    (let ((cached (poly-translate-backend-cache-get backend text from-lang to-lang)))
      (when cached
        (funcall callback cached)
        (return-from poly-translate-backend-translate)))

    ;; Check rate limit (500,000 characters per month for free)
    (unless (poly-translate-backend-check-rate-limit backend 100 60) ; 100 requests per minute
      (funcall error-callback "Rate limit exceeded for DeepL")
      (return-from poly-translate-backend-translate))

    (let* ((url (poly-translate-deepl-get-api-url config))
           (source-lang (unless (string= from-lang "auto")
                          (poly-translate-deepl-get-language-code from-lang)))
           (target-lang (poly-translate-deepl-get-language-code to-lang))
           (params `(("text" . ,text)
                     ("target_lang" . ,target-lang)
                     ,@(when source-lang
                         `(("source_lang" . ,source-lang)))))
           (headers `(("Authorization" . ,(format "DeepL-Auth-Key %s" api-key))
                      ("Content-Type" . "application/x-www-form-urlencoded")))
           (data (poly-translate-backend-encode-url-params params)))
      
      ;; Debug logging
      (when poly-translate-debug
        (message "DeepL API Debug:")
        (message "  URL: %s" url)
        (message "  Pro mode: %s" (plist-get config :pro))
        (message "  Source lang: %s" source-lang)
        (message "  Target lang: %s" target-lang)
        (message "  API key prefix: %s..." (substring api-key 0 (min 8 (length api-key)))))

      (poly-translate-backend-url-retrieve
       url
       (lambda (response)
         (condition-case err
             (let* ((json-data (poly-translate-backend-json-read-from-string response))
                    (translations (cdr (assoc 'translations json-data))))
               ;; Debug: Log the raw response
               (when (bound-and-true-p poly-translate-debug)
                 (message "DeepL raw response: %s" response)
                 (message "DeepL parsed data: %S" json-data))
               (if translations
                   (let ((translation (cdr (assoc 'text (aref translations 0)))))
                     (poly-translate-backend-cache-put backend text from-lang to-lang translation)
                     (funcall callback translation))
                 (funcall error-callback "No translation found in response")))
           (error
            (funcall error-callback (format "Error parsing response: %s" err)))))
       (lambda (err)
         (funcall error-callback (format "DeepL API error: %s" err)))
       "POST" data headers))))

(cl-defmethod poly-translate-backend-detect-language ((backend (eql deepl)) text callback error-callback)
  "Detect language using built-in detection (DeepL auto-detects)."
  (poly-translate-detect-language text callback error-callback))

(cl-defmethod poly-translate-backend-validate-config ((backend (eql deepl)) config)
  "Validate DeepL configuration."
  (unless (plist-get config :api-key)
    (signal 'poly-translate-config-error
            '("DeepL backend requires :api-key in configuration")))
  t)

;; Usage checking
(defun poly-translate-deepl-check-usage (config callback error-callback)
  "Check DeepL API usage."
  (let* ((api-key (plist-get config :api-key))
         (url (if (plist-get config :pro)
                  poly-translate-deepl-usage-url-pro
                poly-translate-deepl-usage-url-free))
         (headers `(("Authorization" . ,(format "DeepL-Auth-Key %s" api-key)))))

    (poly-translate-backend-url-retrieve
     url
     (lambda (response)
       (condition-case err
           (let* ((json-data (poly-translate-backend-json-read-from-string response))
                  (character-count (cdr (assoc 'character_count json-data)))
                  (character-limit (cdr (assoc 'character_limit json-data))))
             (funcall callback
                      `(:used ,character-count
                        :limit ,character-limit
                        :percentage ,(* 100.0 (/ (float character-count) character-limit)))))
         (error
          (funcall error-callback (format "Error parsing usage response: %s" err)))))
     error-callback
     "GET" nil headers)))

;; Register the backend
(poly-translate-register-backend 'deepl
                                  '(:translate poly-translate-backend-translate
                                    :detect-language poly-translate-backend-detect-language
                                    :validate-config poly-translate-backend-validate-config))

;; Convenience functions for registration
;;;###autoload
(defun poly-translate-register-deepl-engine (name input-lang output-lang api-key &optional pro)
  "Register a DeepL engine.
NAME is the engine name.
INPUT-LANG is the source language (\"auto\" for auto-detection).
OUTPUT-LANG is the target language.
API-KEY is the DeepL API key.
PRO indicates whether this is a Pro account (default: nil for free)."
  (poly-translate-register-engine
   `(:name ,name
     :backend deepl
     :input-lang ,input-lang
     :output-lang ,output-lang
     :api-key ,api-key
     ,@(when pro '(:pro t)))))

;; Example configurations
(defun poly-translate-deepl-setup-examples ()
  "Set up example DeepL engines."
  (interactive)
  (let* ((api-key (read-string "DeepL API key: "))
         (pro (y-or-n-p "Is this a Pro account? ")))
    (poly-translate-register-deepl-engine "DeepL Japanese to English" "ja" "en" api-key pro)
    (poly-translate-register-deepl-engine "DeepL English to Japanese" "en" "ja" api-key pro)
    (poly-translate-register-deepl-engine "DeepL Auto to English" "auto" "en" api-key pro)
    (poly-translate-register-deepl-engine "DeepL Auto to Japanese" "auto" "ja" api-key pro)
    (message "DeepL engines registered")))

;; Interactive usage checker
(defun poly-translate-deepl-show-usage ()
  "Show DeepL API usage information."
  (interactive)
  (let* ((engines (poly-translate-list-engines-for-backend 'deepl))
         (engine-name (if (= (length engines) 1)
                          (car engines)
                        (completing-read "Select DeepL engine: " engines nil t)))
         (engine (poly-translate-get-engine engine-name)))
    (if engine
        (poly-translate-deepl-check-usage
         (poly-translate-engine-config engine)
         (lambda (usage)
           (message "DeepL Usage: %d/%d characters (%.1f%%)"
                    (plist-get usage :used)
                    (plist-get usage :limit)
                    (plist-get usage :percentage)))
         (lambda (err)
           (message "Failed to get usage: %s" err)))
      (message "No DeepL engine selected"))))

(provide 'poly-translate-deepl)
;;; poly-translate-deepl.el ends here
