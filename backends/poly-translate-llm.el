;;; poly-translate-llm.el --- LLM backend for poly-translate using gptel -*- lexical-binding: t; -*-

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

;; LLM backends for poly-translate using gptel.
;; Supports OpenAI, Anthropic (Claude), and Gemini APIs.

;;; Code:

(require 'poly-translate-backend)

;; Optional dependency on gptel
(defvar gptel-available-p nil
  "Whether gptel is available.")

(condition-case nil
    (progn
      (require 'gptel)
      (setq gptel-available-p t))
  (error
   (setq gptel-available-p nil)))

;; Translation prompt templates
(defconst poly-translate-llm-prompt-templates
  '((default . "Translate the following text from %s to %s. Only return the translation, no explanations or additional text:\n\n%s")
    (formal . "Please translate the following text from %s to %s, maintaining a formal tone:\n\n%s")
    (casual . "Please translate the following text from %s to %s, using a casual/conversational tone:\n\n%s")
    (technical . "Please translate the following technical text from %s to %s, preserving technical terminology:\n\n%s")
    (literary . "Please translate the following text from %s to %s, preserving the literary style and nuance:\n\n%s"))
  "Translation prompt templates for different styles.")

;; Language name mapping for prompts
(defconst poly-translate-llm-language-names
  '(("auto" . "the source language")
    ("en" . "English")
    ("ja" . "Japanese")
    ("zh" . "Chinese")
    ("ko" . "Korean")
    ("es" . "Spanish")
    ("fr" . "French")
    ("de" . "German")
    ("it" . "Italian")
    ("pt" . "Portuguese")
    ("ru" . "Russian")
    ("ar" . "Arabic")
    ("hi" . "Hindi"))
  "Mapping of language codes to natural language names for prompts.")

;; Helper functions
(defun poly-translate-llm-get-language-name (code)
  "Get natural language name for CODE."
  (or (cdr (assoc code poly-translate-llm-language-names))
      (format "language code %s" code)))

(defun poly-translate-llm-build-prompt (text from-lang to-lang style)
  "Build translation prompt for TEXT from FROM-LANG to TO-LANG using STYLE."
  (let* ((template (or (cdr (assoc style poly-translate-llm-prompt-templates))
                       (cdr (assoc 'default poly-translate-llm-prompt-templates))))
         (from-name (poly-translate-llm-get-language-name from-lang))
         (to-name (poly-translate-llm-get-language-name to-lang)))
    (format template from-name to-name text)))

(defun poly-translate-llm-setup-gptel-backend (config)
  "Set up gptel backend from CONFIG."
  (let ((provider (plist-get config :provider))
        (api-key-raw (plist-get config :api-key))
        (model (plist-get config :model))
        (host (plist-get config :host))
        (stream (plist-get config :stream))
        api-key)
    
    ;; Handle function or string API key
    (setq api-key (if (functionp api-key-raw)
                      (funcall api-key-raw)
                    api-key-raw))

    (cond
     ;; OpenAI
     ((eq provider 'openai)
      (setq gptel-model (or model "gpt-4")
            gptel-backend (gptel-make-openai "OpenAI"
                            :key api-key
                            :models '("gpt-4" "gpt-3.5-turbo")
                            :stream (if stream t nil))))

     ;; Anthropic (Claude)
     ((eq provider 'anthropic)
      (setq gptel-model (or model "claude-3-sonnet-20240229")
            gptel-backend (gptel-make-anthropic "Claude"
                            :key api-key
                            :models '("claude-3-opus-20240229"
                                      "claude-3-sonnet-20240229"
                                      "claude-3-haiku-20240229")
                            :stream (if stream t nil))))

     ;; Gemini
     ((eq provider 'gemini)
      (setq gptel-model (or model "gemini-pro")
            gptel-backend (gptel-make-gemini "Gemini"
                            :key api-key
                            :models '("gemini-pro" "gemini-pro-vision")
                            :stream (if stream t nil))))

     ;; Custom/Other
     (t
      (when host
        (setq gptel-backend (gptel-make-openai "Custom"
                              :key api-key
                              :host host
                              :models (list (or model "default"))
                              :stream (if stream t nil))))))))

;; Backend implementations
(cl-defmethod poly-translate-backend-translate ((backend (eql llm-openai)) text from-lang to-lang config callback error-callback)
  "Translate TEXT using OpenAI via gptel."
  (poly-translate-llm-translate 'openai text from-lang to-lang config callback error-callback))

(cl-defmethod poly-translate-backend-translate ((backend (eql llm-anthropic)) text from-lang to-lang config callback error-callback)
  "Translate TEXT using Anthropic Claude via gptel."
  (poly-translate-llm-translate 'anthropic text from-lang to-lang config callback error-callback))

(cl-defmethod poly-translate-backend-translate ((backend (eql llm-gemini)) text from-lang to-lang config callback error-callback)
  "Translate TEXT using Gemini via gptel."
  (poly-translate-llm-translate 'gemini text from-lang to-lang config callback error-callback))

(defun poly-translate-llm-translate (provider text from-lang to-lang config callback error-callback)
  "Generic LLM translation function."
  (unless gptel-available-p
    (funcall error-callback "gptel is not available. Please install gptel package.")
    (return-from poly-translate-llm-translate))

  ;; Check cache first
  (let ((cached (poly-translate-backend-cache-get provider text from-lang to-lang)))
    (when cached
      (funcall callback cached)
      (return-from poly-translate-llm-translate)))

  ;; Check rate limit (more conservative for LLMs)
  (unless (poly-translate-backend-check-rate-limit provider 10 60) ; 10 requests per minute
    (funcall error-callback (format "Rate limit exceeded for %s" provider))
    (return-from poly-translate-llm-translate))

  (let* ((style (or (plist-get config :style) 'default))
         (prompt (poly-translate-llm-build-prompt text from-lang to-lang style))
         (old-backend gptel-backend)
         (old-model gptel-model))

    ;; Set up backend configuration
    (condition-case err
        (progn
          (poly-translate-llm-setup-gptel-backend
           (plist-put config :provider provider))

          ;; Make the request
          (gptel-request
           prompt
           :callback (lambda (response info)
                       (if response
                           (let ((translation (string-trim response)))
                             (poly-translate-backend-cache-put provider text from-lang to-lang translation)
                             (funcall callback translation))
                         (funcall error-callback
                                  (format "No response from %s: %s" provider info))))
           :stream nil))

      (error
       (funcall error-callback (format "Error setting up %s: %s" provider err))))

    ;; Restore original backend/model
    (setq gptel-backend old-backend
          gptel-model old-model)))

;; Validation methods
(cl-defmethod poly-translate-backend-validate-config ((backend (eql llm-openai)) config)
  "Validate OpenAI configuration."
  (let ((api-key-raw (plist-get config :api-key)))
    (unless (or (stringp api-key-raw) (functionp api-key-raw))
      (signal 'poly-translate-config-error
              '("OpenAI backend requires :api-key in configuration"))))
  t)

(cl-defmethod poly-translate-backend-validate-config ((backend (eql llm-anthropic)) config)
  "Validate Anthropic configuration."
  (let ((api-key-raw (plist-get config :api-key)))
    (unless (or (stringp api-key-raw) (functionp api-key-raw))
      (signal 'poly-translate-config-error
              '("Anthropic backend requires :api-key in configuration"))))
  t)

(cl-defmethod poly-translate-backend-validate-config ((backend (eql llm-gemini)) config)
  "Validate Gemini configuration."
  (let ((api-key-raw (plist-get config :api-key)))
    (unless (or (stringp api-key-raw) (functionp api-key-raw))
      (signal 'poly-translate-config-error
              '("Gemini backend requires :api-key in configuration"))))
  t)

;; Register backends
(when gptel-available-p
  (poly-translate-register-backend 'llm-openai
                                    '(:translate poly-translate-backend-translate
                                      :validate-config poly-translate-backend-validate-config))

  (poly-translate-register-backend 'llm-anthropic
                                    '(:translate poly-translate-backend-translate
                                      :validate-config poly-translate-backend-validate-config))

  (poly-translate-register-backend 'llm-gemini
                                    '(:translate poly-translate-backend-translate
                                      :validate-config poly-translate-backend-validate-config)))

;; Convenience functions for registration
;;;###autoload
(defun poly-translate-register-llm-engine (name provider input-lang output-lang api-key &optional model style)
  "Register an LLM engine.
NAME is the engine name.
PROVIDER is the LLM provider ('openai, 'anthropic, or 'gemini).
INPUT-LANG is the source language.
OUTPUT-LANG is the target language.
API-KEY is the API key.
MODEL is the specific model to use (optional).
STYLE is the translation style (optional)."
  (unless gptel-available-p
    (error "gptel is not available. Please install gptel package."))

  (let ((backend-symbol (intern (format "llm-%s" provider))))
    (poly-translate-register-engine
     `(:name ,name
       :backend ,backend-symbol
       :input-lang ,input-lang
       :output-lang ,output-lang
       :api-key ,api-key
       ,@(when model `(:model ,model))
       ,@(when style `(:style ,style))))))

;; Example configurations
(defun poly-translate-llm-setup-examples ()
  "Set up example LLM engines."
  (interactive)
  (unless gptel-available-p
    (error "gptel is not available. Please install gptel package."))

  (let* ((provider (intern (completing-read "Provider: " '("openai" "anthropic" "gemini") nil t)))
         (api-key (read-string (format "%s API key: " (capitalize (symbol-name provider)))))
         (model (read-string "Model (optional): " nil nil ""))
         (style (intern (completing-read "Style: " '("default" "formal" "casual" "technical" "literary") nil t "default"))))

    (when (string-empty-p model) (setq model nil))

    (poly-translate-register-llm-engine
     (format "%s Auto to Japanese" (capitalize (symbol-name provider)))
     provider "auto" "ja" api-key model style)

    (poly-translate-register-llm-engine
     (format "%s Auto to English" (capitalize (symbol-name provider)))
     provider "auto" "en" api-key model style)

    (poly-translate-register-llm-engine
     (format "%s Japanese to English" (capitalize (symbol-name provider)))
     provider "ja" "en" api-key model style)

    (poly-translate-register-llm-engine
     (format "%s English to Japanese" (capitalize (symbol-name provider)))
     provider "en" "ja" api-key model style)

    (message "%s engines registered with style: %s" (capitalize (symbol-name provider)) style)))

(provide 'poly-translate-llm)
;;; poly-translate-llm.el ends here
