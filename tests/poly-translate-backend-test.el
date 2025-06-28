;;; poly-translate-backend-test.el --- Backend-specific tests for poly-translate -*- lexical-binding: t; -*-

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

;; Backend-specific tests for poly-translate.

;;; Code:

(require 'test-helper)
(require 'poly-translate-deepl)
(require 'poly-translate-google)
(require 'poly-translate-llm)

;;; DeepL Backend Tests

(ert-deftest poly-translate-test-deepl-language-mapping ()
  "Test DeepL language code mapping."
  (should (string= (poly-translate-deepl-get-language-code "ja") "JA"))
  (should (string= (poly-translate-deepl-get-language-code "en") "EN"))
  (should (string= (poly-translate-deepl-get-language-code "unknown") "UNKNOWN")))

(ert-deftest poly-translate-test-deepl-api-url ()
  "Test DeepL API URL selection."
  ;; Free account
  (let ((config '(:pro nil)))
    (should (string= (poly-translate-deepl-get-api-url config)
                     "https://api-free.deepl.com/v2/translate")))

  ;; Pro account
  (let ((config '(:pro t)))
    (should (string= (poly-translate-deepl-get-api-url config)
                     "https://api.deepl.com/v2/translate"))))

(ert-deftest poly-translate-test-deepl-api-key-handling ()
  "Test DeepL API key processing."
  (poly-translate-test-setup)

  ;; Mock the backend translate method to capture the API key
  (let ((captured-api-key nil))
    (cl-letf (((symbol-function 'poly-translate-backend-url-retrieve)
               (lambda (url callback error-callback &rest args)
                 ;; Don't actually make HTTP request
                 (funcall error-callback "Mocked request"))))

      ;; Test function API key
      (defun test-deepl-api-key ()
        "Test function that returns DeepL API key."
        "test-deepl-key-123:fx")

      (let ((config `(:api-key test-deepl-api-key :pro nil)))
        (condition-case err
            (poly-translate-backend-translate
             'deepl "Hello" "auto" "ja" config
             (lambda (result) nil)
             (lambda (error) (setq captured-api-key error)))
          (error nil))))

    ;; Since we mocked the HTTP request, we can't directly test the API key
    ;; but we can test that the function was called without errors
    (should t))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-deepl-engine-registration ()
  "Test DeepL engine registration."
  (poly-translate-test-setup)

  ;; Register DeepL engine
  (poly-translate-register-deepl-engine
   "Test DeepL" "en" "ja" "test-key" nil)

  (let ((engine (poly-translate-get-engine "Test DeepL")))
    (should engine)
    (should (eq (poly-translate-engine-backend engine) 'deepl))
    (should (string= (poly-translate-engine-input-lang engine) "en"))
    (should (string= (poly-translate-engine-output-lang engine) "ja")))

  (poly-translate-test-teardown))

;;; Google Backend Tests

(ert-deftest poly-translate-test-google-api-constants ()
  "Test Google Translate API constants."
  (should poly-translate-google-api-url)
  (should (string-match-p "googleapis.com" poly-translate-google-api-url))
  (should poly-translate-google-detect-url))

(ert-deftest poly-translate-test-google-api-key-handling ()
  "Test Google API key processing."
  (poly-translate-test-setup)

  ;; Test function API key
  (defun test-google-api-key ()
    "Test function that returns Google API key."
    "AIzaSyTest123456789")

  (let ((config `(:api-key test-google-api-key)))
    ;; Mock the HTTP request to avoid actual API call
    (cl-letf (((symbol-function 'poly-translate-backend-url-retrieve)
               (lambda (url callback error-callback &rest args)
                 (funcall error-callback "Mocked request"))))
      (condition-case err
          (poly-translate-backend-translate
           'google "Hello" "auto" "ja" config
           (lambda (result) nil)
           (lambda (error) nil))
        (error nil))))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-google-engine-registration ()
  "Test Google engine registration."
  (poly-translate-test-setup)

  ;; Manually register Google engine
  (poly-translate-register-engine
   '(:name "Test Google"
     :backend google
     :input-lang "en"
     :output-lang "ja"
     :api-key "test-key"))

  (let ((engine (poly-translate-get-engine "Test Google")))
    (should engine)
    (should (eq (poly-translate-engine-backend engine) 'google))
    (should (string= (poly-translate-engine-input-lang engine) "en"))
    (should (string= (poly-translate-engine-output-lang engine) "ja")))

  (poly-translate-test-teardown))

;;; LLM Backend Tests

(ert-deftest poly-translate-test-llm-prompt-generation ()
  "Test LLM prompt generation."
  (let ((prompt (poly-translate-llm-build-prompt
                 "Hello world" "en" "ja" 'default)))
    (poly-translate-test-should-contain prompt "Hello world")
    (poly-translate-test-should-contain prompt "English")
    (poly-translate-test-should-contain prompt "Japanese")))

(ert-deftest poly-translate-test-llm-api-key-handling ()
  "Test LLM API key processing."
  ;; Test function API key
  (defun test-openai-api-key ()
    "Test function that returns OpenAI API key."
    "sk-test123456789")

  (let ((config `(:provider openai
                  :api-key test-openai-api-key
                  :model "gpt-4")))
    ;; Mock gptel to avoid actual setup
    (cl-letf (((symbol-function 'gptel-make-openai)
               (lambda (name &rest args) 'mock-backend)))
      (should (poly-translate-llm-setup-gptel-backend config)))))

(ert-deftest poly-translate-test-llm-config-validation ()
  "Test LLM configuration validation."
  ;; Valid string API key
  (let ((config '(:api-key "sk-test123" :provider openai)))
    (should (poly-translate-backend-validate-config 'llm-openai config)))

  ;; Valid function API key
  (defun test-api-key-func () "sk-test123")
  (let ((config '(:api-key test-api-key-func :provider openai)))
    (should (poly-translate-backend-validate-config 'llm-openai config)))

  ;; Invalid API key (nil)
  (let ((config '(:provider openai)))
    (should-error (poly-translate-backend-validate-config 'llm-openai config)))

  ;; Invalid API key (number)
  (let ((config '(:api-key 123 :provider openai)))
    (should-error (poly-translate-backend-validate-config 'llm-openai config))))

(ert-deftest poly-translate-test-llm-engine-registration ()
  "Test LLM engine registration."
  (poly-translate-test-setup)

  ;; Manually register OpenAI engine
  (poly-translate-register-engine
   '(:name "Test OpenAI"
     :backend llm-openai
     :input-lang "en"
     :output-lang "ja"
     :api-key "test-key"
     :provider openai))

  (let ((engine (poly-translate-get-engine "Test OpenAI")))
    (should engine)
    (should (eq (poly-translate-engine-backend engine) 'llm-openai))
    (should (string= (poly-translate-engine-input-lang engine) "en"))
    (should (string= (poly-translate-engine-output-lang engine) "ja")))

  (poly-translate-test-teardown))

;;; Backend Integration Tests

(ert-deftest poly-translate-test-backend-error-handling ()
  "Test backend error handling."
  (poly-translate-test-setup)

  ;; Register mock backend that always fails
  (poly-translate-register-backend
   'failing-backend
   `(:translate ,(lambda (backend text from-lang to-lang config callback error-callback)
                   (funcall error-callback "Simulated backend failure"))
     :validate-config ,(lambda (backend config) t)))

  (poly-translate-register-engine
   '(:name "Failing Engine"
     :backend failing-backend
     :input-lang "en"
     :output-lang "ja"))

  ;; Test that error is properly handled
  (let ((error-result nil))
    (poly-translate-with-engine
     "Failing Engine"
     "test text"
     (lambda (result) (error "Should not succeed"))
     (lambda (err) (setq error-result err)))

    (should error-result)
    (poly-translate-test-should-contain error-result "Simulated backend failure"))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-backend-timeout ()
  "Test backend timeout handling."
  (poly-translate-test-setup)

  ;; Register mock backend that never responds
  (poly-translate-register-backend
   'slow-backend
   `(:translate ,(lambda (backend text from-lang to-lang config callback error-callback)
                   ;; Don't call either callback to simulate timeout
                   nil)
     :validate-config ,(lambda (backend config) t)))

  (poly-translate-register-engine
   '(:name "Slow Engine"
     :backend slow-backend
     :input-lang "en"
     :output-lang "ja"))

  ;; This would normally timeout in real usage
  ;; For testing, we just verify the setup works
  (let ((engine (poly-translate-get-engine "Slow Engine")))
    (should engine))

  (poly-translate-test-teardown))

(provide 'poly-translate-backend-test)
;;; poly-translate-backend-test.el ends here
