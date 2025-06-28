;;; poly-translate-test.el --- Tests for poly-translate -*- lexical-binding: t; -*-

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

;; Tests for poly-translate.

;;; Code:

(require 'test-helper)

;;; Core functionality tests

(ert-deftest poly-translate-test-engine-registration ()
  "Test engine registration functionality."
  (poly-translate-test-setup)

  ;; Test basic registration
  (poly-translate-register-engine
   '(:name "Test Engine"
     :backend test-backend
     :input-lang "ja"
     :output-lang "en"))

  (let ((engine (poly-translate-get-engine "Test Engine")))
    (should engine)
    (should (string= (poly-translate-engine-name engine) "Test Engine"))
    (should (eq (poly-translate-engine-backend engine) 'test-backend))
    (should (string= (poly-translate-engine-input-lang engine) "ja"))
    (should (string= (poly-translate-engine-output-lang engine) "en")))

  ;; Test engine listing
  (should (member "Test Engine" (poly-translate-list-engines)))

  ;; Test unregistration
  (poly-translate-unregister-engine "Test Engine")
  (should-not (poly-translate-get-engine "Test Engine"))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-engine-validation ()
  "Test engine validation."
  (poly-translate-test-setup)

  ;; Test missing name
  (should-error
   (poly-translate-register-engine
    '(:backend test-backend
      :output-lang "en")))

  ;; Test missing backend
  (should-error
   (poly-translate-register-engine
    '(:name "Test"
      :output-lang "en")))

  ;; Test missing output language
  (should-error
   (poly-translate-register-engine
    '(:name "Test"
      :backend test-backend)))

  ;; Test invalid output language
  (should-error
   (poly-translate-register-engine
    '(:name "Test"
      :backend test-backend
      :output-lang "auto")))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-backend-registration ()
  "Test backend registration functionality."
  (poly-translate-test-setup)

  ;; Register a mock backend
  (poly-translate-test-register-mock-backend 'test-backend)

  (let ((backend (poly-translate-get-backend 'test-backend)))
    (should backend)
    (should (plist-get backend :translate))
    (should (plist-get backend :validate-config)))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-language-detection ()
  "Test language detection functionality."
  (poly-translate-test-setup)

  ;; Test Japanese detection
  (let ((result nil))
    (poly-translate-detect-language
     "こんにちは世界"
     (lambda (lang) (setq result lang))
     (lambda (err) (error "Detection failed: %s" err)))
    (should (string= result "ja")))

  ;; Test English detection
  (let ((result nil))
    (poly-translate-detect-language
     "Hello world"
     (lambda (lang) (setq result lang))
     (lambda (err) (error "Detection failed: %s" err)))
    (should (string= result "en")))

  (poly-translate-test-teardown))

;;; Backend tests

(ert-deftest poly-translate-test-mock-translation ()
  "Test translation with mock backend."
  (poly-translate-test-setup)

  ;; Register mock backend and engine
  (poly-translate-test-register-mock-backend 'mock-backend)
  (poly-translate-test-register-mock-engine "Mock Engine" 'mock-backend "ja" "en")

  ;; Test translation
  (let ((result nil)
        (error-result nil))
    (poly-translate-with-engine
     "Mock Engine"
     "テスト"
     (lambda (translation) (setq result translation))
     (lambda (err) (setq error-result err)))

    (should result)
    (should-not error-result)
    (poly-translate-test-should-contain result "MOCK-TRANSLATION[ja->en]: テスト"))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-cache-functionality ()
  "Test translation caching."
  (poly-translate-test-setup)

  ;; Test cache put and get
  (poly-translate-backend-cache-put 'test-backend "hello" "en" "ja" "こんにちは")
  (let ((cached (poly-translate-backend-cache-get 'test-backend "hello" "en" "ja")))
    (should (string= cached "こんにちは")))

  ;; Test cache miss
  (let ((cached (poly-translate-backend-cache-get 'test-backend "goodbye" "en" "ja")))
    (should-not cached))

  ;; Test cache clear
  (poly-translate-backend-clear-cache)
  (let ((cached (poly-translate-backend-cache-get 'test-backend "hello" "en" "ja")))
    (should-not cached))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-rate-limiting ()
  "Test rate limiting functionality."
  (poly-translate-test-setup)

  ;; Test rate limit check
  (should (poly-translate-backend-check-rate-limit 'test-backend 2 1))
  (should (poly-translate-backend-check-rate-limit 'test-backend 2 1))
  (should-not (poly-translate-backend-check-rate-limit 'test-backend 2 1))

  ;; Wait and test again
  (sleep-for 1.1)
  (should (poly-translate-backend-check-rate-limit 'test-backend 2 1))

  (poly-translate-test-teardown))

;;; UI tests

(ert-deftest poly-translate-test-language-name-formatting ()
  "Test language name formatting."
  (should (string= (poly-translate-language-name "en") "English"))
  (should (string= (poly-translate-language-name "ja") "Japanese"))
  (should (string= (poly-translate-language-name "unknown") "unknown")))

(ert-deftest poly-translate-test-engine-info-formatting ()
  "Test engine information formatting."
  (poly-translate-test-setup)

  (let ((engine (make-poly-translate-engine
                 :name "Test Engine"
                 :backend 'test-backend
                 :input-lang "ja"
                 :output-lang "en")))
    (let ((info (poly-translate-format-engine-info engine)))
      (poly-translate-test-should-contain info "Test Engine")
      (poly-translate-test-should-contain info "test-backend")
      (poly-translate-test-should-contain info "Japanese")
      (poly-translate-test-should-contain info "English")))

  (poly-translate-test-teardown))

;;; Integration tests

(ert-deftest poly-translate-test-engine-listing-by-backend ()
  "Test listing engines by backend."
  (poly-translate-test-setup)

  (poly-translate-test-register-mock-backend 'backend1)
  (poly-translate-test-register-mock-backend 'backend2)

  (poly-translate-test-register-mock-engine "Engine 1" 'backend1 "ja" "en")
  (poly-translate-test-register-mock-engine "Engine 2" 'backend1 "en" "ja")
  (poly-translate-test-register-mock-engine "Engine 3" 'backend2 "ja" "en")

  (let ((backend1-engines (poly-translate-list-engines-for-backend 'backend1))
        (backend2-engines (poly-translate-list-engines-for-backend 'backend2)))
    (should (= (length backend1-engines) 2))
    (should (= (length backend2-engines) 1))
    (should (member "Engine 1" backend1-engines))
    (should (member "Engine 2" backend1-engines))
    (should (member "Engine 3" backend2-engines)))

  (poly-translate-test-teardown))

;;; Error handling tests

(ert-deftest poly-translate-test-nonexistent-engine ()
  "Test handling of nonexistent engine."
  (poly-translate-test-setup)

  (should-error
   (poly-translate-with-engine
    "Nonexistent Engine"
    "test"
    (lambda (result) nil)
    (lambda (err) (error err))))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-nonexistent-backend ()
  "Test handling of nonexistent backend."
  (poly-translate-test-setup)

  (poly-translate-register-engine
   '(:name "Test Engine"
     :backend nonexistent-backend
     :input-lang "ja"
     :output-lang "en"))

  (should-error
   (poly-translate-with-engine
    "Test Engine"
    "test"
    (lambda (result) nil)
    (lambda (err) (error err))))

  (poly-translate-test-teardown))

;;; API Key handling tests

(ert-deftest poly-translate-test-function-api-key ()
  "Test API key function handling."
  (poly-translate-test-setup)

  ;; Create a test function that returns an API key
  (defun test-api-key-function ()
    "Test function that returns an API key."
    "test-api-key-12345")

  ;; Test with function API key
  (let ((config '(:api-key test-api-key-function :pro nil)))
    ;; Simulate DeepL's API key processing
    (let ((api-key-raw (plist-get config :api-key))
          api-key)
      (setq api-key (if (functionp api-key-raw)
                        (funcall api-key-raw)
                      api-key-raw))
      (should (string= api-key "test-api-key-12345"))))

  ;; Test with string API key
  (let ((config '(:api-key "string-api-key" :pro nil)))
    (let ((api-key-raw (plist-get config :api-key))
          api-key)
      (setq api-key (if (functionp api-key-raw)
                        (funcall api-key-raw)
                      api-key-raw))
      (should (string= api-key "string-api-key"))))

  (poly-translate-test-teardown))

;;; Configuration tests

(ert-deftest poly-translate-test-version-info ()
  "Test version information."
  (should poly-translate-version)
  (should (stringp poly-translate-version)))

(ert-deftest poly-translate-test-customization-variables ()
  "Test customization variables."
  (should (boundp 'poly-translate-default-engine))
  (should (boundp 'poly-translate-buffer-name))
  (should (boundp 'poly-translate-language-codes)))

(provide 'poly-translate-test)
;;; poly-translate-test.el ends here
