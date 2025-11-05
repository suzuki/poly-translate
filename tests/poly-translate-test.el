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

;;; Language Detection Edge Cases

(ert-deftest poly-translate-test-language-detection-japanese ()
  "Test Japanese language detection with different character types."
  (poly-translate-test-setup)

  ;; Test with hiragana
  (let ((result nil))
    (poly-translate-detect-language
     "これはひらがなです"
     (lambda (lang) (setq result lang))
     (lambda (err) (error "Detection failed: %s" err)))
    (should (string= result "ja")))

  ;; Test with katakana
  (let ((result nil))
    (poly-translate-detect-language
     "カタカナテスト"
     (lambda (lang) (setq result lang))
     (lambda (err) (error "Detection failed: %s" err)))
    (should (string= result "ja")))

  ;; Test with kanji
  (let ((result nil))
    (poly-translate-detect-language
     "漢字混在文章"
     (lambda (lang) (setq result lang))
     (lambda (err) (error "Detection failed: %s" err)))
    (should (string= result "ja")))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-language-detection-other-languages ()
  "Test language detection for various languages."
  (poly-translate-test-setup)

  ;; Test Korean
  (let ((result nil))
    (poly-translate-detect-language
     "안녕하세요"
     (lambda (lang) (setq result lang))
     (lambda (err) (error "Detection failed: %s" err)))
    (should (string= result "ko")))

  ;; Test Arabic
  (let ((result nil))
    (poly-translate-detect-language
     "مرحبا"
     (lambda (lang) (setq result lang))
     (lambda (err) (error "Detection failed: %s" err)))
    (should (string= result "ar")))

  ;; Test English (default)
  (let ((result nil))
    (poly-translate-detect-language
     "Hello world"
     (lambda (lang) (setq result lang))
     (lambda (err) (error "Detection failed: %s" err)))
    (should (string= result "en")))

  (poly-translate-test-teardown))

;;; Backend Listing Tests

(ert-deftest poly-translate-test-list-backends ()
  "Test backend listing functionality."
  (poly-translate-test-setup)

  ;; Register multiple backends
  (poly-translate-test-register-mock-backend 'backend1)
  (poly-translate-test-register-mock-backend 'backend2)
  (poly-translate-test-register-mock-backend 'backend3)

  (let ((backends (poly-translate-list-backends)))
    (should (= (length backends) 3))
    (should (member 'backend1 backends))
    (should (member 'backend2 backends))
    (should (member 'backend3 backends)))

  (poly-translate-test-teardown))

;;; Engine Information Tests

(ert-deftest poly-translate-test-engine-config-storage ()
  "Test that engine config is properly stored."
  (poly-translate-test-setup)

  (poly-translate-register-engine
   '(:name "Test Engine"
     :backend test-backend
     :input-lang "ja"
     :output-lang "en"
     :api-key "test-key"
     :custom-param "custom-value"))

  (let* ((engine (poly-translate-get-engine "Test Engine"))
         (config (poly-translate-engine-config engine)))
    (should (plist-get config :api-key))
    (should (string= (plist-get config :api-key) "test-key"))
    (should (plist-get config :custom-param))
    (should (string= (plist-get config :custom-param) "custom-value")))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-engine-default-input-lang ()
  "Test that input language defaults to 'auto'."
  (poly-translate-test-setup)

  (poly-translate-register-engine
   '(:name "Test Engine"
     :backend test-backend
     :output-lang "en"))

  (let ((engine (poly-translate-get-engine "Test Engine")))
    (should (string= (poly-translate-engine-input-lang engine) "auto")))

  (poly-translate-test-teardown))

;;; Cache Edge Cases

(ert-deftest poly-translate-test-cache-different-backends ()
  "Test cache isolation between different backends."
  (poly-translate-test-setup)

  ;; Cache same text for different backends
  (poly-translate-backend-cache-put 'backend1 "hello" "en" "ja" "こんにちは")
  (poly-translate-backend-cache-put 'backend2 "hello" "en" "ja" "ハロー")

  ;; Each backend should have its own cached value
  (let ((cache1 (poly-translate-backend-cache-get 'backend1 "hello" "en" "ja"))
        (cache2 (poly-translate-backend-cache-get 'backend2 "hello" "en" "ja")))
    (should (string= cache1 "こんにちは"))
    (should (string= cache2 "ハロー")))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-cache-different-language-pairs ()
  "Test cache with different language pairs."
  (poly-translate-test-setup)

  ;; Cache same text for different language pairs
  (poly-translate-backend-cache-put 'test-backend "hello" "en" "ja" "こんにちは")
  (poly-translate-backend-cache-put 'test-backend "hello" "en" "fr" "bonjour")
  (poly-translate-backend-cache-put 'test-backend "hello" "ja" "en" "hello")

  ;; Each pair should have its own cached value
  (let ((cache-en-ja (poly-translate-backend-cache-get 'test-backend "hello" "en" "ja"))
        (cache-en-fr (poly-translate-backend-cache-get 'test-backend "hello" "en" "fr"))
        (cache-ja-en (poly-translate-backend-cache-get 'test-backend "hello" "ja" "en")))
    (should (string= cache-en-ja "こんにちは"))
    (should (string= cache-en-fr "bonjour"))
    (should (string= cache-ja-en "hello")))

  (poly-translate-test-teardown))

;;; Rate Limiting Edge Cases

(ert-deftest poly-translate-test-rate-limiting-different-backends ()
  "Test that rate limiting is independent per backend."
  (poly-translate-test-setup)

  ;; Backend1 can make 1 request per second
  (should (poly-translate-backend-check-rate-limit 'backend1 1 1))
  (should-not (poly-translate-backend-check-rate-limit 'backend1 1 1))

  ;; Backend2 should not be affected by backend1's limit
  (should (poly-translate-backend-check-rate-limit 'backend2 1 1))
  (should-not (poly-translate-backend-check-rate-limit 'backend2 1 1))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-rate-limiting-burst ()
  "Test rate limiting with burst requests."
  (poly-translate-test-setup)

  ;; Allow 3 requests per 2 seconds
  (should (poly-translate-backend-check-rate-limit 'test-backend 3 2))
  (should (poly-translate-backend-check-rate-limit 'test-backend 3 2))
  (should (poly-translate-backend-check-rate-limit 'test-backend 3 2))
  (should-not (poly-translate-backend-check-rate-limit 'test-backend 3 2))

  (poly-translate-test-teardown))

;;; Duplicate Engine Registration

(ert-deftest poly-translate-test-duplicate-engine-registration ()
  "Test that registering an engine with the same name overwrites the previous one."
  (poly-translate-test-setup)

  (poly-translate-test-register-mock-backend 'backend1)
  (poly-translate-test-register-mock-backend 'backend2)

  ;; Register first engine
  (poly-translate-test-register-mock-engine "Test Engine" 'backend1 "en" "ja")
  (let ((engine (poly-translate-get-engine "Test Engine")))
    (should (eq (poly-translate-engine-backend engine) 'backend1)))

  ;; Register again with same name but different backend
  (poly-translate-test-register-mock-engine "Test Engine" 'backend2 "ja" "en")
  (let ((engine (poly-translate-get-engine "Test Engine")))
    (should (eq (poly-translate-engine-backend engine) 'backend2))
    (should (string= (poly-translate-engine-input-lang engine) "ja"))
    (should (string= (poly-translate-engine-output-lang engine) "en")))

  (poly-translate-test-teardown))

;;; Language Name Formatting

(ert-deftest poly-translate-test-language-name-unknown ()
  "Test language name formatting for unknown codes."
  (let ((name (poly-translate-language-name "xyz")))
    (should (string= name "xyz"))))

(ert-deftest poly-translate-test-language-name-auto ()
  "Test language name formatting for 'auto'."
  ;; Assuming "auto" is handled by poly-translate-language-codes
  (let ((name (poly-translate-language-name "auto")))
    (should (stringp name))))

;;; Empty Text Handling

(ert-deftest poly-translate-test-empty-text-translation ()
  "Test translation with empty text."
  (poly-translate-test-setup)

  (poly-translate-test-register-mock-backend 'mock-backend)
  (poly-translate-test-register-mock-engine "Mock Engine" 'mock-backend "en" "ja")

  ;; Test with empty string
  (let ((result nil))
    (poly-translate-with-engine
     "Mock Engine"
     ""
     (lambda (translation) (setq result translation))
     (lambda (err) (error "Translation failed: %s" err)))
    ;; Empty text should still be processed by backend
    (should result))

  (poly-translate-test-teardown))

;;; Callback Error Handling

(ert-deftest poly-translate-test-callback-with-nil-error-callback ()
  "Test that nil error callback is handled gracefully."
  (poly-translate-test-setup)

  ;; Register backend that always fails
  (poly-translate-register-backend
   'failing-backend
   `(:translate ,(lambda (backend text from-lang to-lang config callback error-callback)
                   (funcall error-callback "Test error"))
     :validate-config ,(lambda (backend config) t)))

  (poly-translate-register-engine
   '(:name "Failing Engine"
     :backend failing-backend
     :input-lang "en"
     :output-lang "ja"))

  ;; Should not crash when error-callback is nil
  (should
   (condition-case nil
       (progn
         (poly-translate-with-engine
          "Failing Engine"
          "test"
          (lambda (result) nil)
          nil)  ; nil error callback
         t)
     (error nil)))

  (poly-translate-test-teardown))

(provide 'poly-translate-test)
;;; poly-translate-test.el ends here
