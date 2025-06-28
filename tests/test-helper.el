;;; test-helper.el --- Test helper for poly-translate -*- lexical-binding: t; -*-

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

;; Test helper functions for poly-translate.

;;; Code:

(require 'ert)

;; Add project root to load path
(let ((project-root (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path project-root))

;; Load the main library
(require 'poly-translate)

;; Test utilities
(defun poly-translate-test-setup ()
  "Set up test environment."
  ;; Clear existing engines and backends
  (clrhash poly-translate-engines)
  (clrhash poly-translate-backends)

  ;; Clear cache
  (poly-translate-backend-clear-cache)

  ;; Load backends
  (add-to-list 'load-path "backends")
  (require 'poly-translate-google)
  (require 'poly-translate-deepl))

(defun poly-translate-test-teardown ()
  "Tear down test environment."
  ;; Clear engines and backends
  (clrhash poly-translate-engines)
  (clrhash poly-translate-backends)

  ;; Clear cache
  (poly-translate-backend-clear-cache))

(defun poly-translate-test-register-mock-engine (name backend input-lang output-lang)
  "Register a mock engine for testing."
  (poly-translate-register-engine
   `(:name ,name
     :backend ,backend
     :input-lang ,input-lang
     :output-lang ,output-lang
     :mock t)))

(defun poly-translate-test-register-mock-backend (backend-symbol)
  "Register a mock backend for testing."
  (poly-translate-register-backend
   backend-symbol
   `(:translate ,(lambda (backend text from-lang to-lang config callback error-callback)
                   (let ((translation (format "MOCK-TRANSLATION[%s->%s]: %s"
                                              from-lang to-lang text)))
                     (funcall callback translation)))
     :validate-config ,(lambda (backend config) t))))

;; Mock HTTP responses for testing
(defvar poly-translate-test-mock-responses nil
  "Alist of mock HTTP responses for testing.")

(defvar poly-translate-test-original-url-retrieve nil
  "Original url-retrieve function for restoration.")

(defun poly-translate-test-mock-http-response (url response)
  "Set up mock HTTP response for URL."
  (setf (alist-get url poly-translate-test-mock-responses nil nil #'string=) response))

(defun poly-translate-test-clear-mock-responses ()
  "Clear all mock HTTP responses."
  (setq poly-translate-test-mock-responses nil))

(defun poly-translate-test-enable-http-mocking ()
  "Enable HTTP request mocking."
  (unless poly-translate-test-original-url-retrieve
    (setq poly-translate-test-original-url-retrieve
          (symbol-function 'poly-translate-backend-url-retrieve)))

  (fset 'poly-translate-backend-url-retrieve
        (lambda (url callback error-callback &rest args)
          (let ((response (alist-get url poly-translate-test-mock-responses nil nil #'string=)))
            (if response
                (if (string-prefix-p "ERROR:" response)
                    (funcall error-callback (substring response 6))
                  (funcall callback response))
              (funcall error-callback (format "No mock response for URL: %s" url)))))))

(defun poly-translate-test-disable-http-mocking ()
  "Disable HTTP request mocking and restore original function."
  (when poly-translate-test-original-url-retrieve
    (fset 'poly-translate-backend-url-retrieve poly-translate-test-original-url-retrieve)
    (setq poly-translate-test-original-url-retrieve nil)))

(defmacro poly-translate-test-with-http-mocking (&rest body)
  "Execute BODY with HTTP mocking enabled."
  `(unwind-protect
       (progn
         (poly-translate-test-enable-http-mocking)
         ,@body)
     (poly-translate-test-disable-http-mocking)
     (poly-translate-test-clear-mock-responses)))

;; Mock API responses
(defun poly-translate-test-deepl-success-response (text)
  "Generate a mock DeepL success response for TEXT."
  (format "{\"translations\":[{\"text\":\"%s\",\"detected_source_language\":\"EN\"}]}" text))

(defun poly-translate-test-google-success-response (text)
  "Generate a mock Google Translate success response for TEXT."
  (format "{\"data\":{\"translations\":[{\"translatedText\":\"%s\"}]}}" text))

(defun poly-translate-test-error-response (message)
  "Generate a mock error response with MESSAGE."
  (format "ERROR:%s" message))

;; Test assertion helpers
(defun poly-translate-test-should-contain (string substring)
  "Assert that STRING contains SUBSTRING."
  (should (string-match-p (regexp-quote substring) string)))

(defun poly-translate-test-should-match (string regexp)
  "Assert that STRING matches REGEXP."
  (should (string-match-p regexp string)))

(defun poly-translate-test-with-timeout (timeout thunk)
  "Run THUNK with TIMEOUT seconds."
  (let ((start-time (current-time))
        (result nil)
        (done nil))
    (while (and (not done)
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (setq result (funcall thunk))
      (when result
        (setq done t))
      (unless done
        (sleep-for 0.1)))
    (if done result
      (error "Timeout after %s seconds" timeout))))

(provide 'test-helper)
;;; test-helper.el ends here
