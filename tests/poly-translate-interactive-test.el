;;; poly-translate-interactive-test.el --- Interactive command tests for poly-translate -*- lexical-binding: t; -*-

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

;; Tests for interactive commands in poly-translate.

;;; Code:

(require 'test-helper)

;;; UI Buffer Tests

(ert-deftest poly-translate-test-result-buffer-creation ()
  "Test translation result buffer creation."
  (poly-translate-test-setup)

  (let ((buffer (poly-translate--get-result-buffer)))
    (should (buffer-live-p buffer))
    (should (string= (buffer-name buffer) "*poly-translate*"))
    (with-current-buffer buffer
      (should (eq major-mode 'poly-translate-mode)))
    (kill-buffer buffer))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-single-result-display ()
  "Test single translation result display."
  (poly-translate-test-setup)

  (let ((buffer (get-buffer-create "*test-translation*")))
    (with-current-buffer buffer
      (poly-translate-mode))

    ;; Mock engine for display
    (let ((mock-engine (make-poly-translate-engine
                        :name "Mock Engine"
                        :backend 'mock
                        :input-lang "en"
                        :output-lang "ja")))
      (cl-letf (((symbol-function 'poly-translate-get-engine)
                 (lambda (name) mock-engine)))

        (poly-translate--display-result
         buffer "Mock Engine" "Hello" "こんにちは")

        (with-current-buffer buffer
          (let ((content (buffer-string)))
            (poly-translate-test-should-contain content "Translation Result")
            (poly-translate-test-should-contain content "Mock Engine")
            (poly-translate-test-should-contain content "Hello")
            (poly-translate-test-should-contain content "こんにちは")))))

    (kill-buffer buffer))

  (poly-translate-test-teardown))

;;; Interactive Command Tests

(ert-deftest poly-translate-test-region-command ()
  "Test poly-translate-region command."
  (poly-translate-test-setup)

  ;; Set up mock backend and engine
  (poly-translate-test-register-mock-backend 'mock-backend)
  (poly-translate-test-register-mock-engine "Mock Engine" 'mock-backend "en" "ja")

  (with-temp-buffer
    (insert "Hello, world!")
    (mark-whole-buffer)

    ;; Mock the engine selection to return our mock engine
    (cl-letf (((symbol-function 'poly-translate--select-engine)
               (lambda () "Mock Engine")))

      ;; Test region translation (should not error)
      (should
       (condition-case nil
           (progn (poly-translate-region (point-min) (point-max)) t)
         (error nil)))))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-region-to-kill-ring ()
  "Test poly-translate-region-to-kill-ring command."
  (poly-translate-test-setup)

  ;; Set up mock backend and engine
  (poly-translate-test-register-mock-backend 'mock-backend)
  (poly-translate-test-register-mock-engine "Mock Engine" 'mock-backend "en" "ja")

  (with-temp-buffer
    (insert "Test text")
    (mark-whole-buffer)

    ;; Mock the engine selection
    (cl-letf (((symbol-function 'poly-translate--select-engine)
               (lambda () "Mock Engine")))

      ;; Test translation to kill ring
      (should
       (condition-case nil
           (progn (poly-translate-region-to-kill-ring (point-min) (point-max)) t)
         (error nil)))))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-string-command ()
  "Test poly-translate-string command."
  (poly-translate-test-setup)

  ;; Set up mock backend and engine
  (poly-translate-test-register-mock-backend 'mock-backend)
  (poly-translate-test-register-mock-engine "Mock Engine" 'mock-backend "en" "ja")

  ;; Mock the engine selection
  (cl-letf (((symbol-function 'poly-translate--select-engine)
             (lambda () "Mock Engine")))

    ;; Test string translation
    (should
     (condition-case nil
         (progn (poly-translate-string "Hello, world!") t)
       (error nil))))

  (poly-translate-test-teardown))

;;; Engine Selection Tests

(ert-deftest poly-translate-test-engine-selection ()
  "Test engine selection logic."
  (poly-translate-test-setup)

  ;; Register multiple engines
  (poly-translate-test-register-mock-backend 'mock-backend)
  (poly-translate-test-register-mock-engine "Engine 1" 'mock-backend "en" "ja")
  (poly-translate-test-register-mock-engine "Engine 2" 'mock-backend "ja" "en")

  ;; Test with default engine set
  (let ((poly-translate-default-engine "Engine 1"))
    (should (string= (poly-translate--select-engine) "Engine 1")))

  ;; Test with invalid default engine
  (let ((poly-translate-default-engine "Nonexistent Engine"))
    ;; Mock completing-read to return a specific engine
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &rest args) "Engine 2")))
      (should (string= (poly-translate--select-engine) "Engine 2"))))

  (poly-translate-test-teardown))

;;; Translation Buffer Mode Tests

(ert-deftest poly-translate-test-translation-buffer-mode ()
  "Test translation buffer major mode."
  (with-temp-buffer
    (poly-translate-mode)

    ;; Test mode activation
    (should (eq major-mode 'poly-translate-mode))
    (should (derived-mode-p 'special-mode))

    ;; Test key bindings
    (should (keymapp poly-translate-mode-map))
    (should (key-binding "q"))  ; quit-window
    (should (key-binding "g"))  ; poly-translate-refresh
    (should (key-binding "y"))  ; poly-translate-yank-translation
    (should (key-binding "e"))  ; poly-translate-change-engine
    (should (key-binding "s")))) ; poly-translate-save-translation

(ert-deftest poly-translate-test-translation-buffer-commands ()
  "Test commands available in translation buffer."
  (poly-translate-test-setup)

  (let ((buffer (get-buffer-create "*test-translation*")))
    (with-current-buffer buffer
      (poly-translate-mode)
      (let ((inhibit-read-only t))
        (insert "Test translation content"))
      (setq-local poly-translate-original-text "Original text")
      (setq-local poly-translate-current-engine "Test Engine"))

    ;; Test yank translation
    (with-current-buffer buffer
      ;; Mock get-current-translation
      (cl-letf (((symbol-function 'poly-translate--get-current-translation)
                 (lambda () "Test translation")))
        (should
         (condition-case nil
             (progn (poly-translate-yank-translation) t)
           (error nil)))))

    ;; Test save translation
    (with-current-buffer buffer
      (cl-letf (((symbol-function 'poly-translate--get-current-translation)
                 (lambda () "Test translation"))
                ((symbol-function 'read-file-name)
                 (lambda (prompt) "/tmp/test-translation.txt")))
        (should
         (condition-case nil
             (progn (poly-translate-save-translation) t)
           (error nil)))))

    (kill-buffer buffer))

  (poly-translate-test-teardown))

;;; Error Handling in Interactive Commands

(ert-deftest poly-translate-test-no-engines-error ()
  "Test error handling when no engines are registered."
  (poly-translate-test-setup)

  ;; Ensure no engines are registered
  (clrhash poly-translate-engines)

  (with-temp-buffer
    (insert "Test text")
    (mark-whole-buffer)

    ;; Should error when no engines available
    (should-error (poly-translate-region (point-min) (point-max))))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-invalid-region ()
  "Test handling of invalid region selection."
  (poly-translate-test-setup)

  ;; Set up mock engine
  (poly-translate-test-register-mock-backend 'mock-backend)
  (poly-translate-test-register-mock-engine "Mock Engine" 'mock-backend "en" "ja")

  (with-temp-buffer
    ;; Empty buffer - region should be empty
    (mark-whole-buffer)

    (cl-letf (((symbol-function 'poly-translate--select-engine)
               (lambda () "Mock Engine")))

      ;; Should handle empty text gracefully
      (should
       (condition-case nil
           (progn (poly-translate-region (point-min) (point-max)) t)
         (error nil)))))

  (poly-translate-test-teardown))

(provide 'poly-translate-interactive-test)
;;; poly-translate-interactive-test.el ends here
