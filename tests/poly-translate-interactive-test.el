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

;;; Edit Original Text Tests

(ert-deftest poly-translate-test-edit-original-text ()
  "Test editing original text and retranslating."
  (poly-translate-test-setup)

  ;; Set up mock backend and engine
  (poly-translate-test-register-mock-backend 'mock-backend)
  (poly-translate-test-register-mock-engine "Mock Engine" 'mock-backend "en" "ja")

  (let ((buffer (get-buffer-create "*test-edit-translation*")))
    (with-current-buffer buffer
      (poly-translate-mode)
      (let ((inhibit-read-only t))
        (insert "Translation Result\n")
        (insert "═════════════════════════════════════════════════\n\n")
        (insert "Original:\n")
        (setq poly-translate--original-start (point))
        (insert "Hello world")
        (setq poly-translate--original-end (point))
        (put-text-property poly-translate--original-start
                           poly-translate--original-end
                           'read-only t)
        (put-text-property poly-translate--original-start
                           poly-translate--original-end
                           'poly-translate-section 'original)
        (insert "\n\n"))
      (setq poly-translate-original-text "Hello world")
      (setq poly-translate-current-engine "Mock Engine"))

    ;; Test starting edit mode
    (with-current-buffer buffer
      (goto-char poly-translate--original-start)
      (poly-translate--start-edit-original)
      (should poly-translate--edit-mode)
      (should (not (get-text-property poly-translate--original-start 'read-only))))

    ;; Test canceling edit
    (with-current-buffer buffer
      (goto-char poly-translate--original-start)
      (insert "Modified ")
      (poly-translate--cancel-edit-original)
      (should (not poly-translate--edit-mode))
      (let ((text (buffer-substring-no-properties
                   poly-translate--original-start
                   poly-translate--original-end)))
        (should (string= text "Hello world"))))

    (kill-buffer buffer))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-toggle-edit-original ()
  "Test toggling edit mode."
  (poly-translate-test-setup)

  (let ((buffer (get-buffer-create "*test-toggle-edit*")))
    (with-current-buffer buffer
      (poly-translate-mode)
      (let ((inhibit-read-only t))
        (setq poly-translate--original-start (point-min))
        (insert "Test text")
        (setq poly-translate--original-end (point))
        (put-text-property poly-translate--original-start
                           poly-translate--original-end
                           'read-only t)))

    ;; Toggle on
    (with-current-buffer buffer
      (poly-translate-toggle-edit-original)
      (should poly-translate--edit-mode))

    ;; Toggle off (finish edit)
    (with-current-buffer buffer
      (setq poly-translate-current-engine "Mock Engine")
      (setq poly-translate-original-text "Test text")
      ;; Mock the retranslation
      (cl-letf (((symbol-function 'poly-translate--retranslate)
                 (lambda (text) (message "Retranslating: %s" text))))
        (poly-translate-toggle-edit-original)
        (should (not poly-translate--edit-mode))))

    (kill-buffer buffer))

  (poly-translate-test-teardown))

;;; All Engines Translation Tests

(ert-deftest poly-translate-test-all-engines-translation ()
  "Test translation with all engines."
  (poly-translate-test-setup)

  ;; Register multiple mock backends and engines
  (poly-translate-test-register-mock-backend 'backend1)
  (poly-translate-test-register-mock-backend 'backend2)
  (poly-translate-test-register-mock-engine "Engine 1" 'backend1 "en" "ja")
  (poly-translate-test-register-mock-engine "Engine 2" 'backend2 "en" "ja")

  ;; Test all engines translation
  (let ((poly-translate-use-all-engines t))
    (should
     (condition-case nil
         (progn
           (poly-translate--do-translate-all-engines "Test text")
           t)
       (error nil))))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-all-engines-with-errors ()
  "Test all engines translation when some engines fail."
  (poly-translate-test-setup)

  ;; Register one successful and one failing backend
  (poly-translate-test-register-mock-backend 'good-backend)
  (poly-translate-register-backend
   'bad-backend
   `(:translate ,(lambda (backend text from-lang to-lang config callback error-callback)
                   (funcall error-callback "Simulated error"))
     :validate-config ,(lambda (backend config) t)))

  (poly-translate-test-register-mock-engine "Good Engine" 'good-backend "en" "ja")
  (poly-translate-register-engine
   '(:name "Bad Engine"
     :backend bad-backend
     :input-lang "en"
     :output-lang "ja"))

  ;; Test that translation completes despite one engine failing
  (should
   (condition-case nil
       (progn
         (poly-translate--do-translate-all-engines "Test text")
         t)
     (error nil)))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-all-engines-empty-list ()
  "Test all engines translation with no engines registered."
  (poly-translate-test-setup)

  ;; Ensure no engines are registered
  (clrhash poly-translate-engines)

  ;; Should error when no engines available
  (should-error
   (poly-translate--do-translate-all-engines "Test text"))

  (poly-translate-test-teardown))

;;; History Management Tests

(ert-deftest poly-translate-test-history-display ()
  "Test translation history display."
  (poly-translate-test-setup)

  ;; Add some history entries
  (setq poly-translate-history
        (list
         (list :engine "Engine 1"
               :original "Hello"
               :translation "こんにちは")
         (list :engine "Engine 2"
               :original "World"
               :translation "世界")))

  ;; Show history
  (poly-translate-show-history)

  (let ((history-buffer (get-buffer "*poly-translate-history*")))
    (should (buffer-live-p history-buffer))
    (with-current-buffer history-buffer
      (let ((content (buffer-string)))
        (poly-translate-test-should-contain content "Translation History")
        (poly-translate-test-should-contain content "Engine 1")
        (poly-translate-test-should-contain content "Hello")
        (poly-translate-test-should-contain content "こんにちは")))
    (kill-buffer history-buffer))

  (poly-translate-test-teardown))

;;; Kill Ring Tests

(ert-deftest poly-translate-test-kill-ring-limit ()
  "Test translation kill ring respects maximum size."
  (poly-translate-test-setup)

  (let ((poly-translate-kill-ring-max 3))
    ;; Add more than max items
    (poly-translate--add-to-kill-ring "Translation 1")
    (poly-translate--add-to-kill-ring "Translation 2")
    (poly-translate--add-to-kill-ring "Translation 3")
    (poly-translate--add-to-kill-ring "Translation 4")
    (poly-translate--add-to-kill-ring "Translation 5")

    ;; Should only have 3 items
    (should (= (length poly-translate-kill-ring) 3))
    ;; Most recent should be first
    (should (string= (car poly-translate-kill-ring) "Translation 5")))

  (poly-translate-test-teardown))

;;; Multiple Results Display Tests

(ert-deftest poly-translate-test-multiple-results-display ()
  "Test multiple translation results display."
  (poly-translate-test-setup)

  ;; Register multiple engines
  (poly-translate-test-register-mock-backend 'backend1)
  (poly-translate-test-register-mock-backend 'backend2)
  (poly-translate-test-register-mock-engine "Engine 1" 'backend1 "en" "ja")
  (poly-translate-test-register-mock-engine "Engine 2" 'backend2 "en" "ja")

  (let ((buffer (get-buffer-create "*test-multiple-results*"))
        (engines '("Engine 1" "Engine 2")))

    ;; Initialize buffer
    (poly-translate--display-multiple-results-init buffer "Test text" engines)

    (with-current-buffer buffer
      (let ((content (buffer-string)))
        (poly-translate-test-should-contain content "Translation Results")
        (poly-translate-test-should-contain content "Test text")
        (poly-translate-test-should-contain content "Engine 1")
        (poly-translate-test-should-contain content "Engine 2")
        (poly-translate-test-should-contain content "Translating...")))

    ;; Update with translation
    (poly-translate--update-multiple-results buffer "Engine 1" "Translated by Engine 1")

    (with-current-buffer buffer
      (let ((content (buffer-string)))
        (poly-translate-test-should-contain content "Translated by Engine 1")))

    (kill-buffer buffer))

  (poly-translate-test-teardown))

;;; Translation Buffer Commands Tests

(ert-deftest poly-translate-test-refresh-command ()
  "Test refresh command in translation buffer."
  (poly-translate-test-setup)

  ;; Set up mock backend and engine
  (poly-translate-test-register-mock-backend 'mock-backend)
  (poly-translate-test-register-mock-engine "Mock Engine" 'mock-backend "en" "ja")

  (let ((buffer (get-buffer-create "*test-refresh*")))
    (with-current-buffer buffer
      (poly-translate-mode)
      (setq poly-translate-original-text "Test text")
      (setq poly-translate-current-engine "Mock Engine"))

    ;; Test refresh
    (with-current-buffer buffer
      (should
       (condition-case nil
           (progn (poly-translate-refresh) t)
         (error nil))))

    (kill-buffer buffer))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-change-engine-command ()
  "Test change engine command."
  (poly-translate-test-setup)

  ;; Register multiple engines
  (poly-translate-test-register-mock-backend 'backend1)
  (poly-translate-test-register-mock-backend 'backend2)
  (poly-translate-test-register-mock-engine "Engine 1" 'backend1 "en" "ja")
  (poly-translate-test-register-mock-engine "Engine 2" 'backend2 "en" "ja")

  (let ((buffer (get-buffer-create "*test-change-engine*")))
    (with-current-buffer buffer
      (poly-translate-mode)
      (setq poly-translate-original-text "Test text")
      (setq poly-translate-current-engine "Engine 1"))

    ;; Test change engine
    (with-current-buffer buffer
      (cl-letf (((symbol-function 'poly-translate--select-engine)
                 (lambda () "Engine 2")))
        (should
         (condition-case nil
             (progn (poly-translate-change-engine) t)
           (error nil)))))

    (kill-buffer buffer))

  (poly-translate-test-teardown))

;;; Customization Tests

(ert-deftest poly-translate-test-custom-separators ()
  "Test custom separator configuration."
  (should (stringp poly-translate-separator-main))
  (should (stringp poly-translate-separator-engine))
  (should (stringp poly-translate-engine-prefix)))

(ert-deftest poly-translate-test-show-original-setting ()
  "Test show original text setting."
  (poly-translate-test-setup)

  (let ((poly-translate-show-original nil)
        (buffer (get-buffer-create "*test-no-original*")))

    (poly-translate--display-result buffer "Mock Engine" "Original" "Translation")

    (with-current-buffer buffer
      (let ((content (buffer-string)))
        ;; When show-original is nil, original text should not be in buffer
        ;; (though it's stored in variable)
        (should (buffer-live-p buffer))))

    (kill-buffer buffer))

  (poly-translate-test-teardown))

(ert-deftest poly-translate-test-auto-select-buffer-setting ()
  "Test auto-select buffer setting."
  (should (boundp 'poly-translate-auto-select-buffer))
  ;; Just verify the variable exists and can be set
  (let ((poly-translate-auto-select-buffer nil))
    (should (not poly-translate-auto-select-buffer)))
  (let ((poly-translate-auto-select-buffer t))
    (should poly-translate-auto-select-buffer)))

(provide 'poly-translate-interactive-test)
;;; poly-translate-interactive-test.el ends here
