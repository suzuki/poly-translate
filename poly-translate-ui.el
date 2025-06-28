;;; poly-translate-ui.el --- User interface for poly-translate -*- lexical-binding: t; -*-

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

;; User interface components for poly-translate, including interactive
;; commands and result display.

;;; Code:

(require 'cl-lib)
(require 'poly-translate-core)

;; External variable declarations
(defvar poly-translate-use-all-engines)
(defvar poly-translate-debug)
(defvar poly-translate-default-engine)
(defvar poly-translate-buffer-name)

;; Forward function declarations

;; Customization
(defcustom poly-translate-show-original t
  "Whether to show original text in translation buffer."
  :type 'boolean
  :group 'poly-translate)

(defcustom poly-translate-auto-select-buffer t
  "Whether to automatically select the translation buffer."
  :type 'boolean
  :group 'poly-translate)

(defcustom poly-translate-kill-ring-max 10
  "Maximum number of translations to keep in kill ring history."
  :type 'integer
  :group 'poly-translate)

(defcustom poly-translate-separator-main "═══════════════════════════════════════════════════"
  "Main section separator line for translation results."
  :type 'string
  :group 'poly-translate)

(defcustom poly-translate-separator-engine "────────────────────────────────────────────────────"
  "Engine block separator line for translation results."
  :type 'string
  :group 'poly-translate)

(defcustom poly-translate-engine-prefix "▶ "
  "Prefix for engine names in translation results.
Use \"\" for no prefix, or customize with other symbols like \"[%d] \" for numbering."
  :type 'string
  :group 'poly-translate)

;; Variables
(defvar poly-translate-history nil
  "History of translation requests.")

(defvar poly-translate-kill-ring nil
  "Kill ring specifically for translations.")

(defvar-local poly-translate-current-engine nil
  "Current engine used in this translation buffer.")

(defvar-local poly-translate-original-text nil
  "Original text being translated.")

;; Variables for editable original text
(defvar-local poly-translate--original-start nil
  "Start position of original text in translation buffer.")

(defvar-local poly-translate--original-end nil
  "End position of original text in translation buffer.")

(defvar-local poly-translate--edit-mode nil
  "Non-nil when original text is in edit mode.")

;; Translation buffer mode
(defvar poly-translate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    (define-key map "g" #'poly-translate-refresh)
    (define-key map "y" #'poly-translate-yank-translation)
    (define-key map "e" #'poly-translate-toggle-edit-original)
    (define-key map "c" #'poly-translate-change-engine)
    (define-key map "s" #'poly-translate-save-translation)
    (define-key map (kbd "C-c C-c") #'poly-translate--finish-edit-original)
    (define-key map (kbd "C-c C-k") #'poly-translate--cancel-edit-original)
    map)
  "Keymap for `poly-translate-mode'.")

(define-derived-mode poly-translate-mode special-mode "Poly-Translate"
  "Major mode for displaying translation results.
\\{poly-translate-mode-map}"
  (setq-local revert-buffer-function #'poly-translate-refresh)
  ;; Ensure UTF-8 encoding for proper display of all languages
  (set-buffer-file-coding-system 'utf-8-unix)
  (setq buffer-file-coding-system 'utf-8-unix))

;; Helper functions
(defun poly-translate--do-translate (text engine &optional to-kill-ring)
  "Translate TEXT using ENGINE.
If TO-KILL-RING is non-nil, add result to kill ring instead of showing buffer."
  (let ((buffer (unless to-kill-ring
                  (poly-translate--get-result-buffer))))
    (poly-translate-with-engine
     engine text
     (lambda (translation)
       (if to-kill-ring
           (progn
             (kill-new translation)
             (poly-translate--add-to-kill-ring translation)
             (message "Translation added to kill ring"))
         (poly-translate--display-result buffer engine text translation)))
     (lambda (error)
       (message "Translation failed: %s" error)))))

(defun poly-translate--select-engine ()
  "Select a translation engine interactively."
  (let ((engines (poly-translate-list-engines)))
    (when (null engines)
      (error "No translation engines registered"))
    (if (and poly-translate-default-engine
             (member poly-translate-default-engine engines))
        poly-translate-default-engine
      (completing-read "Translation engine: " engines nil t))))

;; Interactive commands
;;;###autoload
(defun poly-translate-region (start end &optional engine)
  "Translate region from START to END using ENGINE.
If ENGINE is not specified and `poly-translate-use-all-engines' is t,
use all registered engines. Otherwise, prompt user to select an engine."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (if engine
        ;; Specific engine provided
        (poly-translate--do-translate text engine)
      ;; No engine specified - check preference
      (if poly-translate-use-all-engines
          (poly-translate--do-translate-all-engines text)
        (poly-translate--do-translate text (poly-translate--select-engine))))))

;;;###autoload
(defun poly-translate-region-to-kill-ring (start end &optional engine)
  "Translate region from START to END and add to kill ring.
If ENGINE is not specified, use the default engine or prompt user."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (engine (or engine
                     (poly-translate--select-engine))))
    (poly-translate--do-translate text engine t)))

;;;###autoload
(defun poly-translate-buffer (&optional engine)
  "Translate entire buffer using ENGINE.
If ENGINE is not specified, use the default engine or prompt user."
  (interactive)
  (poly-translate-region (point-min) (point-max) engine))

;;;###autoload
(defun poly-translate-string (text &optional engine)
  "Translate TEXT string using ENGINE.
If ENGINE is not specified, use the default engine or prompt user."
  (interactive "sText to translate: ")
  (let ((engine (or engine (poly-translate--select-engine))))
    (poly-translate--do-translate text engine)))

;; Helper functions
(defun poly-translate--get-result-buffer ()
  "Get or create the translation result buffer."
  (let ((buffer (get-buffer-create poly-translate-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'poly-translate-mode)
        (poly-translate-mode))
      ;; Ensure proper encoding for multi-language support
      (set-buffer-file-coding-system 'utf-8)
      (setq buffer-file-coding-system 'utf-8))
    buffer))

;; Multiple results display functions
(defun poly-translate--display-multiple-results-init (buffer text engines)
  "Initialize BUFFER for multiple translation results display."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (coding-system-for-write 'utf-8)
          (coding-system-for-read 'utf-8))
      (erase-buffer)
      (setq poly-translate-original-text text
            poly-translate-current-engine nil)

      ;; Header
      (insert (propertize "Translation Results\n" 'face 'header-line))
      (insert poly-translate-separator-main "\n\n")

      ;; Original text
      (when poly-translate-show-original
        (insert (propertize "Original:\n" 'face 'font-lock-keyword-face))
        (setq poly-translate--original-start (point))
        (insert text)
        (setq poly-translate--original-end (point))
        ;; Mark original text as read-only initially
        (put-text-property poly-translate--original-start 
                           poly-translate--original-end 
                           'read-only t)
        (put-text-property poly-translate--original-start 
                           poly-translate--original-end 
                           'poly-translate-section 'original)
        (insert "\n\n")
        (insert poly-translate-separator-main "\n\n"))

      ;; Create placeholders for each engine
      (dolist (engine engines)
        (let ((engine-obj (poly-translate-get-engine engine)))
          (when engine-obj
            (let ((input-lang (poly-translate-engine-input-lang engine-obj))
                  (output-lang (poly-translate-engine-output-lang engine-obj)))
              (insert (propertize (format "%s%s (%s → %s)\n"
                                          poly-translate-engine-prefix
                                          engine
                                          (poly-translate-language-name input-lang)
                                          (poly-translate-language-name output-lang))
                                  'face 'font-lock-keyword-face))
              (insert (propertize "Translating..."
                                  'face 'font-lock-comment-face
                                  'engine-name engine) "\n")))))

      ;; Footer placeholder
      (insert (propertize "Press 'q' to quit, 'g' to refresh, 'e' to edit original"
                          'face 'font-lock-comment-face))

      (goto-char (point-min))
      (when poly-translate-auto-select-buffer
        (pop-to-buffer buffer)))))

(defun poly-translate--update-multiple-results (buffer engine translation)
  "Update BUFFER with TRANSLATION result for ENGINE."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        ;; Find the engine's placeholder - search for the engine name line
        (when (search-forward engine nil t)
          (beginning-of-line)
          (forward-line)
          ;; Now we should be at the "Translating..." line
          (when (looking-at "Translating\\.\\.\\.")
            (let ((start (point)))
              ;; Find the end of this section (next ▶ prefix or end of buffer)
              (if (search-forward-regexp (format "^%s" (regexp-quote poly-translate-engine-prefix)) nil t)
                  (progn
                    (beginning-of-line)
                    (delete-region start (point)))
                ;; If no next engine found, delete to end but preserve footer
                (if (search-forward "Press 'q' to quit" nil t)
                    (progn
                      (beginning-of-line)
                      (delete-region start (point)))
                  (delete-region start (point-max))))
              ;; Insert the translation with separator
              (goto-char start)
              (insert translation "\n" poly-translate-separator-engine "\n\n"))))))))

(defun poly-translate--finalize-multiple-results (buffer)
  "Finalize the display of multiple results in BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      ;; Position cursor at the first translation result
      (when poly-translate-show-original
        (search-forward-regexp "^[^─]+→[^─]+:\n" nil t))
      (when poly-translate-auto-select-buffer
        (pop-to-buffer buffer)))))

(defun poly-translate--display-result (buffer engine original translation)
  "Display TRANSLATION in BUFFER for ENGINE and ORIGINAL text."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (coding-system-for-write 'utf-8)
          (coding-system-for-read 'utf-8))
      (erase-buffer)
      (setq poly-translate-original-text original
            poly-translate-current-engine engine)

      ;; Header
      (insert (propertize "Translation Result\n" 'face 'header-line))
      (insert poly-translate-separator-main "\n\n")

      ;; Original text
      (when poly-translate-show-original
        (insert (propertize "Original:\n" 'face 'font-lock-keyword-face))
        (setq poly-translate--original-start (point))
        (insert original)
        (setq poly-translate--original-end (point))
        ;; Mark original text as read-only initially
        (put-text-property poly-translate--original-start 
                           poly-translate--original-end 
                           'read-only t)
        (put-text-property poly-translate--original-start 
                           poly-translate--original-end 
                           'poly-translate-section 'original)
        (insert "\n\n")
        (insert poly-translate-separator-main "\n\n"))

      ;; Engine info and translation
      (let ((engine-obj (poly-translate-get-engine engine)))
        (when engine-obj
          (let ((input-lang (poly-translate-engine-input-lang engine-obj))
                (output-lang (poly-translate-engine-output-lang engine-obj)))
            (insert (propertize (format "%s%s (%s → %s)\n"
                                        poly-translate-engine-prefix
                                        engine
                                        (poly-translate-language-name input-lang)
                                        (poly-translate-language-name output-lang))
                                'face 'font-lock-keyword-face)))))

      ;; Translation
      (insert translation "\n")
      (insert poly-translate-separator-engine "\n\n")

      ;; Footer
      (insert (propertize "Press 'q' to quit, 'g' to refresh, 'y' to yank, 'e' to edit original, 'c' to change engine"
                          'face 'font-lock-comment-face))

      (goto-char (point-min))
      (when poly-translate-auto-select-buffer
        (pop-to-buffer buffer)))))

;; Translation execution
(defun poly-translate--do-translate-all-engines (text &optional to-kill-ring)
  "Translate TEXT using all registered engines.
If TO-KILL-RING is non-nil, add result to kill ring instead of showing buffer."
  (let* ((engines (poly-translate-list-engines))
         (buffer (unless to-kill-ring
                   (poly-translate--get-result-buffer)))
         (results (make-hash-table :test 'equal))
         (completed-count (make-vector 1 0))  ; Use vector for mutable reference
         (total-engines (length engines)))


    (if (= total-engines 0)
        (error "No translation engines registered")
      (if to-kill-ring
          ;; For kill-ring mode, use only the first engine
          (poly-translate--do-translate text (car engines) t)
        ;; For buffer display mode, translate with all engines
        (progn
          ;; Initialize buffer with original text
          (poly-translate--display-multiple-results-init buffer text engines)

          ;; Start translation for each engine
          (dolist (engine engines)
            (when poly-translate-debug
              (message "Starting translation with engine: %s" engine))
            ;; Use lexical binding to properly capture variables
            (let ((current-engine engine)
                  (current-buffer buffer)
                  (current-results results)
                  (current-total total-engines))
              (poly-translate-with-engine
               engine text
               (lambda (translation)
                 (when poly-translate-debug
                   (message "Translation completed for %s: %s" current-engine translation))
                 (puthash current-engine translation current-results)
                 (aset completed-count 0 (1+ (aref completed-count 0)))
                 (poly-translate--update-multiple-results current-buffer current-engine translation)
                 (when (= (aref completed-count 0) current-total)
                   (poly-translate--finalize-multiple-results current-buffer)))
               (lambda (error)
                 (when poly-translate-debug
                   (message "Translation error for %s: %s" current-engine error))
                 (let ((error-msg (format "Error: %s" error)))
                   (puthash current-engine error-msg current-results)
                   (aset completed-count 0 (1+ (aref completed-count 0)))
                   (poly-translate--update-multiple-results current-buffer current-engine error-msg)
                   (when (= (aref completed-count 0) current-total)
                     (poly-translate--finalize-multiple-results current-buffer))))))))))))




;; Helper functions
(defun poly-translate--get-current-translation ()
  "Get the current translation from the buffer.
In multiple engines mode, returns the first non-error translation."
  (save-excursion
    (goto-char (point-min))
    (if poly-translate-current-engine
        ;; Single engine mode
        (when (search-forward "Translation:\n" nil t)
          (let ((start (point)))
            (when (search-forward (make-string 50 ?─) nil t)
              (forward-line -1)
              (buffer-substring-no-properties start (line-end-position)))))
      ;; Multiple engines mode - find first valid translation
      (when (search-forward "Translation Results" nil t)
        (catch 'found
          (while (search-forward-regexp "^\\([^:]+\\).*:\n\\([^\n─]+\\)" nil t)
            (let ((translation (match-string 2)))
              (unless (string-match-p "^\\(Translating...\\|Error:\\)" translation)
                (throw 'found translation)))))))))

(defun poly-translate--add-to-kill-ring (translation)
  "Add TRANSLATION to poly-translate kill ring."
  (push translation poly-translate-kill-ring)
  (when (> (length poly-translate-kill-ring) poly-translate-kill-ring-max)
    (setcdr (nthcdr (1- poly-translate-kill-ring-max) poly-translate-kill-ring) nil)))

;; Original text editing functions
(defun poly-translate-toggle-edit-original ()
  "Toggle edit mode for original text."
  (interactive)
  (if poly-translate--edit-mode
      (poly-translate--finish-edit-original)
    (poly-translate--start-edit-original)))

(defun poly-translate--start-edit-original ()
  "Start editing the original text."
  (when (and poly-translate--original-start 
             poly-translate--original-end)
    (let ((inhibit-read-only t))
      ;; Make original text editable
      (put-text-property poly-translate--original-start 
                         poly-translate--original-end 
                         'read-only nil)
      ;; Add visual feedback
      (put-text-property poly-translate--original-start 
                         poly-translate--original-end 
                         'face 'highlight)
      ;; Set edit mode flag
      (setq poly-translate--edit-mode t)
      ;; Move cursor to original text
      (goto-char poly-translate--original-start)
      (message "Original text editing mode. Press C-c C-c to finish, C-c C-k to cancel."))))

(defun poly-translate--finish-edit-original ()
  "Finish editing original text and retranslate."
  (when poly-translate--edit-mode
    (let ((new-text (buffer-substring-no-properties 
                     poly-translate--original-start 
                     poly-translate--original-end)))
      ;; Update original text variable
      (setq poly-translate-original-text new-text)
      ;; End edit mode
      (let ((inhibit-read-only t))
        (put-text-property poly-translate--original-start 
                           poly-translate--original-end 
                           'read-only t)
        (remove-text-properties poly-translate--original-start 
                                poly-translate--original-end 
                                '(face nil)))
      (setq poly-translate--edit-mode nil)
      ;; Retranslate
      (poly-translate--retranslate new-text)
      (message "Translation updated."))))

(defun poly-translate--cancel-edit-original ()
  "Cancel editing original text without retranslating."
  (when poly-translate--edit-mode
    (let ((inhibit-read-only t))
      ;; Restore original text
      (delete-region poly-translate--original-start poly-translate--original-end)
      (goto-char poly-translate--original-start)
      (insert poly-translate-original-text)
      (setq poly-translate--original-end (point))
      ;; End edit mode
      (put-text-property poly-translate--original-start 
                         poly-translate--original-end 
                         'read-only t)
      (remove-text-properties poly-translate--original-start 
                              poly-translate--original-end 
                              '(face nil)))
    (setq poly-translate--edit-mode nil)
    (message "Edit cancelled.")))

(defun poly-translate--retranslate (new-text)
  "Retranslate NEW-TEXT using current engine(s)."
  (if poly-translate-current-engine
      ;; Single engine retranslation
      (poly-translate--do-translate new-text poly-translate-current-engine)
    ;; Multiple engines retranslation
    (let ((engines (poly-translate-list-engines)))
      (poly-translate--do-translate-all-engines new-text))))

;; Commands in result buffer
(defun poly-translate-yank-translation ()
  "Copy the current translation to kill ring."
  (interactive)
  (let ((translation (poly-translate--get-current-translation)))
    (if translation
        (progn
          (kill-new translation)
          (message "Translation copied to kill ring"))
      (message "No translation to copy"))))

(defun poly-translate-change-engine ()
  "Change translation engine and retranslate."
  (interactive)
  (when poly-translate-original-text
    (let ((new-engine (poly-translate--select-engine)))
      (poly-translate--do-translate poly-translate-original-text new-engine))))

(defun poly-translate-refresh ()
  "Refresh the current translation."
  (interactive)
  (when (and poly-translate-original-text poly-translate-current-engine)
    (poly-translate--do-translate poly-translate-original-text
                                   poly-translate-current-engine)))

(defun poly-translate-save-translation ()
  "Save the current translation to a file."
  (interactive)
  (let ((translation (poly-translate--get-current-translation)))
    (when translation
      (let ((file (read-file-name "Save translation to: ")))
        (with-temp-file file
          (insert translation))
        (message "Translation saved to %s" file)))))

;; History management
(defun poly-translate-show-history ()
  "Show translation history."
  (interactive)
  (let ((buffer (get-buffer-create "*poly-translate-history*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Translation History\n")
      (insert (make-string 50 ?=) "\n\n")
      (dolist (item (reverse poly-translate-history))
        (insert (format "Engine: %s\nOriginal: %s\nTranslation: %s\n\n"
                        (plist-get item :engine)
                        (plist-get item :original)
                        (plist-get item :translation))))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buffer)))

(provide 'poly-translate-ui)
;;; poly-translate-ui.el ends here
