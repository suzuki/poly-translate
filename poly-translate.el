;;; poly-translate.el --- Multi-backend translation library for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Norio Suzuki <norio.suzuki@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: i18n, translation, convenience
;; URL: https://github.com/suzuki/poly-translate

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

;; poly-translate is a multi-backend translation library for Emacs that
;; supports various translation APIs and LLM-based translation services.
;;
;; Features:
;; - Plugin-based architecture for easy addition of new translation backends
;; - Support for Google Translate, DeepL, and LLM-based services (OpenAI, Claude, Gemini)
;; - Translate selected regions or entire buffers
;; - Auto-detection of source language
;; - Display results in dedicated buffer
;;
;; Usage:
;; M-x poly-translate-region RET
;; M-x poly-translate-buffer RET
;;
;; Configuration example:
;; (poly-translate-register-engine
;;  '(:name "Google Japanese to English"
;;    :backend google
;;    :input-lang "ja"
;;    :output-lang "en"))

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Group definition
(defgroup poly-translate nil
  "Multi-backend translation for Emacs."
  :group 'i18n
  :prefix "poly-translate-")

;; Custom variables
(defcustom poly-translate-default-engine nil
  "Default translation engine to use.
Should be the name of a registered engine."
  :type '(choice (const :tag "None" nil)
                 string)
  :group 'poly-translate)

(defcustom poly-translate-buffer-name "*poly-translate*"
  "Name of the buffer to display translation results."
  :type 'string
  :group 'poly-translate)

(defcustom poly-translate-debug nil
  "Enable debug mode for troubleshooting."
  :type 'boolean
  :group 'poly-translate)

(defcustom poly-translate-use-all-engines t
  "Whether to use all registered engines when no specific engine is selected.
If t, `poly-translate-region' will translate using all registered engines
and display results in comparison format.
If nil, user will be prompted to select a single engine."
  :type 'boolean
  :group 'poly-translate)

(defcustom poly-translate-language-codes
  '(("auto" . "Auto-detect")
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
  "Alist of language codes and their display names."
  :type '(alist :key-type string :value-type string)
  :group 'poly-translate)

;; Load sub-modules
(require 'poly-translate-core)
(require 'poly-translate-backend)
(require 'poly-translate-ui)

;; Load available backends
(with-eval-after-load 'poly-translate
  (let ((backends-dir (expand-file-name "backends"
                                        (file-name-directory
                                         (or load-file-name buffer-file-name)))))
    (when (file-directory-p backends-dir)
      (dolist (file (directory-files backends-dir t "\\.el\\'"))
        (condition-case err
            (load file nil t)
          (error (message "Failed to load backend %s: %s" file err)))))))

;; Version information
(defconst poly-translate-version "0.1.0"
  "Current version of poly-translate.")

(defun poly-translate-version ()
  "Display poly-translate version."
  (interactive)
  (message "poly-translate version %s" poly-translate-version))

(provide 'poly-translate)
;;; poly-translate.el ends here
