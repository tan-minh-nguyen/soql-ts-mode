;;; soql-language-server.el --- Language server configuration for SOQL -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author     : Tan Nguyen
;; Maintainer : Tan Nguyen
;; Created    : December 2024
;; Keywords   : soql salesforce languages lsp eglot
;; Package-Requires: ((emacs "29.1"))
;; Version    : 1.0.0

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Configuration for SOQL Language Server Protocol integration.
;; Supports both Eglot and LSP-Bridge clients.
;;
;; Usage with Eglot:
;;   (require 'soql-language-server)
;;   (add-hook 'soql-ts-mode-hook #'eglot-ensure)
;;
;; Usage with LSP-Bridge:
;;   (require 'soql-language-server)
;;   (add-hook 'soql-ts-mode-hook #'lsp-bridge-mode)
;;
;; The language server is automatically configured for both clients
;; when this module is loaded.

;;; Code:

(defcustom soql-lsp-path "soql-lsp"
  "Path to SOQL language server executable.
Can be an absolute path or a command name in PATH."
  :type 'string
  :group 'soql)

(defcustom soql-lsp-eglot-config '()
  "Additional configuration for Eglot LSP initialization.
This should be a plist of initialization options passed to the language server.

Example:
  (setq soql-lsp-eglot-config
        '(:initializationOptions (:completionEnabled t)))"
  :type 'list
  :group 'soql)

(defun soql-lsp--generate-server-command ()
  "Generate command to run SOQL language server.
Returns a list suitable for use in `eglot-server-programs'."
  `(,soql-lsp-path "--stdio"))

(defvar soql-lsp-bridge-language-dir
  (expand-file-name "language-server"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing LSP Bridge language server configuration.
This directory should contain the soql.json configuration file.")

(defun soql-lsp-setup-bridge ()
  "Setup LSP Bridge for current SOQL buffer.
Sets the custom language server directory for the current buffer."
  (setq-local lsp-bridge-user-langserver-dir soql-lsp-bridge-language-dir))

;;;###autoload
(defun soql-language-server-eglot ()
  "Configure Eglot for SOQL language server.
Adds SOQL mode to `eglot-server-programs' with the appropriate
language server command and configuration."
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(soql-ts-mode . (,@(soql-lsp--generate-server-command)
                                   ,@soql-lsp-eglot-config)))))

;;;###autoload
(defun soql-language-server-bridge ()
  "Configure LSP Bridge for SOQL language server.
Adds SOQL mode to LSP Bridge's single language server mode list
and sets up the custom language server directory hook."
  (with-eval-after-load 'lsp-bridge
    ;; Add SOQL to single language server mode list
    (add-to-list 'lsp-bridge-single-lang-server-mode-list
                 '(soql-ts-mode . "soql"))
    
    ;; Add hook to set custom language server directory
    (add-hook 'soql-ts-mode-hook #'soql-lsp-setup-bridge)))

;; Auto-configure both LSP clients when this module is loaded
(soql-language-server-eglot)
(soql-language-server-bridge)

(provide 'soql-language-server)
;;; soql-language-server.el ends here
