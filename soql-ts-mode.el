;;; soql-ts-mode.el --- tree-sitter support for SOQL -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author     : Tan Nguyen <tan.nguyen.w.information@gmail.com>
;; Maintainer : Tan Nguyen <tan.nguyen.w.information@gmail.com>
;; Created    : December 2024
;; Keywords   : soql salesforce languages tree-sitter
;; Package-Requires: ((emacs "29.1"))
;; Version    : 1.0.0
;; URL        : https://github.com/tan-minh-nguyen/soql-ts-mode

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
;; Tree-sitter powered major mode for Salesforce SOQL queries.
;; Provides syntax highlighting, indentation, imenu, and LSP support.

;;; Code:

(require 'treesit)
(require 'cl-lib)
(eval-when-compile (require 'rx))
(require 'c-ts-common) ; For comment handling

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-query-capture "treesit.c")

;;; Customization

(defgroup soql nil
  "Major mode for editing SOQL queries."
  :group 'languages
  :prefix "soql-")

(defcustom soql-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `soql-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'soql)

;;; Language Server Support

(defcustom soql-ts-mode-lsp-bin "soql-language-server"
  "Path to SOQL language server executable.

https://github.com/forcedotcom/soql-language-server"
  :type 'string
  :group 'soql)

(defcustom soql-ts-mode-lsp-eglot-config '()
  "Additional configuration for Eglot LSP initialization.
This should be a plist of initialization options passed to the language server."
  :type 'plist
  :group 'soql)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               (cons soql-ts-mode
                     (lambda (&rest _)
                       `( ,soql-ts-mode-lsp-bin "--stdio"
                          ,@soql-ts-mode-lsp-eglot-config)))))

;;; Faces

(defface font-lock-soql-error
  '((t :foreground "red" :underline t))
  "Face used for highlighting syntax errors in `soql-ts-mode'."
  :group 'soql)

(defvar font-lock-soql-error-face 'font-lock-soql-error
  "SOQL error face.")

;;; Variables

(defvar soql-load-directory (file-name-directory load-file-name)
  "Root directory of soql-ts-mode.")

(defvar soql-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Operators and punctuation
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?+  "."     table)
    (modify-syntax-entry ?-  "."     table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?%  "."     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?|  "."     table)
    (modify-syntax-entry ?\240 "."   table)
    ;; Comments (C-style)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)
    table)
  "Syntax table for `soql-ts-mode'.")

;;; Keywords and Operators

(defvar soql-ts-mode--keywords
  '("SELECT" "FROM" "LIMIT" "ORDER_BY"
    "GROUP_BY" "HAVING" "DESC" "ASC" "OR" "AND"
    "UPDATE" "EXCLUDES" "NULL" "WHERE" "WITH")
  "Keywords use for soql statement.")

(defvar soql-ts-mode--operators
  '("=" "!=" "<>" ">" "<" ">=" "<="
    "+" "-" "*" "/" ".")
  "SOQL operators for tree-sitter font-locking.")

;;; Font Lock

(defvar soql-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'soql
   :override t
   :feature 'keyword
   `([,@soql-ts-mode--keywords] @font-lock-keyword-face)

   :language 'soql
   :override t
   :feature 'operator
   `([,@soql-ts-mode--operators] @font-lock-operator-face)

   :language 'soql
   :override t
   :feature 'type
   '([(fields_type) (update_type)] @font-lock-type-face)

   :language 'soql
   :override t
   :feature 'definition
   '((field_identifier) @font-lock-property-use-face
     (storage_identifier) @font-lock-constant-face)

   :language 'soql
   :override t
   :feature 'number
   '([(int) (decimal)] @font-lock-number-face)

   :language 'soql
   :override t
   :feature 'alias
   '((storage_alias (identifier) @font-lock-variable-name-face))

   :language 'soql
   :override t
   :feature 'error
   '([(ERROR)] @font-lock-soql-error-face)

   :language 'soql
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'soql
   :override t
   :feature 'string
   '((string_literal) @font-lock-string-face)

   :language 'soql
   :feature 'delimiter
   '((["," ":" ";"]) @font-lock-delimiter-face))
  "Tree-sitter font-lock settings for `soql-ts-mode'.")

;;; Indentation

(defvar soql-ts-mode--indent-rules
  `((soql
     ;; Root level
     ((parent-is "parser_output") column-0 0)
     ((node-is ")") parent-bol 0)

     ;; Query structure
     ((parent-is "soql_query_body") parent-bol soql-ts-mode-indent-offset)
     ((parent-is "subquery") parent-bol soql-ts-mode-indent-offset)

     ;; Clauses
     ((parent-is "select_clause") parent-bol soql-ts-mode-indent-offset)
     ((parent-is "from_clause") parent-bol 0)
     ((parent-is "where_clause") parent-bol soql-ts-mode-indent-offset)
     ((parent-is "group_by_clause") parent-bol soql-ts-mode-indent-offset)
     ((parent-is "order_by_clause") parent-bol soql-ts-mode-indent-offset)
     ((parent-is "limit_clause") parent-bol 0)
     ((parent-is "offset_clause") parent-bol 0)

     ;; Expressions
     ((parent-is "comparison_expression") parent-bol soql-ts-mode-indent-offset)
     ((parent-is "and_expression") parent-bol soql-ts-mode-indent-offset)
     ((parent-is "or_expression") parent-bol soql-ts-mode-indent-offset)

     ;; Comments
     ((and (parent-is "comment") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     ((parent-is "comment") prev-adaptive-prefix 0)))
  "Tree-sitter indent rules for `soql-ts-mode'.")

;;; Utilities

(defun soql-ts-mode-p ()
  "Check if current major mode is `soql-ts-mode'."
  (eq major-mode 'soql-ts-mode))

;;; Completion Setup

(defun soql-ts-mode--setup-completion ()
  "Setup completion backend based on available packages.
Uses Company backend with SObject field completion from extensions."
  (when (and (require 'company nil t)
             (require 'soql-company nil t))
    (soql-company-setup)))

;;; Mode Setup

(defun soql-ts-mode-setup ()
  "Setup tree-sitter configuration for `soql-ts-mode'."
  ;; Comments
  (c-ts-common-comment-setup)

  ;; Indentation
  (setq-local c-ts-common-indent-offset 'soql-ts-mode-indent-offset)
  (setq-local treesit-simple-indent-rules soql-ts-mode--indent-rules)

  ;; Electric indentation
  (setq-local electric-indent-chars
              (append "(),;:" electric-indent-chars))

  ;; Navigation
  (setq-local treesit-defun-type-regexp
              (regexp-opt '("soql_query" "subquery")))

  ;; Font-lock
  (setq-local treesit-font-lock-settings soql-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment)
                (keyword definition type alias)
                (string number function error)
                (bracket delimiter operator)))

  ;; Imenu
  (setq-local treesit-simple-imenu-settings
              '(("Object" "\\`storage_identifier\\'" nil nil)
                ("Field" "\\`field_identifier\\'" nil nil)
                ("Subquery" "\\`subquery\\'" nil nil)))

  ;; Completion
  (soql-ts-mode--setup-completion)

  ;; Finalize setup
  (treesit-major-mode-setup))

;;; Mode Definition

;;;###autoload
(define-derived-mode soql-ts-mode prog-mode "SOQL"
  "Major mode for editing Salesforce SOQL queries, powered by tree-sitter.

\\{soql-ts-mode-map}"
  :group 'soql
  :syntax-table soql-ts-mode--syntax-table

  (unless (treesit-ready-p 'soql)
    (error "Tree-sitter for SOQL isn't available"))

  (treesit-parser-create 'soql)
  (soql-ts-mode-setup))

;;; Auto Mode
(add-to-list 'auto-mode-alist '("\\.soql\\'" . soql-ts-mode))

(when (functionp 'derived-mode-add-parents)
  (derived-mode-add-parents 'soql-ts-mode '(soql-mode)))

(provide 'soql-ts-mode)
;;; soql-ts-mode.el ends here
