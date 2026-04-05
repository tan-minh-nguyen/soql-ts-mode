;;; soql-fold.el --- Tree-sitter folding support for SOQL -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author     : Tan Nguyen
;; Maintainer : Tan Nguyen
;; Created    : December 2024
;; Keywords   : soql salesforce languages folding tree-sitter
;; Package-Requires: ((emacs "29.1") (treesit-fold "0.1.0"))
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
;; Provides tree-sitter based code folding for SOQL queries.
;; Requires the `treesit-fold' package to be installed.
;;
;; Foldable elements:
;; - Subqueries: Nested SELECT statements
;; - Query bodies: Main query structure
;; - Comments: Both line and block comments
;;
;; Usage:
;;   (require 'soql-fold)  ; Automatically loaded with soql-ts-mode
;;   (treesit-fold-mode 1) ; Enable folding in buffer
;;
;; Keybindings (when treesit-fold-mode is active):
;;   C-c C-s C-c  - Toggle fold at point
;;   C-c C-s C-o  - Open fold at point
;;   C-c C-s C-a  - Toggle all folds
;;
;; Example SOQL with foldable regions:
;;
;;   SELECT Id, Name,
;;          (SELECT FirstName, LastName FROM Contacts),  ← Foldable
;;          (SELECT Amount FROM Opportunities)           ← Foldable
;;   FROM Account
;;   WHERE CreatedDate > LAST_MONTH
;;
;; When folded, subqueries appear as: (SELECT...)▼

;;; Code:

(require 'treesit-fold)

;; Register SOQL folding rules with treesit-fold
(with-eval-after-load 'treesit-fold
  (add-to-list 'treesit-fold-range-alist
               '(soql-ts-mode . ((soql_query_body . treesit-fold-range-seq)
                                 (subquery . treesit-fold-range-seq)
                                 (block_comment . treesit-fold-range-block-comment)
                                 (line_comment . treesit-fold-range-c-like-comment)))))

(provide 'soql-fold)
;;; soql-fold.el ends here
