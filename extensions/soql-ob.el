;;; soql-ob.el --- org-babel functions for SOQL evaluation -*- lexical-binding: t -*-

;; Copyright (C) 2025 Tan Nguyen

;; Author: Tan Nguyen <tan.nguyen.w.information@gmail.com>
;; Keywords: literate programming, reproducible research, salesforce, soql
;; Homepage: https://github.com/tan-minh-nguyen/soql-ts-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (soql-ts-mode "1.0"))
;; This file is part of soql-ts-mode extensions.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Requirements:

;; Use this section to list the requirements of this language.  Most
;; languages will require that at least the language be installed on
;; the user's system, and the Emacs major mode relevant to the
;; language be installed as well.

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'salesforce-data)
(require 'salesforce-project)
(require 'ob-soql-core)
(require 'tablist-plus)

(add-to-list 'org-babel-tangle-lang-exts '("soql" . "soql"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:soql `((:results . "output raw table replace")
                                             (:org . "")
                                             (:workspace . "")
                                             (:limit . "2000")
                                             (:output . "org-table")
                                             (:sobject . "")
                                             (:editable . nil)))

(defvar org-babel-default-inline-header-args:soql `((:results . "output raw table replace")
                                                    (:org . "")
                                                    (:workspace . "")
                                                    (:limit . "2000")
                                                    (:output . "org-table")
                                                    (:sobject . "")
                                                    (:editable . nil)))

;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:template' function below. Variables get concatenated in
;; the `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.

(defun org-babel-expand-body:soql (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-template nil t)
  (let ((vars (org-babel--get-vars (or processed-params
                                       (org-babel-process-params params))))
        (soql (if (string-match-p "LIMIT" body) body
                (format "%s LIMIT %s" body (ob-soql--get-param :limit processed-params)))))

    (concat "\n" (ob-soql--binding-declare-variable soql vars) "\n")))

;; This is the main function which is called to evaluate a code
;; block.

;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; output means that the output to STDOUT will be captured and returned
;; value means that the value of the last statement in the source code block will be returned

;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.

;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.

(defun org-babel-execute:soql (body params)
  "Execute a block of SOQL code with org-babel.
BODY is the SOQL query, PARAMS are header arguments."
  (let* ((async-debug t)
         (processed-params (org-babel-process-params params))
         (full-body (org-babel-expand-body:soql body params processed-params))
         (file-temp (make-temp-file "soql"))
         (org (ob-soql--get-param :org processed-params))
         (org-url (ob-soql-core--org-url org))
         (output-format (intern (or (ob-soql--get-param :output processed-params) "org-table")))
         (sobject (ob-soql-core--extract-sobject full-body))
         (editable (ob-soql--get-param :editable processed-params)))

    (if (and org org-url)
        (progn (write-region full-body nil file-temp)
               (ob-soql--data file-temp `(,org ,org-url ,full-body ,output-format ,sobject ,editable)))
      "Something error")))

(defun ob-soql--data (file org-attr)
  "Execute SOQL query stored in FILE against ORG-ATTR.
ORG-ATTR is a list: (ORG URL QUERY OUTPUT-FORMAT SOBJECT EDITABLE).
Results are inserted asynchronously."
  (pcase-let ((`(,org ,_url ,_query ,_output-format ,sobject ,_editable) org-attr))
    (emacs-job
     (salesforce-core--data-process
      :args `("query" "-f" ,file "-o" ,org "--result-format=csv")
      :parser #'emacs-pp-parser-raw)
     (lambda (csv-content)
       (let ((data (ob-soql--convert-csv-to-lisp-data csv-content)))
         (ob-soql--display-tablist-results data header))))))

(defun ob-soql--tablist-results (csv url query sobject editable)
  "Display CSV results in a tablist-plus buffer.
CSV is the raw data, URL for hyperlinks, QUERY for context.
SOBJECT is the object type, EDITABLE enables edit actions."
  (let* ((lines (split-string csv "\n" t))
         (headers (split-string (car lines) ","))
         (rows (cdr lines))
         (columns (ob-soql--build-columns headers))
         (data (ob-soql--build-table-data rows headers url)))
    (let ((table (tablist-plus-create-table
                  columns
                  :data data
                  :page-size 50
                  :buffer-name (format "*SOQL: %s*" (or sobject "Results")))))
      (tablist-plus-table-render table)
      (pop-to-buffer (tablist-plus-table-buffer table)))))

(defun ob-soql--get-param (key param-list)
  "Extract param in list."
  (cdr (assq key param-list)))

(defun ob-soql--binding-declare-variable (soql pair)
  "Handle binding value of variable to execute content."
  (cl-loop for (key . value) in pair
           as cast-value = (format "%s" value)
           do (setq soql (string-replace (format ":%s" key)
                                         (cond ((string-match-p "^'" cast-value)
                                                (format "'%s'" cast-value))
                                               ((string-match-p "^\(" cast-value)
                                                (format "'%s'" cast-value))
                                               (t (format "'%s'" cast-value)))
                                         soql))
           finally return soql))

;; Hints value base on value of header arguments 
(defun org-babel-prep-session:soql (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS.")

(defun org-babel-elisp-var-to-soql (var)
  "Convert an elisp var into a string of template source code
specifying a var of the same value."
  (format "%s" value))

(defun org-babel-template-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")))

(provide 'soql-ob)

;;; soql-ob.el ends here
