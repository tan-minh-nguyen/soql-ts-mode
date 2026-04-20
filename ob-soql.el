;;; ob-soql.el --- org-babel functions for SOQL evaluation -*- lexical-binding: t -*-

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
(require 'ob-soql-utils)
(require 'tablist-plus)


(add-to-list 'org-babel-tangle-lang-exts '("soql" . "soql"))

(add-to-list 'org-src-lang-modes '("soql" . soql-ts))
(add-to-list 'org-babel-load-languages '(soql . t))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:soql `((:results . "output raw table replace")
                                             (:org . "")
                                             (:workspace . "")
                                             (:limit . "2000")
                                             (:output . "org-table")
                                             (:editable . nil)))

(defvar org-babel-default-inline-header-args:soql `((:results . "output raw table replace")
                                                    (:org . "")
                                                    (:workspace . "")
                                                    (:limit . "2000")
                                                    (:output . "org-table")
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

(defvar-local ob-soql-block-results (make-hash-table :test #'equal)
  "Save result of block in session.")

(defun org-babel-execute:soql (body params)
  "Execute a block of SOQL code with org-babel.
BODY is the SOQL query, PARAMS are header arguments.
Dispatches to org-table or tablist output based on :output parameter."
  (let* ((processed-params (org-babel-process-params params))
         (context (org-babel-expand-body:soql body params processed-params))
         (file-temp (make-temp-file "soql"))
         (output-format (ob-soql--get-param :output processed-params))
         (sobject (ob-soql--extract-sobject context))
         (result-params (assq :results processed-params))
         (info (org-babel-get-src-block-info)))

    (write-region context nil file-temp)

    (pcase output-format
      ("org-table"
       (ob-soql--output-org-table file-temp context info processed-params))
      (_
       (ob-soql--output-tablist file-temp context sobject)))))

(defun ob-soql--tablist-columns (columns)
  "Create HEADERS for tablist."
  (apply #'vector
         (cl-loop for header in columns
                  collect (list header 30 t))))

(defun ob-soql--tablist-data (data)
  "Create DATA for tablist."
  (cl-loop for item in data
           as id = (elt item 0)
           collect (cons id
                         (make-instance 'tablist-plus-data
                                        :key id
                                        :data item))))

(cl-defun ob-soql--tablist-results (header &key data buffer)
  "Display CSV results in a tablist-plus buffer.
CSV is the raw data, URL for hyperlinks, QUERY for context.
SOBJECT is the object type, EDITABLE enables edit actions."
  (declare (indent 1))
  (let* ((columns (ob-soql--tablist-columns header))
         (data (ob-soql--tablist-data data))
         (table (apply #'ob-soql--create-tablist columns
                       :data data
                       (list :page-size 50
                          :buffer buffer))))
    (tablist-plus-table-render table)
    (pop-to-buffer (tablist-plus-table-buffer table))))

(defun ob-soql--get-param (key param-list)
  "Extract param in list."
  (cdr (assq key param-list)))

(defun ob-soql--binding-declare-variable (soql pair)
  "Handle binding value of variable to execute content."
  (cl-loop for (key . value) in pair
           as bind-value = (org-babel-elisp-var-to-soql-value value)
           do (setq soql
                    (string-replace (format ":%s" key)
                                    (org-babel--soql-value-format bind-value)
                                    soql))
           finally return soql))

(defun ob-soql--replace-job-id (buffer job-id result)
  "In BUFFER, replace JOB-ID placeholder with RESULT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (when (search-forward job-id nil t)
          (replace-match result t t))))))

(defun ob-soql--output-org-table (file _context _info _result-params)
  "Execute SOQL from FILE and insert result as org-table.
Returns job-id which org-babel inserts as placeholder.
:finally replaces placeholder with actual org-table result."
  (let* ((org (salesforce-project-org salesforce-project-session))
         (src-buffer (current-buffer))
         result)
    (emacs-pp-job-enqueue
     (lambda ()
       (salesforce-core--data-process
        :args `("query" "-f" ,file "-o" ,org "--result-format=csv")
        :parser #'emacs-pp-parser-raw))
     (lambda (csv)
       (setq result csv))
     :finally
     (lambda (job)
       ;; Save block result
       (puthash (emacs-pp-job-id job)
                (ob-soql--csv-to-lisp result)
                ob-soql-block-results)
       (ob-soql--replace-job-id src-buffer
                                (emacs-pp-job-id job)
                                (ob-soql--csv-to-org-table))))))

(defun ob-soql--output-tablist (file context sobject)
  "Execute SOQL from FILE and display in tablist buffer.
This is the original ob-soql-dispatch-soql behavior."
  (let ((org (salesforce-project-org salesforce-project-session))
        result)
    (emacs-pp-job-enqueue
     (lambda ()
       (salesforce-core--data-process
        :args `("query" "-f" ,file "-o" ,org "--result-format=csv")
        :parser #'ob-soql--parse-csv-to-lisp))
     (lambda (data)
       ;; Save data
       (setq result data)

       (let ((header (car data))
             (raw-data (cdr data)))

         (ob-soql--tablist-results header
           :data raw-data
           :buffer (generate-new-buffer (format "*SOQL: %s*" (or sobject "Results"))))))
     :finally
     (lambda (job)
       (puthash (emacs-pp-job-id job)
                result)))))

;; Hints value base on value of header arguments
(defun org-babel-prep-session:soql (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS.")

(defun org-babel--soql-value-format (value)
  "Convert an elisp var into a string of template source code
specifying a var of the same value."
  (pcase value
    ((pred listp)
     (concat "("
             (string-join
              (cl-loop for bind-val in value
                       as formatted = (format "'%s'" bind-val)
                       collect formatted)
              ",")
             ")"))
    ((and (pred (stringp value))
        (pred (string-match-p "^'" value)))
     value)
    (_ (format "'%s'" value))))

(defun org-babel-elisp-var-to-soql-value (value)
  "Convert an elisp var into a string of template source code
specifying a var of the same value."
  (let ((value (format "%s" value)))
    (cond
     ((string-prefix-p "emacs-pp" value)
      (let ((column (ob-soql--extract-column value)))
        (ob-soql--extract-result-data (gethash value ob-soql-block-results)
          :column column)))
     (t value))))

(defun org-babel-template-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")))

(provide 'ob-soql)
;;; ob-soql.el ends here
