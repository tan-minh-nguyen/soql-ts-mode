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

(cl-defun org-babel-expand-body:soql (body params &key job-results)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-template nil t)
  (let* ((parse-params (org-babel-process-params params))
         (vars (org-babel--get-vars parse-params))
         (soql (ob-soql--ensure-query-limit body :params parse-params)))

    (cl-loop for (var . value) in vars
             as bind-value = (org-babel--soql-value:generic value :job-results job-results :var var)
             do (setq soql
                      (string-replace (format ":%s" var) bind-value soql))
             finally return soql)))

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

(defvar-local ob-soql--queue-jobs nil
  "Save queue jobs wait for execute.")

(defcustom ob-soql-throttle 1
  "Time that delay for running jobs.")

(defvar-local ob-soql-timer nil
  "Timer run jobs.")

(defun org-babel-execute:soql (body params)
  "Execute a block of SOQL code with org-babel.
BODY is the SOQL query, PARAMS are header arguments.
Dispatches to org-table or tablist output based on :output parameter."
  (when (timerp ob-soql-timer)
    (cancel-timer ob-soql-timer))
  (let* ((buffer (current-buffer))
         (processed-params (org-babel-process-params params))
         (output-format (ob-soql--get-param :output processed-params))
         ;; (context (org-babel-expand-body:soql body params))
         ;; (sobject (ob-soql--extract-sobject context))
         (job-id (ob-soql-execute:src body params))
         (run-seq-jobs
          (lambda (jobs)
            (apply #'emacs-pp-jobs-sequence
                   :complete
                   (lambda (job-results)
                     (let ((data (gethash job-id job-results)))
                       (with-current-buffer buffer
                         (setq-local ob-soql--queue-jobs nil)
                         (cancel-timer ob-soql-timer))
                       (pcase output-format
                         ("tablist"
                          (let ((header (car data))
                                (raw-data (cdr data)))
                            (ob-soql--tablist-results header
                              :data raw-data
                              :buffer (generate-new-buffer (format "*SOQL: %s*" (or "Results"))))))
                         ("org-table"
                          (ob-soql--replace-job-id
                           buffer
                           job-id
                           (with-temp-buffer
                             (insert (orgtbl-to-orgtbl data nil))
                             (buffer-string)))))))
                   jobs))))

    (prog1 job-id
      (push job-id ob-soql--queue-jobs)
      (setq-local ob-soql-timer
                  (run-with-timer
                   ob-soql-throttle nil
                   run-seq-jobs
                   (reverse ob-soql--queue-jobs))))))

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

(cl-defun ob-soql-execute:src (body params)
  "Execute dependent block to use as chain.

BODY: content of source block.
PARAMS: parameter of source block."
  ;; TODO: handle dependent source of current block
  (emacs-pp-job
   :ready-p nil
   (lambda (job-results)
     (let* ((processed-params (org-babel-process-params params))
            (context (org-babel-expand-body:soql body params :job-results job-results))
            (file-temp (make-temp-file "soql"))
            (org (or (ob-soql--get-param :org processed-params)
                     (salesforce-project-org salesforce-project-session))))

       (prog1 (cons file-temp org)
         (write-region context nil file-temp))))

   (pcase-lambda (`(,file . ,org))
     (salesforce-core--data-process
      :args `("query" "-f" ,file "-o" ,org "--result-format=csv")
      :parser #'ob-soql--parse-csv-to-lisp))))

(defun ob-soql--replace-job-id (buffer job-id result)
  "In BUFFER, replace JOB-ID placeholder with RESULT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (when (search-forward job-id nil t)
          (replace-match result t t))))))

;; Hints value base on value of header arguments
(defun org-babel-prep-session:soql (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS.")

(defun org-babel--soql-value:number (value)
  "Convert an elisp var into a string of template source code
specifying a var of the same value."
  (format "%s" value))

(defun org-babel--soql-value:string (value)
  "Convert an elisp var into a string of template source code
specifying a var of the same value."
  (format "'%s'" (replace-regexp-in-string "'\\|\"" "" value)))

(cl-defun org-babel--soql-value:job (value &key job-results field)
  "Convert an elisp var into a string of template source code
specifying a var of the same value."
  (let* ((data (gethash value job-results))
         (values (ob-soql--extract-job-data data
                   :column field)))

    (if values
        (concat "(" (string-join values ",") ")")
      "null")))

(defun org-babel--soql-object-field (var)
  "Get field of this var to bind, default use Id."
  (let* ((var (format "%s" var))
         (fields (cdr (split-string var "\\."))))
    (string-join (or fields (list "Id")) ".")))

(cl-defun org-babel--soql-value:generic (value &key job-results var)
  "Convert an elisp var into a string of template source code
specifying a var of the same value."
  (cond
   ((string-prefix-p "emacs-pp-pipeline" value)
    (let ((field (org-babel--soql-object-field var)))
      (org-babel--soql-value:job value :job-results job-results :field field)))
   ((or (string-prefix-p "'" value)
        (string-prefix-p "\"" value))
    (org-babel--soql-value:string value))
   ((numberp value)
    (org-babel--soql-value:number value))
   (t (format "%s" value))))

(defun org-babel-template-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")))

(provide 'ob-soql)
;;; ob-soql.el ends here
