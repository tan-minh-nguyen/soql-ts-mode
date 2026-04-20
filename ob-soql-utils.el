;;; ob-soql-utils.el --- Utility functions for SOQL org-babel -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author     : Tan Nguyen <tan.nguyen@furucrm.com>
;; Maintainer : Tan Nguyen
;; Created    : December 2024
;; Keywords   : soql salesforce org-babel
;; Package-Requires: ((emacs "29.1"))
;; Version    : 1.0.0

;;; Commentary:
;;
;; Core functionality for SOQL org-babel integration.
;;
;; Section 1: SOQL-to-Apex Variable Passing
;; Section 2: Org-Table Display

;;; Code:

(require 'ob)

;; Optional: Salesforce project integration for org URL lookup
(require 'salesforce-project nil t)

;;; ========================================
;;; Section 1: SOQL-to-Apex Variable Passing
;;; ========================================

(defvar ob-soql-vars--query-cache (make-hash-table :test 'equal)
  "Cache mapping block names to SOQL query strings.")

(defun ob-soql-vars-store-query (name query-string)
  "Store QUERY-STRING under NAME for use by Apex blocks."
  (when (and name query-string)
    (let ((normalized (ob-soql-vars--normalize-query query-string)))
      (puthash name normalized ob-soql-vars--query-cache)
      normalized)))

(defun ob-soql-vars-get-query (name)
  "Retrieve stored SOQL query string by NAME."
  (gethash name ob-soql-vars--query-cache))

(defun ob-soql-vars--normalize-query (query)
  "Normalize QUERY for use in Apex."
  (let* ((trimmed (string-trim query))
         (single-line (string-join (split-string trimmed "\n" t "[ \t]+") " ")))
    (replace-regexp-in-string "[ \t]+" " " single-line)))

(defun ob-soql--parse-csv (process)
  "Convert csv to org-table then lisp-data."
  (let ((csv-content (emacs-pp-parser-raw process)))
    (with-temp-buffer
      (insert csv-content)
      (org-table-convert-region (point-min) (point-max))
      (org-table-to-lisp
       (buffer-substring-no-properties (point-min) (point-max))))))

(defun ob-soql--parse-csv-org-table (process)
  "Convert CSV to org-table string directly."
  (let ((csv-content (emacs-pp-parser-raw process)))
    (with-temp-buffer
      (insert csv-content)
      (org-table-convert-region (point-min) (point-max))
      (buffer-string))))

(cl-defun ob-soql--create-tablist (columns &rest args &key data &allow-other-keys)
  "Display CSV results in a tablist-plus buffer.
CSV is the raw data, URL for hyperlinks, QUERY for context.
SOBJECT is the object type, EDITABLE enables edit actions."
  (declare (indent 1))
  (let ((args (seq-difference args (list :data data))))
    (apply #'tablist-plus-create-table
           columns :data data args)))

(defun ob-soql-vars--extract-sobject (query)
  "Extract SObject type from SOQL QUERY."
  (when (string-match "\\bFROM\\s-+\\([A-Za-z0-9_]+\\)" query)
    (match-string 1 query)))

(defun ob-soql-vars-to-apex-query (var-name query-string)
  "Generate Apex code to execute QUERY-STRING and store in VAR-NAME."
  (let ((sobject (ob-soql-vars--extract-sobject query-string)))
    (if sobject
        (format "List<%s> %s = [%s];" sobject var-name query-string)
      (format "List<SObject> %s = Database.query('%s');"
              var-name
              (replace-regexp-in-string "'" "\\\\'" query-string)))))

(defun ob-soql-vars--capture-query (body params)
  "Capture SOQL BODY before execution."
  (when-let* ((info (org-babel-get-src-block-info))
              (name (nth 4 info)))
    (ob-soql-vars-store-query name body)))

(defun ob-soql--extract-sobject (query)
  "Extract primary SObject from SOQL QUERY."
  (when (string-match "FROM[[:space:]]+\\([[:alnum:]_]+\\)" query)
    (match-string 1 query)))

(defun ob-soql--display-as-org-table (csv-data)
  "Convert CSV-DATA to org-table string."
  (let ((buf (generate-new-buffer " *ob-soql-temp*")))
    (unwind-protect
        (with-current-buffer buf
          (insert csv-data)
          (org-table-convert-region (point-min) (point-max))
          (buffer-string))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun ob-soql-utils--org-url (org)
  "Return the Salesforce instance URL for ORG."
  (salesforce-project--user-data org "instanceUrl"))

(defun ob-soql-utils--modify-csv (csv org-hyperlink)
  "Return CSV after converting Id field values into ORG-HYPERLINK."
  (if (or (null csv) (string-empty-p csv))
      csv
    (let* ((lines (string-split csv "\n" t))
           (headers (car lines))
           (rows (cdr lines))
           (header-fields (string-split headers ","))
           (id-pos (cl-position "id" header-fields
                                :test (lambda (a b)
                                        (string= (downcase a) (downcase b))))))
      (if id-pos
          (string-join
           (cons headers
                 (mapcar (lambda (line)
                           (let ((cols (string-split line ",")))
                             (when (< id-pos (length cols))
                               (setf (nth id-pos cols)
                                     (ob-soql-utils--convert-id-to-hyperlink
                                      (nth id-pos cols) org-hyperlink)))
                             (string-join cols ",")))
                         rows))
           "\n")
        csv))))

(defun ob-soql-utils--convert-id-to-hyperlink (id org-hyperlink)
  "Convert Salesforce ID into an ORG-HYPERLINK."
  (format "[[%s/%s][%s]]" org-hyperlink id id))

(provide 'ob-soql-utils)

;;; ob-soql-utils.el ends here
