;;; soql-capf.el --- SOQL completion-at-point support -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author     : Tan Nguyen
;; Keywords   : soql completion
;; Package-Requires: ((emacs "29.1"))
;; Version    : 1.0.0

;;; Commentary:
;; Provides completion-at-point-functions support for SOQL.
;; Compatible with Corfu, Company-capf, and built-in completion.
;;
;; This provides basic keyword completion.
;; For SObject field completion, install salesforce-minor-mode.

;;; Code:

(require 'treesit)

;;; Keywords for completion

(defvar soql-capf--keywords
  '("SELECT" "FROM" "WHERE" "AND" "OR" "NOT"
    "ORDER" "BY" "ASC" "DESC" "NULLS" "FIRST" "LAST"
    "GROUP" "HAVING" "ROLLUP" "CUBE"
    "LIMIT" "OFFSET"
    "IN" "LIKE" "INCLUDES" "EXCLUDES"
    "TRUE" "FALSE" "NULL"
    "TODAY" "YESTERDAY" "TOMORROW"
    "LAST_WEEK" "THIS_WEEK" "NEXT_WEEK"
    "LAST_MONTH" "THIS_MONTH" "NEXT_MONTH"
    "LAST_QUARTER" "THIS_QUARTER" "NEXT_QUARTER"
    "LAST_YEAR" "THIS_YEAR" "NEXT_YEAR"
    "LAST_N_DAYS" "NEXT_N_DAYS"
    "LAST_90_DAYS" "NEXT_90_DAYS"
    "WITH" "DATA" "CATEGORY"
    "FOR" "VIEW" "REFERENCE" "UPDATE")
  "SOQL keywords for completion.")

(defvar soql-capf--functions
  '("COUNT" "COUNT_DISTINCT" "SUM" "AVG" "MIN" "MAX"
    "CALENDAR_MONTH" "CALENDAR_QUARTER" "CALENDAR_YEAR"
    "DAY_IN_MONTH" "DAY_IN_WEEK" "DAY_IN_YEAR" "DAY_ONLY"
    "FISCAL_MONTH" "FISCAL_QUARTER" "FISCAL_YEAR"
    "HOUR_IN_DAY" "WEEK_IN_MONTH" "WEEK_IN_YEAR"
    "FORMAT" "TOLABEL" "CONVERTCURRENCY" "CONVERTIMEZONE"
    "GROUPING" "DISTANCE" "GEOLOCATION")
  "SOQL aggregate functions for completion.")

;;; Completion at point

(defun soql-capf--in-query-p ()
  "Return non-nil if point is in a SOQL query context."
  (or (eq major-mode 'soql-ts-mode)
      (when-let ((node (treesit-node-at (point))))
        (treesit-parent-until node
                              (lambda (n)
                                (member (treesit-node-type n)
                                        '("soql_query_body" "soql_query")))))))

(defun soql-capf--get-bounds ()
  "Get completion bounds at point."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
        bounds
      (cons (point) (point)))))

(defun soql-capf ()
  "Completion-at-point function for SOQL.
Provides keyword and function name completion."
  (when (soql-capf--in-query-p)
    (let* ((bounds (soql-capf--get-bounds))
           (start (car bounds))
           (end (cdr bounds))
           (prefix (upcase (buffer-substring-no-properties start end)))
           (candidates (append soql-capf--keywords soql-capf--functions)))
      (list start end
            (completion-table-dynamic
             (lambda (_str)
               (cl-remove-if-not
                (lambda (kw)
                  (or (string-empty-p prefix)
                      (string-prefix-p prefix kw t)))
                candidates)))
            :exclusive 'no
            :annotation-function
            (lambda (candidate)
              (if (member candidate soql-capf--functions)
                  " <function>"
                " <keyword>"))))))

;;; Setup

(defun soql-capf-setup ()
  "Setup SOQL completion-at-point."
  (add-hook 'completion-at-point-functions #'soql-capf nil t))

(provide 'soql-capf)
;;; soql-capf.el ends here
