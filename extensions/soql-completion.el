;;; soql-completion.el --- Core SOQL completion logic -*- lexical-binding: t; -*-

;;; Commentary:
;; Backend-agnostic SOQL completion core.
;; Provides field completion for SOQL queries using tree-sitter and SObject metadata.

;;; Code:

(require 'treesit)
(require 'salesforce-core)

;;; Customization

(defcustom soql-completion-type-table '(("string" . "S")
                                        ("picklist" . "P") 
                                        ("date" . "D")
                                        ("datetime" . "DT")
                                        ("time" . "T")
                                        ("reference" . "R")
                                        ("textarea" . "A")
                                        ("currency" . "C")
                                        ("int" . "I")
                                        ("url" . "L")
                                        ("phone" . "P")
                                        ("double" . "F")
                                        ("id" . "UUID")
                                        ("boolean" . "B"))
  "Alist mapping field type to annotation string."
  :type '(alist :key-type string :value-type string)
  :group 'soql-completion)

(defvar-local soql-completion-workspace nil
  "Manually set workspace if not part of project.")

(defconst soql-completion-pattern "__([A-Z_]+)"
  "Pattern to trigger SOQL completion.")

;;; Metadata Access

(defun soql-completion--sobject-metadata (sobject-name)
  "Return metadata JSON file path for SOBJECT-NAME, or nil if not found."
  (let* ((file-name (concat sobject-name ".json"))
         (sobject-dir (if (string-suffix-p "__c" sobject-name)
                          salesforce-custom-objects-dir
                        salesforce-standard-objects-dir))
         (base-folder (if (salesforce-project-p)
                          (concat (salesforce-core--tools-folder)
                                  "/" salesforce-soql-metadata-dir "/"
                                  sobject-dir)
                        (concat (salesforce-core--tools-folder)
                                "/" salesforce-soql-metadata-dir
                                "/" sobject-dir)))
         (file-path (expand-file-name file-name base-folder)))
    (when (file-exists-p file-path)
      file-path)))

;;; Field Data Extraction

(defun soql-completion--picklist-values (field)
  "Get all available picklist options in FIELD."
  (cl-loop for option across (map-elt field "picklistValues")
           collect (map-elt option "value")))

(defun soql-completion--field-type (field)
  "Get type annotation for FIELD."
  (let ((type (map-elt field "type")))
    (or (alist-get type soql-completion-type-table nil nil #'string=)
        type)))

(defun soql-completion--field-annotation (field)
  "Build annotation string for FIELD."
  (soql-completion--field-type field))

(defun soql-completion--field-meta (field)
  "Build meta information for FIELD."
  (soql-completion--picklist-values field))

;;; Tree-sitter Context

(defun soql-completion--statement-root ()
  "Find SOQL statement root node."
  (treesit-parent-until (treesit-node-at (point))
                        (lambda (node)
                          (string= "soql_query_body" (treesit-node-type node)))))

(defun soql-completion--statement-p ()
  "Return non-nil if point is in a SOQL statement."
  (not (null (soql-completion--statement-root))))

(defun soql-completion--current-sobject ()
  "Get SObject name in current SOQL statement."
  (when-let* ((soql-root (soql-completion--statement-root))
              (clauses (treesit-query-capture soql-root '((from_clause) @sobject))))
    (treesit-node-text (treesit-node-child (assoc-default 'sobject clauses) 1) t)))

;;; Completion Bounds

(defun soql-completion--get-bounds ()
  "Get completion bounds at point.
Returns (start . end) cons cell."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
        bounds
      (cons (point) (point)))))

;;; Candidate Generation

(defun soql-completion--match-fields (prefix sobject)
  "Find fields matching PREFIX in SOBJECT.
Returns list of (name . properties) where properties is a plist."
  (when-let* ((metadata-file (soql-completion--sobject-metadata sobject))
              (fields (map-elt (with-current-buffer (find-file-noselect metadata-file)
                                 (goto-char (point-min))
                                 (json-parse-buffer :object-type 'hash-table))
                               "fields")))
    (cl-loop for field across fields
             as name = (map-elt field "name")
             as type = (soql-completion--field-type field)
             as annotation = (soql-completion--field-annotation field)
             as meta = (soql-completion--field-meta field)
             when (if prefix 
                      (string-prefix-p prefix name t)
                    (string-match-p ".+" name))
             collect (cons name (list :type type
                                      :annotation annotation
                                      :meta meta
                                      :field field)))))

(defun soql-completion--candidates (&optional prefix)
  "Get completion candidates matching PREFIX for current SOQL statement.
Returns list of (name . properties) pairs."
  (when-let ((sobject (soql-completion--current-sobject)))
    (soql-completion--match-fields prefix sobject)))

(provide 'soql-completion)

;;; soql-completion.el ends here
