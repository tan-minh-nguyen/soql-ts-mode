;;; soql-company.el --- Company backend for SOQL completion -*- lexical-binding: t; -*-

;;; Commentary:
;; Company-mode backend for SOQL field completion.
;; This is a thin adapter around soql-completion.el core logic.

;;; Code:

(require 'soql-completion)
(require 'company)

;;; Company Backend

(defun company-soql (command &optional arg &rest ignored)
  "Company backend for SOQL field completion.
COMMAND, ARG, and IGNORED follow Company backend protocol."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-soql))
    
    (prefix 
     (and (soql-completion--statement-p)
          (company-grab-symbol)))
    
    (candidates 
     (let ((candidates (soql-completion--candidates arg)))
       (mapcar (lambda (cand)
                 (let ((name (car cand))
                       (props (cdr cand)))
                   (propertize name 'soql-completion-data props)))
               candidates)))
    
    (annotation 
     (when-let ((data (get-text-property 0 'soql-completion-data arg)))
       (concat " " (plist-get data :annotation))))
    
    (meta
     (when-let ((data (get-text-property 0 'soql-completion-data arg)))
       (let ((meta (plist-get data :meta)))
         (when meta
           (format "Picklist values: %s" (string-join meta ", "))))))
    
    (doc-buffer
     (when-let* ((data (get-text-property 0 'soql-completion-data arg))
                 (field (plist-get data :field)))
       (company-doc-buffer
        (format "Field: %s\nType: %s\nLabel: %s"
                (map-elt field "name")
                (map-elt field "type")
                (map-elt field "label")))))))

;;; Setup Function

(defun soql-company-setup ()
  "Setup Company backend for SOQL completion."
  (add-to-list 'company-backends 'company-soql))

(provide 'soql-company)

;;; soql-company.el ends here
