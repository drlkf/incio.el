;;; incio-incident.el --- Incident API for incident.io -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'incio-core)

(cl-defstruct (incio-incident (:constructor incio-incident-create))
  id reference name summary status-id status-name status-category severity
  permalink role-assignments raw)

(defun incio-incident--parse (object)
  "Parse an incident OBJECT alist."
  (let ((status (incio--alist-get 'incident_status object)))
    (incio-incident-create
     :id (incio--alist-get 'id object)
     :reference (incio--alist-get 'reference object)
     :name (incio--alist-get 'name object)
     :summary (incio--alist-get 'summary object)
     :status-id (incio--alist-get 'id status)
     :status-name (incio--alist-get 'name status)
     :status-category (incio--alist-get 'category status)
     :severity (incio--alist-get 'severity object)
     :permalink (incio--alist-get 'permalink object)
     :role-assignments (incio--alist-get 'incident_role_assignments object)
     :raw object)))

(defun incio-incident-severity-name (incident)
  (incio--alist-get 'name (incio-incident-severity incident)))

(defun incio-incident-timestamp (incident label)
  (incio--alist-get
   'value
   (incio--alist-get
    'value
    (seq-find (lambda (entry)
                (equal label
                       (incio--alist-get
                        'name (incio--alist-get 'incident_timestamp entry))))
              (incio--alist-get 'incident_timestamp_values
                                (incio-incident-raw incident))))))

(defun incio-incident-roles (incident)
  (mapcar (lambda (assignment)
            (cons (incio--alist-get 'name (incio--alist-get 'role assignment))
                  (incio--alist-get 'name (incio--alist-get 'assignee assignment))))
          (incio-incident-role-assignments incident)))

(defun incio-incident-custom-fields (incident)
  (mapcar (lambda (entry)
            (cons (incio--alist-get 'name (incio--alist-get 'custom_field entry))
                  (mapconcat (lambda (value)
                               (or (incio--alist-get 'name (incio--alist-get 'value_catalog_entry value))
                                   (incio--alist-get 'value (incio--alist-get 'value_option value))
                                   (incio--alist-get 'value_text value)
                                   (incio--alist-get 'value_numeric value)
                                   (incio--alist-get 'value_link value)
                                   ""))
                             (incio--alist-get 'values entry) ", ")))
          (incio--alist-get 'custom_field_entries (incio-incident-raw incident))))

(defun incio-incident--list-args (status-categories severity-ids sort-by)
  "Build CLI arguments for incident list filters."
  (append '("incidents" "list")
          (when status-categories
            (list "--status-category" (string-join status-categories ",")))
          (when severity-ids
            (list "--severity-id" (string-join severity-ids ",")))
          (when sort-by (list "--sort-by" sort-by))))

(defun incio-incident-fetch-list (&optional status-categories severity-ids sort-by)
  "Return incidents matching filters."
  (mapcar #'incio-incident--parse
          (apply #'incio--run-json
                 (incio-incident--list-args
                  (or status-categories '("live")) severity-ids sort-by))))

(defun incio-incident-show (id)
  "Return incident ID or reference."
  (incio-incident--parse
   (incio--run-json "incidents" "show" id)))

(defun incio-incident--edit (incident fields)
  "Edit INCIDENT with FIELDS via the incident.io edit action."
  (incio--api "POST" (format "/v2/incidents/%s/actions/edit" (incio-incident-id incident))
              `((incident . ,fields) (notify_incident_channel . t))))

(defvar incio-incident--statuses nil
  "Cached list of incident statuses.  Set to nil to refetch.")

(defun incio-incident-statuses ()
  "Return the list of incident statuses, fetching and caching on first use."
  (or incio-incident--statuses
      (setq incio-incident--statuses
            (incio--alist-get 'incident_statuses
                              (incio--api "GET" "/v1/incident_statuses")))))

(defun incio-incident-status-by-category (category)
  "Return the status alist with CATEGORY, or signal a user-error."
  (or (seq-find (lambda (status) (equal category (incio--alist-get 'category status)))
                (incio-incident-statuses))
      (user-error "No incident status with category %s" category)))

(defun incio-roles ()
  "Return the list of incident roles."
  (incio--run-json "roles" "list" "--limit" "0"))

(defun incio-users ()
  "Return the list of users."
  (incio--run-json "users" "list" "--limit" "0"))

(defun incio-incident-set-status (incident status-id)
  "Set INCIDENT's status to STATUS-ID."
  (incio-incident--edit incident `((incident_status_id . ,status-id))))

(defun incio-incident-assign-role (incident role-id user-id)
  "Assign USER-ID to ROLE-ID on INCIDENT."
  (incio-incident--edit
   incident
   `((incident_role_assignments
      . [((incident_role_id . ,role-id)
          (assignee . ((id . ,user-id))))]))))

(defun incio-incident-ack (incident)
  "Acknowledge INCIDENT by setting its status to the live category."
  (incio-incident-set-status
   incident (incio--alist-get 'id (incio-incident-status-by-category "live"))))

(defun incio-incident-reject (incident)
  "Reject INCIDENT by setting its status to the declined category."
  (incio-incident-set-status
   incident (incio--alist-get 'id (incio-incident-status-by-category "declined"))))

(defun incio-incident-merge (incident target-id)
  "Merge INCIDENT into TARGET-ID."
  (incio-incident--edit
   incident
   `((incident_status_id . ,(incio--alist-get 'id (incio-incident-status-by-category "merged")))
     (merged_into_incident_id . ,target-id))))

(defun incio-incident-close (incident)
  "Close INCIDENT."
  (incio--run "incidents" "close" (incio-incident-id incident)))

(defun incio-incident-post-update (incident message)
  "Post MESSAGE to INCIDENT."
  (incio--api "POST" "/v2/incident_updates"
              `((incident_id . ,(incio-incident-id incident))
                (message . ,message))))

(defun incio-incident-add-follow-up (incident title)
  "Add a follow-up TITLE to INCIDENT."
  (incio--api "POST" "/v2/follow_ups"
              `((incident_id . ,(incio-incident-id incident))
                (title . ,title))))

(provide 'incio-incident)

;;; incio-incident.el ends here
