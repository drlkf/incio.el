;;; incio-ui.el --- Interactive UI for incident.io -*- lexical-binding: t; -*-

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'goto-addr)
(require 'seq)
(require 'tabulated-list)
(require 'incio-incident)
(require 'incio-resources)

(defvar-local incio--incidents nil)
(defvar-local incio--incident nil)
(defvar-local incio--filters nil)
(defvar-local incio--all nil)
(defvar incio-incident-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'incio-incident-visit)
    (define-key map (kbd "g") #'incio-refresh)
    (define-key map (kbd "r") #'incio-refresh)
    (define-key map (kbd "s") #'incio-incident-set-status-at-point)
    (define-key map (kbd "a") #'incio-incident-assign-at-point)
    (define-key map (kbd "A") #'incio-incident-ack-at-point)
    (define-key map (kbd "R") #'incio-incident-reject-at-point)
    (define-key map (kbd "m") #'incio-incident-merge-at-point)
    (define-key map (kbd "c") #'incio-incident-close-at-point)
    (define-key map (kbd "u") #'incio-incident-post-update-at-point)
    (define-key map (kbd "F") #'incio-incident-follow-up-at-point)
    (define-key map (kbd "t") #'incio-incident-set-title-at-point)
    (define-key map (kbd "S") #'incio-incident-set-summary-at-point)
    (define-key map (kbd "v") #'incio-incident-set-severity-at-point)
    (define-key map (kbd "T") #'incio-incident-set-timestamp-at-point)
    (define-key map (kbd "f") #'incio-incident-set-custom-field-at-point)
    (define-key map (kbd "w") #'incio-incident-browse)
    (define-key map (kbd "/") #'incio-incident-filter)
    (define-key map (kbd "|") #'incio-incident-filter-clear)
    (define-key map (kbd "*") #'incio-incident-list-all)
    (define-key map (kbd "O") #'incio-schedule-list)
    (define-key map (kbd "?") #'incio-help)
    (define-key map (kbd "q") #'quit-window)
    map))

(defvar incio-incident-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map incio-incident-list-mode-map)
    (define-key map (kbd "RET") nil)
    map))

(defcustom incio-incident-list-custom-fields nil
  "Custom fields or roles shown as extra columns in the incident list.
Each element is a field or role name, or (NAME . WIDTH)."
  :type '(repeat (choice string (cons string integer))) :group 'incio)

(defcustom incio-incident-list-column-labels nil
  "Header labels for incident list columns, as (NAME . LABEL).
Transforms stay keyed on NAME."
  :type '(alist :key-type string :value-type string) :group 'incio)

(defcustom incio-incident-list-column-widths nil
  "Width overrides for incident list columns, as (NAME . WIDTH)."
  :type '(alist :key-type string :value-type integer) :group 'incio)

(defcustom incio-incident-list-field-transforms nil
  "Transforms applied to incident list cells.
Each element is (COLUMN-NAME . FUNCTION); FUNCTION is called with the
cell string and the incident, and returns the string to display."
  :type '(alist :key-type string :value-type function) :group 'incio)

(defun incio--incident-list-columns ()
  (vconcat [ ("Reference" 12 t)
             ("Status" 14 t)
             ("Severity" 10 t) ]
           (mapcar (lambda (field)
                     (if (consp field)
                         (list (car field) (cdr field) t)
                       (list field 16 t)))
                   incio-incident-list-custom-fields)
           [ ("Name" 0 t) ]))

(defun incio--incident-list-format (&optional columns)
  (cl-map 'vector
          (lambda (col)
            (cl-list* (or (cdr (assoc (car col) incio-incident-list-column-labels)) (car col))
                      (or (cdr (assoc (car col) incio-incident-list-column-widths)) (cadr col))
                      (cddr col)))
          (or columns (incio--incident-list-columns))))

(define-derived-mode incio-incident-list-mode tabulated-list-mode "Incio-Incidents"
  "Major mode for incident.io incidents."
  (setq-local tabulated-list-format (incio--incident-list-format))
  (setq-local tabulated-list-padding 1)
  (tabulated-list-init-header)
  (hl-line-mode 1))

(define-derived-mode incio-incident-mode special-mode "Incio-Incident"
  "Major mode for an incident.io incident."
  (use-local-map incio-incident-mode-map)
  (goto-address-mode))

(incio--evilify 'incio-incident-list-mode incio-incident-list-mode-map)
(incio--evilify 'incio-incident-mode incio-incident-mode-map)

(defun incio--incident-at-point ()
  (or (tabulated-list-get-id)
      incio--incident))

(defun incio--incident-time (value)
  (when (stringp value)
    (format-time-string "%F %R" (date-to-time value))))

(defun incio--insert-detail (label value)
  (when value
    (insert (propertize (format "%-18s" (concat label ":"))
                        'face 'font-lock-comment-face)
            (format "%s\n" value))))

(defun incio--render-incident (incident)
  (let ((inhibit-read-only t)
        (raw (incio-incident-raw incident)))
    (erase-buffer)
    (insert (propertize (or (incio-incident-reference incident) "") 'face 'incio-reference)
            "  "
            (propertize (or (incio-incident-status-name incident) "")
                        'face (incio-status-face (incio-incident-status-category incident)))
            "  "
            (propertize (or (incio-incident-severity-name incident) "")
                        'face (incio-severity-face (incio-incident-severity-name incident)))
            "\n\n"
            (format "%s\n\n" (or (incio-incident-name incident) "")))
    (incio--insert-detail "Declared"
                          (incio--incident-time
                           (incio-incident-timestamp incident "Declared at")))
    (incio--insert-detail "Impact started"
                          (incio--incident-time
                           (incio-incident-timestamp incident "Impact started at")))
    (incio--insert-detail "Resolved"
                          (incio--incident-time
                           (incio-incident-timestamp incident "Resolved at")))
    (incio--insert-detail "Last activity"
                          (incio--incident-time (incio--alist-get 'last_activity_at raw)))
    (incio--insert-detail "Type" (incio--alist-get 'name (incio--alist-get 'incident_type raw)))
    (incio--insert-detail "Mode" (incio--alist-get 'mode raw))
    (incio--insert-detail "Visibility" (incio--alist-get 'visibility raw))
    (dolist (role (incio-incident-roles incident))
      (incio--insert-detail (car role) (cdr role)))
    (dolist (field (incio-incident-custom-fields incident))
      (incio--insert-detail (car field) (cdr field)))
    (incio--insert-detail "Slack" (incio--alist-get 'slack_channel_url raw))
    (incio--insert-detail "Permalink" (incio-incident-permalink incident))
    (insert (propertize "Summary\n" 'face 'font-lock-comment-face)
            (or (incio-incident-summary incident) "") "\n")))

(defun incio--render-incidents (incidents)
  (let ((inhibit-read-only t)
        (columns (incio--incident-list-columns)))
    (setq tabulated-list-format (incio--incident-list-format columns))
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (mapcar (lambda (incident)
                    (list incident
                          (cl-map 'vector
                                  (lambda (col cell)
                                    (let ((fn (cdr (assoc (car col) incio-incident-list-field-transforms))))
                                      (if fn (funcall fn cell incident) cell)))
                                  columns
                                  (vconcat
                                   (vector (propertize (or (incio-incident-reference incident) "") 'face 'incio-reference)
                                           (propertize (or (incio-incident-status-name incident) "")
                                                       'face (incio-status-face (incio-incident-status-category incident)))
                                           (propertize (or (incio-incident-severity-name incident) "")
                                                       'face (incio-severity-face (incio-incident-severity-name incident))))
                                   (let ((fields (append (incio-incident-custom-fields incident)
                                                         (incio-incident-roles incident))))
                                     (mapcar (lambda (field)
                                               (or (cdr (assoc (if (consp field) (car field) field) fields)) ""))
                                             incio-incident-list-custom-fields))
                                   (vector (or (incio-incident-name incident) ""))))))
                  incidents))
    (tabulated-list-print t)))

(defvar incio-incident-filter-keys
  '("status_category[one_of]=" "status[one_of]=" "severity[one_of]="
    "severity[gte]=" "incident_type[one_of]=" "mode[one_of]="
    "created_at[gte]=" "created_at[lte]=" "updated_at[gte]="
    "custom_field[ID][one_of]=" "incident_role[ID][one_of]=")
  "Completion candidates for `incio-incident-filter'.")

(defun incio--filter-pairs (items)
  "Return (ID . NAME) pairs from ITEMS alists."
  (mapcar (lambda (item) (cons (incio--alist-get 'id item) (incio--alist-get 'name item)))
          items))

(defun incio--filter-value-ids (choices)
  "Return (ID . NAME) pairs from custom field CHOICES."
  (mapcar (lambda (choice) (cons (cdar (cdr choice)) (car choice))) choices))

(defun incio--filter-candidates (input fetch)
  "Return (CANDIDATE . ANNOTATION) pairs completing filter INPUT.
FETCH is called with a cache key and a thunk to memoize lookups."
  (cond
   ((string-match "\\`\\(custom_field\\|incident_role\\)\\[\\([^]]+\\)\\]\\[one_of\\]=" input)
    (let ((prefix (match-string 0 input))
          (id (match-string 2 input)))
      (mapcar (lambda (pair) (cons (concat prefix (car pair)) (cdr pair)))
              (if (equal (match-string 1 input) "incident_role")
                  (funcall fetch 'users (lambda () (incio--filter-pairs (incio-users))))
                (funcall fetch (intern id)
                         (lambda ()
                           (let ((field (seq-find (lambda (f) (equal id (incio--alist-get 'id f)))
                                                  (funcall fetch 'fields #'incio-custom-fields))))
                             (and field (incio--filter-value-ids (incio-custom-field-choices field))))))))))
   ((string-match "\\`\\(custom_field\\|incident_role\\)\\[" input)
    (let ((kind (match-string 1 input)))
      (mapcar (lambda (item)
                (cons (format "%s[%s][one_of]=" kind (incio--alist-get 'id item))
                      (incio--alist-get 'name item)))
              (if (equal kind "incident_role")
                  (funcall fetch 'roles #'incio-roles)
                (funcall fetch 'fields #'incio-custom-fields)))))
   ((string-match "\\`[^=]+=" input)
    (let ((prefix (match-string 0 input)))
      (mapcar (lambda (pair) (cons (concat prefix (car pair)) (cdr pair)))
              (pcase prefix
                ("status_category[one_of]="
                 (mapcar #'list '("triage" "declined" "merged" "canceled"
                                  "live" "learning" "closed" "paused")))
                ("mode[one_of]="
                 (mapcar #'list '("standard" "retrospective" "test" "tutorial")))
                ("status[one_of]="
                 (funcall fetch 'statuses (lambda () (incio--filter-pairs (incio-incident-statuses)))))
                ((or "severity[one_of]=" "severity[gte]=")
                 (funcall fetch 'severities (lambda () (incio--filter-pairs (incio-severities)))))
                ("incident_type[one_of]="
                 (funcall fetch 'types (lambda () (incio--filter-pairs (incio-incident-types)))))))))
   (t (mapcar #'list incio-incident-filter-keys))))

(defun incio--read-filters (current)
  (let* ((cache nil)
         (fetch (lambda (key thunk)
                  (let ((hit (assq key cache)))
                    (if hit (cdr hit)
                      (let ((value (funcall thunk)))
                        (push (cons key value) cache)
                        value)))))
         (annotations (make-hash-table :test #'equal))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata (annotation-function
                                  . ,(lambda (c)
                                       (let ((a (gethash c annotations)))
                                         (and a (concat "  " a))))))
                    (let ((cands (incio--filter-candidates string fetch)))
                      (dolist (c cands) (puthash (car c) (cdr c) annotations))
                      (complete-with-action action (mapcar #'car cands) string pred))))))
    (completing-read-multiple "Filters (key=value, comma separated): "
                              table nil nil (string-join current ","))))

(defun incio-incident-filter (filters)
  "Show incidents matching API FILTERS."
  (interactive (list (incio--read-filters incio--filters)))
  (setq incio--filters filters incio--all nil)
  (incio-incident-refresh))

(defun incio-incident-filter-clear ()
  (interactive)
  (incio-incident-filter nil))

(defun incio-incident-list-all ()
  "Show all incidents, without any filter."
  (interactive)
  (setq incio--filters nil incio--all t)
  (incio-incident-refresh))

(defun incio-incident-refresh ()
  (interactive)
  (setq mode-name (cond (incio--all "Incio-Incidents[all]")
                        (incio--filters
                         (format "Incio-Incidents[%s]" (string-join incio--filters ",")))
                        (t "Incio-Incidents")))
  (setq incio--incidents (if (or incio--all incio--filters)
                             (incio-incident-fetch-filtered incio--filters)
                           (incio-incident-fetch-list)))
  (incio--render-incidents incio--incidents)
  (message "Loaded %d incident(s)" (length incio--incidents)))

(defun incio-refresh ()
  (interactive)
  (if (derived-mode-p 'incio-incident-mode)
      (progn
        (setq incio--incident (incio-incident-show (incio-incident-id incio--incident)))
        (incio--render-incident incio--incident))
    (incio-incident-refresh)))

(defun incio-incident-visit ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (let ((buffer (get-buffer-create
                   (format "*incio-incident: %s*"
                           (incio-incident-reference incident)))))
      (with-current-buffer buffer
        (incio-incident-mode)
        (setq incio--incident incident)
        (incio--render-incident incident)
        (pop-to-buffer buffer)))))

(defun incio-incident-browse ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (browse-url (incio-incident-permalink incident))))

(defun incio-incident-set-status-at-point ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (let* ((statuses (incio-incident-statuses))
           (name (completing-read "Status: "
                                  (mapcar (lambda (s) (incio--alist-get 'name s)) statuses)
                                  nil t nil nil (incio-incident-status-name incident)))
           (status (seq-find (lambda (s) (equal name (incio--alist-get 'name s))) statuses)))
      (when (yes-or-no-p (format "Change status to %s? " name))
        (incio-incident-set-status incident (incio--alist-get 'id status))
        (incio-refresh)))))

(defun incio-incident-assign-at-point ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (let* ((roles (incio-roles))
           (role-name (completing-read "Role: "
                                       (mapcar (lambda (r) (incio--alist-get 'name r)) roles)
                                       nil t))
           (role (seq-find (lambda (r) (equal role-name (incio--alist-get 'name r))) roles))
           (users (incio-users))
           (user-names (mapcar (lambda (u) (format "%s <%s>"
                                                   (incio--alist-get 'name u)
                                                   (incio--alist-get 'email u)))
                               users))
           (user-name (completing-read "Assignee: " user-names nil t))
           (user (nth (seq-position user-names user-name) users)))
      (when (yes-or-no-p (format "Assign %s to %s? " role-name user-name))
        (incio-incident-assign-role incident (incio--alist-get 'id role) (incio--alist-get 'id user))
        (incio-refresh)))))

(defun incio--edit-at-point (incident description fields)
  (when (yes-or-no-p (format "Set %s on %s? " description
                             (incio-incident-reference incident)))
    (incio-incident--edit incident fields)
    (incio-refresh)))

(defun incio-incident-set-title-at-point ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (let ((name (read-string "Title: " (incio-incident-name incident))))
      (incio--edit-at-point incident "title" `((name . ,name))))))

(defun incio-incident-set-summary-at-point ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (let ((summary (read-string "Summary: " (incio-incident-summary incident))))
      (incio--edit-at-point incident "summary" `((summary . ,summary))))))

(defun incio-incident-set-severity-at-point ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (let* ((severities (incio-severities))
           (name (completing-read "Severity: "
                                  (mapcar (lambda (s) (incio--alist-get 'name s)) severities)
                                  nil t nil nil (incio-incident-severity-name incident)))
           (severity (seq-find (lambda (s) (equal name (incio--alist-get 'name s))) severities)))
      (incio--edit-at-point incident (format "severity %s" name)
                            `((severity_id . ,(incio--alist-get 'id severity)))))))

(defun incio-incident-set-timestamp-at-point ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (let* ((timestamps (incio-incident-timestamps))
           (name (completing-read "Timestamp: "
                                  (mapcar (lambda (s) (incio--alist-get 'name s)) timestamps)
                                  nil t))
           (timestamp (seq-find (lambda (s) (equal name (incio--alist-get 'name s))) timestamps))
           (value (read-string (format "%s (ISO 8601): " name)
                               (or (incio-incident-timestamp incident name)
                                   (format-time-string "%FT%T%:z")))))
      (date-to-time value)
      (when (yes-or-no-p (format "Set %s to %s? " name value))
        (incio-incident-set-timestamp incident (incio--alist-get 'id timestamp) value)
        (incio-refresh)))))

(defun incio--select-multiple (name names selected)
  "Toggle NAMES one pick at a time, starting from SELECTED, until done."
  (setq selected (seq-intersection selected names))
  (let (pick)
    (while (not (equal (setq pick
                             (completing-read
                              (format "%s [%s]: " name (string-join selected ", "))
                              (cons "[done]"
                                    (mapcar (lambda (n) (if (member n selected) (concat "* " n) n))
                                            names))
                              nil t))
                       "[done]"))
      (let ((n (string-remove-prefix "* " pick)))
        (setq selected (if (member n selected) (delete n selected) (append selected (list n))))))
    selected))

(defun incio-incident-set-custom-field-at-point ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (let* ((fields (incio-custom-fields))
           (name (completing-read "Custom field: "
                                  (mapcar (lambda (f) (incio--alist-get 'name f)) fields)
                                  nil t))
           (field (seq-find (lambda (f) (equal name (incio--alist-get 'name f))) fields))
           (current (cdr (assoc name (incio-incident-custom-fields incident))))
           (choices (incio-custom-field-choices field))
           (values
            (cond
             ((equal "multi_select" (incio--alist-get 'field_type field))
              (mapcar (lambda (choice) (cdr (assoc choice choices)))
                      (incio--select-multiple name (mapcar #'car choices)
                                              (and current (split-string current ", " t)))))
             (choices
              (list (cdr (assoc (completing-read (format "%s: " name) choices nil t nil nil current)
                                choices))))
             (t (list (incio-custom-field-free-value
                       field (read-string (format "%s: " name) current)))))))
      (incio-incident-set-custom-field incident (incio--alist-get 'id field) (delq nil values))
      (incio-refresh))))

(defun incio-incident-ack-at-point ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (when (yes-or-no-p (format "Acknowledge %s? " (incio-incident-reference incident)))
      (incio-incident-ack incident)
      (incio-refresh))))

(defun incio-incident-reject-at-point ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (when (yes-or-no-p (format "Reject %s? " (incio-incident-reference incident)))
      (incio-incident-reject incident)
      (incio-refresh))))

(defun incio--incidents-in-region (beg end)
  (save-excursion
    (goto-char beg)
    (let (incidents)
      (while (and (< (point) end) (not (eobp)))
        (when-let ((id (tabulated-list-get-id))) (push id incidents))
        (forward-line 1))
      (nreverse incidents))))

(defun incio-incident-merge-region (beg end)
  (let* ((incidents (incio--incidents-in-region beg end))
         (target (car incidents))
         (sources (cdr incidents)))
    (unless sources (user-error "Select at least two incidents"))
    (when (yes-or-no-p (format "Merge %s into %s? "
                               (mapconcat #'incio-incident-reference sources ", ")
                               (incio-incident-reference target)))
      (dolist (i sources) (incio-incident-merge i (incio-incident-id target)))
      (deactivate-mark)
      (incio-refresh))))

(defun incio-incident-merge-at-point ()
  (interactive)
  (if (and (use-region-p) (derived-mode-p 'tabulated-list-mode))
      (incio-incident-merge-region (region-beginning) (region-end))
    (incio-incident-merge-single)))

(defun incio-incident-merge-single ()
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (let* ((candidates (seq-remove (lambda (i) (eq i incident)) incio--incidents))
           (reference (completing-read "Merge into: "
                                       (mapcar #'incio-incident-reference candidates)))
           (target (seq-find (lambda (i) (equal reference (incio-incident-reference i))) candidates))
           (target-id (if target (incio-incident-id target)
                        (incio-incident-id (incio-incident-show reference)))))
      (when (yes-or-no-p (format "Merge %s into %s? "
                                 (incio-incident-reference incident) reference))
        (incio-incident-merge incident target-id)
        (incio-refresh)))))

(defun incio-incident-close-at-point ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (when (yes-or-no-p (format "Close %s? " (incio-incident-reference incident)))
      (when (string-empty-p (or (incio-incident-summary incident) ""))
        (let ((summary (read-string "Summary (required to close): ")))
          (when (string-empty-p summary) (user-error "Summary required"))
          (incio-incident--edit incident `((summary . ,summary)))))
      (incio-incident-close incident)
      (incio-refresh))))

(defun incio-incident-post-update-at-point ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (incio-incident-post-update incident (read-string "Update: "))
    (message "Update posted")))

(defun incio-incident-follow-up-at-point ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (incio-incident-add-follow-up incident (read-string "Follow-up: "))
    (message "Follow-up added")))

;;;###autoload
(defun incio-incident-list (&optional filters)
  "Display active incidents, or those matching FILTERS with a prefix arg."
  (interactive (list (when current-prefix-arg (incio--read-filters nil))))
  (let ((buffer (get-buffer-create "*incio-incidents*")))
    (with-current-buffer buffer
      (incio-incident-list-mode)
      (setq incio--filters filters)
      (incio-incident-refresh))
    (pop-to-buffer buffer)))

(provide 'incio-ui)

;;; incio-ui.el ends here
