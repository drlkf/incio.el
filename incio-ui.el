;;; incio-ui.el --- Interactive UI for incident.io -*- lexical-binding: t; -*-

;;; Code:

(require 'browse-url)
(require 'goto-addr)
(require 'seq)
(require 'tabulated-list)
(require 'incio-incident)

(defvar-local incio--incidents nil)
(defvar-local incio--incident nil)
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
    (define-key map (kbd "w") #'incio-incident-browse)
    (define-key map (kbd "?") #'incio-help)
    (define-key map (kbd "q") #'quit-window)
    map))

(defvar incio-incident-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map incio-incident-list-mode-map)
    (define-key map (kbd "RET") nil)
    map))

(define-derived-mode incio-incident-list-mode tabulated-list-mode "Incio-Incidents"
  "Major mode for incident.io incidents."
  (setq-local tabulated-list-format
              [ ("Reference" 12 t)
                ("Status" 14 t)
                ("Severity" 10 t)
                ("Name" 0 t) ])
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
  (let ((inhibit-read-only t))
    (setq tabulated-list-entries
          (mapcar (lambda (incident)
                    (list incident
                          (vector (propertize (or (incio-incident-reference incident) "") 'face 'incio-reference)
                                  (propertize (or (incio-incident-status-name incident) "")
                                              'face (incio-status-face (incio-incident-status-category incident)))
                                  (propertize (or (incio-incident-severity-name incident) "")
                                              'face (incio-severity-face (incio-incident-severity-name incident)))
                                  (or (incio-incident-name incident) ""))))
                  incidents))
    (tabulated-list-print t)))

(defun incio-incident-refresh ()
  (interactive)
  (setq incio--incidents (incio-incident-fetch-list))
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

(defun incio-incident-merge-at-point ()
  (interactive)
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
(defun incio-incident-list ()
  "Display active incidents."
  (interactive)
  (let ((buffer (get-buffer-create "*incio-incidents*")))
    (with-current-buffer buffer
      (incio-incident-list-mode)
      (incio-incident-refresh))
    (pop-to-buffer buffer)))

(provide 'incio-ui)

;;; incio-ui.el ends here
