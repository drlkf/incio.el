;;; incio-ui.el --- Interactive UI for incident.io -*- lexical-binding: t; -*-

;;; Code:

(require 'browse-url)
(require 'goto-addr)
(require 'seq)
(require 'incio-incident)

(defvar-local incio--incidents nil)
(defvar-local incio--incident nil)
(defvar incio-incident-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'incio-incident-visit)
    (define-key map (kbd "g") #'incio-refresh)
    (define-key map (kbd "r") #'incio-refresh)
    (define-key map (kbd "s") #'incio-incident-set-status-at-point)
    (define-key map (kbd "c") #'incio-incident-close-at-point)
    (define-key map (kbd "u") #'incio-incident-post-update-at-point)
    (define-key map (kbd "F") #'incio-incident-follow-up-at-point)
    (define-key map (kbd "w") #'incio-incident-browse)
    (define-key map (kbd "q") #'quit-window)
    map))

(defvar incio-incident-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map incio-incident-list-mode-map)
    (define-key map (kbd "RET") nil)
    map))

(define-derived-mode incio-incident-list-mode special-mode "Incio-Incidents"
  "Major mode for incident.io incidents."
  (setq-local truncate-lines t)
  (hl-line-mode 1))

(define-derived-mode incio-incident-mode special-mode "Incio-Incident"
  "Major mode for an incident.io incident."
  (use-local-map incio-incident-mode-map)
  (goto-address-mode))

(defun incio--incident-at-point ()
  (or (get-text-property (line-beginning-position) 'incio-incident)
      incio--incident))

(defun incio--incident-time (value)
  (when value (format-time-string "%F %R" (date-to-time value))))

(defun incio--insert-detail (label value)
  (when value
    (insert (propertize (format "%-18s" (concat label ":"))
                        'face 'font-lock-comment-face)
            (format "%s\n" value))))

(defun incio--render-incident (incident)
  (let ((inhibit-read-only t)
        (raw (incio-incident-raw incident)))
    (erase-buffer)
    (insert (propertize (format "%s  %s  %s\n\n"
                                (incio-incident-reference incident)
                                (or (incio-incident-status-name incident) "")
                                (or (incio-incident-severity-name incident) ""))
                        'face 'bold)
            (format "%s\n\n" (or (incio-incident-name incident) "")))
    (incio--insert-detail "Declared"
                          (incio--incident-time
                           (incio--alist-get
                            'value
                            (seq-find (lambda (entry)
                                        (equal "Declared at"
                                               (incio--alist-get 'name
                                                                 (incio--alist-get 'incident_timestamp entry))))
                                      (incio--alist-get 'incident_timestamp_values raw)))))
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
    (erase-buffer)
    (dolist (incident incidents)
      (insert (propertize
               (format "%-9s %-12s %-10s %s\n"
                       (or (incio-incident-reference incident) "")
                       (or (incio-incident-status-name incident) "")
                       (or (incio--alist-get 'name
                                             (incio-incident-severity incident))
                           "")
                       (or (incio-incident-name incident) ""))
               'incio-incident incident)))
    (goto-char (point-min))))

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
    (let ((status (read-string "Status ID: "
                               (incio-incident-status-id incident))))
      (when (yes-or-no-p "Change incident status? ")
        (incio-incident-set-status incident status)
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
