;;; incio-ui.el --- Interactive UI for incident.io -*- lexical-binding: t; -*-

;;; Code:

(require 'browse-url)
(require 'incio-incident)

(defvar-local incio--incidents nil)
(defvar incio-incident-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'incio-incident-visit)
    (define-key map (kbd "g") #'incio-incident-refresh)
    (define-key map (kbd "r") #'incio-incident-refresh)
    (define-key map (kbd "s") #'incio-incident-set-status-at-point)
    (define-key map (kbd "c") #'incio-incident-close-at-point)
    (define-key map (kbd "u") #'incio-incident-post-update-at-point)
    (define-key map (kbd "F") #'incio-incident-follow-up-at-point)
    (define-key map (kbd "w") #'incio-incident-browse)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode incio-incident-list-mode special-mode "Incio-Incidents"
  "Major mode for incident.io incidents."
  (setq-local truncate-lines t)
  (hl-line-mode 1))

(defun incio--incident-at-point ()
  (get-text-property (line-beginning-position) 'incio-incident))

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

(defun incio-incident-visit ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (let ((buffer (get-buffer-create
                   (format "*incio-incident: %s*"
                           (incio-incident-reference incident)))))
      (with-current-buffer buffer
        (special-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "%s  %s\n\n%s\n\n%s\n"
                          (incio-incident-reference incident)
                          (incio-incident-status-name incident)
                          (incio-incident-name incident)
                          (or (incio-incident-summary incident) ""))))
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
        (incio-incident-refresh)))))

(defun incio-incident-close-at-point ()
  (interactive)
  (let ((incident (incio--incident-at-point)))
    (unless incident (user-error "No incident on this line"))
    (when (yes-or-no-p (format "Close %s? " (incio-incident-reference incident)))
      (incio-incident-close incident)
      (incio-incident-refresh))))

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
