;;; incio-resources.el --- Alerts, escalations, and schedules -*- lexical-binding: t; -*-

;;; Code:

(require 'incio-core)

(defvar incio-resource-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "?") #'incio-help)
    map))

(define-derived-mode incio-resource-mode special-mode "Incio-Resource"
  "Major mode for incident.io resource lists.")

(incio--evilify 'incio-resource-mode incio-resource-mode-map)

(defun incio-alert-fetch-list (&optional status)
  "Return alerts, optionally filtered by STATUS."
  (apply #'incio--run-json
         (append '("alerts" "list")
                 (when status (list "--status" status)))))

(defun incio-escalation-fetch-list ()
  "Return live escalations."
  (incio--run-json "escalations" "list"))

(defun incio-schedule-fetch-list ()
  "Return schedules."
  (incio--run-json "schedules" "list"))

(defun incio-schedule-fetch-entries (schedule-id &optional from until)
  "Return entries for SCHEDULE-ID between FROM and UNTIL."
  (apply #'incio--run-json
         (append (list "schedules" "entries" schedule-id)
                 (when from (list "--from" from))
                 (when until (list "--until" until)))))

(defun incio-resource-list (kind)
  "Display a simple list for resource KIND."
  (let* ((data (pcase kind
                 ('alerts (incio-alert-fetch-list))
                 ('escalations (incio-escalation-fetch-list))
                 ('schedules (incio-schedule-fetch-list))))
         (buffer (get-buffer-create (format "*incio-%s*" kind))))
    (with-current-buffer buffer
      (incio-resource-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (object data)
          (insert (format "%s\n"
                          (or (alist-get 'name object)
                              (alist-get 'title object)
                              (alist-get 'id object) object))))))
    (pop-to-buffer buffer)))

;;;###autoload
(defun incio-alert-list ()
  (interactive)
  (incio-resource-list 'alerts))

;;;###autoload
(defun incio-escalation-list ()
  (interactive)
  (incio-resource-list 'escalations))

;;;###autoload
(defun incio-schedule-list ()
  (interactive)
  (incio-resource-list 'schedules))

(provide 'incio-resources)

;;; incio-resources.el ends here
