;;; incio-resources.el --- Alerts, escalations, and schedules -*- lexical-binding: t; -*-

;;; Code:

(require 'incio-core)
(require 'tabulated-list)

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
                 ('escalations (incio-escalation-fetch-list))))
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

(defcustom incio-schedule-window-days 7
  "Number of days shown at once in a schedule view."
  :type 'integer :group 'incio)

(defvar-local incio--schedule nil)
(defvar-local incio--schedule-from nil)

(defvar incio-schedule-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'incio-schedule-visit)
    (define-key map (kbd "g") #'incio-schedule-refresh)
    (define-key map (kbd "r") #'incio-schedule-refresh)
    (define-key map (kbd "?") #'incio-help)
    (define-key map (kbd "q") #'quit-window)
    map))

(defvar incio-schedule-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map incio-schedule-list-mode-map)
    (define-key map (kbd "RET") nil)
    (define-key map (kbd "]") #'incio-schedule-next)
    (define-key map (kbd "[") #'incio-schedule-previous)
    (define-key map (kbd ".") #'incio-schedule-now)
    map))

(define-derived-mode incio-schedule-list-mode tabulated-list-mode "Incio-Schedules"
  "Major mode for incident.io schedules."
  (setq-local tabulated-list-format [("Name" 40 t) ("Timezone" 0 t)])
  (setq-local tabulated-list-padding 1)
  (tabulated-list-init-header)
  (hl-line-mode 1))

(define-derived-mode incio-schedule-mode tabulated-list-mode "Incio-Schedule"
  "Major mode for the entries of an incident.io schedule."
  (setq-local tabulated-list-format
              [("Start" 22 t) ("End" 22 t) ("User" 28 t) ("Rotation" 0 t)])
  (setq-local tabulated-list-padding 1)
  (setq-local tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(incio--evilify 'incio-schedule-list-mode incio-schedule-list-mode-map)
(incio--evilify 'incio-schedule-mode incio-schedule-mode-map)

(defun incio--schedule-iso (time)
  (format-time-string "%FT%TZ" time t))

(defun incio--schedule-window (from)
  "Return (FROM-ISO UNTIL-ISO) for a window starting at FROM."
  (list (incio--schedule-iso from)
        (incio--schedule-iso
         (time-add from (days-to-time incio-schedule-window-days)))))

(defun incio--schedule-entry-row (entry rotations now)
  (let* ((start (date-to-time (alist-get 'start_at entry)))
         (end (date-to-time (alist-get 'end_at entry)))
         (current (and (time-less-p start now) (time-less-p now end)))
         (cell (lambda (s) (if current (propertize s 'face 'bold) s))))
    (list entry
          (vector (funcall cell (format-time-string "%a %F %R" start))
                  (funcall cell (format-time-string "%a %F %R" end))
                  (funcall cell (or (alist-get 'name (alist-get 'user entry)) ""))
                  (or (cdr (assoc (alist-get 'rotation_id entry) rotations)) "")))))

(defun incio--render-schedule (entries)
  (let ((rotations (mapcar (lambda (r) (cons (alist-get 'id r) (alist-get 'name r)))
                           (alist-get 'rotations (alist-get 'config incio--schedule))))
        (now (current-time)))
    (setq tabulated-list-entries
          (mapcar (lambda (e) (incio--schedule-entry-row e rotations now))
                  (seq-sort-by (lambda (e) (alist-get 'start_at e)) #'string<
                               entries)))
    (tabulated-list-print t)))

(defun incio-schedule-refresh ()
  "Reload the current schedule view."
  (interactive)
  (if (derived-mode-p 'incio-schedule-mode)
      (let ((window (incio--schedule-window incio--schedule-from)))
        (incio--render-schedule
         (alist-get 'final (apply #'incio-schedule-fetch-entries
                                  (alist-get 'id incio--schedule) window)))
        (message "%s: %s → %s" (alist-get 'name incio--schedule)
                 (substring (car window) 0 10) (substring (cadr window) 0 10)))
    (setq tabulated-list-entries
          (mapcar (lambda (s)
                    (list s (vector (or (alist-get 'name s) "")
                                    (or (alist-get 'timezone s) ""))))
                  (incio-schedule-fetch-list)))
    (tabulated-list-print t)))

(defun incio-schedule--shift (days)
  (setq incio--schedule-from (time-add incio--schedule-from (days-to-time days)))
  (incio-schedule-refresh))

(defun incio-schedule-next ()
  "Show the next window of schedule entries."
  (interactive)
  (incio-schedule--shift incio-schedule-window-days))

(defun incio-schedule-previous ()
  "Show the previous window of schedule entries."
  (interactive)
  (incio-schedule--shift (- incio-schedule-window-days)))

(defun incio-schedule-now ()
  "Show schedule entries starting now."
  (interactive)
  (setq incio--schedule-from (current-time))
  (incio-schedule-refresh))

(defun incio-schedule-visit ()
  "Open the schedule at point."
  (interactive)
  (let ((schedule (tabulated-list-get-id)))
    (unless schedule (user-error "No schedule on this line"))
    (let ((buffer (get-buffer-create
                   (format "*incio-schedule: %s*" (alist-get 'name schedule)))))
      (with-current-buffer buffer
        (incio-schedule-mode)
        (setq incio--schedule schedule)
        (incio-schedule-now))
      (pop-to-buffer buffer))))

;;;###autoload
(defun incio-schedule-list ()
  "Display on-call schedules."
  (interactive)
  (let ((buffer (get-buffer-create "*incio-schedules*")))
    (with-current-buffer buffer
      (incio-schedule-list-mode)
      (incio-schedule-refresh))
    (pop-to-buffer buffer)))

(provide 'incio-resources)

;;; incio-resources.el ends here
