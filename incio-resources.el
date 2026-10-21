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
(defvar-local incio--schedule-calendar nil)

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
    (define-key map (kbd "c") #'incio-schedule-toggle-calendar)
    (define-key map (kbd "o") #'incio-schedule-override-at-point)
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

(defun incio--schedule-rotations ()
  "Alist of (ID . ROTATION), keeping the last config version of each."
  (let (result)
    (dolist (r (alist-get 'rotations (alist-get 'config incio--schedule)))
      (setf (alist-get (alist-get 'id r) result nil nil #'equal) r))
    (nreverse result)))

(defun incio--schedule-key (entry)
  (list (alist-get 'start_at entry) (alist-get 'end_at entry)
        (alist-get 'id (alist-get 'user entry))))

(defun incio--schedule-entry-face (entry overrides now)
  (let ((start (date-to-time (alist-get 'start_at entry)))
        (end (date-to-time (alist-get 'end_at entry))))
    (delq nil (list (when (member (incio--schedule-key entry) overrides)
                      'incio-schedule-override)
                    (when (and (time-less-p start now) (time-less-p now end))
                      'bold)))))

(defun incio--schedule-entry-row (entry rotations overrides now)
  (let* ((face (incio--schedule-entry-face entry overrides now))
         (cell (lambda (s) (if face (propertize s 'face face) s))))
    (list entry
          (vector (funcall cell (format-time-string
                                 "%a %F %R" (date-to-time (alist-get 'start_at entry))))
                  (funcall cell (format-time-string
                                 "%a %F %R" (date-to-time (alist-get 'end_at entry))))
                  (funcall cell (or (alist-get 'name (alist-get 'user entry)) ""))
                  (or (alist-get 'name (cdr (assoc (alist-get 'rotation_id entry)
                                                   rotations)))
                      "")))))

(defun incio--schedule-day (time offset)
  "Local midnight OFFSET days after TIME's day."
  (let ((d (decode-time time)))
    (encode-time 0 0 0 (+ offset (nth 3 d)) (nth 4 d) (nth 5 d))))

(defun incio--schedule-calendar-cell (entries day end overrides now)
  (let ((shifts (seq-filter
                 (lambda (e)
                   (and (time-less-p (date-to-time (alist-get 'start_at e)) end)
                        (time-less-p day (date-to-time (alist-get 'end_at e)))))
                 entries)))
    (mapconcat
     (lambda (e)
       (let* ((face (incio--schedule-entry-face e overrides now))
              (name (or (alist-get 'name (alist-get 'user e)) ""))
              (start (date-to-time (alist-get 'start_at e)))
              (text (if (cdr shifts)
                        (concat (format-time-string
                                 "%R " (if (time-less-p start day) day start))
                                name)
                      name)))
         (if face (propertize text 'face face) text)))
     shifts ", ")))

(defun incio--render-schedule-calendar (entries rotations overrides now)
  (let* ((rows
          (mapcar
           (lambda (i)
             (let* ((day (incio--schedule-day incio--schedule-from i))
                    (end (incio--schedule-day incio--schedule-from (1+ i)))
                    (label (format-time-string "%a %F" day)))
               (list `((day . ,day))
                     (apply #'vector
                            (if (and (time-less-p day now) (time-less-p now end))
                                (propertize label 'face 'bold)
                              label)
                            (mapcar (lambda (r)
                                      (incio--schedule-calendar-cell
                                       (seq-filter (lambda (e) (equal (car r) (alist-get 'rotation_id e)))
                                                   entries)
                                       day end overrides now))
                                    rotations)))))
           (number-sequence 0 (1- incio-schedule-window-days))))
         (columns
          (seq-map-indexed
           (lambda (r i)
             (let ((name (or (alist-get 'name (cdr r)) "")))
               (list name
                     (if (= i (1- (length rotations))) 0
                       (min 40 (apply #'max (length name)
                                      (mapcar (lambda (row) (length (aref (cadr row) (1+ i))))
                                              rows))))
                     nil)))
           rotations)))
    (setq tabulated-list-format (apply #'vector '("Day" 15 t) columns))
    (tabulated-list-init-header)
    (setq tabulated-list-entries rows)
    (tabulated-list-print t)))

(defun incio--render-schedule (data)
  (let ((entries (seq-sort-by (lambda (e) (alist-get 'start_at e)) #'string<
                              (alist-get 'final data)))
        (rotations (incio--schedule-rotations))
        (overrides (mapcar #'incio--schedule-key (alist-get 'overrides data)))
        (now (current-time)))
    (if incio--schedule-calendar
        (incio--render-schedule-calendar entries rotations overrides now)
      (setq tabulated-list-format
            [("Start" 22 t) ("End" 22 t) ("User" 28 t) ("Rotation" 0 t)])
      (tabulated-list-init-header)
      (setq tabulated-list-entries
            (mapcar (lambda (e) (incio--schedule-entry-row e rotations overrides now))
                    entries))
      (tabulated-list-print t))))

(defun incio-schedule-refresh ()
  "Reload the current schedule view."
  (interactive)
  (if (derived-mode-p 'incio-schedule-mode)
      (let ((window (incio--schedule-window
                     (if incio--schedule-calendar
                         (incio--schedule-day incio--schedule-from 0)
                       incio--schedule-from))))
        (incio--render-schedule
         (apply #'incio-schedule-fetch-entries
                (alist-get 'id incio--schedule) window))
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

(defun incio-schedule-toggle-calendar ()
  "Switch between the shift list and a per-day calendar."
  (interactive)
  (setq incio--schedule-calendar (not incio--schedule-calendar))
  (incio-schedule-refresh))

(defun incio--schedule-pick (prompt items label)
  "Pick one of ITEMS, shown with LABEL, prompting only when needed."
  (if (cdr items)
      (let ((choices (mapcar (lambda (i) (cons (funcall label i) i)) items)))
        (cdr (assoc (completing-read prompt choices nil t) choices)))
    (car items)))

(defun incio--schedule-read-time (prompt default)
  (let ((value (read-string prompt default)))
    (date-to-time value)
    value))

(defun incio-schedule-override-at-point ()
  "Override the shift or day at point with another user."
  (interactive)
  (unless (derived-mode-p 'incio-schedule-mode)
    (user-error "Not in a schedule view"))
  (let* ((id (or (tabulated-list-get-id) (user-error "No shift on this line")))
         (day (alist-get 'day id))
         (rotations (mapcar #'cdr (incio--schedule-rotations)))
         (rotation (if day
                       (incio--schedule-pick "Rotation: " rotations
                                             (lambda (r) (alist-get 'name r)))
                     (seq-find (lambda (r) (equal (alist-get 'id r)
                                                  (alist-get 'rotation_id id)))
                               rotations)))
         (_ (unless rotation (user-error "Unknown rotation")))
         (layer (or (incio--schedule-pick
                     "Layer: " (alist-get 'layers rotation)
                     (lambda (l) (let ((name (alist-get 'name l)))
                                   (if (member name '(nil "")) (alist-get 'id l) name))))
                    (user-error "Rotation has no layer")))
         (user (incio--schedule-pick
                "User: "
                (seq-uniq (alist-get 'users rotation)
                          (lambda (a b) (equal (alist-get 'id a) (alist-get 'id b))))
                (lambda (u) (format "%s <%s>" (alist-get 'name u) (alist-get 'email u)))))
         (start (incio--schedule-read-time
                 "Start (ISO 8601): "
                 (or (alist-get 'start_at id) (incio--schedule-iso day))))
         (end (incio--schedule-read-time
               "End (ISO 8601): "
               (or (alist-get 'end_at id)
                   (incio--schedule-iso (incio--schedule-day day 1))))))
    (when (yes-or-no-p (format "Override %s with %s from %s to %s? "
                               (alist-get 'name rotation) (alist-get 'name user)
                               start end))
      (incio--run "schedules" "override"
                  "--schedule-id" (alist-get 'id incio--schedule)
                  "--rotation-id" (alist-get 'id rotation)
                  "--layer-id" (alist-get 'id layer)
                  "--user" (alist-get 'id user)
                  "--start" start "--end" end)
      (incio-schedule-refresh))))

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
