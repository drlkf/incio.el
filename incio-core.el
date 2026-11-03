;;; incio-core.el --- Core support for incident.io -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1"))

;;; Code:

(require 'json)
(require 'seq)

(defgroup incio nil
  "Interact with incident.io from Emacs."
  :group 'tools
  :prefix "incio-")

(defface incio-reference '((t (:inherit font-lock-constant-face))) "Face for incident references." :group 'incio)
(defface incio-status-default '((t (:inherit default))) "Fallback status face." :group 'incio)
(defface incio-status-triage '((t (:inherit warning))) "Face for triage status." :group 'incio)
(defface incio-status-live '((t (:inherit error))) "Face for live status." :group 'incio)
(defface incio-status-paused '((t (:inherit shadow))) "Face for paused status." :group 'incio)
(defface incio-status-learning '((t (:inherit font-lock-keyword-face))) "Face for learning status." :group 'incio)
(defface incio-status-closed '((t (:inherit success))) "Face for closed status." :group 'incio)
(defface incio-status-declined '((t (:inherit shadow))) "Face for declined status." :group 'incio)
(defface incio-status-merged '((t (:inherit shadow))) "Face for merged status." :group 'incio)
(defface incio-severity-critical '((t (:inherit error))) "Face for critical severity." :group 'incio)
(defface incio-severity-major '((t (:inherit warning))) "Face for major severity." :group 'incio)
(defface incio-severity-minor '((t (:inherit font-lock-doc-face))) "Face for minor severity." :group 'incio)
(defface incio-schedule-override '((t (:inherit font-lock-warning-face))) "Face for overridden shifts." :group 'incio)
(defface incio-severity-default '((t (:inherit default))) "Fallback severity face." :group 'incio)
(defcustom incio-severity-faces
  '(("critical" . incio-severity-critical) ("major" . incio-severity-major)
    ("minor" . incio-severity-minor))
  "Map lowercased severity names to faces."
  :type '(alist :key-type string :value-type face) :group 'incio)
(defun incio-status-face (category)
  (let* ((name (downcase (or category "")))
         (face (intern (format "incio-status-%s" name))))
    (if (facep face) face 'incio-status-default)))
(defun incio-severity-face (name)
  (or (cdr (assoc (downcase (or name "")) incio-severity-faces)) 'incio-severity-default))

(defcustom incio-inc-executable "inc"
  "Path to the `inc' executable."
  :type 'string :group 'incio)

(define-error 'incio-error "incident.io error")
(define-error 'incio-not-authenticated "Not authenticated with incident.io"
              'incio-error)

(defun incio--run (&rest args)
  "Run `inc' with ARGS and return stdout."
  (with-temp-buffer
    (let ((status (apply #'call-process incio-inc-executable nil t nil args)))
      (unless (eq status 0)
        (signal 'incio-error
                (list (format "inc exited with status %s: %s"
                              status (string-trim (buffer-string))))))
      (buffer-string))))

(defun incio--run-json (&rest args)
  "Run `inc' with ARGS and parse its JSON output."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false nil)
        (json-null nil))
    (condition-case err
        (json-read-from-string
         (apply #'incio--run (append args '("--output" "json"))))
      (error (signal 'incio-error (list (error-message-string err)))))))

(defun incio--api (method path &optional fields)
  "Call METHOD and PATH through `inc api'.  FIELDS is an alist body."
  (let ((args (list "api" method path)))
    (if fields
        (progn
          (setq args (append args (list "--input" "-")))
          (let ((process-connection-type nil)
                (coding-system-for-write 'utf-8))
            (with-temp-buffer
              (insert (json-encode fields))
              (let ((status (apply #'call-process-region (point-min) (point-max)
                                   incio-inc-executable t t nil args)))
                (unless (eq status 0)
                  (signal 'incio-error (list (buffer-string))))
                (goto-char (point-min))
                (let ((json-object-type 'alist) (json-array-type 'list)
                      (json-key-type 'symbol) (json-false nil) (json-null nil))
                  (json-read))))))
      (apply #'incio--run-json args))))

(defun incio--alist-get (key object &optional default)
  "Get KEY from OBJECT, returning DEFAULT when absent."
  (let ((value (alist-get key object)))
    (if value value default)))

(defvar incio-help-sections
  '(("Navigation" incio-incident-visit incio-refresh incio-incident-browse
     incio-incident-filter incio-incident-filter-clear incio-incident-list-all incio-help quit-window)
    ("Incident actions" incio-incident-set-status-at-point
     incio-incident-assign-at-point incio-incident-ack-at-point
     incio-incident-reject-at-point incio-incident-merge-at-point
     incio-incident-close-at-point incio-incident-post-update-at-point
     incio-incident-follow-up-at-point)
    ("Incident attributes" incio-incident-set-title-at-point
     incio-incident-set-summary-at-point incio-incident-set-severity-at-point
     incio-incident-set-timestamp-at-point
     incio-incident-set-custom-field-at-point)
    ("Schedules" incio-schedule-visit incio-schedule-refresh
     incio-schedule-next incio-schedule-previous incio-schedule-now
     incio-schedule-toggle-calendar incio-schedule-toggle-timeline
     incio-schedule-toggle-preview incio-schedule-override-at-point
     incio-schedule-list))
  "Sections of the help buffer, as (TITLE COMMAND...).")

(defun incio--help-keys (command map)
  "Keys for COMMAND in MAP, ignoring evil auxiliary state maps."
  (seq-remove (lambda (key)
                (and (symbolp (aref key 0))
                     (string-suffix-p "-state" (symbol-name (aref key 0)))))
              (where-is-internal command (list map))))

(defun incio--help-section (title rows)
  (when rows
    (concat (propertize title 'face 'bold) "\n"
            (mapconcat (lambda (row) (format "  %-14s %s\n" (car row) (cdr row)))
                       rows "")
            "\n")))

(defun incio--help-text (map-symbol)
  (let* ((map (symbol-value map-symbol))
         (known (apply #'append (mapcar #'cdr incio-help-sections)))
         other)
    (map-keymap
     (lambda (key def)
       (let ((keys (vector key)))
         (when (and (not (consp key)) (commandp def) (not (keymapp def))
                    (not (memq def known))
                    (not (memq def '(digit-argument negative-argument)))
                    (eq def (lookup-key map keys))
                    (not (assoc (key-description keys) other)))
           (push (cons (key-description keys) def) other))))
     map)
    (concat
     (mapconcat
      (lambda (section)
        (incio--help-section
         (car section)
         (delq nil (mapcar (lambda (command)
                             (when-let ((keys (incio--help-keys command map)))
                               (cons (mapconcat #'key-description keys ", ")
                                     command)))
                           (cdr section)))))
      incio-help-sections "")
     (incio--help-section "Other" (nreverse other)))))

(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key* "evil-core")

(defun incio--evilify (mode map)
  "Start MODE in an evil state where MAP's own keys stay available.
Uses Spacemacs' evilified state when present, `motion' otherwise."
  (with-eval-after-load 'evil
    (let ((state (if (fboundp 'evil-evilified-state) 'evilified 'motion)))
      (evil-set-initial-state mode state)
      (map-keymap-internal
       (lambda (key def)
         (when (and def (not (keymapp def)) (not (eq key ?g)))
           (evil-define-key* state map (vector key) def)))
       map)
      (when-let ((refresh (lookup-key map "g")))
        (evil-define-key* state map "gr" refresh)))))

(defun incio-help ()
  "Toggle the list of keys available in the current view."
  (interactive)
  (if-let ((window (get-buffer-window "*incio-help*")))
      (delete-window window)
    (let ((text (incio--help-text (intern (format "%s-map" major-mode))))
          (buffer (get-buffer-create "*incio-help*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text))
        (special-mode)
        (goto-char (point-min)))
      (display-buffer buffer
                      '(display-buffer-in-side-window
                        (side . bottom)
                        (window-height . fit-window-to-buffer))))))

(provide 'incio-core)

;;; incio-core.el ends here
