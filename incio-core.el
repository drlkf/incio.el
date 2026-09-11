;;; incio-core.el --- Core support for incident.io -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1"))

;;; Code:

(require 'json)

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

(defun incio--help-text (map-symbol)
  (substitute-command-keys (format "\\{%s}" map-symbol)))

(defun incio-help ()
  "Toggle the list of keys available in the current view."
  (interactive)
  (if-let ((window (get-buffer-window "*incio-help*")))
      (quit-window nil window)
    (with-help-window "*incio-help*"
      (princ (incio--help-text
              (intern (format "%s-map" major-mode)))))))

(provide 'incio-core)

;;; incio-core.el ends here
