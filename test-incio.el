;;; test-incio.el --- Tests for incio -*- lexical-binding: t; -*-

(require 'ert)
(require 'incio-incident)
(require 'incio-ui)

(ert-deftest incio-test-help-renders-mode-bindings ()
  (should (string-match-p "incio-incident-visit"
                          (incio--help-text 'incio-incident-list-mode-map)))
  (should (string-match-p "incio-incident-assign-at-point"
                          (incio--help-text 'incio-incident-list-mode-map)))
  (should (string-match-p "incio-incident-ack-at-point"
                          (incio--help-text 'incio-incident-list-mode-map)))
  (should (string-match-p "incio-incident-reject-at-point"
                          (incio--help-text 'incio-incident-list-mode-map)))
  (should (string-match-p "incio-incident-merge-at-point"
                          (incio--help-text 'incio-incident-list-mode-map))))

(ert-deftest incio-test-help-uses-calling-buffer-map ()
  (with-temp-buffer
    (incio-incident-mode)
    (incio-help))
  (let ((text (with-current-buffer "*incio-help*" (buffer-string))))
    (should (string-match-p "^Navigation$" text))
    (should (string-match-p "^Incident actions$" text))
    (should (string-match-p "incio-incident-set-status-at-point" text))
    (should-not (string-match-p "help-go-back" text))))

(ert-deftest incio-test-evil-keeps-mode-keys ()
  (skip-unless (require 'evil nil t))
  (let ((state (if (fboundp 'evil-evilified-state) 'evilified 'motion)))
    (should (eq state (evil-initial-state 'incio-incident-list-mode)))
    (with-temp-buffer
      (incio-incident-mode)
      (evil-local-mode 1)
      (should (eq 'incio-incident-set-status-at-point (key-binding "s")))
      (should (eq 'incio-incident-browse (key-binding "w")))
      (should (eq 'incio-refresh (key-binding "gr"))))))

(ert-deftest incio-test-parses-incident ()
  (let ((incident
         (incio-incident--parse
          '((id . "01INC")
            (reference . "INC-42")
            (name . "Database outage")
            (summary . "Queries failing")
            (incident_status . ((id . "status-1")
                                (name . "Investigating")
                                (category . "live")))
            (permalink . "https://app.incident.io/incidents/INC-42")))))
    (should (equal "INC-42" (incio-incident-reference incident)))
    (should (equal "Investigating" (incio-incident-status-name incident)))
    (should (equal "live" (incio-incident-status-category incident)))))

(ert-deftest incio-test-builds-incident-list-arguments ()
  (should
   (equal '("incidents" "list" "--status-category" "live" "--severity-id"
            "sev-1" "--sort-by" "created_at_newest_first")
          (incio-incident--list-args '("live") '("sev-1")
                                     "created_at_newest_first"))))

(ert-deftest incio-test-exposes-incident-details ()
  (let ((incident (incio-incident-create :severity '((name . "Major")))))
    (should (equal "Major" (incio-incident-severity-name incident)))
    (should (equal nil (incio-incident-roles incident)))
    (should (equal nil (incio-incident-custom-fields incident)))))

(ert-deftest incio-test-list-custom-field-columns ()
  (let ((incio-incident-list-custom-fields '("Cluster" "Service")))
    (with-temp-buffer
      (incio-incident-list-mode)
      (should (equal '("Reference" "Status" "Severity" "Alerts" "Cluster" "Service" "Name")
                     (mapcar #'car tabulated-list-format)))
      (setq incio-incident-list-custom-fields '(("Count" . 5) "Service"))
      (incio--render-incidents
       (list (incio-incident-create
              :raw '((custom_field_entries
                      . [((custom_field (name . "Service"))
                          (values . [((value_text . "api"))]))
                         ((custom_field (name . "Count"))
                          (values . [((value_numeric . 3))]))])))))
      (should (equal '(("Count" 5 t) ("Service" 16 t))
                     (append (seq-subseq tabulated-list-format 4 6) nil)))
      (should (equal ["" "" "" "" "3" "api" ""]
                     (cadr (car tabulated-list-entries)))))))

(ert-deftest incio-test-list-field-transforms ()
  (let ((incio-incident-list-custom-fields '("Service"))
        (incio-incident-list-field-transforms
         `(("Service" . ,(lambda (v _) (upcase v)))
           ("Name" . ,(lambda (v i) (concat (incio-incident-reference i) ":" v))))))
    (with-temp-buffer
      (incio-incident-list-mode)
      (incio--render-incidents
       (list (incio-incident-create
              :reference "INC-1" :name "boom"
              :raw '((custom_field_entries
                      . [((custom_field (name . "Service"))
                          (values . [((value_text . "api"))]))])))))
      (let ((row (cadr (car tabulated-list-entries))))
        (should (equal "API" (aref row 4)))
        (should (equal "INC-1:boom" (aref row 5)))))))

(ert-deftest incio-test-list-column-labels ()
  (let ((incio-incident-list-custom-fields nil)
        (incio-incident-list-column-labels '(("Severity" . "Sev")))
        (incio-incident-list-column-widths '(("Severity" . 5)))
        (incio-incident-list-field-transforms
         `(("Severity" . ,(lambda (_value _incident) "x")))))
    (with-temp-buffer
      (incio-incident-list-mode)
      (incio--render-incidents (list (incio-incident-create)))
      (should (equal '("Reference" "Status" "Sev" "Alerts" "Name")
                     (mapcar #'car tabulated-list-format)))
      (should (equal 5 (cadr (aref tabulated-list-format 2))))
      (should (equal "x" (aref (cadr (car tabulated-list-entries)) 2))))))

(ert-deftest incio-test-selects-status-and-severity-faces ()
  (should (eq 'incio-status-live (incio-status-face "live")))
  (should (eq 'incio-status-default (incio-status-face "unknown")))
  (should (eq 'incio-severity-major (incio-severity-face "Major")))
  (should (eq 'incio-severity-default (incio-severity-face "P3"))))

(ert-deftest incio-test-reads-nested-timestamp-value ()
  (let ((incident
         (incio-incident-create
          :raw '((incident_timestamp_values
                  . (((incident_timestamp . ((name . "Declared at")))
                      (value . ((value . "2026-09-10T15:28:44.577Z"))))))))))
    (should (equal "2026-09-10T15:28:44.577Z"
                   (incio-incident-timestamp incident "Declared at")))
    (should-not (incio-incident-timestamp incident "Resolved at"))))

(ert-deftest incio-test-incident-columns-stay-aligned ()
  (let ((incidents
         (list (incio-incident-create
                :reference "INC-1" :status-name "Live" :severity '((name . "Major"))
                :name "First")
               (incio-incident-create
                :reference "INC-123456789012345" :status-name "Live"
                :severity '((name . "Major")) :name "Second"))))
    (with-temp-buffer
      (incio-incident-list-mode)
      (incio--render-incidents incidents)
      (goto-char (point-min))
      (should (eq (tabulated-list-get-id) (car incidents)))
      (forward-line 1)
      (should (eq (tabulated-list-get-id) (cadr incidents))))))

(ert-deftest incio-test-set-status-edits-incident ()
  (let ((incident (incio-incident-create :id "01INC"))
        (captured nil))
    (cl-letf (((symbol-function 'incio--api)
               (lambda (_method _path &optional fields) (setq captured fields))))
      (incio-incident-set-status incident "status-1")
      (should (equal '((incident . ((incident_status_id . "status-1")))
                       (notify_incident_channel . t))
                     captured)))))

(ert-deftest incio-test-fetch-filtered-reads-paginated-pages ()
  (cl-letf (((symbol-function 'incio--run)
             (lambda (&rest _)
               "[{\"incidents\":[{\"id\":\"a\"}]},{\"incidents\":[{\"id\":\"b\"}]}]")))
    (should (equal '("a" "b")
                   (mapcar #'incio-incident-id (incio-incident-fetch-filtered nil))))))

(ert-deftest incio-test-assign-role-edits-incident-with-vector ()
  (let ((incident (incio-incident-create :id "01INC"))
        (captured nil))
    (cl-letf (((symbol-function 'incio--api)
               (lambda (_method _path &optional fields) (setq captured fields))))
      (incio-incident-assign-role incident "role-1" "user-1")
      (should (equal
               `((incident . ((incident_role_assignments
                               . [((incident_role_id . "role-1")
                                   (assignee . ((id . "user-1"))))])))
                 (notify_incident_channel . t))
               captured)))))

(ert-deftest incio-test-ack-sets-live-status ()
  (let ((incio-incident--statuses
         '(((id . "s-live") (name . "Live") (category . "live"))
           ((id . "s-declined") (name . "Declined") (category . "declined"))))
        (incident (incio-incident-create :id "01INC"))
        (captured nil))
    (cl-letf (((symbol-function 'incio--api)
               (lambda (_method _path &optional fields) (setq captured fields))))
      (incio-incident-ack incident)
      (should (equal "s-live"
                     (incio--alist-get 'incident_status_id
                                       (incio--alist-get 'incident captured)))))))

(ert-deftest incio-test-reject-sets-declined-status ()
  (let ((incio-incident--statuses
         '(((id . "s-live") (name . "Live") (category . "live"))
           ((id . "s-declined") (name . "Declined") (category . "declined"))))
        (incident (incio-incident-create :id "01INC"))
        (captured nil))
    (cl-letf (((symbol-function 'incio--api)
               (lambda (_method _path &optional fields) (setq captured fields))))
      (incio-incident-reject incident)
      (should (equal "s-declined"
                     (incio--alist-get 'incident_status_id
                                       (incio--alist-get 'incident captured)))))))

(ert-deftest incio-test-merge-sets-merged-status-and-target ()
  (let ((incio-incident--statuses
         '(((id . "s-merged") (name . "Merged") (category . "merged"))))
        (incident (incio-incident-create :id "01INC"))
        (captured nil))
    (cl-letf (((symbol-function 'incio--api)
               (lambda (_method _path &optional fields) (setq captured fields))))
      (incio-incident-merge incident "02INC")
      (let ((body (incio--alist-get 'incident captured)))
        (should (equal "s-merged" (incio--alist-get 'incident_status_id body)))
        (should (equal "02INC" (incio--alist-get 'merged_into_incident_id body)))))))

(ert-deftest incio-test-merge-region-merges-into-first ()
  (let ((a (incio-incident-create :id "A" :reference "INC-1"))
        (b (incio-incident-create :id "B" :reference "INC-2"))
        (c (incio-incident-create :id "C" :reference "INC-3"))
        merged)
    (with-temp-buffer
      (tabulated-list-mode)
      (setq tabulated-list-format [("Ref" 10 nil)])
      (setq tabulated-list-entries
            (list (list a ["INC-1"]) (list b ["INC-2"]) (list c ["INC-3"])))
      (tabulated-list-print)
      (cl-letf (((symbol-function 'yes-or-no-p) #'always)
                ((symbol-function 'incio-refresh) #'ignore)
                ((symbol-function 'incio-incident-merge)
                 (lambda (i target) (push (cons (incio-incident-id i) target) merged))))
        (incio-incident-merge-region (point-min) (point-max))
        (should (equal '(("B" . "A") ("C" . "A")) (nreverse merged)))))))

(ert-deftest incio-test-status-by-category-errors-when-missing ()
  (let ((incio-incident--statuses '(((id . "s-live") (category . "live")))))
    (should-error (incio-incident-status-by-category "declined") :type 'user-error)))

(ert-deftest incio-test-api-with-fields-runs-once ()
  (let ((calls 0))
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (&rest args)
                 (setq calls (1+ calls))
                 0)))
      (incio--api "POST" "/test" '((name . "value")))
      (should (= 1 calls)))))

(require 'incio-resources)

(ert-deftest incio-test-schedule-navigates-windows ()
  (let ((incio-schedule-window-days 7)
        calls)
    (cl-letf (((symbol-function 'incio--run-json)
               (lambda (&rest args)
                 (push args calls)
                 '((final . (((rotation_id . "rot-1")
                              (user . ((name . "Bob")))
                              (start_at . "2026-10-22T08:00:00Z")
                              (end_at . "2026-10-23T08:00:00Z"))
                             ((rotation_id . "rot-1")
                              (user . ((name . "Alice")))
                              (start_at . "2026-10-21T08:00:00Z")
                              (end_at . "2026-10-22T08:00:00Z"))))))))
      (with-temp-buffer
        (incio-schedule-mode)
        (setq incio--schedule '((id . "sch-1") (name . "Primary")
                                (config . ((rotations . (((id . "rot-1")
                                                          (name . "Weekly"))))))))
        (setq incio--schedule-from (date-to-time "2026-10-21T00:00:00Z"))
        (incio-schedule-refresh)
        (should (equal '("schedules" "entries" "sch-1"
                         "--from" "2026-10-21T00:00:00Z"
                         "--until" "2026-10-28T00:00:00Z")
                       (car calls)))
        (goto-char (point-min))
        (should (string-match-p "Alice.*Weekly"
                                (buffer-substring (point) (line-end-position))))
        (incio-schedule-next)
        (should (equal "2026-10-28T00:00:00Z" (nth 4 (car calls))))
        (incio-schedule-previous)
        (incio-schedule-previous)
        (should (equal "2026-10-14T00:00:00Z" (nth 4 (car calls))))))))

(ert-deftest incio-test-schedule-list-renders ()
  (cl-letf (((symbol-function 'incio--run-json)
             (lambda (&rest _) '(((id . "sch-1") (name . "Primary")
                                  (timezone . "Europe/Paris"))))))
    (with-temp-buffer
      (incio-schedule-list-mode)
      (incio-schedule-refresh)
      (goto-char (point-min))
      (should (equal "sch-1" (alist-get 'id (tabulated-list-get-id))))
      (should (eq 'incio-schedule-next
                  (lookup-key incio-schedule-mode-map "]"))))))

(defconst incio-test--schedule
  '((id . "sch-1") (name . "Primary")
    (config . ((rotations . (((id . "rot-1") (name . "Old"))
                             ((id . "rot-1") (name . "Weekly")
                              (layers . (((id . "lay-1") (name . ""))))
                              (users . (((id . "u-2") (name . "Carol")
                                         (email . "carol@example.com")))))))))))

(ert-deftest incio-test-schedule-calendar-and-overrides ()
  (let ((process-environment (cons "TZ=UTC" process-environment))
        (alice '((rotation_id . "rot-1") (user . ((id . "u-1") (name . "Alice")))
                 (start_at . "2026-10-21T08:00:00Z") (end_at . "2026-10-22T08:00:00Z"))))
    (set-time-zone-rule "UTC")
    (unwind-protect
        (cl-letf (((symbol-function 'incio--run-json)
                   (lambda (&rest _) `((final . (,alice)) (overrides . (,alice))))))
          (with-temp-buffer
            (incio-schedule-mode)
            (setq incio--schedule incio-test--schedule)
            (setq incio--schedule-from (date-to-time "2026-10-21T12:00:00Z"))
            (incio-schedule-refresh)
            (goto-char (point-min))
            (should (memq 'incio-schedule-override
                          (get-text-property (next-single-property-change (point) 'face)
                                             'face)))
            (incio-schedule-toggle-calendar)
            (should (equal ["Day" "Weekly"] (vconcat (mapcar #'car tabulated-list-format))))
            (should (= incio-schedule-window-days (length tabulated-list-entries)))
            (should (equal ["Wed 2026-10-21" "Alice"]
                           (vconcat (mapcar #'substring-no-properties
                                            (cadr (car tabulated-list-entries))))))
            (should (equal ["Thu 2026-10-22" "Alice"]
                           (vconcat (mapcar #'substring-no-properties
                                            (cadr (cadr tabulated-list-entries))))))
            (should (equal "" (aref (cadr (nth 2 tabulated-list-entries)) 1)))))
      (set-time-zone-rule nil))))

(ert-deftest incio-test-schedule-timeline-and-preview ()
  (let ((process-environment (cons "TZ=UTC" process-environment))
        (incio-schedule-window-days 2)
        (incio-schedule-timeline-slot-hours 6)
        (entries '(((rotation_id . "rot-1") (user . ((name . "Alice")))
                    (start_at . "2026-10-21T00:00:00Z") (end_at . "2026-10-21T12:00:00Z"))
                   ((rotation_id . "rot-1") (user . ((name . "Bob")))
                    (start_at . "2026-10-21T12:00:00Z") (end_at . "2026-10-22T06:00:00Z")))))
    (set-time-zone-rule "UTC")
    (unwind-protect
        (let ((from (date-to-time "2026-10-21T00:00:00Z"))
              (now (date-to-time "2026-10-21T13:00:00Z")))
          (should (equal "Wed Thu " (incio--schedule-timeline-ticks from)))
          (should (equal "AlBob···" (substring-no-properties
                                     (incio--schedule-timeline-row entries from now))))
          (let ((preview (incio--schedule-preview incio-test--schedule entries now)))
            (should-not (string-match-p "Alice" preview))
            (should (string-match-p "Bob.*Weekly" preview)))
          (should (eq 'incio-schedule-list (lookup-key incio-incident-list-mode-map "O"))))
      (set-time-zone-rule nil))))

(ert-deftest incio-test-schedule-override-args ()
  (let (calls)
    (cl-letf (((symbol-function 'incio--run-json)
               (lambda (&rest _)
                 '((final . (((rotation_id . "rot-1") (user . ((id . "u-1") (name . "Alice")))
                              (start_at . "2026-10-21T08:00:00Z")
                              (end_at . "2026-10-22T08:00:00Z")))))))
              ((symbol-function 'incio--run) (lambda (&rest args) (push args calls) ""))
              ((symbol-function 'read-string) (lambda (_prompt default) default))
              ((symbol-function 'yes-or-no-p) #'always))
      (with-temp-buffer
        (incio-schedule-mode)
        (setq incio--schedule incio-test--schedule)
        (setq incio--schedule-from (date-to-time "2026-10-21T00:00:00Z"))
        (incio-schedule-refresh)
        (goto-char (point-min))
        (incio-schedule-override-at-point)
        (should (equal '("schedules" "override" "--schedule-id" "sch-1"
                         "--rotation-id" "rot-1" "--layer-id" "lay-1" "--user" "u-2"
                         "--start" "2026-10-21T08:00:00Z" "--end" "2026-10-22T08:00:00Z")
                       (car calls)))))))

;;; test-incio.el ends here

(ert-deftest incio-test-set-custom-field-payload ()
  (let (sent)
    (cl-letf (((symbol-function 'incio--api)
               (lambda (_method path fields) (setq sent (cons path fields)))))
      (incio-incident-set-custom-field
       (incio-incident-create :id "01INC") "cf-1"
       (list (incio-custom-field-free-value '((field_type . "numeric")) "4"))))
    (should (equal "/v2/incidents/01INC/actions/edit" (car sent)))
    (should (equal "{\"incident\":{\"custom_field_entries\":[{\"custom_field_id\":\"cf-1\",\"values\":[{\"value_numeric\":\"4\"}]}]},\"notify_incident_channel\":true}"
                   (json-encode (cdr sent))))))

(ert-deftest incio-test-filter-args-unfiltered ()
  (should (equal '("api" "GET" "/v2/incidents" "--paginate" "--output" "json")
                 (incio-incident--filter-args nil))))

(ert-deftest incio-test-fetch-filtered-paginated ()
  (let (args)
    (cl-letf (((symbol-function 'incio--run)
               (lambda (&rest a)
                 (unless (member "/v2/incident_alerts" a) (setq args a))
                 "{\"incidents\":[{\"id\":\"1\"}]}\n{\"incidents\":[{\"id\":\"2\"}]}\n")))
      (should (equal '("1" "2")
                     (mapcar #'incio-incident-id
                             (incio-incident-fetch-filtered '("mode[one_of]=standard"))))))
    (should (equal '("api" "GET" "/v2/incidents" "--field" "mode[one_of]=standard"
                     "--paginate" "--output" "json")
                   args))))

(ert-deftest incio-test-filter-candidates ()
  (let ((fetch (lambda (_k thunk) (funcall thunk))))
    (cl-letf (((symbol-function 'incio-severities)
               (lambda () '(((id . "sev1") (name . "Critical")))))
              ((symbol-function 'incio-custom-fields)
               (lambda () '(((id . "cf1") (name . "Team")))))
              ((symbol-function 'incio-custom-field-choices)
               (lambda (_f) '(("Core" . ((value_option_id . "opt1")))))))
      (should (member '("mode[one_of]=test") (incio--filter-candidates "mode[one_of]=" fetch)))
      (should (equal '(("severity[gte]=sev1" . "Critical"))
                     (incio--filter-candidates "severity[gte]=" fetch)))
      (should (equal '(("custom_field[cf1][one_of]=" . "Team"))
                     (incio--filter-candidates "custom_field[" fetch)))
      (should (equal '(("custom_field[cf1][one_of]=opt1" . "Core"))
                     (incio--filter-candidates "custom_field[cf1][one_of]=" fetch))))))

(ert-deftest incio-test-incident-alerts-count ()
  (let ((incident (incio-incident-create :id "inc-1")))
    (cl-letf (((symbol-function 'incio--run)
               (lambda (&rest args)
                 (should (member "incident_id=inc-1" args))
                 "{\"incident_alerts\":[{\"alert\":{\"status\":\"firing\"}},{\"alert\":{\"status\":\"resolved\"}}]}")))
      (incio-incident-fetch-alerts incident))
    (should (equal "1/2" (incio-incident-alerts-count incident)))
    (should (equal "" (incio-incident-alerts-count (incio-incident-create))))))

(ert-deftest incio-test-toggle-alerts ()
  (let ((incident (incio-incident-create
                   :id "inc-1" :reference "INC-1"
                   :alerts '(((status . "firing") (title . "CPU high"))))))
    (with-temp-buffer
      (incio-incident-list-mode)
      (incio--render-incidents (list incident))
      (goto-char (point-min))
      (incio-incident-toggle-alerts)
      (should (search-forward "CPU high" nil t))
      (should (equal "CPU high" (alist-get 'title (get-text-property (point) 'incio-alert))))
      (should (eq incident (get-text-property (point) 'tabulated-list-id)))
      (goto-char (point-min))
      (incio-incident-toggle-alerts)
      (should-not (search-forward "CPU high" nil t)))))
