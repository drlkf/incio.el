;;; test-incio.el --- Tests for incio -*- lexical-binding: t; -*-

(require 'ert)
(require 'incio-incident)
(require 'incio-ui)

(ert-deftest incio-test-help-renders-mode-bindings ()
  (should (string-match-p "incio-incident-visit"
                          (incio--help-text 'incio-incident-list-mode-map))))

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

(ert-deftest incio-test-api-with-fields-runs-once ()
  (let ((calls 0))
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (&rest args)
                 (setq calls (1+ calls))
                 0)))
      (incio--api "POST" "/test" '((name . "value")))
      (should (= 1 calls)))))

;;; test-incio.el ends here
