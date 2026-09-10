;;; test-incio.el --- Tests for incio -*- lexical-binding: t; -*-

(require 'ert)
(require 'incio-incident)

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

(ert-deftest incio-test-api-with-fields-runs-once ()
  (let ((calls 0))
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (&rest args)
                 (setq calls (1+ calls))
                 0)))
      (incio--api "POST" "/test" '((name . "value")))
      (should (= 1 calls)))))

;;; test-incio.el ends here
