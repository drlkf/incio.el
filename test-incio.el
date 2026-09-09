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

;;; test-incio.el ends here
