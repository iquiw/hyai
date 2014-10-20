(When "^I call hyai-indent-candidates at the current point"
  (lambda ()
    (setq hyai-test-candidates-output (hyai-indent-candidates))))

(Then "^indent candidates are \"\\(.*\\)\"$"
  (lambda (expected)
    (should (equal hyai-test-candidates-output (read expected)))))
