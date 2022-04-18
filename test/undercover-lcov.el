;;; undercover-lcov.el --- Helpers for org-google-qiita-test.el -*- lexical-binding: t -*-
(message "Use lcov for coverage")

(require 'undercover)
(undercover "org-sync-qiita.el"
            (:report-format 'lcov)
            (:send-report nil)
            (:exclude "org-sync-qiita-api.el"))
;;; usdercover-lcov.el ends here
