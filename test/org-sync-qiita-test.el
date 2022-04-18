;;; org-sync-qiita-test.el --- Tests for org-sync-qiita

(require 'org-sync-qiita)

(require 'el-mock)

(ert-deftest org-sync-qiita--make-tags-test ()
  (let ((tags '()))
    (should-error (org-sync-qiita--make-tags tags)))
  (let ((tags '("A")))
    (should (equal
            (org-sync-qiita--make-tags tags)
            '[(("name" . "A"))])))
  (let ((tags '("A" "B")))
    (should (equal
            (org-sync-qiita--make-tags tags)
            '[(("name" . "A")) (("name" . "B"))]))))

;;; org-sync-qiita-test.el ends here
