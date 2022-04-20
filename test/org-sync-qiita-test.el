;;; org-sync-qiita-test.el --- Tests for org-sync-qiita

(require 'org-sync-qiita)

(require 'el-mock)
(require 'org)

;;; Tests
(ert-deftest org-sync-qiita-test--make-tags ()
  (let (tags)
    (setq tags '())
    (should-error (org-sync-qiita--make-tags tags))
    (setq tags '("A"))
    (should (equal
             (org-sync-qiita--make-tags tags)
             '[(("name" . "A"))]))
    (setq tags '("A" "B"))
    (should (equal
         (org-sync-qiita--make-tags tags)
         '[(("name" . "A")) (("name" . "B"))]))))


;;; Org related tests

(defmacro org-sync-qiita-test--with-org-buffer (text &rest body)
  "A macro to write test functions which changes org texts.

Ref: https://github.com/bzg/org-mode/blob/6d73cd34a07796c33f9435bfc8c9a19e67656c7a/testing/org-test.el#L206"
  `(let* ((file (make-temp-file "org-"))
          (buffer (find-file-noselect file)))
     (with-current-buffer buffer
       (org-mode)
       (insert ,text)
       (unwind-protect
           (progn ,@body)
	 (set-buffer-modified-p nil)
	 (kill-buffer buffer)
         (delete-file file)))))

(ert-deftest org-sync-qiita-test--put-article-id ()
  (should (string=
           "* headline
:PROPERTIES:
:QIITA-ID: SOMEQIITAID
:END:
"
           (org-sync-qiita-test--with-org-buffer
            "* headline\n"
            (org-sync-qiita--put-article-id '((id . "SOMEQIITAID")))
            (buffer-string)))))


(ert-deftest org-sync-qiita-test--find-ancestor ()
  (should (string=
           "SOMEQIITAID"
           (org-sync-qiita-test--with-org-buffer
            "* headline
:PROPERTIES:
:QIITA-ID: SOMEQIITAID
:END:
"
            (org-sync-qiita--find-ancestor))))
  (should (string=
           "SOMEQIITAID"
           (org-sync-qiita-test--with-org-buffer
            "* headline
:PROPERTIES:
:QIITA-ID: SOMEQIITAID
:END:
** headline 2
"
            (insert "|")
            (message "%s" (buffer-string))
            (org-sync-qiita--find-ancestor)))))
;;; org-sync-qiita-test.el ends here
