;;; org-sync-qiita.el --- Oynchronize Org subtree and Qiita post -*- lexical-binding: t -*-
;; Author: Yoshihide Chubachi
;; URL: https://github.com/ychubachi/org-sync-qiita
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (request-deferred "0.2.0"))
;;; Commentary:
;; This program is free software
;;; Code:

(require 'plstore)
(require 'request-deferred)

(require 'org-sync-qiita-api)

(defun org-sync-qiita--make-tags (tags)
  "Make qiita tags array for its API. TAGS is a list of strings."
  (if (null tags) (error "Qiita needs at least one tag"))
  (let* ((v (make-vector (length tags) nil)))
    (dotimes (i (length v))
      (setf (aref v i)
            `(("name" . ,(nth i tags)))))
    v))

(defun org-sync-qiita--create-article (title body tags private)
  (org-sync-qiita--api-items-post
   title
   body
   (org-sync-qiita--make-tags tags)
   private
   #'org-sync-qiita--put-properties))

(defun org-sync-qiita--delete-article (id)
  (org-sync-qiita--api-items-delete id))

(defun org-sync-qiita--update-article (id title body tags private)
  (org-sync-qiita--api-items-patch
   id
   title
   body
   (org-sync-qiita--make-tags tags)
   private
   #'org-sync-qiita--put-properties))

;; (org-sync-qiita--post-article "これがタイトル公開" "# そして\nぼでぃ\n" '("TAG") nil)

;; (id . "0e214dd921653db51bf5")

(defun org-sync-qiita--put-properties (response)
  (if-let ((id (cdr (assoc 'id response))))
      (org-entry-put nil "QIITA-ID" id))
  (let ((private (cdr (assoc 'private response))))
    (if private (setq private "true") (setq private "false"))
    (org-entry-put nil "QIITA-PRIVATE" private))
    ;; (org-entry-put nil "QIITA-TAGS" (cdr (assoc 'private response)))
  )

;; (org-sync-qiita--api-items-post "てすとTest from Emacs 9"
;;                                 "# はじめに\n# つぎに\n"
;;                                 '[(("name" . "Emacs")) (("name" . "org-mode"))]
;;                                 t
;;                                 #'org-sync-qiita--call-back)

(defun org-sync-qiita-at-point ()
  (interactive)
  (let ((id      (org-entry-get nil "QIITA-ID"))
        (title   (org-entry-get nil "ITEM"))
        (private (org-entry-get nil "QIITA-PRIVATE"))
        (body    "# はじめに\nおわり\n")
        (tags    '("TAG")))
    (cond (id
           ;; TODO: Update the article
           (org-sync-qiita--update-article id title
                                           "# そして\nぼでぃ\n" tags private))
          (t
           ;; Create a new article
           (org-sync-qiita--create-article title
                                           "# そして\nぼでぃ\n" tags private)))))

(defun org-sync-qiita-unsync-at-point ()
  (interactive)
  (let ((id (org-entry-get nil "QIITA-ID")))
    (org-sync-qiita--delete-article id)))

(provide 'org-sync-qiita)
;;; org-sync-qiita.el ends here
