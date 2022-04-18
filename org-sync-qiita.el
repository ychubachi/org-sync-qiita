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
  (if (null tags) (error "Qiita needs at least one tag"))
  (let* ((v (make-vector (length tags) nil)))
    (dotimes (i (length v))
      (setf (aref v i)
            `(("name" . ,(nth i tags)))))
    v))

(defun org-sync-qiita--post-article (title body tags private)
  (org-sync-qiita--api-items-post
   title
   body
   (org-sync-qiita--make-tags tags)
   private
   ;; TODO: add call back to add Qiita ID to the Org headline
   ;; TODO: add Qiita ID to args
   ))

;; (org-sync-qiita--post-article "これがタイトル公開" "# そして\nぼでぃ\n" '("TAG") nil)
;; bind C-c 9
(provide 'org-sync-qiita)
;;; org-sync-qiita.el ends here
