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

;; ;;; Requests
;; (defun org-sync-qiita--api-items-post ()
;;   (deferred:$
;;     (request-deferred "https://qiita.com/api/v2/items"
;;                       :type "POST"
;;                       :headers
;;                       `(("Authorization"
;;                          . ,(concat "Bearer " (org-sync-qiita--access-token)))
;;                         ("Content-Type" . "application/json"))
;;                       :data
;;                       (json-encode
;;                        '(("body" . "# Exampleさんぷる")
;;                          ("tags"
;;                           . [(("name" . "Emacs"))
;;                              (("name" . "Org"))])
;;                          ("title" . "てすとTest from Emacs 5")
;;                          ("private" . t)))
;;                       :parser 'json-read
;;                       :encoding 'utf-8)
;;     (deferred:nextc it
;;       (lambda (response)
;;         (message "Got: %S" (request-response-data response))))))


  (provide 'org-sync-qiita)
;;; org-sync-qiita.el ends here
