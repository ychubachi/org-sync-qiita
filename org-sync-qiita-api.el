;;; org-sync-qiita-api.el --- Oynchronize Org subtree and Qiita post -*- lexical-binding: t -*-

(require 'plstore)
(require 'request-deferred)

;;; Qiita access token
(defcustom org-sync-qiita-access-token-file
  (concat user-emacs-directory "qiita.plstore")
  "The Qiita access token file."
  :type '(string)
  :group 'org-sync-qiita)

(defun org-sync-qiita--save-access-token ()
  (let ((access-token (read-from-minibuffer "Qiita access-token: "))
        (store (plstore-open org-sync-qiita-access-token-file)))
    (plstore-put store "qiita" nil `(:access-token ,access-token))
    (plstore-save store)
    (plstore-close store)
    access-token))

;; Test Case:
;; (org-sync-qiita--save-access-token)
;; => token

(defun org-sync-qiita--get-access-token ()
  (let* ((store (plstore-open org-sync-qiita-access-token-file))
         (plist (plstore-get store "qiita")))
    (plstore-close store)
    (plist-get (cdr plist) :access-token)))

;; Test Case:
;; (org-sync-qiita--get-access-token)

(defun org-sync-qiita--access-token ()
  "Get the qiita access token from the plstore file."
  (cond ((file-exists-p org-sync-qiita-access-token-file)
         (org-sync-qiita--get-access-token))
        (t
         (org-sync-qiita--save-access-token))))

;; Test Case:
;; 1. Delete the plstore file if exists.
;;   (delete-file org-sync-qiita-access-token-file)
;; 2. Save a new token.
;;   (org-sync-qiita--access-token)
;; 3. Get the token.
;;   (org-sync-qiita--access-token)

;;; Requests
(defun org-sync-qiita--api-items-post (title body tags private)
  (if (eq private nil) (setq private json-false))
  (deferred:$
    (request-deferred "https://qiita.com/api/v2/items"
                      :type "POST"
                      :headers
                      `(("Authorization"
                         . ,(concat "Bearer " (org-sync-qiita--access-token)))
                        ("Content-Type" . "application/json"))
                      :data
                      (json-encode
                       `(("body" . ,body)
                         ("tags" . ,tags)
                         ("title" . ,title)
                         ("private" . ,private)))
                      :parser 'json-read
                      :encoding 'utf-8)
    (deferred:nextc it
      (lambda (response)
        (message "Got: %S" (request-response-data response))))))

;; Test Case:

;; (org-sync-qiita--api-items-post "てすとTest from Emacs 7"
;;                                 "# はじめに\n# つぎに\n"
;;                                 '[(("name" . "Emacs")) (("name" . "org-mode"))])

;; (json-encode `(("private" . ,json-false)))
;; "{\"private\":false}"
;; "{\"private\":\"false\"}"
;; "{\"private\":\"f\"}"
;; "{\"private\":null}"
;; "{\"private\":true}"

  (provide 'org-sync-qiita-api)
;;; org-sync-qiita-api.el ends here
