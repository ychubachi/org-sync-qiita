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
(defun org-sync-qiita--api-items-post (title body tags private &optional cbfunc)
  (if (eq private nil) (setq private json-false))
  (deferred:$
    (request-deferred "https://qiita.com/api/v2/items"
                      :type "POST"
                      :headers
                      `(("Authorization"
                         . ,(concat "Bearer " (org-sync-qiita--access-token)))
                        ("Content-Type" . "application/json"))
                      :data
                      (let ((data
                             `(("body" . ,body)
                               ("tags" . ,tags)
                               ("title" . ,title)
                               ("private" . ,private))))
                        (json-encode data))
                      :parser 'json-read
                      :encoding 'utf-8)
    (deferred:nextc it
      (lambda (response)
        (if cbfunc
            (funcall cbfunc (request-response-data response)))))))

;; Test Case:

;; (defun org-sync-qiita--call-back (response)
;;   "Development use"
;;   (message "Got: id=%S" (assoc 'id (cdr response))))

;; (org-sync-qiita--api-items-post "てすとTest from Emacs 9"
;;                                 "# はじめに\n# つぎに\n"
;;                                 '[(("name" . "Emacs")) (("name" . "org-mode"))]
;;                                 t
;;                                 #'org-sync-qiita--call-back)

(defun org-sync-qiita--api-items-patch (id title body tags private &optional cbfunc)
  (if (eq private nil) (setq private json-false) (setq private t))
  (deferred:$
    (request-deferred (format "https://qiita.com/api/v2/items/%s" id)
                      :type "PATCH"
                      :headers
                      `(("Authorization"
                         . ,(concat "Bearer " (org-sync-qiita--access-token)))
                        ("Content-Type" . "application/json"))
                      :data
                      (let ((data
                             `(("body" . ,body)
                               ("tags" . ,tags)
                               ("title" . ,title)
                               ("private" . ,private))))
                        (json-encode data))
                      :parser 'json-read
                      :encoding 'utf-8)
    (deferred:nextc it
      (lambda (response)
        (if cbfunc
            (funcall cbfunc (request-response-data response)))))))

;; Test Case:

;; (org-sync-qiita--api-items-patch "9175aaaf466f5a77acfb"
;;                                  "Emacsから投稿した記事 12"
;;                                  "# この記事のID\ndeab1f72742467c87ec3\n# つぎに\n"
;;                                  '[(("name" . "Emacs")) (("name" . "org-mode"))]
;;                                  t nil)

(defun org-sync-qiita--api-items-delete (id &optional cbfunc)
  (deferred:$
    (request-deferred (format "https://qiita.com/api/v2/items/%s" id)
                      :type "DELETE"
                      :headers
                      `(("Authorization"
                         . ,(concat "Bearer " (org-sync-qiita--access-token)))
                        ("Content-Type" . "application/json"))
                      :parser 'json-read
                      :encoding 'utf-8)
    (deferred:nextc it
      (lambda (response)
        (if cbfunc
            (funcall cbfunc (request-response-data response)))))))

;; (org-sync-qiita--api-items-delete "1a46875af4df6a7a1539")

(provide 'org-sync-qiita-api)
;;; org-sync-qiita-api.el ends here
