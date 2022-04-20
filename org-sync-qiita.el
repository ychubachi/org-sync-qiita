;;; org-sync-qiita.el --- Oynchronize Org subtree and Qiita post -*- lexical-binding: t -*-
;; Author: Yoshihide Chubachi
;; URL: https://github.com/ychubachi/org-sync-qiita
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (request-deferred "0.2.0") (ox-qmd "1.0.5"))
;;; Commentary:
;; This program is free software
;;; Code:

(require 'org-sync-qiita-api)

(require 'plstore)
(require 'request-deferred)
(require 'ox-qmd)

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

;; (org-sync-qiita--update-article "585b1a87479e561e68ec" "このIDはとっとこう非公開" "

;; # こんにちはａ

;; ははむう


;; # さようなら

;; ｅｕのい

;; (org-entry-get nil \"QIITA-ID\" t)\"585b1a87479e561e68ec\"

;; " '("TAG") t)

;; (id . "0e214dd921653db51bf5")

(defun org-sync-qiita--put-properties (response)
  (if (not (derived-mode-p 'org-mode))
      (error "Please use this command in org-mode"))
  (save-excursion
    (if (org-entry-get nil "QIITA-ID" t)
        (goto-char org-entry-property-inherited-from))
    (if-let ((id (cdr (assoc 'id response))))
      (org-entry-put nil "QIITA-ID" id))
    (let ((private (cdr (assoc 'private response))))
      (if (eq private :json-false) (setq private "false") (setq private "true"))
      (org-entry-put nil "QIITA-PRIVATE" private)))
  ;; (org-entry-put nil "QIITA-TAGS" (cdr (assoc 'private response)))
  (message "org-sync-qiita: Properties updated"))

;; (org-sync-qiita--api-items-post "てすとTest from Emacs 9"
;;                                 "# はじめに\n# つぎに\n"
;;                                 '[(("name" . "Emacs")) (("name" . "org-mode"))]
;;                                 t
;;                                 #'org-sync-qiita--call-back)

;; To find ancestor which has the property defined.
;; https://github.com/yyr/org-mode/blob/d8494b5668ad4d4e68e83228ae8451eaa01d2220/lisp/org.el#L15974
;;
;; (let ((id (org-entry-get nil "QIITA-ID" t)))
;;   )
;; (while t
;;   (org-back-to-heading t)
;;   )

(defun org-sync-qiita-at-point ()
  (interactive)
  (save-excursion
    (let ((id (org-entry-get nil "QIITA-ID" t)))
      ;; go to headline which has the GIITA-ID
      (if id (goto-char org-entry-property-inherited-from))
      (let ((title   (org-entry-get nil "ITEM"))
            (private (org-entry-get nil "QIITA-PRIVATE"))
            (body    (org-sync-qiita-qmd))
            (tags    '("TAG")))
        (if (equal private "false") (setq private nil) (setq private t))
        (cond
         (id
          ;; Update the article
          (org-sync-qiita--update-article id title body tags private))
         (t
          ;; Create a new article as private
          (org-sync-qiita--create-article title body tags t)))))))

(defun org-sync-qiita-unsync-at-point ()
  (interactive)
  (let ((id (org-entry-get nil "QIITA-ID")))
    (org-sync-qiita--delete-article id))
  (mapc (lambda (x)
          (if (string-match "^QIITA-" (car x))
              (org-entry-delete nil (car x))))
        (org-entry-properties nil)))

(defun org-sync-qiita-qmd ()
  (interactive)
  (let (qiita-markdown original-value)
    ;; Store the original value and change it.
    (setq original-value org-export-show-temporary-export-buffer)
    (setq org-export-show-temporary-export-buffer nil)
    ;; Export
    (save-excursion
      (org-qmd-export-as-markdown nil t t))
    (setq qiita-markdown
          (with-current-buffer "*Org QMD Export*" (buffer-string)))
    ;; Restore the original value.
    (setq org-export-show-temporary-export-buffer original-value)
    ;; Kill the export buffer.
    (kill-buffer "*Org QMD Export*")
    qiita-markdown))

(provide 'org-sync-qiita)
;;; org-sync-qiita.el ends here
