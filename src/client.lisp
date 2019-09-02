(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

(defun client-request (route &optional json-post-data)
  "Send request to mpm-play"
  (let ((base-url #?"http://localhost:${*port*}/${route}"))
    (if json-post-data
        (dex:post base-url :content (cl-json:encode-json-to-string json-post-data)
                           :headers '(("Content-Type" . "application/json")))
        (dex:get base-url))))

(defun client-query-add (query)
  (client-request "add" `(("query" . ,query))))

(defun client-next ()
  (client-request "next"))

(defun client-prev ()
  (client-request "prev"))

(defun client-toggle ()
  (client-request "toggle"))
