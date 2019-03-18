;;;; Command line handling for bbq

(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

(defvar *player-url* "http://127.0.0.1:6672"
  "Url for mpm-play server")

(defun init-config (config-path)
  (let ((config (yaml:parse config-path)))
    (setf *db-path* (truename (gethash "database" config)))
    (setf *player-url* #?"http://127.0.0.1:${(gethash "port" (gethash "player" config))}")))

(defun player-request (route &optional post-data)
  "Send request to mpm-play"
  (let ((base-url #?"${*player-url*}/${route}"))
    (if post-data
        (dex:post base-url :content post-data)
        (dex:get base-url))))

(defun item-ids (items)
  "Return a list of ids from given items."
  (let ((id-idx (position "id" *item-fields* :test #'string=)))
    (mapcar (cut nth id-idx <>) items)))

(defun enqueue-items (items)
  "Add items to the playlist"
  (let ((ids (item-ids items)))
    (player-request "add" `(("ids" . ,(join ids :separator ","))))))

(defun item-alist (item)
  "Create alist from item."
  (pairlis *item-fields* item))

(defun format-item (item)
  "Format item in a string"
  (let ((paired (mapcar #'list *item-fields* item)))
    (format nil "~{~{~6A ~}~%~}" paired)))

(defun print-items (items &optional sexp)
  "Print items formatted properly"
  (if sexp (print (mapcar #'item-alist items))
      (progn
        (format t "~{~A~%~}" (mapcar #'format-item items))
        (format t "Total ~A items" (length items)))))

(defun state ()
  "Return state of the player as json string"
  (let ((current (json:decode-json-from-string (player-request "current")))
        (vars (json:decode-json-from-string (player-request "state"))))
    (json:encode-json-alist-to-string `((:vars . ,vars)
                                        (:item . ,current)))))

(defun reset-and-play (items)
  "Clear playlist. Add items and play."
  (player-request "clear")
  (enqueue-items (shuffle items))
  (player-request "next"))

(defun next ()
  (player-request "next"))

(defun prev ()
  (player-request "prev"))

(defun toggle ()
  (player-request "toggle"))

(defun toggle-repeat ()
  (player-request "repeat"))

(defun print-or-play (items &optional print print-sexp)
  (if print (print-items items print-sexp) (reset-and-play items)))

(defun dispatch-command (cmd flags terms &optional (config-path #p"~/.mpm.d/config"))
  "Execute given command"
  (init-config config-path)
  (sqlite:with-open-database (*db* *db-path*)
    (let ((print-only (member "--list" flags :test #'string=))
          (print-sexp (member "--sexp" flags :test #'string=)))
      (cond ((string= cmd ":next") (next))
            ((string= cmd ":prev") (prev))
            ((string= cmd ":toggle") (toggle))
            ((string= cmd ":repeat") (toggle-repeat))
            ((string= cmd ":state") (princ (state)))
            (t (print-or-play (dispatch-search (cons cmd terms)) print-only print-sexp))))))
