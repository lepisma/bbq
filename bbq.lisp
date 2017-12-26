;;;; Command line handling for bbq

(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

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

(defun enqueue-items (items)
  "Add items to the playlist"
  (let ((ids (item-ids items)))
    (player-request "add" `(("ids" . ,(join ids :separator ","))))))

(defun format-item (item)
  "Format item in a string"
  (let ((paired (mapcar #'list *item-fields* item)))
    (format nil "~{~{~6A ~}~%~}" paired)))

(defun print-items (items)
  "Print items formatted properly"
  (format t "~{~A~%~}" (mapcar #'format-item items))
  (format t "Total ~A items" (length items)))

(defun now-playing ()
  "Return short string for current song"
  (let ((data (json:decode-json-from-string (player-request "current"))))
    #?"${(cdr (assoc :TITLE data))} - ${(cdr (assoc :ARTIST data))}"))

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

(defun print-or-play (items &optional print)
  (if print (print-items items) (reset-and-play items)))

(defun dispatch-command (cmd flags terms &optional (config-path #p"~/.mpm.d/config"))
  "Execute given command"
  (init-config config-path)
  (sqlite:with-open-database (*db* *db-path*)
    (let ((print-only (member "--list" flags :test #'string=)))
      (cond ((string= cmd ":new") (print-or-play (new-items (parse-integer (car terms))) print-only))
            ((string= cmd ":cap") (print-or-play (artist-cap-items (parse-integer (car terms))) print-only))
            ((string= cmd ":next") (next))
            ((string= cmd ":prev") (prev))
            ((string= cmd ":toggle") (toggle))
            ((string= cmd ":current") (print (now-playing)))
            (t (print-or-play (search-items terms) print-only))))))
