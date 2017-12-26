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

(defun print-items (items)
  "Print items formatted properly"
  (format t "~{~A~%~}" (mapcar #'format-item items)))

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

(defun dispatch-command (cmd flags terms &optional (config-path #p"~/.mpm.d/config"))
  "Execute given command"
  (init-config config-path)
  (sqlite:with-open-database (*db* *db-path*)
    (cond ((string= cmd ":new") (reset-and-play (new-items (parse-integer (car terms)))))
          ((string= cmd ":cap") (reset-and-play (artist-cap-items (parse-integer (car terms)))))
          ((string= cmd ":next") (next))
          ((string= cmd ":prev") (prev))
          ((string= cmd ":toggle") (toggle))
          ((string= cmd ":current") (print (now-playing)))
          (t (reset-and-play (search-items (join terms :separator " ")))))))
