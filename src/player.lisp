;;;; Command line handling for bbq

(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

(defvar *player-url* "http://127.0.0.1:6672"
  "Url for mpm-play server")

(defun player-request (route &optional post-data)
  "Send request to mpm-play"
  (let ((base-url #?"${*player-url*}/${route}"))
    (if post-data
        (dex:post base-url :content post-data)
        (dex:get base-url))))

(defun enqueue-songs (songs)
  "Add items to the playlist"
  (player-request "add" `(("ids" . ,(join (mapcar #'bbq-db::song-id songs) :separator ",")))))

(defun reset-and-play (songs)
  "Clear playlist. Add items and play."
  (player-request "clear")
  (enqueue-songs songs)
  (player-request "next"))

(defun next ()
  (player-request "next"))

(defun prev ()
  (player-request "prev"))

(defun toggle ()
  (player-request "toggle"))

(defun toggle-repeat ()
  (player-request "repeat"))
