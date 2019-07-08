;;;; Player server component

(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

;;; Temporary playback source resolvers. Will get cleaned up after full
;;; migration.

(defvar *player-cache-dir* (truename "/run/media/lepisma/Data/Music/.cache.d"))

(defmethod playback-url-local ((s bbq-db::song))
  "Return local url for playing given song. Right now, there is only cache which
can be queried."
  (let ((file-name (probe-file (join (list *player-cache-dir* (bbq-db::song-id s))))))
    (when file-name
      (format nil "~A" file-name))))

(defmethod playback-url-yt ((s bbq-db::song))
  "Return stream url using youtube url for the item"
  (let ((components (split (bbq-db:song-url s) ":")))
    (when (string= "yt" (car components))
      (yt::url-audio-stream (yt::id-to-url (second components))))))

(defmethod playback-url-yt-search ((s bbq-db::song))
  "Return stream url using youtube search."
  (let ((search-string (format "~A ~A" (bbq-db:song-artist s) (bbq-db:song-title s))))
    (yt::url-audio-stream (car (yt::text-search search-string)))))

(defmethod playback-url ((s bbq-db::song))
  "Return a playback url for given song"
  (or (playback-url-local s)
      (playback-url-yt s)
      (playback-url-yt-search s)))

;;; Player abstraction

(defclass bbq-player ()
  ((playlist :accessor playlist
             :initarg :playlist
             :initform nil
             :documentation "List of song items that represent current playlist.")
   (sleep :initarg :sleep
          :initform nil
          :documentation "Number of items to wait for before going in sleep.")
   (cycle :initarg :cycle
          :initform nil
          :type symbol
          :documentation "Repeat behavior")
   (index :initarg :index
          :initform nil
          :documentation "Current index in the playlist")
   (mp :accessor mp
       :initarg :mp
       :documentation "mpv instance"))
  (:documentation "Main player that talks to the underlying tool, mpv at the
  moment. Also this is basically a clone of mpm-play's main object and so is
  just trying to copy its behavior instead of acting more correctly."))

(defun make-bbq-player ()
  (make-instance 'bbq-player :mp (mpv::make-mpv-player)))

(defmethod current-song ((p bbq-player))
  "Return current song for the player."
  (with-slots (index playlist) p
    (when (and index playlist)
      (nth index playlist))))

(defmethod player-play ((p bbq-player))
  "Play current item in playlist. We assume that index and playlist both are
  valid."
  (let ((url (playback-url (current-song p))))
    (mpv::play-path (mp p) url)))

(defmethod player-reset ((p bbq-player))
  (with-slots (index playlist) p
    (setf index nil playlist nil)))

(defmethod player-enqueue ((p bbq-player) songs)
  (with-slots (playlist) p
    (appendf playlist songs)))

(defmethod player-next ((p bbq-player))
  ())

(defmethod player-prev ((p bbq-player))
  ())

(defmethod player-toggle ((p bbq-player))
  (mpv::toggle (mp p)))

(defmethod player-state ((p bbq-player))
  ())

;;; Server stuff

(defparameter *player* (make-bbq-player)
  "Global variable holding a player instance")

(defvar *port* 6672
  "Port to listen at for the server")

(defparameter *app* (make-instance 'ningle:<app>))

(defun respond-json (content)
  (appendf (lack.response:response-headers ningle:*response*)
           '("Content-Type" "application/json"))
  (cl-json:encode-json-to-string content))

(defun request-get (params key)
  (serapeum:assocdr key params :test #'string=))

(setf (ningle:route *app* "/")
      (lambda (params)
        (declare (ignore params))
        (respond-json :hello)))

(setf (ningle:route *app* "/add" :method :POST)
      (lambda (params)
        "Here we expect a `query' string for queuing items."
        (let ((songs (bbq-element::string-search (request-get params "query"))))
          (player-reset *player*)
          (player-enqueue *player* songs)
          (setf (slot-value *player* 'index) 0)
          (player-play *player*)
          (respond-json :ok))))

(setf (ningle:route *app* "/next")
      (lambda (params)
        (declare (ignore params))
        (player-next *player*)
        (respond-json :ok)))

(setf (ningle:route *app* "/prev")
      (lambda (params)
        (declare (ignore params))
        (player-prev *player*)
        (respond-json :ok)))

;; TODO Skipping seek, sleep, cycle for now

(setf (ningle:route *app* "/toggle")
      (lambda (params)
        (declare (ignore params))
        (player-toggle *player*)
        (respond-json :ok)))

(setf (ningle:route *app* "/state")
      (lambda (params)
        (declare (ignore params))
        (respond-json (player-state *player*))))

(defun server-start ()
  (clack:clackup *app* :port *port* :use-thread nil))

;;; Client functions

(defun client-request (route &optional json-post-data)
  "Send request to mpm-play"
  (let ((base-url #?"http://localhost:${*port*}/${route}"))
    (if json-post-data
        (dex:post base-url :content (cl-json:encode-json-to-string json-post-data)
                  :headers '(("Content-Type" . "application/json")))
        (dex:get base-url))))

(defun client-reset-and-play (query)
  (client-request "add" `(("query" . ,query))))

(defun client-next ()
  (client-request "next"))

(defun client-prev ()
  (client-request "prev"))

(defun client-toggle ()
  (client-request "toggle"))
