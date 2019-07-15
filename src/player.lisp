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
  (let ((search-string (format nil "~A ~A" (bbq-db:song-artist s) (bbq-db:song-title s))))
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
   (sleep :accessor sleep-state
          :initarg :sleep
          :initform nil
          :documentation "Number of items to wait for before going in sleep.")
   (cycle :accessor cycle-state
          :initarg :cycle
          :initform nil
          :type symbol
          :documentation "Repeat behavior")
   (current-index :accessor current-index
                  :initarg :current-index
                  :initform nil
                  :documentation "Current index in the playlist")
   (mp :accessor mp
       :initarg :mp
       :documentation "mpv instance")
   (lock :accessor lock
         :initform (bt:make-lock)
         :documentation "Lock for metadata and other updates")
   (poll-thread :accessor poll-thread
                :initform nil
                :documentation "Thread for polling")
   (alive? :accessor alive?
           :initform t)
   (should-play? :accessor should-play?
                 :initform nil
                 :documentation "Internal flag for keeping track of things."))
  (:documentation "Main player that talks to the underlying tool, mpv at the
  moment. Also this is basically a clone of mpm-play's main object and so is
  just trying to copy its behavior instead of acting more correctly."))

(defun make-bbq-player ()
  (let ((player (make-instance 'bbq-player :mp (mpv::make-mpv-player))))
    (setf (poll-thread player) (bt:make-thread (lambda () (poll-loop player)) :name "bbq-player-poll"))
    player))

(defmethod shutdown ((p bbq-player))
  (setf (alive? p) nil)
  (bt:join-thread (poll-thread p))
  (mpv::shutdown (mp p)))

(define-condition player-empty (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Player is empty. You need to add items before performing actions.")))
  (:documentation "Player empty condition"))

(defmethod empty? ((p bbq-player))
  "Tell if the player is active. A non empty playlist means activity."
  (or (null (current-index p))
      (null (playlist p))))

(defmethod need-songs ((p bbq-player))
  (when (empty? p)
    (error 'player-empty)))

(defmethod current-song ((p bbq-player))
  "Return current song for the player."
  (need-songs p)
  (nth (current-index p) (playlist p)))

(defmethod poll-loop ((p bbq-player))
  "Loop for checking the player state and acting if needed."
  (loop
    while (alive? p)
    do (progn
         (sleep 0.1)
         (bt:with-lock-held ((lock p))
           (when (should-play? p)
             (cond
               ((mpv::idle? (mp p)) (next p))))))))
               ;; ((paused? p) (toggle p))
               ;; ((playing? p) (princ-to-string "TODO: call mark-played"))))))))

(defmethod play-current ((p bbq-player))
  "Play current item in playlist. We assume that index and playlist both are
  valid."
  (need-songs p)
  (let ((url (playback-url (current-song p))))
    (mpv::play-path (mp p) url)
    (setf (should-play? p) t)))

(defmethod reset ((p bbq-player))
  (setf (current-index p) nil
        (playlist p) nil
        (should-play? p) nil)
  (mpv::stop (mp p)))

(defmethod enqueue ((p bbq-player) songs)
  (appendf (playlist p) songs))

(defmethod next ((p bbq-player))
  "Go to the next item and play."
  (need-songs p)
  (with-slots (current-index playlist) p
    (incf current-index)
    (when (>= current-index (length playlist))
      (setf current-index 0))
    (play-current p)))

(defmethod prev ((p bbq-player))
  "Go to the previous item and play."
  (need-songs p)
  (with-slots (current-index playlist) p
    (decf current-index)
    (when (<= current-index 0)
      (setf current-index (- (length playlist) 1)))
    (play-current p)))

(defmethod toggle ((p bbq-player))
  (if (and (playlist p)
           (null (current-index p)))
      ;; This means items have just been added after a reset
      (progn
        (setf (current-index p) 0)
        (play-current p))
      ;; Other just do regular toggling
      (progn
        (mpv::toggle (mp p))
        (setf (should-play? p) (not (should-play? p))))))

(defmethod state ((p bbq-player))
  "Return current state variables of the player. Mostly useful for outside
systems trying to interact."
  (let ((vars `(("repeat" . ,(cycle-state p))
                ("sleep" . ,(sleep-state p))
                ("total" . ,(length (playlist p)))
                ("current" . ,(current-index p)))))
    (if (empty? p)
        `(("vars" . ,vars) ("item" . nil))
        `(("vars" . ,vars) ("item" . ,(bbq-db::to-alist (current-song p) t))))))

;;; Queries

;; TODO: This primitive parsing is duplicate in cli file too. Should make
;;       parsing go in one place.
(defparameter *query-actions* '(:new :cap)
  "Valid actions to pass in queries. Notice that at the moment, we allow actions
  at the car of query. Finally, we will allowing proper pipelining.")

(defun query (query-string)
  "Run the query string and return a list of songs."
  (let ((splits (split query-string)))
    (if (and splits (starts-with (car splits) ":"))
        (case (read-from-string (car splits))
          (:new (bbq-element::new (parse-integer (second splits))))
          (:cap (bbq-element::artist-cap (parse-integer (second splits)))))
        ;; This is just plain old string search
        (bbq-element::string-search query-string))))

;;; Server stuff
(defparameter *player* nil
  "Global variable holding a player instance")
(defparameter *port* 6672
  "Port to listen at for the server")
(defparameter *app* (make-instance 'ningle:<app>)
  "Ningle application")
(defparameter *clack-server* nil
  "Variable holding clack server for graceful shutdowns etc.")

(defun respond-json (content)
  (appendf (lack.response:response-headers ningle:*response*)
           '("Content-Type" "application/json"))
  (cl-json:encode-json-to-string content))

(defun request-get (params key)
  (serapeum:assocdr key params :test #'string=))

(defmacro r/ (route-args &rest body)
  (let ((path (if (stringp route-args) (list route-args) route-args)))
    `(setf (ningle:route *app* ,@path)
           (lambda (params)
             (respond-json (handler-case (progn ,@body)
                             (player-empty (c)
                               `((error . "player-empty")))))))))

(r/ "/" :hello)

(r/ "/clear"
    (reset *player*)
    :ok)

(r/ ("/add" :method :POST)
    (enqueue *player* (query (request-get params "query")))
    :ok)

(r/ "/next"
    (next *player*)
    :ok)

(r/ "/prev"
    (prev *player*)
    :ok)

(r/ "/toggle"
    (toggle *player*)
    :ok)

(r/ "/state"
    (state *player*))

(defun start-server (&optional background)
  (setf *player* (make-bbq-player))
  (setf *clack-server* (clack:clackup *app* :port *port* :use-thread background)))

(defun stop-server ()
  (clack:stop *clack-server*)
  (shutdown *player*)
  (setf *player* nil
        *clack-server* nil))

;;; Client functions
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
