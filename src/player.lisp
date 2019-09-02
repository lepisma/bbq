;;;; Player server component

(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

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
   (played? :accessor played?
            :initform nil
            :documentation "Tell if this song is (marked as) played.")
   (should-play? :accessor should-play?
                 :initform nil
                 :documentation "Internal flag for keeping track of things."))
  (:documentation "Main player that talks to the underlying tool, mpv at the
  moment. Also this is basically a clone of mpm-play's main object and so is
  just trying to copy its behavior instead of acting more correctly."))

(defun make-bbq-player ()
  (let ((player (make-instance 'bbq-player :mp (mpv:make-mpv-player))))
    (setf (poll-thread player) (bt:make-thread (lambda () (poll-loop player)) :name "bbq-player-poll"))
    player))

(defmethod shutdown ((p bbq-player))
  (setf (alive? p) nil)
  (bt:join-thread (poll-thread p))
  (mpv:shutdown (mp p)))

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

(defmethod mark-played ((p bbq-player))
  "Mark current song as played."
  (unless (played? p)
    (when (mpv:played? (mp p))
      (setf (played? p) t)
      (bbq-log:mark-played (current-song p)))))

(defmethod poll-loop ((p bbq-player))
  "Loop for checking the player state and acting if needed."
  (loop
    while (alive? p)
    do (progn
         (sleep 0.1)
         (bt:with-lock-held ((lock p))
           (when (should-play? p)
             (cond
               ((mpv:idle? (mp p)) (next p))
               ((mpv:paused? (mp p)) (mpv:toggle (mp p)))
               (t (mark-played p))))))))

(defmethod play-current ((p bbq-player))
  "Play current item in playlist. We assume that index and playlist both are
  valid."
  (need-songs p)
  (let ((url (bbq-db:playback-url (current-song p))))
    (mpv:play-path (mp p) url)
    (setf (should-play? p) t
          (played? p) nil)
    ;; NOTE: We run song `change' hook here since any change triggers play also
    ;;       for us and we don't allow just a change in current-index.
    (bt:make-thread #'run-song-change-hook)))

(defmethod reset ((p bbq-player))
  (setf (current-index p) nil
        (playlist p) nil
        (should-play? p) nil
        (played? p) nil)
  (mpv:stop (mp p)))

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
      ;; Otherwise just do regular toggling
      (progn
        (mpv:toggle (mp p))
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
        `(("vars" . ,vars) ("item" . ,(bbq-db:to-alist (current-song p) t))))))
