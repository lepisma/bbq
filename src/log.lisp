;;;; Log parsing

(in-package #:bbq-log)
(cl-interpol:enable-interpol-syntax)

(defparameter *scrobbled-at-file* (merge-pathnames "scrobbled-at" bbq-config::*config-dir*)
  "File to keep last scrobbling time.")

(defun scrobbled-at ()
  "Return time stamp when last scrobbling was done"
  (if (probe-file *scrobbled-at-file*)
      (let ((text (alexandria:read-file-into-string *scrobbled-at-file*)))
        (if (string= "" text)
            0
            (parse-number:parse-number text)))
      0))

(defmethod mark-played ((s bbq-db::song))
  "Mark song as played using current timestamp in the database"
  (let ((ts (serapeum:get-unix-time)))
    (bbq-db:with-db db
      (execute-non-query db "INSERT INTO play_log (time, song_id) VALUES (?, ?)" ts (bbq-db::song-id s)))))

(defun items-to-scrobble ()
  "Return a list of songs to scrobble."
  (let* ((last-scrobble-time (scrobbled-at))
         (rows (bbq-db:with-db db
                 (execute-to-list db "SELECT time, song_id FROM play_log WHERE time >= ?" last-scrobble-time))))
    (mapcar (lambda (row) (cons (car row) (bbq-db::song-by-id (cdr row)))) rows)))

(defun update-scrobbled-at-file ()
  (let ((ts (serapeum:get-unix-time)))
    (alexandria:write-string-into-file (format nil "~A" ts) *scrobbled-at-file* :if-exists :supersede)))

(defun scrobble ()
  (let ((items (items-to-scrobble))
        (cl-progress-bar:*progress-bar-enabled* t))
    (cl-progress-bar:with-progress-bar ((length items) "Total items to scrobble: ~a." (length items))
      (loop for item in items
            do (progn (lastfm:track-scrobble (bbq-db:song-artist (cdr item))
                                             (bbq-db:song-title (cdr item))
                                             (car item))
                      (cl-progress-bar:update 1))))
    (update-scrobbled-at-file)))

;; (defvar *player-log* (truename "~/.mpm.d/play-log")
;;   "Log file of player.")

;; (defvar *last-fm-log* (truename "~/.mpm.d/last-fm.csv")
;;   "Last fm dump saved using https://benjaminbenben.com/lastfm-to-csv/.
;; The time format there is something like '16 Sep 2018 10:49'. Time zone looks
;; like UTC.")

;; (defun line-empty? (line)
;;   (string-equal "" (string-trim '(#\ ) line)))

;; (defun read-lines (file-path)
;;   (with-open-file (fp file-path)
;;     (loop for line = (read-line fp nil)
;;           while line when (not (line-empty? line)) collect line)))

;; (defun read-player-log (&optional (file-path *player-log*))
;;   "Read logs from the music player itself."
;;   (mapcar (lambda (line) (apply #'cons (mapcar #'parse-integer (cl-strings:split line #\,))))
;;           (read-lines file-path)))

;; (defun read-lastfm-log (&optional (file-path *last-fm-log*))
;;   (mapcar (lambda (line)
;;             (ematch (cl-strings:split line #\,)
;;               ((list artist album title timestring)
;;                `(:artist ,artist
;;                  :album ,(unless (string-equal "" album) album)
;;                  :title ,title
;;                  :timestring ,timestring))))
;;           (read-lines file-path)))
