;;;; bbq.lisp

(in-package #:bbq)

(export '(init-config
          mpm-player-request
          mpm-player-current-string
          mpm-player-clear-play
          search-items
          new-items
          artist-cap-items))

(defvar *mpm-config-path* #p"~/.mpm.d/config")

(defvar *mpm-db* nil
  "Path to mpm sqlite database")

(defvar *mpm-player-url* nil
  "Url for mpm-player server")

(defun init-config ()
  (let ((mpm-config (yaml:parse *mpm-config-path*)))
    (setf *mpm-db* (pathname (gethash "database" mpm-config)))
    (setf *mpm-player-url* (format nil "http://127.0.0.1:~A" (gethash "port" (gethash "player" mpm-config))))))

(defun search-items (query)
  "Simple search across album, artist and title"
  (let ((fields '("album" "artist" "title")))
    (remove-if (lambda (item) (string-equal "" item))
               (reduce
                (lambda (a b)
                  (union a b :test 'string-equal))
                (mapcar (lambda (field)
                          (cl-strings:split
                           (let ((stmt (format nil "SELECT id FROM songs WHERE lower(~A) LIKE '%~A%'" field query)))
                             (inferior-shell:run/ss `(sqlite3 ,*mpm-db* ,stmt))) #\Linefeed)) fields)))))

(defun new-items (n)
  "Return n new items"
  (let* ((stmt (format nil "SELECT id FROM songs ORDER BY mtime DESC LIMIT ~A" n))
         (results (inferior-shell:run/ss `(sqlite3 ,*mpm-db* ,stmt))))
    (cl-strings:split results #\Linefeed)))

(defun artist-cap-items (n)
  "Return items with 'artist items' <= n"
  (let* ((stmt (format nil "SELECT id FROM songs WHERE artist IN (SELECT artist FROM songs GROUP BY artist HAVING count(*) <= ~A)" n))
         (results (inferior-shell:run/ss `(sqlite3 ,*mpm-db* ,stmt))))
    (cl-strings:split results #\Linefeed)))

(defun mpm-player-request (route &optional data-string)
  "Send request to mpm"
  (let ((base-url (format nil "~A/~A" *mpm-player-url* route)))
    (if data-string
        (dex:get (cl-strings:join (list base-url "?" data-string)))
        (dex:get base-url))))

(defun mpm-player-add-items (items)
  "Add items to the playlist after shuffling"
  (mpm-player-request "add" (format nil "ids=~A" (cl-strings:join items :separator ","))))

(defun mpm-player-current-string ()
  "Return short string for current song"
  (let ((data (json:decode-json-from-string (mpm-player-request "current"))))
    (format nil "~A - ~A" (cdr (assoc :TITLE data)) (cdr (assoc :ARTIST data)) )))

(defun mpm-player-clear-play (items)
  "Clear playlist. Add items and play."
  (setf *random-state* (make-random-state t))
  (mpm-player-request "clear")
  (mpm-player-add-items (sort items (lambda (x y) (zerop (random 2)))))
  (mpm-player-request "next"))
