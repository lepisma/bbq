;;;; bbq.lisp

(in-package #:bbq)

(export '(mpm-clear-play
          search-items
          new-items
          artist-cap-items))

(defvar *config-path* #p"~/.bbq")

(defvar *mpm-db* nil
  "Path to mpm sqlite database")

(defvar *mpm-player-url* "http://127.0.0.1:6672"
  "Url for mpm-player server")

(with-open-file (fp *config-path*)
  (let ((config (read fp)))
    (setf *mpm-db* (cdr (assoc 'mpm-db config)))))

(defun search-items (query)
  "Simple search across album, artist and title"
  (let ((fields '(album artist title)))
    (remove-if (lambda (item) (string-equal "" item))
               (reduce
                (lambda (a b)
                  (union a b :test 'string-equal))
                (mapcar (lambda (field)
                          (cl-strings:split
                           (inferior-shell:run/ss `(mpc search ,field ,query)) #\Linefeed)) fields)))))

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

(defun mpm-add-items (items)
  "Add items to the playlist"
  (dex:get (format nil "~A/add?ids=~A" *mpm-player-url* (cl-strings:join items :separator ","))))

(defun mpm-clear ()
  (dex:get (format nil "~A/clear" *mpm-player-url*)))

(defun mpm-next ()
  (dex:get (format nil "~A/next" *mpm-player-url*)))

(defun mpm-clear-play (items)
  "Clear playlist. Add items and play."
  (mpm-clear)
  (mpm-add-items items)
  (mpm-next))
