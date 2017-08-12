;;;; bbq.lisp

(in-package #:bbq)

(export '(mpc-clear-play
          search-items
          new-items
          artist-cap-items))

(defvar *config-path* #p"~/.bbq")

(defvar *music-dir* nil
  "Path to music directory")
(defvar *beets-db* nil
  "Path to beets sqlite database")

(with-open-file (fp *config-path*)
  (let ((config (read fp)))
    (setf *music-dir* (cdr (assoc 'music-dir config))
          *beets-db* (cdr (assoc 'beets-db config)))))

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
  (let* ((stmt (format nil "SELECT substr(path, ~A) FROM items ORDER BY mtime DESC LIMIT ~A" (+ 1 (length *music-dir*)) n))
         (results (inferior-shell:run/ss `(sqlite3 ,*beets-db* ,stmt))))
    (cl-strings:split results #\Linefeed)))

(defun artist-cap-items (n)
  "Return items with 'artist items' <= n"
  (let* ((stmt (format nil "SELECT substr(path, ~A) FROM items WHERE artist IN (SELECT artist FROM items GROUP BY artist HAVING count(*) <= ~A)"
                       (+ 1 (length *music-dir*)) n))
         (results (inferior-shell:run/ss `(sqlite3 ,*beets-db* ,stmt))))
    (cl-strings:split results #\Linefeed)))

(defun mpc-add-items (items)
  "Add items to the playlist"
  (inferior-shell:run/ss `(mpc add ,@items)))

(defun mpc-clear-play (items)
  "Clear playlist. Add items and play."
  (inferior-shell:run/ss '(mpc clear))
  (mpc-add-items items)
  (inferior-shell:run/ss '(mpc play)))
