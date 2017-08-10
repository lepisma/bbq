;;;; bbq.lisp

(in-package #:bbq)

(export '(mpc-clear-play
          search-items
          new-items))

(defvar *music-dir* "/media/lepisma/Data/Music/")
(defvar *beets-db* "/media/lepisma/Data/Music/beets.db")

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

(defun mpc-add-items (items)
  "Add items to the playlist"
  (mapcar #'print items)
  (inferior-shell:run/ss `(mpc add ,@items)))

(defun mpc-clear-play (items)
  "Clear playlist. Add items and play."
  (inferior-shell:run/ss '(mpc clear))
  (mpc-add-items items)
  (inferior-shell:run/ss '(mpc play)))
