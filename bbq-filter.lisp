;;; Filtering and searching codes for bbq

(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

(defun search-items (query)
  "Simple search across album, artist and title"
  (let ((fields '("album" "artist" "title")))
    (remove-if (cut string-equal "" <>)
               (reduce (cut union <> <> :test 'string-equal)
                       (mapcar (lambda (field)
                                 (split
                                  (let ((stmt (format nil "SELECT id FROM songs WHERE lower(~A) LIKE '%~A%'" field query)))
                                    (inferior-shell:run/ss `(sqlite3 ,*db-path* ,stmt))) #\Linefeed)) fields)))))

(defun new-items (n)
  "Return n new items"
  (let* ((stmt (format nil "SELECT id FROM songs ORDER BY mtime DESC LIMIT ~A" n))
         (results (inferior-shell:run/ss `(sqlite3 ,*db-path* ,stmt))))
    (split results #\Linefeed)))

(defun artist-cap-items (n)
  "Return items with 'artist items' <= n"
  (let* ((stmt (format nil "SELECT id FROM songs WHERE artist IN (SELECT artist FROM songs GROUP BY artist HAVING count(*) <= ~A)" n))
         (results (inferior-shell:run/ss `(sqlite3 ,*db-path* ,stmt))))
    (split results #\Linefeed)))
