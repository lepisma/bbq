;;; Filtering and searching codes for bbq

(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

(defun items-query-string (query)
  "Return query string for items"
  #?"SELECT ${(join *item-fields* :separator ", ")} FROM songs ${query}")

(defun search-items (search-for)
  "Simple search across album, artist and title"
  (let ((query (items-query-string "WHERE lower(artist || ' ' || title) LIKE '%' || ? || '%'"))
        (query-album (items-query-string "WHERE album IS NOT NULL AND lower(album) LIKE '%' || ? || '%'"))
        (search-for (string-downcase search-for)))
    (union (sqlite:execute-to-list *db* query search-for)
           (sqlite:execute-to-list *db* query-album search-for) :key #'car)))

(defun new-items (n)
  "Return n new items"
  (sqlite:execute-to-list *db* (items-query-string "ORDER BY mtime DESC LIMIT ?") n))

(defun artist-cap-items (n)
  "Return items with 'artist items' <= n"
  (sqlite:execute-to-list *db* (items-query-string "WHERE artist IN (SELECT artist FROM songs GROUP BY artist HAVING count(*) <= ?)") n))

(defun format-item (item)
  "Format item in a string"
  (let ((paired (mapcar #'list item-fields item)))
    (format nil "~{~{~6A ~}~%~}" paired)))

(defun item-ids (items)
  "Return a list of ids from given items."
  (let ((id-idx (position "id" item-fields :test #'string=)))
    (mapcar (cut nth id-idx <>) items)))
