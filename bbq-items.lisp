;;; Filtering and searching codes for bbq

(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

(defun items-query-string (query)
  "Return query string for items"
  #?"SELECT ${(join *item-fields* :separator ", ")} FROM songs ${query}")

(defun basic-search-items (terms)
  "Simple search across album, artist and title using intersection of results on terms"
  (let ((query (items-query-string "WHERE lower(artist || ' ' || title) LIKE '%' || ? || '%'"))
        (query-album (items-query-string "WHERE album IS NOT NULL AND lower(album) LIKE '%' || ? || '%'"))
        (terms (mapcar #'string-downcase terms)))
    (reduce (cut intersection <> <> :key #'car)
            (mapcar (lambda (term)
                      (union (sqlite:execute-to-list *db* query term)
                             (sqlite:execute-to-list *db* query-album term)
                             :key #'car)) terms))))

(defun parse-pm-query (terms &optional (last-op "+") p-acc m-acc)
  "Group plus minus queries"
  (if (null terms) (values p-acc m-acc)
      (let* ((current-terms (loop for term in terms
                               while (not (member term '("+" "-") :test #'string=))
                               collect term))
             (rest-terms (nthcdr (length current-terms) terms))
             (next-op (or (car rest-terms) "+")))
        (if (string= last-op "+")
            (parse-pm-query (cdr rest-terms) next-op
                            (cons current-terms p-acc) m-acc)
            (parse-pm-query (cdr rest-terms) next-op
                            p-acc (cons current-terms m-acc))))))

(defun pm-search-items (terms)
  "Search using the plus minus syntax"
  (multiple-value-bind (p-groups m-groups) (parse-pm-query terms)
    (let ((positives (reduce (cut union <> <> :key #'car) (mapcar #'basic-search-items p-groups)))
          (negatives (let ((results (mapcar #'basic-search-items m-groups)))
                       (if results (reduce (cut union <> <> :key #'car) results)
                           nil))))
      (set-difference positives negatives :key #'car))))

(defun new-items (n)
  "Return n new items"
  (sqlite:execute-to-list *db* (items-query-string "ORDER BY mtime DESC LIMIT ?") n))

(defun artist-cap-items (n)
  "Return items with 'artist items' <= n"
  (sqlite:execute-to-list *db* (items-query-string "WHERE artist IN (SELECT artist FROM songs GROUP BY artist HAVING count(*) <= ?)") n))

(defun item-ids (items)
  "Return a list of ids from given items."
  (let ((id-idx (position "id" *item-fields* :test #'string=)))
    (mapcar (cut nth id-idx <>) items)))
