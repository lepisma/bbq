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

(defun is-pm? (term)
  (member term '("+" "-") :test #'string=))

(defun remove-multiple-pms (terms &optional acc taken?)
  "Remove multiple +-es and keep the latest one."
  (if (null terms) (nreverse acc)
      (let ((this (car terms)))
        (remove-multiple-pms
         (cdr terms)
         (if (and (is-pm? this) taken?)
             (cons this (cdr acc))
             (cons this acc))
         (is-pm? this)))))

(defun fix-first-pm (terms)
  "Fix + and - in the beginning"
  (cond ((string= (car terms) "-") (cons "" terms))
        ((string= (car terms) "+") (cdr terms))
        (t terms)))

(defun sanitize-pm-query (terms)
  "Remove stupid things"
  (->> terms
     (remove-if #'null)
     (remove-multiple-pms)
     (fix-first-pm)))

(defun parse-pm-query (terms &optional (last-op "+") p-acc m-acc)
  "Group plus minus queries"
  (if (null terms) (values p-acc m-acc)
      (let* ((current-terms (loop for term in terms
                               while (not (is-pm? term))
                               collect term))
             (rest-terms (nthcdr (length current-terms) terms))
             (next-op (or (car rest-terms) "+")))
        (if (string= last-op "+")
            (parse-pm-query (cdr rest-terms) next-op (cons current-terms p-acc) m-acc)
            (parse-pm-query (cdr rest-terms) next-op p-acc (cons current-terms m-acc))))))

(defun new-items (n)
  "Return n new items"
  (sqlite:execute-to-list *db* (items-query-string "ORDER BY mtime DESC LIMIT ?") n))

(defun artist-cap-items (n)
  "Return items with 'artist items' <= n"
  (sqlite:execute-to-list *db* (items-query-string "WHERE artist IN (SELECT artist FROM songs GROUP BY artist HAVING count(*) <= ?)") n))

(defun cmd-search-items (terms)
  "Use cmd to figure out results"
  (let ((cmd (car terms)))
    (cond ((string= cmd ":new") (new-items (parse-integer (cadr terms))))
          ((string= cmd ":cap") (artist-cap-items (parse-integer (cadr terms))))
          (t (basic-search-items terms)))))

(defun dispatch-search (terms)
  (multiple-value-bind (p-groups m-groups) (parse-pm-query (sanitize-pm-query terms))
    (let ((positives (reduce (cut union <> <> :key #'car) (mapcar #'cmd-search-items p-groups)))
          (negatives (let ((nresults (mapcar #'cmd-search-items m-groups)))
                       (if nresults (reduce (cut union <> <> :key #'car) nresults)
                           nil))))
      (set-difference positives negatives :key #'car))))
