;;; Queries

(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

;; TODO: This primitive parsing is duplicate in cli file too. Should make
;;       parsing go in one place.
(defparameter *query-actions* '(:new :newp :cap)
  "Valid actions to pass in queries. Notice that at the moment, we allow actions
  at the car of query. Finally, we will allowing proper pipelining.")

(defun parse (string)
  "Basic parsing allowing a main source followed by optional filters."
  (let* ((pieces (mapcar #'clean (split string ">>")))
         (source-query (car pieces)))))

(defun query (query-string)
  "Run the query string and return a list of songs."
  (let ((splits (split query-string)))
    (if (and splits (starts-with (car splits) ":"))
        (case (read-from-string (car splits))
          (:new (new (parse-integer (second splits))))
          (:newp (newp (parse-integer (second splits))))
          (:cap (artist-cap (parse-integer (second splits)))))
        ;; This is just plain old string search
        (string-search query-string))))

(defsource all ()
  "Return all songs from the db."
  (bbq-db:song-query))

(defsource new (n)
  "Return n new items."
  (bbq-db:song-query "ORDER BY mtime DESC LIMIT ?" n))

(defsource newp (n)
  "Return n recently played items."
  (bbq-db:song-query "INNER JOIN play_log ON play_log.song_id = songs.id ORDER BY play_log.time DESC LIMIT ?" n))

(defsource artist-cap (n)
  "Return items with artist counts <= n"
  (bbq-db:song-query "WHERE artist IN (SELECT artist FROM songs GROUP BY artist HAVING count(*) <= ?)" n))

(defun token-search (token)
  (let ((songs (bbq-db:song-query "WHERE lower(artist || ' ' || title) LIKE '%' || ? || '%'" (string-downcase token)))
        (songs-album (bbq-db:song-query "WHERE album IS NOT NULL AND lower(album) LIKE '%' || ? || '%'" (string-downcase token))))
    (union songs songs-album :key #'bbq-db:song-id)))

(defsource string-search (string)
  (let ((tokens (cl-strings:split string)))
    (reduce (cl-cut:cut intersection <> <> :key #'bbq-db:song-id) (mapcar #'token-search tokens))))

(defun parse-source-thunk (string)
  "Parse string as a source specification and return a thunk."
  (let ((splits (split string)))
    (if (and splits (starts-with (car splits) ":"))
        (case (read-from-string (car splits))
          (:new (lambda () (funcall (get-element :source 'new) (parse-integer (second splits)))))))))
