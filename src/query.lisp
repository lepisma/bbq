;;; Queries

(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

;; TODO: This primitive parsing is duplicate in cli file too. Should make
;;       parsing go in one place.
(defparameter *query-actions* '(:new :newp :oldp :cap :shuf)
  "Valid actions to pass in queries. Notice that at the moment, we allow actions
  at the car of query. Finally, we will allowing proper pipelining.")

(defun get-all ()
  "Return all songs from the db."
  (bbq-db:song-query))

(defun get-new (n)
  "Return n new items."
  (bbq-db:song-query "ORDER BY mtime DESC LIMIT ?" n))

(defun get-newp (n)
  "Return n recently played items."
  (bbq-db:song-query "INNER JOIN play_log ON play_log.song_id = songs.id ORDER BY play_log.time DESC LIMIT ?" n))

(defun get-oldp (n)
  "Return n oldest played items."
  (bbq-db:song-query "INNER JOIN play_log ON play_log.song_id = songs.id ORDER BY play_log.time LIMIT ?" n))

(defun get-artist-cap (n)
  "Return items with artist counts <= n"
  (bbq-db:song-query "WHERE artist IN (SELECT artist FROM songs GROUP BY artist HAVING count(*) <= ?)" n))

(defun token-search (token)
  (let ((songs (bbq-db:song-query "WHERE lower(artist || ' ' || title) LIKE '%' || ? || '%'" (string-downcase token)))
        (songs-album (bbq-db:song-query "WHERE album IS NOT NULL AND lower(album) LIKE '%' || ? || '%'" (string-downcase token))))
    (union songs songs-album :key #'bbq-db:song-id)))

(defun get-string-search (string)
  (let ((tokens (cl-strings:split string)))
    (reduce (cl-cut:cut intersection <> <> :key #'bbq-db:song-id) (mapcar #'token-search tokens))))

(defun transform-shuffle (items)
  (alexandria:shuffle items))

(defun parse-source-thunk (string)
  "Parse string as a source specification and return a thunk."
  (let ((splits (split string)))
    (if (and splits (starts-with (car splits) ":"))
        (case (read-from-string (car splits))
          (:new (lambda () (get-new (parse-integer (second splits)))))
          (:newp (lambda () (get-newp (parse-integer (second splits)))))
          (:oldp (lambda () (get-oldp (parse-integer (second splits)))))
          (:cap (lambda () (get-artist-cap (parse-integer (second splits)))))
          ;; TODO: Should continue with a new source specification after asking
          ;;       the user
          (t (error #?"Unknown source action ${(car splits)}")))
        ;; This is plain token search
        (lambda () (get-string-search string)))))

(defun parse-transform-thunk (string)
  "Parse string as a transform specification and return a function taking a
single argument which is a list of items to be transformed."
  (let ((splits (split string)))
    (if (and splits (starts-with (car splits) ":"))
        (case (read-from-string (car splits))
          (:shuf #'transform-shuffle)
          ;; TODO: Should continue with a new transform specification after
          ;;       asking the user
          (t (error #?"Unknown transform action ${(car splits)}")))
        ;; TODO: Throw a warning since this is not supposed to happen
        #'identity)))

(defun query (string)
  "Basic parsing allowing a main source followed by optional filters."
  (let* ((pieces (mapcar #'clean (split string ">>")))
         (transform-thunks (mapcar #'parse-transform-thunk (cdr pieces)))
         (items (funcall (parse-source-thunk (car pieces)))))
    (if transform-thunks
        (funcall (apply #'alexandria:compose (reverse transform-thunks)) items)
        items)))
