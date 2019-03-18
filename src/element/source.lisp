(in-package #:bbq-element)

(defsource all ()
  "Return all songs from the db."
  (bbq-db:song-query))

(defsource new (n)
  "Return n new items."
  (bbq-db:song-query "ORDER BY mtime DESC LIMIT ?" n))

(defsource artist-cap (n)
  "Return items with artist counts <= n"
  (bbq-db:song-query "WHERE artist IN (SELECT artist FROM songs GROUP BY artist HAVING count(*) <= ?)" n))

(defun token-search (token)
  (let ((songs (bbq-db:song-query "WHERE lower(artist || ' ' || title) LIKE '%' || ? || '%'" (string-downcase token)))
        (songs-album (bbq-db:song-query "WHERE album IS NOT NULL AND lower(album) LIKE '%' || ? || '%'" (string-downcase token))))
    (union songs songs-album :key #'bbq-db::song-id)))

(defsource string-search (string)
  (let ((tokens (cl-strings:split string)))
    (reduce (cl-cut:cut intersection <> <> :key #'bbq-db::song-id) (mapcar #'token-search tokens))))
