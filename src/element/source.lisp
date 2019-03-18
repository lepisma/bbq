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
