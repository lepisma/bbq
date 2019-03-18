(in-package #:bbq-element)

(defsource all ()
  "Return all songs from the db"
  (bbq-db::all-songs))
