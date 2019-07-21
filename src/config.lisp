(in-package #:bbq-config)

(defparameter *config-dir* #P"~/.config/bbq/"
  "Path of configuration directory")

(defparameter *config* (parse (read-file-into-string (merge-pathnames "config.yaml" *config-dir*)))
  "Main config hash table")

(defparameter *db-path* (merge-pathnames "database.sqlite" *config-dir*)
  "Path to main database")

(defparameter *cache-dir* (gethash "cache-dir" *config*)
  "Cache dir for songs.")
