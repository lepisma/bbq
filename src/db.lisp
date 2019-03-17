;; Functions for working with bbq database, slowly porting functions from mpm

(in-package #:bbq-db)
(cl-interpol:enable-interpol-syntax)

;; TODO: Rename path to bbq
(defvar *db-path* (truename "~/.mpm.d/database")
  "Path to mpm sqlite database")

(defvar *db* nil
  "Connection to db")

(defmacro with-db (&rest body)
  `(with-open-database (*db* *db-path*)
     ,@body))

(defun add-alist (table alist)
  "Add alist in the table."
  (with-db
      (let ((keys (cl-strings:join (mapcar #'car alist) :separator ", "))
            (plcs (cl-strings:join (serapeum:repeat-sequence '("?") (length alist)) :separator ", ")))
        (apply #'execute-non-query *db* #?"INSERT INTO ${table} (${keys}) VALUES (${plcs})" (mapcar #'cdr alist)))))



(defstruct song
  id
  artist
  title
  album
  url
  (mtime (serapeum:get-unix-time) :type integer))

(defmethod present? ((s song))
  "Tell if the given song is present in the db."
  (if (and (song-artist s) (song-title s))
      (with-db
          (execute-to-list *db* "SELECT * FROM songs WHERE artist = ? AND title = ?" (song-artist s) (song-title s)))
      (error "song underspecified")))

(defmethod add ((s song))
  (if (and (song-artist s) (song-title s))
      ;; There should be a struct to alist
      (add-alist "songs" `(("artist" . ,(song-artist s))
                           ("title" . ,(song-title s))
                           ("mtime" . ,(song-mtime s))
                           ("url" . ,(song-url s))
                           ("album". ,(song-album s))))
      (error "song underspecified")))
