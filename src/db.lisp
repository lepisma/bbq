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

(defstruct song
  id artist title album url mtime)

(defun add-alist (table alist)
  "Add alist in the table."
  (with-db
      (let ((keys (cl-strings:join (mapcar #'car alist) :separator ", "))
            (plcs (cl-strings:join (serapeum:repeat-sequence '("?") (length alist)) :separator ", ")))
        (apply #'execute-non-query *db* #?"INSERT INTO ${table} (${keys}) VALUES (${plcs})" (mapcar #'cdr alist)))))

(defun add-song (artist title url &optional album)
  "Unconditionally add item in the database.

TODO: This will change to take ids instead of url (which are raw https type) and
have someone else work on getting those ids."
  (let ((ytid (yt:get-id url)))
    ;; HACK: basic heuristic to check for youtube links
    (if (and (= (length ytid) 11) (string-not-equal ytid url))
      (add-alist "songs" `(("artist" . ,artist)
                           ("title" . ,title)
                           ("mtime" . ,(serapeum:get-unix-time))
                           ("url" . ,#?"yt:${ytid}")
                           ("album" . ,album)))
      (error "url (possibly) not from youtube"))))
