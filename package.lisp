;;;; package.lisp

(defpackage #:bbq-db
  (:use #:cl
        #:sqlite)
  (:export #:with-db
           #:make-song
           #:add
           #:present?
           #:song-artist
           #:song-title
           #:song-url
           #:song-mtime
           #:song-album))

(defpackage #:bbq-import
  (:use #:cl)
  (:export #:from-url))

(defpackage #:bbq
  (:use #:cl
        #:alexandria
        #:anaphora
        #:cl-strings
        #:cl-cut
        #:cl-arrows
        #:serapeum)
  (:shadowing-import-from #:cl-strings
   :starts-with :ends-with :parse-number :split)
  (:shadowing-import-from #:cl-arrows
   :->)
  (:shadowing-import-from #:serapeum
   :scan)
  (:export #:dispatch-command
           #:known-commands
           #:known-flags))

(defpackage #:bbq-log
  (:use #:cl
        #:cl-strings
        #:trivia))

(defpackage #:yt
  (:use #:cl
        #:cl-strings)
  (:export #:get-metadata
           #:url-valid?))
