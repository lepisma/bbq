;;;; package.lisp

(defpackage #:yt
  (:use #:cl
        #:cl-strings)
  (:export #:url-metadata
           #:url-valid?
           #:text-search))

(defpackage #:mpv
  (:use #:cl #:cffi))

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
           #:song-album
           #:song-query))

(defpackage #:bbq-log
  (:use #:cl
        #:cl-strings
        #:trivia))

(defpackage #:bbq-element
  (:use #:cl)
  (:export #:*elements*
           #:defelement
           #:defsource
           #:deffilter))

(defpackage #:bbq-import
  (:use #:cl)
  (:export #:from-url))

(defpackage #:bbq
  (:use #:cl
        #:alexandria
        #:anaphora
        #:cl-strings
        #:cl-cut
        #:cl-arrows)
  (:shadowing-import-from #:cl-strings
   :starts-with :ends-with :parse-number :split)
  (:shadowing-import-from #:cl-arrows
   :->))
