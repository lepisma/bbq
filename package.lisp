;;;; package.lisp

(defpackage #:yt
  (:use #:cl
        #:cl-strings)
  (:export #:url-metadata
           #:url-valid?
           #:text-search
           #:url-audio-stream
           #:id-to-url
           #:url-to-id))

(defpackage #:mpv
  (:use #:cl #:cffi)
  (:export #:make-mpv-player
           #:shutdown
           #:played?
           #:idle?
           #:paused?
           #:toggle
           #:play-path
           #:stop))

(defpackage #:bbq-utils
  (:use #:cl)
  (:export #:interleave))

(defpackage #:bbq-config
  (:use #:cl
        #:cl-yaml
        #:alexandria)
  (:export *config*
           *cache-dir*
           *db-path*
           *config-dir*))

(defpackage #:bbq-db
  (:use #:cl
        #:sqlite)
  (:export #:with-db
           #:make-song
           #:add
           #:present?
           #:song
           #:song-artist
           #:song-title
           #:song-url
           #:song-mtime
           #:song-album
           #:song-id
           #:song-query
           #:song-by-id
           #:to-alist
           #:playback-url))

(defpackage #:bbq-reco
  (:use #:cl))

(defpackage #:bbq-log
  (:use #:cl
        #:cl-strings
        #:sqlite
        #:trivia)
  (:export #:mark-played
           #:scrobble))

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
   :->)
  (:export #:start-server
           #:stop-server
           #:client-query-add
           #:client-next
           #:client-prev
           #:client-toggle
           #:run
           #:parse-args
           #:generate-help))
