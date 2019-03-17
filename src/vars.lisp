;;;; Variables for bbq

(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

(defvar *player-url* "http://127.0.0.1:6672"
  "Url for mpm-play server")

(defparameter *item-fields* '("id" "artist" "title" "album" "url" "mtime")
  "Fields for describing an item")

(defparameter known-commands
  '(":new" ":cap" ":next" ":prev" ":toggle" ":state" ":repeat")
  "Commands known to bbq")

(defparameter known-flags '("--list" "--sexp")
  "Flags known to bbq")
