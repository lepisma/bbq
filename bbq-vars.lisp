;;;; Variables for bbq

(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

(defvar *db-path* #p"~/.mpm.d/database"
  "Path to mpm sqlite database")

(defvar *player-url* "http://127.0.0.1:6672"
  "Url for mpm-play server")

(defparameter known-commands
  '(":new" ":cap" ":next" ":prev" ":toggle" ":current")
  "Commands known to bbq")

(defparameter known-flags '("#ls")
  "Flags known to bbq")
