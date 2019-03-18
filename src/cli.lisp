;; Functions for cli usage

(in-package #:bbq)

(defparameter known-commands
  '(":new" ":cap" ":next" ":prev" ":toggle" ":state" ":repeat")
  "Commands known to bbq")

(defparameter known-flags '("--list" "--sexp")
  "Flags known to bbq")
