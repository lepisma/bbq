;;;; package.lisp

(defpackage #:bbq
  (:use #:cl #:cl-strings #:cl-cut)
  (:export
   #:dispatch-command
   #:known-commands
   #:known-flags))
