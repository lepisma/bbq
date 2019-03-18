(in-package #:bbq-element)

(defvar *elements* nil
  "List of registered elements")

(defmacro defelement (name (&rest args) &body body)
  `(progn
     (defun ,name ,args
       ,@body)
     (push ',name *elements*)))

(defmacro deffilter (name (song) &body body)
  `(defelement ,name (,song)
     ,@body))

(defmacro defsource (name () &body body)
  `(defelement ,name ()
     ,@body))
