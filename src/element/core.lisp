(in-package #:bbq-element)

(defvar *elements* (make-hash-table)
  "Map of registered elements")

(defmacro defelement (type name (&rest args) &body body)
  "Define and register an element for working with songs and lists."
  `(progn
     (defun ,name ,args
       ,@body)
     (setf (gethash ',name *elements*) (cons ,type #',name))))

(defmacro deffilter (name (songs &rest args) &body body)
  "Define a filter which takes a list of songs and return another list."
  `(defelement :filter ,name (,songs ,@args)
     ,@body))

(defmacro defsource (name (&rest args) &body body)
  "Define a source of songs."
  `(defelement :source ,name ,args
     ,@body))
