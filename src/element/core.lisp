(in-package #:bbq-element)

(defvar *elements* (make-hash-table)
  "Map of registered elements")

(defmacro defelement (type name (&rest args) &body body)
  `(progn
     (defun ,name ,args
       ,@body)
     (setf (gethash ',name *elements*) (cons ,type #',name))))

(defmacro deffilter (name (song &rest args) &body body)
  `(defelement :filter ,name (,song ,@args)
     ,@body))

(defmacro defsource (name (&rest args) &body body)
  `(defelement :source ,name ,args
     ,@body))
