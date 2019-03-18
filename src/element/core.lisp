(in-package #:bbq-element)

(defvar *elements* (make-hash-table)
  "Map of registered elements")

(defmacro defelement (type name (&rest args) &body body)
  `(progn
     (defun ,name ,args
       ,@body)
     (setf (gethash ',name *elements*) (cons ,type #',name))))

(defmacro deffilter (name (song) &body body)
  `(defelement :filter ,name (,song)
     ,@body))

(defmacro defsource (name () &body body)
  `(defelement :source ,name ()
     ,@body))
