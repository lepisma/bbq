(in-package #:bbq-utils)

(defun interleave (list1 list2 &optional acc)
  "Interleave two lists. Assume equal length."
  (if (or (null list1) (null list2))
      acc
      (interleave (cdr list1) (cdr list2) (append acc (list (car list1) (car list2))))))
