;;;; bbq.lisp

(in-package #:bbq)

;;; "bbq" goes here. Hacks and glory await!

(defun basic (query)
  "Basic query unifying the mpc interface"
  (inferior-shell:run/ss '(mpc clear))
  (inferior-shell:run/ss (list 'mpc 'searchadd 'artist query))
  (inferior-shell:run/ss (list 'mpc 'searchadd 'album query))
  (inferior-shell:run/ss (list 'mpc 'searchadd 'title query))
  (inferior-shell:run/ss '(mpc play)))
