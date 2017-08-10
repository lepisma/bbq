;;;; bbq.lisp

(in-package #:bbq)

;;; "bbq" goes here. Hacks and glory await!

(defun search-basic (query)
  "Simple search across album, artist and title"
  (let ((fields '(album artist title)))
    (remove-if (lambda (item) (string-equal "" item))
               (reduce
                (lambda (a b)
                  (union a b :test 'string-equal))
                (mapcar (lambda (field)
                          (cl-strings:split
                           (inferior-shell:run/ss `(mpc search ,field ,query)) #\Linefeed)) fields)))))

(defun mpc-add (items)
  "Add items to the playlist"
  (inferior-shell:run/ss `(mpc add ,@items)))

(defun mpc-clear-play (items)
  "Clear playlist. Add items and play."
  (inferior-shell:run/ss '(mpc clear))
  (mpc-add items)
  (inferior-shell:run/ss '(mpc play)))
