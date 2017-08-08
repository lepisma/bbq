;;;; bbq.asd

(asdf:defsystem #:bbq
  :description "bbq"
  :author "Abhinav Tushar <abhinav.tushar.vs@gmail.com>"
  :license "GPLv3"
  :depends-on (:inferior-shell :cl-strings)
  :serial t
  :components ((:file "package")
               (:file "bbq")))

