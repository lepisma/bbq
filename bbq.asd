;;;; bbq.asd

(asdf:defsystem #:bbq
  :description "Frontend for mpm-play"
  :author "Abhinav Tushar <lepisma@fastmail.com>"
  :license "GPLv3"
  :depends-on (#:alexandria
               #:anaphora
               #:cl-arrows
               #:cl-cut
               #:cl-interpol
               #:cl-json
               #:cl-ppcre
               #:cl-strings
               #:cl-yaml
               #:dexador
               #:serapeum
               #:sqlite
               #:trivia)
  :serial t
  :components ((:file "package")
               (:file "yt")
               (:file "bbq-vars")
               (:file "bbq-items")
               (:file "bbq")))
