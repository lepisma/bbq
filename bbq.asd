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
               #:cl-strings
               #:cl-yaml
               #:dexador
               #:serapeum
               #:sqlite)
  :serial t
  :components ((:file "package")
               (:file "bbq-vars")
               (:file "bbq-items")
               (:file "bbq")))
