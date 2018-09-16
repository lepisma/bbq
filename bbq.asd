;;;; bbq.asd

(defsystem #:bbq
  :description "Client for mpm-play"
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
  :components
  ((:file "package")
   (:module "src"
    :depends-on ("package")
    :serial t
    :components
    ((:module "utils" :components ((:file "yt")))
     (:file "vars")
     (:file "item")
     (:file "parser")
     (:file "er")
     (:file "log")
     (:file "bbq")))))
