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
               #:drakma ;; I should probably use one of dexador, drakma
               #:lquery
               #:plump
               #:serapeum
               #:sqlite
               #:trivia)
  :components
  ((:file "package")
   (:module "src"
    :depends-on ("package")
    :serial t
    :components
    ((:file "vars")
     (:file "db")
     (:module "element"
      :serial t
      :components ((:file "core")
                   (:file "source")
                   (:file "filter")))
     (:file "log")
     (:file "bbq")
     (:module "utils" :components ((:file "yt")))
     (:file "import")))))
