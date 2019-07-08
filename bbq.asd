;;;; bbq.asd

(defsystem #:bbq
  :description "Music player"
  :author "Abhinav Tushar <lepisma@fastmail.com>"
  :license "GPLv3"
  :depends-on (#:alexandria
               #:anaphora
               #:cffi
               #:clack
               #:cl-arrows
               #:cl-cut
               #:cl-interpol
               #:cl-json
               #:cl-ppcre
               #:cl-strings
               #:cl-yaml
               #:dexador
               #:drakma ;; I should probably use one of dexador, drakma
               #:inferior-shell
               #:lquery
               #:ningle
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
    ((:module "utils" :components ((:file "mpv") (:file "yt")))
     (:file "db")
     (:file "log")
     (:module "element"
      :serial t
      :components ((:file "core")
                   (:file "source")
                   (:file "filter")))
     (:file "player")
     (:file "import")
     (:file "cli")))))
