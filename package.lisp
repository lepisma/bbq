;;;; package.lisp

(defpackage #:bbq-db
  (:use #:cl
        #:sqlite)
  (:export #:with-db))

(defpackage #:bbq
  (:use #:cl
        #:alexandria
        #:anaphora
        #:cl-strings
        #:cl-cut
        #:cl-arrows
        #:serapeum)
  (:shadowing-import-from #:cl-strings
   :starts-with :ends-with :parse-number :split)
  (:shadowing-import-from #:cl-arrows
   :->)
  (:shadowing-import-from #:serapeum
   :scan)
  (:export #:dispatch-command
           #:known-commands
           #:known-flags))

(defpackage #:bbq-log
  (:use #:cl
        #:cl-strings
        #:trivia))

(defpackage #:bbq-er
  (:use #:cl
        #:cl-strings
        #:trivia))

(defpackage #:yt
  (:use #:cl)
  (:export #:get-id))
