;;;; package.lisp

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
