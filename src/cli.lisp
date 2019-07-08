;; Functions for cli usage

(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

(defparameter *cli-options* '(:list :help)
  "Options for cli. These are reflected in cli as --list, --help kind arguments
  anywhere in the list of all arguments provided. At the moment, we only support
  parameter less options.")

(defparameter *cli-actions* '(:new :cap :toggle :cycle :state :prev :next :serve)
  "List of valid actions. Actions in cli are present as the first argument along
  with the initial colon sign.")

(defun option-to-string (option)
  (string-downcase (format nil "--~A" option)))

(defun string-to-option (string)
  (find string *cli-options* :key #'option-to-string :test #'string=))

(defun option-candidate? (string)
  (starts-with string "-"))

(defun action-candidate? (string)
  (starts-with string ":"))

(defun generate-help ()
  (join (list
         ":: bbq"
         "   Options"
         (format nil "     ~A" (mapcar #'option-to-string *cli-options*))
         "   Actions"
         (format nil "     ~A" *cli-actions*))
        :separator (make-string 1 :initial-element #\Newline)))

(define-condition parsing-error (error) ()
  (:report (lambda (_condition stream)
             (format stream #?"Error in usage.\n~A" (generate-help))))
  (:documentation "Condition for cli parsing error."))

(defun parse-action (args)
  (when (and args (action-candidate? (car args)))
    (car (member (read-from-string (car args)) *cli-actions*))))

(defun parse-options (args &optional parsed)
  (if (null args)
      parsed
      (if (option-candidate? (car args))
          (let ((option (string-to-option (car args))))
            (if option
                (parse-options (cdr args) (cons option parsed))
                (error 'parsing-error)))
          (parse-options (cdr args) parsed))))

(defun parse-query (args)
  (let ((args (remove-if (disjoin #'action-candidate? #'option-candidate?) args)))
    (if args (join args :separator " ") " ")))

(defun parse-args (args)
  `((:action ,(parse-action args))
    (:options ,(parse-options args))
    (:query ,(parse-query args))))

(defun run (action query options)
  (princ "Not implemented"))
