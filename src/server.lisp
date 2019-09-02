(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

(defparameter *player* nil
  "Global variable holding a player instance")
(defparameter *port* (gethash "port" bbq-config:*config*)
  "Port to listen at for the server")
(defparameter *app* (make-instance 'ningle:<app>)
  "Ningle application")
(defparameter *clack-server* nil
  "Variable holding clack server for graceful shutdowns etc.")

(defun respond-json (content)
  (appendf (lack.response:response-headers ningle:*response*)
           '("Content-Type" "application/json"))
  (cl-json:encode-json-to-string content))

(defun request-get (params key)
  (serapeum:assocdr key params :test #'string=))

(defmacro r/ (route-args &rest body)
  (let ((path (if (stringp route-args) (list route-args) route-args)))
    `(setf (ningle:route *app* ,@path)
           (lambda (params)
             (respond-json (handler-case (progn ,@body)
                             (player-empty (c)
                               `((error . "player-empty")))))))))

(r/ "/" :hello)

(r/ "/clear"
    (reset *player*)
    :ok)

(r/ ("/add" :method :POST)
    (let ((songs (query (request-get params "query"))))
      (enqueue *player* songs)
      (format nil "Added ~A song(s)" (length songs))))

(r/ "/next"
    (next *player*)
    :ok)

(r/ "/prev"
    (prev *player*)
    :ok)

(r/ "/toggle"
    (toggle *player*)
    :ok)

(r/ "/state"
    (state *player*))

(defun start-server (&optional background)
  (setf *player* (make-bbq-player))
  (setf *clack-server* (clack:clackup *app* :port *port* :use-thread background)))

(defun stop-server ()
  (clack:stop *clack-server*)
  (shutdown *player*)
  (setf *player* nil
        *clack-server* nil))
