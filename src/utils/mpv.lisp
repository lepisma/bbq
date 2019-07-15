(in-package #:mpv)

;; This is needed otherwise mpv gets sad
(foreign-funcall "setlocale" :int 1 :string "C")

(define-foreign-library libmpv
  (:unix "libmpv.so"))

(use-foreign-library libmpv)

(defcfun "mpv_create" :pointer)

(defcfun "mpv_initialize" :int
  (mpv-handle :pointer))

(defcfun "mpv_destroy" :void
  (mpv-handle :pointer))

(defcfun "mpv_get_property_string" :string
  (mpv-handle :pointer)
  (name :string))

(defcfun "mpv_set_property_string" :int
  (mpv-handle :pointer)
  (name :string)
  (data :string))

(defcfun "mpv_set_option_string" :int
  (mpv-handle :pointer)
  (name :string)
  (data :string))

(defcfun "mpv_command" :int
  (mpv-handle :pointer)
  (args :pointer))

(defclass mpv-player ()
  ((handle :accessor handle
           :initarg :handle
           :documentation "Pointer to mpv-context"))
  (:documentation "Wrapper around mpv player. Don't really need anything other
  than the handle, but wrapping just in case."))

(defun make-mpv-player ()
  (let ((mp (make-instance 'mpv-player :handle (mpv-create))))
    (mpv-set-option-string (handle mp) "vid" "no")
    (mpv-initialize (handle mp))
    mp))

(defmethod duration ((mp mpv-player))
  (parse-number:parse-positive-real-number
   (mpv-get-property-string (handle mp) "duration")))

(defmethod time-pos ((mp mpv-player))
  (parse-number:parse-positive-real-number
   (mpv-get-property-string (handle mp) "time-pos")))

(defmethod paused? ((mp mpv-player))
  ;; TODO: Need to handle cases where prop is nil
  (let ((prop (mpv-get-property-string (handle mp) "pause")))
    (string= prop "yes")))

(defmethod pause ((mp mpv-player))
  (mpv-set-property-string (handle mp) "pause" "yes"))

(defmethod play ((mp mpv-player))
  (mpv-set-property-string (handle mp) "pause" "no"))

(defmethod toggle ((mp mpv-player))
  (if (paused? mp) (play mp) (pause mp)))

(defmethod stop ((mp mpv-player))
  (with-foreign-object (args :string 1)
    (setf (mem-aref args :string 0) "stop")
    (mpv-command (handle mp) args)))

(defmethod play-path ((mp mpv-player) path)
  (with-foreign-object (args :string 2)
    (setf (mem-aref args :string 0) "loadfile"
          (mem-aref args :string 1) path)
    (mpv-command (handle mp) args)))

(defmethod played? ((mp mpv-player))
  "Tell if the current track is `played' according to last.fm's scrobbling
  definitions. NOTE: We don't consider null duration at the moment."
  (> (time-pos mp) (min (* 4 60) (/ (duration mp) 2))))

(defmethod idle? ((mp mpv-player))
  (string= "yes" (mpv-get-property-string (handle mp) "idle-active")))

(defmethod shutdown ((mp mpv-player))
  (mpv-destroy (handle mp)))
