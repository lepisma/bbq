;;;; Log parsing

(in-package #:bbq-log)

#|
(defun attr-init-sym (attr-form)
  (let ((name (if (listp attr-form) (car attr-form) attr-form)))
    (intern (symbol-name name) "KEYWORD")))

(defmacro defclass+ (name &optional (attrs ()))
  "No boilerplate!"
  `(defclass ,name ()
     ,(mapcar (lambda (attr)
                (if (listp attr)
                    (if (member :initarg attr)
                        attr
                        (append attr `(:initarg ,(attr-init-sym attr))))
                    (list attr :initarg (attr-init-sym attr))))
       attrs)))
|#

(defstruct entry
  id timestamp artist title album)

(defun line-empty-p (line)
  (string-equal "" (string-trim '(#\ ) line)))

(defun read-lines (file-path)
  (with-open-file (fp file-path)
    (loop for line = (read-line fp nil)
          while (and line (not (line-empty-p line)))
          collect line)))

(defun read-player-log (&optional (file-path (truename "~/.mpm.d/play-log")))
  "Read logs from the music player itself."
  (let ((lines (read-lines file-path)))
    (mapcar (lambda (line)
              (ematch (cl-strings:split line #\,)
                ((list timestamp id) (make-entry
                                      :id (parse-integer id)
                                      :timestamp (parse-integer timestamp)))))
            lines)))

(defun parse-lastfm-timestring (timestring)
  "Convert to timestamp. TODO:"
  timestring)

(defun read-lastfm-log (&optional (file-path (truename "~/.mpm.d/last-fm.csv")))
  "Read lastfm dump saved using https://benjaminbenben.com/lastfm-to-csv/.
The time format there is something like '16 Sep 2018 10:49'. Time zone looks like UTC."
  (let ((lines (read-lines file-path)))
    (mapcar (lambda (line)
              (ematch (cl-strings:split line #\,)
                ((list artist album title timestring)
                 (make-entry :timestamp (parse-lastfm-timestring timestring)
                             :artist artist
                             :title title
                             :album (unless (string-equal "" album) album)))))
            lines)))
