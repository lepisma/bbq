(in-package #:bbq-import)
(cl-interpol:enable-interpol-syntax)

(defun confirm-song (s)
  "Confirm song using interactive user input."
  (restart-case (error #?"confirm import of ${s}")
    (accept () s)
    (swap-artist-and-title ()
      (rotatef (bbq-db:song-artist s) (bbq-db:song-title s))
      s)
    (edit-artist (artist)
      :report "Input artist"
      :interactive (lambda () (list (progn (format t "Enter artist: ") (read-line))))
      (setf (bbq-db:song-artist s) artist)
      s)
    (edit-title (title)
      :report "Input title"
      :interactive (lambda () (list (progn (format t "Enter title: ") (read-line))))
      (setf (bbq-db:song-title s) title)
      s)
    (edit-values (artist title)
      :report "Input artist and title"
      :interactive (lambda ()
                     (list
                      (progn (format t "Enter artist: ") (read-line))
                      (progn (format t "Enter title: ") (read-line))))
      (setf (bbq-db:song-artist s) artist
            (bbq-db:song-title s) title)
      s)))

(defun from-yt-url (url)
  (confirm-song (apply #'bbq-db:make-song (yt:url-metadata url))))

(defun from-url (url)
  "Import song from supplied url."
  (let ((s (cond ((yt:url-valid? url) (from-yt-url url))
                 (t (error #?"url ${url} not supported")))))
    (if (bbq-db:present? s)
        (error #?"song ${s} already present")
        (bbq-db:add s))))
