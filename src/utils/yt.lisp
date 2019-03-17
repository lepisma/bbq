(in-package #:yt)
(cl-interpol:enable-interpol-syntax)

(defun get-id (url)
  "Return 11 char youtube identifier. From https://gist.github.com/takien/4077195"
  (let* ((url (ppcre:regex-replace-all "(>|<)" url ""))
         (splits (ppcre:split "(vi\/|v%3D|v=|\/v\/|youtu\.be\/|\/embed\/)" url)))
    (if (> (length splits) 1)
        (car (ppcre:split "(?i)[^0-9a-z_\-]" (nth 1 splits)))
        (car splits))))

(defun url-valid? (url)
  (ppcre:scan "^https?:\/\/(www\.)?(youtube\.co)|(youtu\.be)" url))

(defun get-title (url)
  (let ((page (plump:parse (drakma:http-request url))))
    (aref (lquery:$ page "title" (text)) 0)))

(defun detect-metadata (page-title)
  "Return cons of artist and title."
  (let ((splits (mapcar #'clean (butlast (split page-title #\-)))))
    (if (= (length splits) 2)
        ;; This is probably artist - title pair
        (apply #'cons splits)
        ;; Let the user decide
        (cons page-title page-title))))

(defun get-song (url)
  "Make song struct from the url."
  (let* ((title (get-title url))
         (artist-title (detect-metadata title)))
    (bbq-db:make-song
     :artist (car artist-title)
     :title (cdr artist-title)
     :url #?"yt:${(get-id url)}")))
