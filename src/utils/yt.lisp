(in-package #:yt)
(cl-interpol:enable-interpol-syntax)

(defparameter *ytdl-bin* "~/.pyenv/shims/youtube-dl"
  "Path to ytdl. I shouldn't be needing to specify this to be honest.")

(defun url-id (url)
  "Return 11 char youtube identifier. From https://gist.github.com/takien/4077195"
  (let* ((url (ppcre:regex-replace-all "(>|<)" url ""))
         (splits (ppcre:split "(vi\/|v%3D|v=|\/v\/|youtu\.be\/|\/embed\/)" url)))
    (if (> (length splits) 1)
        (car (ppcre:split "(?i)[^0-9a-z_\-]" (nth 1 splits)))
        (car splits))))

(defun id-to-url (id)
  #?"https://youtube.com/watch?v=${id}")

(defun url-valid? (url)
  (ppcre:scan "^https?:\/\/(www\.)?(youtube\.co)|(youtu\.be)" url))

(defun text-search (text)
  "Search and return youtube-urls using yts
https://github.com/lepisma/cfg/blob/master/scripts/bin/yts"
  (mapcar #'id-to-url (split (inferior-shell:run/ss `(yts ,text)) #\Linefeed)))

(defun clean-title (title)
  "Remove the kind of junk usually present in youtube videos."
  (ppcre:regex-replace-all "(?i) *\\(official ?(video|audio)\\)" title ""))

(defun url-title (url)
  (let ((page (plump:parse (dex:get url))))
    (clean-title (aref (lquery:$ page "title" (text)) 0))))

(defun url-audio-stream (url)
  "Return best audio stream url for given youtube-url"
  (inferior-shell:run/ss `(,(pathname *ytdl-bin*) ,url -g -x --audio-format best)))

(defun url-metadata (url)
  "Return metadata for the song."
  (let* ((page-title (url-title url))
         (splits (mapcar #'clean (butlast (split page-title #\-))))
         (artist-title (if (= (length splits) 2)
                           ;; This is probably artist - title pair
                           (apply #'cons splits)
                           ;; Let the user decide
                           (cons page-title page-title))))
    `(:artist ,(car artist-title)
      :title ,(cdr artist-title)
      :url ,#?"yt:${(url-id url)}")))
