(in-package #:yt)
(cl-interpol:enable-interpol-syntax)

(defparameter *ytdl-bin* "~/.pyenv/shims/youtube-dl"
  "Path to ytdl. I shouldn't be needing to specify this to be honest.")

(defun url-to-id (url)
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

(defun parse-playlist (url &optional (max-n 50))
  "Parse a complete playlist. Note that this could take long time so we provide
a default clip on number of items."
  (let* ((cmd `(,(pathname *ytdl-bin*) ,url --yes-playlist --dump-single-json --playlist-end ,max-n))
         (cmd-out (inferior-shell:run/ss cmd)))
    (cl-json:decode-json-from-string cmd-out)))

(defun normalize-dash (text)
  "Replace variations of dash with the canonical - in given text."
  (let ((variations (list "‒" "–" "—" "―")))
    (reduce (lambda (acc it) (cl-strings:replace-all acc it "-")) variations :initial-value text)))

(defun parse-page-title (page-title)
  "Run a rough heuristic to get artist and title pair from page title."
  (let ((splits (mapcar #'clean (split (normalize-dash page-title) #\-))))
    (if (= (length splits) 2)
        ;; This is probably artist - title pair
        (apply #'cons splits)
        ;; Let the user decide
        (cons page-title page-title))))

(defun url-metadata (url)
  "Return metadata for the song."
  (let* ((page-title (url-title url))
         (artist-title (parse-page-title page-title)))
    `(:artist ,(car artist-title)
      :title ,(cdr artist-title)
      :url ,#?"yt:${(url-to-id url)}")))
