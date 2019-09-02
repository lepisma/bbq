;; Functions for working with bbq database, slowly porting functions from mpm

(in-package #:bbq-db)
(cl-interpol:enable-interpol-syntax)

(defmacro with-db (var &rest body)
  `(with-open-database (,var bbq-config:*db-path*)
     ,@body))

(defun add-alist (table alist)
  "Add alist in the table."
  (with-db db
    (let ((keys (cl-strings:join (mapcar #'car alist) :separator ", "))
          (plcs (cl-strings:join (serapeum:repeat-sequence '("?") (length alist)) :separator ", ")))
      (apply #'execute-non-query db #?"INSERT INTO ${table} (${keys}) VALUES (${plcs})" (mapcar #'cdr alist)))))

(defstruct song
  id
  artist
  title
  album
  url
  (mtime (serapeum:get-unix-time) :type integer))

(defmethod to-alist ((s song) &optional with-id)
  "Convert given song to an alist. `with-id' tells whether to also expect and
export the `id' key."
  (let ((base-alist `(("title" . ,(song-title s))
                      ("url" . ,(song-url s))
                      ("artist" . ,(song-artist s))
                      ("album" . ,(song-album s))
                      ("mtime" . ,(song-mtime s)))))
    (if with-id
        (cons (cons "id" (song-id s)) base-alist)
        base-alist)))

(defmethod present? ((s song))
  "Tell if the given song is present in the db."
  (if (and (song-artist s) (song-title s))
      (with-db db
        (execute-to-list db "SELECT * FROM songs WHERE artist = ? AND title = ?" (song-artist s) (song-title s)))
      (error "song underspecified")))

(defmethod add ((s song))
  (if (and (song-artist s) (song-title s))
      (add-alist "songs" (to-alist s))
      (error "song underspecified")))

(defun song-query (&optional (condition-str "") &rest condition-args)
  "Use condition query to return songs."
  (with-db db
    (let* ((fields '(:id :artist :title :album :url :mtime))
           (items (apply #'execute-to-list db #?"SELECT songs.id, songs.artist, songs.title, songs.album, songs.url, songs.mtime FROM songs ${condition-str}" condition-args)))
      (mapcar (lambda (it) (apply #'make-song (bbq-utils:interleave fields it))) items))))

(defun song-by-id (id)
  (car (song-query #?"WHERE id = ${id}")))

(defmethod playback-url-local ((s song))
  "Return local url for playing given song. Right now, there is only cache which
can be queried."
  (let ((file-name (probe-file (cl-strings:join (list bbq-config:*cache-dir* (song-id s))))))
    (when file-name
      (format nil "~A" file-name))))

(defmethod playback-url-yt ((s song))
  "Return stream url using youtube url for the item"
  (let ((components (cl-strings:split (song-url s) ":")))
    (when (string= "yt" (car components))
      (yt:url-audio-stream (yt:id-to-url (second components))))))

(defmethod playback-url-yt-search ((s song))
  "Return stream url using youtube search."
  (let ((search-string (format nil "~A ~A" (song-artist s) (song-title s))))
    (yt:url-audio-stream (car (yt:text-search search-string)))))

(defmethod playback-url ((s song))
  "Return a playback url for given song"
  (or (playback-url-local s)
      (playback-url-yt s)
      (playback-url-yt-search s)))
