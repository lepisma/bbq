;;;; Command line handling for bbq

(in-package #:bbq)
(cl-interpol:enable-interpol-syntax)

(defun init-config (config-path)
  (let ((config (yaml:parse config-path)))
    (setf *db-path* (pathname (gethash "database" config)))
    (setf *player-url* #?"http://127.0.0.1:${(gethash "port" (gethash "player" config))}")))

(defun player-request (route &optional data-string)
  "Send request to mpm-play"
  (let ((base-url #?"${*player-url*}/${route}"))
    (if data-string
        (dex:get (join (list base-url "?" data-string)))
        (dex:get base-url))))

(defun enqueue-items (items)
  "Add items to the playlist"
  (player-request "add" #?"ids=${(join items :separator ",")}"))

(defun now-playing ()
  "Return short string for current song"
  (let ((data (json:decode-json-from-string (player-request "current"))))
    #?"${(cdr (assoc :TITLE data))} - ${(cdr (assoc :ARTIST data))}"))

(defun reset-and-play (items)
  "Clear playlist. Add items and play."
  (setf *random-state* (make-random-state t))
  (player-request "clear")
  (enqueue-items (sort items (lambda (x y) (zerop (random 2)))))
  (player-request "next"))

(defun next ()
  (player-request "next"))

(defun prev ()
  (player-request "prev"))

(defun toggle ()
  (player-request "prev"))

(defun dispatch-command (cmd flags terms &optional (config-path #p"~/.mpm.d/config"))
  "Execute given command"
  (init-config config-path)
  (cond ((string= cmd ":new") (reset-and-play (new-items (parse-integer (car terms)))))
        ((string= cmd ":cap") (reset-and-play (artist-cap-items (parse-integer (car terms)))))
        ((string= cmd ":next") (next))
        ((string= cmd ":prev") (prev))
        ((string= cmd ":toggle") (toggle))
        ((string= cmd ":current") (print (now-playing)))
        (t (reset-and-play (search-items (join terms :separator " "))))))
