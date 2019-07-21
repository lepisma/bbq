;;;; Log parsing

(in-package #:bbq-log)

;; (defvar *player-log* (truename "~/.mpm.d/play-log")
;;   "Log file of player.")

;; (defvar *last-fm-log* (truename "~/.mpm.d/last-fm.csv")
;;   "Last fm dump saved using https://benjaminbenben.com/lastfm-to-csv/.
;; The time format there is something like '16 Sep 2018 10:49'. Time zone looks
;; like UTC.")

;; (defun line-empty? (line)
;;   (string-equal "" (string-trim '(#\ ) line)))

;; (defun read-lines (file-path)
;;   (with-open-file (fp file-path)
;;     (loop for line = (read-line fp nil)
;;           while line when (not (line-empty? line)) collect line)))

;; (defun read-player-log (&optional (file-path *player-log*))
;;   "Read logs from the music player itself."
;;   (mapcar (lambda (line) (apply #'cons (mapcar #'parse-integer (cl-strings:split line #\,))))
;;           (read-lines file-path)))

;; (defun read-lastfm-log (&optional (file-path *last-fm-log*))
;;   (mapcar (lambda (line)
;;             (ematch (cl-strings:split line #\,)
;;               ((list artist album title timestring)
;;                `(:artist ,artist
;;                  :album ,(unless (string-equal "" album) album)
;;                  :title ,title
;;                  :timestring ,timestring))))
;;           (read-lines file-path)))
