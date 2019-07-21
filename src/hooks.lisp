(in-package :bbq)

(defun run-song-change-hook ()
  "Custom things to run after a song change"
  (inferior-shell:run/s `(polybar-msg hook bbq 1)))
