;;; org-bbq.el --- bbq playlists in org

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (dash "2.13.0") (dash-functional "2.13.0") (f "0.20.0") (s "1.12.0"))
;; URL: https://github.com/lepisma/bbq

;;; Commentary:

;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'org)
(require 's)
(require 'url)
(require 'url-util)
(require 'dash)
(require 'dash-functional)

(org-add-link-type "bbq" #'org-bbq-play #'org-bbq-export)

(defun org-bbq--yt-search (term)
  "Search for term in youtube and return ids. I am using yts
script which is in my config here ->
https://github.com/lepisma/cfg/blob/master/scripts/bin/yts"
  (with-temp-buffer
    (call-process "yts" nil t nil term)
    (s-split "\n" (s-trim (buffer-string)))))

(defun org-bbq--format-yt (yid)
  (format
   (concat "<iframe width=\"200\""
           " height=\"130\""
           " src=\"https://www.youtube.com/embed/%s\""
           " frameborder=\"0\""
           " allowfullscreen></iframe>") yid))

(defun org-bbq-play (path)
  (apply #'start-process "bbq" nil "bbq" (s-split " " path)))

(defun org-bbq--format-item (item)
  (let* ((title (cdr (assoc "title" item)))
         (artist (cdr (assoc "artist" item)))
         (yid (car (org-bbq--yt-search (concat title " " artist)))))
    (format "<div class=\"bbq-item\">
               %s
               <div class=\"bbq-item-info\">%s - <em>%s</em></div>
             </div>" (org-bbq--format-yt yid) title artist)))

(defun org-bbq--get-items (path)
  (read (shell-command-to-string (format "bbq --list --sexp %s" path))))

(defun org-bbq-export (path desc backend)
  "Export list of items in the playlist"
  (if (eq backend 'html)
      (let ((items (org-bbq--get-items path)))
        (print (point))
        (print "see that ^")
        (format "<div class=\"bbq-list\">
                   <h3 class=\"bbq-list-info\">
                     Playlist: %s <code>[%s]</code>
                   </h3>
                   <div class=\"bbq-list-container\">%s</div>
                 </div>" desc path
                (apply #'concat (-map #'org-bbq--format-item items))))))

(provide 'org-bbq)

;;; org-bbq.el ends here
