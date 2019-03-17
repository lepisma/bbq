;;; bbq.el --- Interface for bbq

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.13.0") (org-cliplink) (s "1.12.0") (emacs "25"))
;; URL: https://github.com/lepisma/bbq

;;; Commentary:

;; Interfaces mpm with Emacs, allowing easy music clipping
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

(require 'dash)
(require 'org-cliplink)
(require 's)

(defun bbq-detect-metadata (page-title)
  "Try to figure out song metadata from PAGE-TITLE.
Return a cons cell like this (artist . title).
TODO: Save artists for better heuristic."
  (let ((parts (->> page-title
                  (s-split "-")
                  butlast
                  (mapcar #'s-trim)
                  (mapcar #'s-collapse-whitespace))))
    (if (= (length parts) 2)
        ;; This is probably artist - title pair
        (apply #'cons parts)
      ;; This is weird, just let the user decide
      (let ((main-title (s-join "-" parts)))
        (cons main-title main-title)))))

(defun bbq-url-valid? (url)
  (let ((url-main (->> url
                     (s-chop-prefix "http://")
                     (s-chop-prefix "https://")
                     (s-chop-prefix "www."))))
    (or (s-starts-with? "youtube.com" url-main)
        (s-starts-with? "youtu.be" url-main))))

(defun bbq-insert-cmd (artist title url)
  "Insert command for use in slime repl."
  (insert (pp-to-string `(bbq-db:add-song ,artist ,title ,url :album nil))))

;;;###autoload
(defun bbq-cliplink ()
  "Save the current link to mpm database."
  (interactive)
  (let ((url (substring-no-properties (current-kill 0))))
    (if (bbq-url-valid? url)
        (org-cliplink-retrieve-title url (lambda (url page-title)
                                           (let ((meta (bbq-detect-metadata page-title)))
                                             (bbq-insert-cmd (car meta) (cdr meta) url))))
      (message "Not a valid url"))))

(provide 'bbq)

;;; bbq.el ends here
