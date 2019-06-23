(in-package #:cl-user)
(defpackage #:bbq-test
  (:use #:cl #:prove))

(in-package #:bbq-test)

(let ((maps (list (cons "http://youtube.googleapis.com/v/4e_kz79tjb8?version=3" "4e_kz79tjb8")
                  (cons "https://www.youtube.com/watch?feature=g-vrec&v=Y1xs_xPb46M" "Y1xs_xPb46M")
                  (cons "http://www.youtube.com/watch?feature=player_embedded&v=Ab25nviakcw#" "Ab25nviakcw")
                  (cons "http://youtu.be/Ab25nviakcw" "Ab25nviakcw")
                  (cons "http://www.youtube.com/watch?v=Ab25nviakcw" "Ab25nviakcw")
                  (cons "<iframe width=\"420\" height=\"315\" src=\"http://www.youtube.com/embed/Ab25nviakcw\" frameborder=\"0\" allowfullscreen></iframe>" "Ab25nviakcw")
                  (cons "<object width=\"420\" height=\"315\"><param name=\"movie\" value=\"http://www.youtube-nocookie.com/v/Ab25nviakcw?version=3&amp;hl=en_US\"></param><param name=\"allowFullScreen\" value=\"true\"></param><param name=\"allowscriptaccess\" value=\"always\"></param><embed src=\"http://www.youtube-nocookie.com/v/Ab25nviakcw?version=3&amp;hl=en_US\" type=\"application/x-shockwave-flash\" width=\"420\" height=\"315\" allowscriptaccess=\"always\" allowfullscreen=\"true\"></embed></object>" "Ab25nviakcw")
                  (cons "http://i1.ytimg.com/vi/Ab25nviakcw/default.jpg" "Ab25nviakcw")
                  (cons "https://www.youtube.com/watch?v=BGL22PTIOAM&feature=g-all-xit" "BGL22PTIOAM")
                  (cons "BGL22PTIOAM" "BGL22PTIOAM"))))
  (loop for url-map in maps
        do (is (yt:url-id (car url-map)) (cdr url-map))))
