;; SPDX-License-Identifier: GPL-3.0-only
;; Copyright 2022, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fundamental-mode text-mode conf-mode ;; Available everywhere

(today (p (format-time-string "%Y-%m-%d")))

(date (completing-read "Date: "
                       (mapcar 'mb-f-get-date mb-f-time-formats)
                       nil t))

(copyright (mb-f-comment-start)
           (spdx-license-format)
           (mb-f-comment-end)
           n>
           (mb-f-comment-start)
           "Copyright " (mb-f-get-year) ", "
           (mb-f-get-user-full-name) " "
           "<" (mb-f-get-user-mail-address) ">"
           (mb-f-comment-end)
           n>)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
prog-mode

(fixme (mb-f-comment-start) "FIXME: ")
(todo  (mb-f-comment-start) "TODO: ")
(bug   (mb-f-comment-start) "BUG: ")
(hack  (mb-f-comment-start) "HACK: ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sh-mode

(! "#!/bin/bash")
(function "function " (p "name") " {" n> r> n "}")
