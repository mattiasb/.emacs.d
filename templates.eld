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
yaml-mode :when ansible

(play "- name: " (p "name" name)
      n> "hosts: " (p "hosts" hosts)
      n> "tasks:"
      n> q)
(task >
      "- name: \"" (p "name" name) "\""
      n>
      (p "module" module) ":"
      n> q)

(task "\"{{ " q " }}\"")
(var "\"{{ " q " }}\"")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sh-mode

(! "#!/bin/bash")
(function "function " (p "name") " {" n> r> n "}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
emacs-lisp-mode

(autoload     ";;;###autoload")
(lambda       "lambda (" p ")" n> r>)
(defvar       "defvar " p "\n  \"" p "\"")
(defvar-local "defvar-local " p "\n  \"" p "\"")
(defconst     "defconst " p "\n  \"" p "\"")
(defcustom    "defcustom " p "\n  \"" p "\"" n> ":type '" p)
(defface      "defface " p " '((t :inherit " p "))\n  \"" p "\"")
(defgroup     "defgroup " p " nil"
  n> "\"" p "\""
  n> ":group '" p
  n> ":prefix \"" p "-\"")
(defmacro     "defmacro " p " (" p ")\n  \"" p "\"" n> r>)
(defalias     "defalias '" p " '" p)
(defun        "defun " p " (" p ")\n  \"" p "\"" n> r>)
(cond         "cond" n "(" q "))" >)
(pcase        "pcase " (p "scrutinee") n "(" q ")" >)
(let          "let ((" p "))" n> r>)
(let*         "let* ((" p "))" n> r>)
(dotimes      "dotimes (" p ")" n> r>)
(dolist       "dolist (" p ")" n> r>)
(provide      "provide '"
              (file-name-base (or (buffer-file-name) (buffer-name)))
              ")"
              n
              ";;; "
              (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
              " ends here" n)
