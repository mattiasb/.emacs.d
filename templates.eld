;; SPDX-License-Identifier: GPL-3.0-only
;; Copyright 2022, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fundamental-mode text-mode conf-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))
(date (completing-read "Date: "
                       (mapcar 'mb-f-get-date mb-f-time-formats)
                       nil t))

(name    (mb-f-get-user-full-name))
(mail    (mb-f-get-user-mail-address))
(mailbox (mb-f-get-user-mailbox))

(copyright (mb-f-comment-start)
           "SPDX-License-Identifier: " (p (spdx-identifier-only))
           (mb-f-comment-end)
           n>
           (mb-f-comment-start)
           "Copyright " (mb-f-get-year) ", "
           (mb-f-get-user-mailbox)
           (mb-f-comment-end)
           n>)

(calc (s formula) " = " (format "%s" (calc-eval formula)) q)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
prog-mode

(fixme (mb-f-comment-start) "FIXME: ")
(todo  (mb-f-comment-start) "TODO: ")
(bug   (mb-f-comment-start) "BUG: ")
(hack  (mb-f-comment-start) "HACK: ")
(see   (mb-f-comment-start) "See: ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
yaml-mode :when ansible

(play & n
      o "- name: "  (p "name")
      n "  hosts: " (p "hosts")
      n "  tasks:"
      n
      (i task))

(task & n o "    "
      "- name: \"" (p "name") "\""
      n> (p "module") ": " q
      n)

(var    "{{ " p " }}")
(varq "\"{{ " p " }}\"")

(with_items   "with_items: "      (i varq) q)
(with_items>- "with_items: >-" n> (i var)  q)

(with_dict    "with_dict: "       (i varq) q)
(with_dict>-  "with_dict: >-"  n> (i var)  q)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sh-mode

(! "#!/bin/bash")
(function "function " (p "name") " {" n> r> n "}")
(if "if " (p "[   ]") "; then"
    n> (p ":") q
    n> "fi")
(for "for " (p "item" item) " in " (concat "\"${" item "s" "[@]}\"") "; do"
     n> (p ":") q
     n "done")
($ "\"${" q "}\"")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
emacs-lisp-mode

(autoload     ";;;###autoload")

(lambda       "lambda "       "(" p ")" n> r>)
(cond         "cond"          n "(" q "))" >)
(pcase        "pcase "        (p "scrutinee") n "(" q ")" >)
(let          "let "          "((" p "))" n> r>)
(let*         "let* "         "((" p "))" n> r>)
(dotimes      "dotimes "      "(" p ")" n> r>)
(dolist       "dolist "       "(" (s elem) " " elem "s)" n> r>)
(if           "if "           "(" p ")" n> "(" p ")" n> "(" p ")")
(defvar       "defvar "       (mb-f-elisp-get-namespace) "-" p n> "\"" p "\"")
(defvar-local "defvar-local " (mb-f-elisp-get-namespace) "-" p n> "\"" p "\"")
(defconst     "defconst "     (mb-f-elisp-get-namespace) "-" p n> "\"" p "\"")
(defalias     "defalias '"    (mb-f-elisp-get-namespace) "-" p " '" p)

(defcustom "defcustom " (mb-f-elisp-get-namespace) "-" p
  n> "\"" p "\""
  n> ":type '" p)

(defface "defface " (mb-f-elisp-get-namespace) "-" p " '((t :inherit " p "))"
  n> "\"" p "\"")

(defgroup "defgroup " (mb-f-elisp-get-namespace) "-" p " nil"
  n> "\"" p "\""
  n> ":group '" p
  n> ":prefix \"" p "-\"")

(defmacro "defmacro " (mb-f-elisp-get-namespace) "-" p " (" p ")"
          n> "\"" p "\""
          n> r>)

(defun "defun " (mb-f-elisp-get-namespace) "-" p " (" p ")"
       n> "\"" p "\""
       n> r>)

(add-hook "add-hook '" (p "mode" mode) "-hook"
          " #'" (p (concat (mb-f-elisp-get-namespace) "--" mode "-hook")))

(configure-feature &
                   n ";; " (mb-f-kebab-to-human mode)
                   n "(defun " (mb-f-elisp-get-namespace) "--" mode "-mode ()"
                   n> "\"My `" mode "' mode hook.\")"
                   n
                   n "(with-eval-after-load '" (p "feature" mode)
                   n> q
                   "(add-hook '" mode "-mode-hook #'"
                   (mb-f-elisp-get-namespace) "--" mode "-mode))"
                   o n)

(header ";;; " (mb-f-buf-name) " --- " p " -*- lexical-binding: t -*-" n n)
(meta &
      n ";; Author           : " (p (mb-f-get-user-mailbox))
      n ";; Version          : " (p (format-time-string "%Y%m%d"))
      n ";; Keywords         : " (p "local")
      n ";; Package-Requires : ((emacs " (p (mb-f-emacs-version)) "))"
      n ";; URL              : " (p "https://github.com/moonlite/.emacs.d")
      n ";; Compatibility    : GNU Emacs: " (p (mb-f-emacs-version-major)) ".x"
      n)

(sections & n
          ";;; Commentary:"
          n n
          ";;; Note:"
          n n
          ";;; Code:"
          n)

(provide "provide '" (mb-f-buf-base))
(end ";;; " (mb-f-buf-name) " ends here" n)
(provideq n "(" (i provide) ")" n (i end))

(package (i header)
         (i copyright)
         (i meta)
         (i sections)
         n q n
         (i provideq))
