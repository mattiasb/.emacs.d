;; SPDX-License-Identifier: GPL-3.0-only
;; Copyright 2022, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fundamental-mode text-mode conf-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))
(date (completing-read "Date: "
                       (mapcar 'mb-f-get-date mb-f-time-formats)
                       nil t))

(password   (mb-f-get-password))

(name    (mb-f-get-user-full-name))
(mail    (mb-f-get-user-mail-address))
(mailbox (mb-f-get-user-mailbox))

(copyright (beginning-of-buffer)
           (mb-f-comment-start)
           ;; Make `reuse lint` pass ↓↓↓↓
           "SPDX-License-Identifier" ": " (p (spdx-identifier-only))
           (mb-f-comment-end)
           n>
           (mb-f-comment-start)
           "Copyright " (mb-f-get-year) ", "
           (mb-f-get-user-mailbox)
           (mb-f-comment-end)
           n o)

(calc (s formula) " = " (format "%s" (calc-eval formula)) q)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
prog-mode yaml-mode

(fixme (mb-f-comment-start) "FIXME: ")
(todo  (mb-f-comment-start) "TODO: ")
(bug   (mb-f-comment-start) "BUG: ")
(hack  (mb-f-comment-start) "HACK: ")
(see   (mb-f-comment-start) "See: ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
yaml-mode :when ansible

(! (beginning-of-buffer)
   "#!/usr/bin/env ansible-playbook"
   n o)

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

(var    "{{ " r " }}")
(varq "\"{{ " r " }}\"")

(with_items   "with_items: "      (i varq) q)
(with_items>- "with_items: >-" n> (i var)  q)

(with_dict    "with_dict: "       (i varq) q)
(with_dict>-  "with_dict: >-"  n> (i var)  q)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
python-mode

(!        (beginning-of-buffer)
          "#!/usr/bin/env python3"
          n o)

(def      & "def " p "(" p "):" n> q)
(if       & "if " p ":" n> q )
(elif     & "elif " p ":" n> q )
(else     & "else:" n> q)
(for      & "for " p " in " p ":" n> q)
(while    & "while " p ":" n> q)
(with     & "with " p " as " p ":"
          n> r>)
(init     & "def __init__(self):" n> (p "pass"))
(class    & "class " (p "ClassName") "(" (p "object") "):"
          n>"\"\"\"" p "\"\"\""
          n>
          n> (i init))
(from     & "from "   (p "lib") " import " (p "what")  n> q)
(import   & "import " (p "lib") n> q)
(lambda     "lambda " (p "x") ": " q)
(property o & "@property" n> "def " (p "property" prop) "(self):"
          n> "return self.__" (s field)
          n>
          n> "@" prop ".setter"
          n> "def " prop "(self, " prop "):"
          n> "self.__" field " = " prop
          n q)
(try      & "try:" n> p n "except " (p "Exception") ":"
          n> q)
(super    & (replace-regexp-in-string "\\([.]\\)[^.]+$" "(self)."
                                      (python-info-current-defun)
                                      nil nil 1)
          "(" p ")" n> q)
(@classmethod "@classmethod"
              n> (i def))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sh-mode

(! (beginning-of-buffer)
   "#!/bin/bash"
   n o)
(case & o n "case \"" p "\" in"
      n> (p "*") ")"
      n> r>
      n> "return"
      n> ";;"
      n "esac"
      o n)
(debug & o "echo \"" (r "variable" variable) ": ${" variable "}\"" o q)
(function & o n "function " (p "name") " {" n> r> n "}" o n)
(if & o n "if " (p "[   ]") "; then"
    n> r>
    n "fi"
    o n)
(elif & o n "elif " (p "[   ]") "; then"
      n> r>
      o)
(else & o n "else "
      n> r>
      o)
(for & o n "for " (p "item" item) " in " (concat "\"${" item "s" "[@]}\"") "; do"
     n> r>
     n "done"
     o n)
(heredoc & o "cat <<EOM"
         n r
         n "EOM" n)
(read-file & o n
           "while read \"" p "\"; do"
           n> r>
           n "done <\"${" (p "file") "}\""
           o n)
(select & o n "select " (p "name") " in " (p "words") "; do"
        n> r>
        n "done"
        o n)
(var  "${" r "}")
(varq "\"${" r "}\"")
(while & o n "while " p "; do"
       n> r>
       n "done"
       o n)
(alias & o "alias " (s cmd) "='" cmd " " q "'" o)

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
