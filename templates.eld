;; SPDX-License-Identifier: GPL-3.0-only
;; Copyright 2022, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fundamental-mode text-mode conf-mode ;; Available everywhere

;; Make sure there's always an auto-insert to override
(auto-insert "")

(today (format-time-string "%Y-%m-%d"))
(date (completing-read "Date: "
                       (mapcar 'mb-f-get-date mb-f-time-formats)
                       nil t))
(year (mb-f-get-year))

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

(divider o n (make-string 80 (if comment-start (aref comment-start 0) ?\=))
         n
         :pre (beginning-of-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
markdown-mode

;; TODO: Convert filename to `A Nice Header`.
(auto-insert "# " (p (mb-f-buf-base)) " #"
             n n q
             :pre (beginning-of-buffer))

(== (p "Header 1" header)
    n (make-string (string-width header) ?\=)
    n n)

(-- (p "Header 2" header)
    n (make-string (string-width header) ?\-)
    n n)

(------ & n o (make-string 80 ?-) n)

(h1 "# "      (r "Header 1") " #"      n n q)
(h2 "## "     (r "Header 2") " ##"     n n q)
(h3 "### "    (r "Header 3") " ###"    n n q)
(h4 "#### "   (r "Header 4") " ####"   n n q)
(h5 "##### "  (r "Header 5") " #####"  n n q)
(h6 "###### " (r "Header 6") " ######" n n q)

(img   "![" (p "Text") "](" (r "") ")" q)
(rimg  "![" (p "Text") "][" p      "]" q)
(link   "[" (r "Text") "](" p      ")" q)
(rlink  "[" (r "Text") "][" p      "]" q)

(ref    "[" (p "1") "]: " (r "https://") q)

(code  "```" (p "format") n r n "```")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
prog-mode yaml-mode

(fixme (mb-f-comment-start) "FIXME: ")
(todo  (mb-f-comment-start) "TODO: ")
(todo  (mb-f-comment-start) "NOTE: ")
(bug   (mb-f-comment-start) "BUG: ")
(hack  (mb-f-comment-start) "HACK: ")
(see   (mb-f-comment-start) "See: ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
yaml-mode

(auto-insert "---" n :pre (beginning-of-buffer))

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
;; Can't get this to be `jinja2-mode yaml-mode :when ansible` so that I can
;; share the var definitions. :/
jinja2-mode

(var    "{{ " r " }}")
(varq "\"{{ " r " }}\"")

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

(auto-insert n "set -e"
             n "set -o pipefail"
             n n
             :pre (tempel-insert '!))
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

(auto-insert :post (tempel-insert 'pkg))

(autoload     ";;;###autoload")

(lambda  "lambda "   "(" p ")" n> r>                     :pre (mb-f-wrap-point))
(cond    "cond"      n "(" q "))" >                      :pre (mb-f-wrap-point))
(pcase   "pcase "    (p "var") n "(" q ")" >             :pre (mb-f-wrap-point))
(let     "let "      "((" p "))" n> r>                   :pre (mb-f-wrap-point))
(let*    "let* "     "((" p "))" n> r>                   :pre (mb-f-wrap-point))
(dotimes "dotimes "  "(" p ")" n> r>                     :pre (mb-f-wrap-point))
(dolist  "dolist "   "(" (s elem) " " elem "s)" n> r>    :pre (mb-f-wrap-point))
(if      "if "       "(" p ")" n> "(" p ")" n> "(" p ")" :pre (mb-f-wrap-point))

(defvar       "defvar "       (mb-f-elisp-get-namespace) "-" p n> "\"" p "\""
  :pre (mb-f-wrap-point))
(defvar-local "defvar-local " (mb-f-elisp-get-namespace) "-" p n> "\"" p "\""
  :pre (mb-f-wrap-point))
(defconst     "defconst "     (mb-f-elisp-get-namespace) "-" p n> "\"" p "\""
  :pre (mb-f-wrap-point))
(defalias     "defalias '"    (mb-f-elisp-get-namespace) "-" p " '" p
  :pre (mb-f-wrap-point))

(defcustom "defcustom " (mb-f-elisp-get-namespace) "-" p
  n> "\"" p "\""
  n> ":type '" p
  :pre (mb-f-wrap-point))

(defface "defface " (mb-f-elisp-get-namespace) "-" p " '((t :inherit " p "))"
  n> "\"" p "\""
  :pre (mb-f-wrap-point))

(defgroup "defgroup " (mb-f-elisp-get-namespace) "-" p " nil"
  n> "\"" p "\""
  n> ":group '" p
  n> ":prefix \"" p "-\"")

(defmacro "defmacro " (mb-f-elisp-get-namespace) "-" p " (" p ")"
          n> "\"" p "\""
          n> r>
          :pre (mb-f-wrap-point))

(defun "defun " (mb-f-elisp-get-namespace) "-" p " (" p ")"
       n> "\"" p "\""
       n> r>
       :pre (mb-f-wrap-point))

(cmd (i autoload) n
     "(defun " (mb-f-elisp-get-namespace) "-" p " (" p ")"
     n> "\"" p "\""
     n> "(interactive)"
     n> r> ")"
     o n)

(add-hook "add-hook '" (p "mode" mode) "-hook"
          " #'" (p (concat (mb-f-elisp-get-namespace) "--" mode "-hook"))
          :pre (mb-f-wrap-point))

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

(pkg ";;; " (mb-f-buf-name) " --- " p " -*- lexical-binding: t -*-" n n
     n ";; Author           : " (p (mb-f-get-user-mailbox))
     n ";; Version          : " (p (format-time-string "%Y%m%d"))
     n ";; Keywords         : " (p "local")
     n ";; Package-Requires : ((emacs " (p (mb-f-emacs-version)) "))"
     n ";; URL              : " (p "https://github.com/mattiasb/.emacs.d")
     n ";; Compatibility    : GNU Emacs: " (p (mb-f-emacs-version-major)) ".x"
     n n
     ";;; Commentary:"
     n n
     ";;; Note:"
     n n
     ";;; Code:"
     n n n q
     :pre (beginning-of-buffer)
     :post (tempel-insert 'foot))

(foot & n
      "(provide '" (mb-f-buf-base) ")"
      n
      ";;; " (mb-f-buf-name) " ends here" n q
      :pre (end-of-buffer)
      :post (previous-line 4))

