;;; sh-extra-font-lock.el --- Extra font-locks for sh-mode -*- lexical-binding: t; -*-

;; Copyright ⓒ 2015 Anders Lindgren
;;
;; sh-extra-font-lock is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with This program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Author: Anders Lindgren <andlind@gmail.com>
;;         Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Version: 20150625
;; Keywords: sh
;; Package-Requires: ()
;; URL: http://emacs.stackexchange.com/a/13408/450
;; Compatibility: GNU Emacs: 24.x

;;; Commentary:

;; `sh-extra-font-lock' provides syntax highlighting of variables inside
;; strings for `sh-mode'. Written by Anders Lindgren and packaged up
;; (including some docstring and naming fixes) by Mattias Bengtsson.
;;
;; To activate this just add the following to your `init.el':
;; (add-hook 'sh-mode-hook 'sh-script-extra-font-lock-activate)

;;; Note:

;;; Code:

(defun sh-extra-font-lock--is-in-double-quoted-string ()
  "Non-nil if point in inside a double-quoted string."
  (let ((state (syntax-ppss)))
    (eq (nth 3 state) ?\")))

(defun sh-extra-font-lock--match-var-in-double-quoted-string (limit)
  "Search for variables in double-quoted strings bounded by LIMIT."
  (let (res)
    (while
        (and (setq res
                   (re-search-forward
                    "\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\)"
                    limit t))
             (not (sh-extra-font-lock--is-in-double-quoted-string))))
    res))

(defvar sh-extra-font-lock--keywords
  '((sh-extra-font-lock--match-var-in-double-quoted-string
     (2 font-lock-variable-name-face prepend))))

;;;###autoload
(defun sh-extra-font-lock-activate ()
  "Activate sh-extra-font-lock."
  (interactive)
  (font-lock-add-keywords nil sh-extra-font-lock--keywords)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

(provide 'sh-extra-font-lock)
;;; sh-extra-font-lock.el ends here
