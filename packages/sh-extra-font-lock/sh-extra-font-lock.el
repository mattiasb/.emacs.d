;;; sh-extra-font-lock.el --- Extra font-locks for sh-mode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2015, Anders Lindgren <https://www.emacswiki.org/emacs/AndersLindgren>

;; Author: Anders Lindgren <https://www.emacswiki.org/emacs/AndersLindgren>
;; Version: 20170222
;; Keywords: languages
;; Package-Requires: ()
;; URL: http://emacs.stackexchange.com/a/13408/450
;; Compatibility: GNU Emacs: 24.x

;;; Commentary:

;; This package provides syntax highlighting of variables inside strings for
;; `sh-mode'.
;;
;; Written by Anders Lindgren and packaged up (including some docstring and
;; naming fixes) by Mattias Bengtsson.
;;
;; To activate this just add the following to your `init.el':
;; (add-hook 'sh-mode-hook 'sh-extra-font-lock-activate)

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
