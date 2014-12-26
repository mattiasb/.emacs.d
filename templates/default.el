;;; `(buffer-name)` --- $1 -*- lexical-binding: t; -*-

;; Copyright (C) ${2:`(my/insert-year)`} ${3:`(if (boundp 'user-full-name) user-full-name "Author")`}

;; Author: ${4:$3} <${5:`(if (boundp 'user-mail-address) user-mail-address "user@example.com")`}>
;; Version: ${6:`(my/insert-date '(4))`}
;; Keywords: ${7:none}
;; Package-Requires: ($8)
;; URL: ${9:TBA}
;; Doc URL: ${10:TBA}
;; Compatibility: ${11:GNU Emacs: 24.x}

;; ${12:$$(yas-choose-value (directory-files "~/.emacs.d/snippets/licenses/" nil "^[A-Za-z0-9-+_][A-Za-z0-9-+_.]*$"))}

;;; Commentary:

;;; Note:

;;; Code:

$0

(provide '`(file-name-sans-extension (buffer-name))`)
;;; `(buffer-name)` ends here
