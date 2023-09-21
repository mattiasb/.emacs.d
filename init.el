;;; init.el --- My init file

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2013-2022, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20141020
;; Keywords         : local
;; Package-Requires : ((emacs "27.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 27.x

;;; Commentary:

;; My init file

;;; Note:

;;; Code:

(cd "~")

;;; Early init code
(eval-and-compile
  (add-to-list 'load-path (concat user-emacs-directory "lisp/")))

(make-directory "~/.cache/emacs/elpa" t)
(require 'mb-custom)
(require 'mb-custom-extra)
(require 'mb-loadpaths)
(require 'mb-f)
(require 'mb-init)
(mb-init)

(provide 'init)
;;; init.el ends here
