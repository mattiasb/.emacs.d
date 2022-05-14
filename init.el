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

;;; Settings
(load (concat user-emacs-directory "custom.el"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;;; Early init code

(require 'mb-f)
(mb-f-package-install-all)
(load-theme 'madhat2r t)

;;; Post package initialization

(require 'mb-init)
(mb-init)

(provide 'init)
;;; init.el ends here
