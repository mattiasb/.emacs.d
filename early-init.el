;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2013-2020, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20141020
;; Keywords         : local
;; Package-Requires : ((emacs "29.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 29.x

;;; Commentary:

;; My early init file

;;; Note:

;;; Code:

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; These has to go here apparently
(let ((cache-dir (string-replace ".config" ".cache" user-emacs-directory)))
  (setcar native-comp-eln-load-path (file-name-concat cache-dir "eln-cache"))
  (setq package-user-dir (file-name-concat cache-dir "elpa")))

(provide 'early-init)
;;; early-init.el ends here
