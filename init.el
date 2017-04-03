;;; init.el --- My init file

;; Copyright ⓒ 2013-2016 Mattias Bengtsson
;;
;; This program is free software: you can redistribute it and/or modify
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
;; Author: Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Version          : 20141020
;; Keywords         : local
;; Package-Requires : ((emacs "25.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 25.x

;;; Commentary:

;;; Note:

;;; Code:

;;; Settings

(load (concat user-emacs-directory "custom.el"))

(add-to-list 'load-path "~/.local/share/emacs/site-lisp/rtags/")
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;;; Early init code

(require 'mb-fe)
(mb-fe-package-init)
(toggle-frame-maximized)
(load-theme 'madhat2r t)

;;; Post package initialization

(require 'mb-init)
(mb-init)

(provide 'init)
;;; init.el ends here
