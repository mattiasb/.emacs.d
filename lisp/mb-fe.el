;;; mb-fe.el --- Defuns needed during early emacs boot -*- lexical-binding: t; -*-

;; Copyright ⓒ 2017 Mattias Bengtsson
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

;; Version	    : 20170308
;; Keywords	    : local
;; Package-Requires : ((emacs "25.1"))
;; URL		    : https://github.com/moonlite/.emacs.d
;; Doc URL	    : TBA
;; Compatibility    : GNU Emacs: 25.x

;;; Commentary:

;;; Note:

;;; Code:

(require 'cl)

(cl-flet ((always-yes (&rest _) t))
  (defun mb-fe-no-confirm (fun &rest args)
    "Apply FUN to ARGS, skipping user confirmations."
    (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
              ((symbol-function 'yes-or-no-p) #'always-yes))
      (apply fun args))))

(defun mb-fe-package-init ()
  "Initialize the package system."
  (package-initialize)
  (unless (seq-every-p #'package-installed-p
                       package-selected-packages)
    (package-refresh-contents)
    (mb-fe-install-packages-in-dir (concat user-emacs-directory "packages/"))
    (mb-fe-no-confirm #'package-install-selected-packages)))

(defun mb-fe-install-packages-in-dir (directory)
  "Install all packages in DIRECTORY."
  (mapc #'package-install-file
        (directory-files directory t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))

(provide 'mb-fe)
;;; mb-fe.el ends here
