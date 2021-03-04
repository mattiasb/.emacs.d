;;; mb-advices.el --- My advices -*- lexical-binding: t; -*-

;; Copyright ⓒ 2017-2020 Mattias Bengtsson
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
;; Package-Requires : ((emacs "27.1"))
;; URL		    : https://github.com/moonlite/.emacs.d
;; Doc URL	    : TBA
;; Compatibility    : GNU Emacs: 27.x

;;; Commentary:

;; My custom advices.

;;; Note:

;;; Code:

(require 'term)
(require 'god-mode-isearch)
(require 'mb-cmd)
(require 'flycheck)
(require 'flycheck-pos-tip)

(defun mb-advices-activate ()
  "Activate my advices."

  (advice-add #'isearch-forward-symbol-at-point
              :after  #'god-mode-isearch-activate)
  (advice-add #'mb-cmd-isearch-backward-symbol-at-point
              :after  #'god-mode-isearch-activate)

  (advice-add #'ialign
              :around (lambda (func &rest args)
                        (unwind-protect (apply func args))))

  (advice-add #'backward-page :after  #'recenter)
  (advice-add #'forward-page  :after  #'recenter)

  (mapc #'mb-f-advice-other-window-after
        '(projectile-ag
          projectile-ripgrep
          projectile-compile-project
          flycheck-list-errors
          diff-buffer-with-file))

  (mapc #'mb-f-advice-describe-func
        '(package-menu-describe-package
          describe-variable
          describe-mode
          describe-function
          describe-bindings
          describe-symbol
          describe-package
          describe-theme))

  ;; Kill terminal buffer when the terminal process exits
  (advice-add #'term-sentinel
              :after (lambda (proc _)
                       (when (memq (process-status proc) '(signal exit))
                         (kill-buffer (process-buffer proc)))))

  (advice-add #'projectile-switch-project-by-name
              :before
              (lambda (project-path &optional arg &rest _)
                (let ((project-name (funcall projectile-project-name-function
                                             project-path))
                      (tab-names (mapcar (lambda (tab)
                                           (alist-get 'name tab))
                                         (tab-bar-tabs))))
                  (if (member project-name tab-names)
                      (tab-bar-select-tab-by-name project-name)
                    (tab-bar-new-tab)
                    (tab-bar-rename-tab project-name)))))

  (advice-add #'ansi-term
              :before (lambda (&rest _)
                        (interactive (list "/bin/bash"))))

  (advice-add #'save-buffers-kill-emacs
              :around (lambda (func &rest args)
                        (cl-flet ((process-list ()))
                          (apply func args))))

  (advice-add #'save-buffers-kill-terminal
              :around (lambda (func &rest args)
                        (require 'server)
                        (unless (display-graphic-p)
                          (mb-f-reset-terminal-cursors))
                        (apply func args)))

  (advice-add #'flycheck-pos-tip-error-messages
              :around (lambda (func &rest args)
                        (let ((x-gtk-use-system-tooltips nil))
                          (apply func args))))

  (advice-add #'ask-user-about-lock
              :around (lambda (file opponent)
                        (let ((efile (expand-file-name file))
                              (erecentf (format "%s/recentf"
                                                user-emacs-directory)))
                          (if (eq efile erecentf)
                              t
                            (ask-user-about-lock file opponent))))))


(provide 'mb-advices)
;;; mb-advices.el ends here
