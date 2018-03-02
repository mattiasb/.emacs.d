;;; mb-advices.el --- My advices -*- lexical-binding: t; -*-

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
  (advice-add #'popup-create
              :before #'mb-f-fci-turn-off)
  (advice-add #'popup-delete
              :after  #'mb-f-fci-turn-on)

  (advice-add #'ialign
              :around (lambda (func &rest args)
                        (mb-f-fci-turn-off)
                        (unwind-protect
                            (apply func args)
                          (mb-f-fci-turn-on))))

  (advice-add #'backward-page :after  #'recenter)
  (advice-add #'forward-page  :after  #'recenter)

  (advice-add #'rtags-show-target-in-other-window
              :after  #'balance-windows)

  (mapc #'mb-f-advice-other-window-after
        '(rtags-find-all-references-at-point
          rtags-find-references
          rtags-find-references-at-point
          rtags-find-references-current-dir
          rtags-find-references-current-file
          rtags-references-tree
          rtags-show-target-in-other-window
          projectile-ag
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

  (advice-add #'ansi-term
              :before (lambda (&rest _)
                        (interactive (list "/bin/bash"))))

  (advice-add #'custom-save-all
              :around (lambda (func &rest args)
                        (let ((print-quoted t))
                          (apply func args))))

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
                          (apply func args)))))


(provide 'mb-advices)
;;; mb-advices.el ends here
