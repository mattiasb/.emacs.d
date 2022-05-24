;;; mb-advices.el --- My advices -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2017-2020, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
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
(require 'mb-cmd)
(require 'flycheck)
(require 'flycheck-pos-tip)

(defun mb-advices-activate ()
  "Activate my advices."

  (advice-add #'backward-page :after  #'recenter)
  (advice-add #'forward-page  :after  #'recenter)

  (mapc #'mb-f-advice-other-window-after
        '(projectile-ag
          projectile-compile-project
          flycheck-list-errors
          diff-buffer-with-file))

  (mb-f-advice-describe-func #'projectile-ripgrep "*ripgrep-search*")
  (mb-f-advice-describe-func #'display-local-help "*eldoc*")

  (mapc #'mb-f-advice-describe-func
        '(package-menu-describe-package
          describe-variable
          describe-mode
          describe-function
          describe-bindings
          describe-symbol
          describe-package
          describe-theme))

  (advice-add 'comint-send-eof
              :after
              (lambda ()
                (kill-buffer (current-buffer))))

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
                            (ask-user-about-lock file opponent)))))

  (advice-add #'kill-line
              :before (lambda (&optional arg)
                        (when (and (eolp) (not (bolp)))
                          (save-excursion
                            (forward-char 1)
                            (just-one-space 1))))))


(provide 'mb-advices)
;;; mb-advices.el ends here
