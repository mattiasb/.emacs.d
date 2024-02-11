;;; mb-advices.el --- My advices -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2017-2020, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version	    : 20170308
;; Keywords	    : local
;; Package-Requires : ((emacs "29.1"))
;; URL		    : https://github.com/moonlite/.emacs.d
;; Doc URL	    : TBA
;; Compatibility    : GNU Emacs: 29.x

;;; Commentary:

;; My custom advices.

;;; Note:

;;; Code:

(require 'term)
(require 'mb-cmd)

(defun mb-advices-drop-args (func)
  "Wrap FUNC such that it accepts but drops all ARGS."
  (lambda (&rest _args) (funcall func)))

(defun mb-advices-switch-to (buffer)
  "Advice DESCRIBE-FUNCTION to switch to BUFFER after popping it up."
  (lambda (&rest _args) (mb-f-select-buffer buffer)))

(defun mb-advices-before (funcs advice)
  "ADVICE a bunch of FUNCS."
  (dolist (func funcs)
    (advice-add func :before advice)))

(defun mb-advices-around (funcs advice)
  "ADVICE a bunch of FUNCS."
  (dolist (func funcs)
    (advice-add func :around advice)))

(defun mb-advices-after (funcs advice)
  "ADVICE a bunch of FUNCS."
  (dolist (func funcs)
    (advice-add func :after advice)))

(defun mb-advices-activate ()
  "Activate my advices."
  (mb-f-req 'eat)

  (mb-advices-after '(backward-page forward-page)
                    (mb-advices-drop-args #'recenter))

  (mb-advices-before '(ansi-term eat)
                     (lambda (&rest _)
                       (interactive (list "/bin/bash"))))

  (advice-add 'comint-send-eof
              :after
              (lambda ()
                (kill-buffer (current-buffer))))

  (advice-add #'save-buffers-kill-emacs
              :around (lambda (func &rest args)
                        (cl-flet ((process-list ()))
                          (apply func args))))

  (advice-add #'save-buffers-kill-terminal
              :before (lambda (&rest _)
                        (require 'server)
                        (unless (display-graphic-p)
                          (mb-f-reset-terminal-cursors))))

  (advice-add #'ask-user-about-lock
              :around (lambda (func file opponent)
                        (if (and (boundp 'recentf-save-file)
                                 (eq recentf-save-file (expand-file-name file)))
                            t
                          (funcall func file opponent))))

  (advice-add #'kill-line
              :before (lambda (&optional _)
                        (when (and (eolp) (not (bolp)))
                          (save-excursion
                            (forward-char 1)
                            (just-one-space 1))))))

(provide 'mb-advices)
;;; mb-advices.el ends here
