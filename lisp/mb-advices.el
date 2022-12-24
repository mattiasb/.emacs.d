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

(defun mb-advices--git-link (func &rest args)
  "Support http for work by advicing FUNC w/ ARGS."
  (let ((url (apply func args)))
    (replace-regexp-in-string "^\\(https://git.smarteye\\).*"
                              "http://git.smarteye"
                              url
                              nil
                              nil
                              1)))

(defun mb-advices-switch-to (buffer)
  "Advice DESCRIBE-FUNCTION to switch to BUFFER after popping it up."
  (lambda (&rest _args) (select-window (get-buffer-window buffer))))

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

  (mb-advices-after '(backward-page forward-page) #'recenter)

  (advice-add #'flycheck-list-errors
              :after (mb-advices-switch-to "*Flycheck errors*"))
  (advice-add #'projectile-ripgrep
              :after (mb-advices-switch-to "*ripgrep-search*"))
  (advice-add #'display-local-help
              :after (mb-advices-switch-to "*eldoc*"))
  (advice-add #'diff-buffer-with-file
              :after (mb-advices-switch-to "*Diff*"))
  (advice-add #'projectile-compile-project
              :after (mb-advices-switch-to "*compilation*"))

  (mb-advices-after '(package-menu-describe-package
                      describe-variable
                      describe-mode
                      describe-function
                      describe-bindings
                      describe-symbol
                      describe-package
                      describe-theme)
                    (mb-advices-switch-to "*Help*"))

  (mb-advices-around '(git-link-gitlab
                       git-link-commit-github
                       git-link-homepage-github)
                     #'mb-advices--git-link)

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
              :before (lambda (&rest _)
                        (require 'server)
                        (unless (display-graphic-p)
                          (mb-f-reset-terminal-cursors))))

  (advice-add #'ask-user-about-lock
              :around (lambda (func file opponent)
                        (if (eq recentf-save-file (expand-file-name file))
                            t
                          (funcall func file opponent))))

  (advice-add #'kill-line
              :before (lambda (&optional _)
                        (when (and (eolp) (not (bolp)))
                          (save-excursion
                            (forward-char 1)
                            (just-one-space 1)))))

  ;; See: https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize " → " 'face 'vertico-current)
                   "   ")
                 cand))))


(provide 'mb-advices)
;;; mb-advices.el ends here
