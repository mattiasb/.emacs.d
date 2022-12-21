;;; mb-init.el --- My after-init-hook file -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2022, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20160417
;; Keywords         : local
;; Package-Requires : ((emacs "27.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 27.x

;;; Commentary:

;; My after-init-hook file

;;; Note:

;;; Code:

(require 'mb-f)
(require 'mb-keys)
(require 'mb-modes)
(require 'mb-advices)

(defun mb-init--terminal-workarounds ()
  "Activate terminal workarounds."
  (if (getenv "TMUX")
      (tmux-keys))

  (menu-bar-mode -1)
  (mb-f-set-terminal-cursors)
  (diff-hl-margin-mode)
  ;; TODO: Fix this. It currently borks whitespace-mode
  (mb-f-set-terminal-window-separator))

(defun mb-init--global-keybindings ()
  "Activate global keybindings."

  (mb-keys-activate)
  (windmove-default-keybindings))

(defun mb-init--modes ()
  "Activate a bunch of global modes."
  (unless (display-graphic-p)
    (mb-init--terminal-workarounds))

  (require 'iso-transl)
  (require 'dired-imenu)
  (require 'visual-regexp-steroids)
  (require 'lastpass)
  ;; Needed for license snippet
  (require 'spdx)

  (su-mode +1)
  (auto-fill-mode 1)
  (global-diff-hl-mode)
  (global-emojify-mode)
  (global-emojify-mode-line-mode)
  (global-corfu-mode)
  (flimenu-global-mode)
  (doom-modeline-mode)
  (fancy-narrow-mode)
  (history-mode)
  (savehist-mode)
  (vertico-mode)
  (all-the-icons-completion-mode)
  (bury-successful-compilation)
  (which-key-mode)
  (auto-insert-mode)
  (auto-compile-on-save-mode)
  (auto-compile-on-load-mode)
  (browse-kill-ring-default-keybindings)
  (easy-repeat-mode)
  (smart-region-on)
  (recentf-mode)
  (abbrev-mode)
  (projectile-mode)
  (auto-dim-other-buffers-mode)
  (yas-global-mode)
  (yatemplate-fill-alist)
  (auto-insert-mode))

;;;

(defun mb-init ()
  "Initialize Emacs."
  (require 'mb-hooks)
  (mb-modes-activate)
  (mb-advices-activate)
  (mb-init--global-keybindings)
  (mb-init--modes))

(provide 'mb-init)
;;; mb-init.el ends here
