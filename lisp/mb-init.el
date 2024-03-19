;;; mb-init.el --- My after-init-hook file -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2022, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20160417
;; Keywords         : local
;; Package-Requires : ((emacs "29.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 29.x

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
  (mb-f-req 'tmux-keys)
  (if (getenv "TMUX")
      (tmux-keys))

  (menu-bar-mode -1)
  (mb-f-set-terminal-cursors)

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
  (mb-f-req 'su)
  (mb-f-req 'jinx)
  (mb-f-req 'git-gutter)
  (mb-f-req 'corfu)
  (mb-f-req 'doom-modeline)
  (mb-f-req 'history)
  (mb-f-req 'vertico)
  (mb-f-req 'all-the-icons-completion)
  (mb-f-req 'which-key)
  (mb-f-req 'auto-compile)
  (mb-f-req 'browse-kill-ring)
  (mb-f-req 'easy-repeat)
  (mb-f-req 'smart-region)
  (mb-f-req 'projectile)
  (mb-f-req 'auto-dim-other-buffers)
  (mb-f-req 'envrc)
  (mb-f-req 'auto-dark)
  (mb-f-req 'hl-anything)
  (mb-f-req 'treesit-auto)
  (auto-dark-mode)
  (su-mode +1)
  (auto-fill-mode 1)
  (hl-highlight-mode)
  (global-git-gutter-mode)
  (global-corfu-mode)
  (global-goto-address-mode)
  (doom-modeline-mode)
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
  ;; TODO(mattiasb): Change keybindings and re-enable.
  ;; (winner-mode)
  (abbrev-mode)
  (electric-layout-mode)
  (electric-pair-mode)
  (projectile-mode)
  (auto-dim-other-buffers-mode)
  (envrc-global-mode)
  (global-treesit-auto-mode))

;;;

(defun mb-init ()
  "Initialize Emacs."
  (require 'mb-hooks)
  (mb-f-make-cache-dirs)
  (mb-f-package-install-all)
  (mb-modes-activate)
  (mb-advices-activate)
  (mb-init--global-keybindings)
  (mb-init--modes))

(provide 'mb-init)
;;; mb-init.el ends here
