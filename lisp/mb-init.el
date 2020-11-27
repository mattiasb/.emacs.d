;;; mb-init.el --- My after-init-hook file -*- lexical-binding: t; -*-

;; Copyright ⓒ 2013-2020 Mattias Bengtsson
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

(defun mb-init--make ()
  "Run Makefile to ensure licenses and git-hook etc is set up properly."
  (let ((default-directory user-emacs-directory))
    (message (shell-command-to-string "make"))
    (message nil)))

(defun mb-init--terminal-workarounds ()
  "Activate terminal workarounds."
  (if (getenv "TMUX")
      (tmux-keys))

  (menu-bar-mode -1)
  (evil-esc-mode)
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
  (require 'control-mode)
  (require 'lastpass)
  ;; (require 'perspective)
  (auto-sudoedit-mode)
  (auto-fill-mode 1)
  (global-git-gutter-mode)
  (global-emojify-mode)
  (global-emojify-mode-line-mode)
  (flimenu-global-mode)
  (fancy-narrow-mode)
  (ido-mode)
  (ido-vertical-mode)
  (ido-ubiquitous-mode)
  (flx-ido-mode)
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
  (powerline-major-mode)
  (powerline-default-theme)
  (yatemplate-fill-alist)
  (auto-insert-mode))

;;;

(defun mb-init ()
  "Initialize Emacs."
  (mb-init--make)
  (require 'mb-hooks)
  (mb-modes-activate)
  (mb-advices-activate)
  (mb-init--global-keybindings)
  (mb-init--modes))

(provide 'mb-init)
;;; mb-init.el ends here
