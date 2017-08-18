;;; mb-init.el --- My after-init-hook file -*- lexical-binding: t; -*-

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

;; Version          : 20160417
;; Keywords         : local
;; Package-Requires : ((emacs "25.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 25.x

;;; Commentary:

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
  (evil-esc-mode)
  (mb-f-set-terminal-cursors)
  (mb-f-set-terminal-window-separator))

(defun mb-init--global-keybindings ()
  "Activate global keybindings."

  (mb-keys-activate)
  (windmove-default-keybindings))

(defun mb-init--yas ()
  "Activate YASnippet."
  ;; This needs to be set here, or customize will bork.
  (setq-default yas-snippet-dirs (concat user-emacs-directory "snippets/"))
  (yas-global-mode))

(defun mb-init--yatemplate ()
  "Activate YATemplate."

  (yatemplate-fill-alist)
  (auto-insert-mode 1))

(defun mb-init--modes ()
  "Activate a bunch of global modes."
  (unless (display-graphic-p)
    (mb-init--terminal-workarounds))
  ;; Make ^ work
  (require 'iso-transl)
  (require 'dired-imenu)
  (require 'visual-regexp-steroids)
  (require 'control-mode)
  (require 'perspective)

  (global-git-gutter-mode)
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
  (global-aggressive-indent-mode)
  (recentf-mode)
  (abbrev-mode)
  (projectile-mode)
  (auto-dim-other-buffers-mode)
  (powerline-major-mode)
  (powerline-default-theme)
  (mb-init--yas)
  (mb-init--yatemplate))

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
