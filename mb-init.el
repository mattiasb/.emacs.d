;;; mb-init.el --- My after-init-hook file -*- lexical-binding: t; -*-

;; Copyright ⓒ 2013-2016 Mattias Bengtsson

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20160417
;; Keywords         : init
;; Package-Requires : ((emacs "25.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 25.x

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

;;; Commentary:

;;; Note:

;;; Code:

(require 'mb-f "~/.emacs.d/lisp/mb-f.el")
(require 'mb-keys "~/.emacs.d/mb-keys.el")

(defun mb-init--terminal-workarounds ()
  "Activate terminal workarounds."
  (if (getenv "TMUX")
      (tmux-keys))

  (evil-esc-mode)
  (my/set-terminal-cursors))

(defun mb-init--global-keybindings ()
  "Activate global keybindings."

  (mb-keys-activate)
  (windmove-default-keybindings))

(defun mb-init--visual-regexp ()
  "Activate visual-regexp."
  (require 'visual-regexp-steroids)
  (my/define-keys esc-map
                  '(( "C-r" . vr/isearch-backward)
                    ( "C-s" . vr/isearch-forward))))

(defun mb-init--yas ()
  "Activate YASnippet."
  ;; This needs to be set here, or customize will bork.
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode))

(defun mb-init--yatemplate ()
  "Activate YATemplate."

  (yatemplate-fill-alist)
  (auto-insert-mode 1))

(defvar god-mode-isearch-map)
(defun mb-init--god-mode-isearch ()
  "Activate `god-mode-isearch'."
  (require 'god-mode-isearch)
  (my/define-keys isearch-mode-map
                  '(( "<escape>" . god-mode-isearch-activate)
                    ( "<insert>" . god-mode-isearch-activate)))
  (my/define-keys god-mode-isearch-map
                  '(( "g"        . isearch-cancel)
                    ( "i"        . god-mode-isearch-disable)
                    ( "<insert>" . god-mode-isearch-disable))))

(defun mb-init--control-mode ()
  "Activate Control Mode."
  (require 'control-mode)
  (add-hook 'after-change-major-mode-hook
            #'my/control-mode-set-cursor)
  (global-control-mode)
  (mb-init--god-mode-isearch))

(defun mb-init--modes ()
  "Activate a bunch of global modes."
  (unless (display-graphic-p)
    (mb-init--terminal-workarounds))
  (mb-init--control-mode)
  (powerline-major-mode)
  (powerline-default-theme)
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
  (mb-init--god-mode-isearch)
  (mb-init--visual-regexp)
  (mb-init--yas)
  (mb-init--yatemplate))

;;;

(defun mb-init ()
  "Initialize Emacs."
  (mb-init--global-keybindings)
  (mb-init--modes))

(provide 'mb-init)
;;; mb-init.el ends here
