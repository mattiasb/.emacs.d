;;; my-after-init.el --- My after-init-hook file -*- lexical-binding: t; -*-

;; Copyright ⓒ 2013-2016 Mattias Bengtsson

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20160417
;; Keywords         : init
;; Package-Requires : ()
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 24.x

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

(require 'cask  "~/.emacs.d/lisp/cask/cask.el")
(require 'funcs "~/.emacs.d/lisp/funcs.el")

(defun my/activate-global-keybindings ()
  "Activate global keybindings."

  ;; Fix keys if we're in a tmux shell
  (if (getenv "TMUX") (tmux-keys))

  (defvar my/global-remap-keys
    '((occur                    . my/occur-dwim)
      (isearch-forward          . my/isearch-forward-symbol-with-prefix)
      (isearch-backward         . my/isearch-backward-symbol-with-prefix)
      (execute-extended-command . smex)
      (delete-other-windows     . zygospore-toggle-delete-other-windows)
      (fill-paragraph           . my/fill-or-unfill)))

  ;; TODO: Why isn't this working above?
  (global-set-key [remap fill-paragraph] #'my/fill-or-unfill)

  (defvar my/global-keybindings
    '(
      ;; Global overrides
      ( "M-."         .  nil)
      ( "C-M-."       .  nil)

      ;; Windows
      ( "C-z"         .  nil)
      ( "C-z w c"     .  customize)
      ( "C-z w p"     .  list-packages)
      ( "C-z w t"     .  ansi-term)
      ( "C-z w r"     .  my/restclient)
      ( "C-z w e"     .  ielm)

      ;; Documentation
      ( "C-z h i"     .  info-display-manual)
      ( "C-z h m"     .  woman)

      ;; Toggle modes
      ( "C-z t w"     .  whitespace-mode)
      ( "C-z t a"     .  aggressive-indent-mode)
      ( "C-z t b"     .  magit-blame)
      ( "<escape>"    .  my/control-mode-on)
      ( "<insert>"    .  global-control-mode)

      ;; Other
      ( "C-z D"       .  diff-buffer-with-file)
      ( "C-z R"       .  restart-emacs)

      ;; NAVIGATION

      ;; General
      ( "C-'"         .  ace-jump-word-mode)
      ( "C-z g"       .  imenu)
      ( "C-z b"       .  browse-url-at-point)
      ( "C-z o"       .  my/open-with)
      ( "C-z n"       .  make-frame)
      ( "C-<next>"    .  forward-page)
      ( "C-<prior>"   .  backward-page)
      ( "M-<left>"    .  pop-tag-mark)
      ( "M-<right>"   .  nil)

      ;; Move buffers
      ( "<C-S-up>"    .  buf-move-up)
      ( "<C-S-down>"  .  buf-move-down)
      ( "<C-S-left>"  .  buf-move-left)
      ( "<C-S-right>" .  buf-move-right)


      ;; TEXT MANIPULATION

      ;; General
      ( "M-<up>"      .  md/move-lines-up)
      ( "M-<down>"    .  md/move-lines-down)
      ( "C-z D"       .  md/duplicate-up)
      ( "C-z d"       .  md/duplicate-down)
      ( "C-z a"       .  align-string)
      ( "C-z ."       .  align-by-current-symbol)
      ( "C-z c"       .  my/toggle-comment)
      ( "C-z <up>"    .  my/toggle-programming-case-word-at-point)
      ( "C-z <down>"  .  my/toggle-programming-case-word-at-point-reverse)
      ( "C-\""        .  cycle-quotes)
      ( "C-z u"       .  insert-char)
      ( "C-z ="       .  my/calc-thing-at-point)
      ( "C-z +"       .  shift-number-up)
      ( "C-z -"       .  shift-number-down)
      ( "C-z c"       .  my/ispell-word-then-abbrev)
      ( "C-z i f"     .  my/insert-file-name)
      ( "C-a"         .  mwim-beginning-of-code-or-line)
      ( "C-e"         .  mwim-end-of-code-or-line)

      ;; Replace
      ( "C-z r"       .  vr/replace)
      ( "C-z q"       .  vr/query-replace)

      ;; YAS
      ( "C-z s i"     .  yas-insert-snippet)
      ( "C-z s c"     .  yas-new-snippet)
      ( "C-z s e"     .  yas-visit-snippet-file)
      ( "C-z s r"     .  yas-reload-all)
      ( "C-z s t"     .  auto-insert)))

  (my/global-remap-keys my/global-remap-keys)
  (my/global-define-keys my/global-keybindings)
  (windmove-default-keybindings))

(defun my/activate-visual-regexp ()
  "Activate visual-regexp."
  (require 'visual-regexp-steroids)
  (my/define-keys esc-map
                  '(( "C-r" . vr/isearch-backward)
                    ( "C-s" . vr/isearch-forward))))

(defun my/activate-yas ()
  "Activate YASnippet."
  ;; This needs to be set here, or customize will bork.
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode))

(defvar god-mode-isearch-map)
(defun my/activate-god-mode-isearch ()
  "Activate `god-mode-isearch'."
  (require 'god-mode-isearch)
  (my/define-keys isearch-mode-map
                  '(( "<escape>" . god-mode-isearch-activate)
                    ( "<insert>" . god-mode-isearch-activate)))
  (my/define-keys god-mode-isearch-map
                  '(( "g"        . isearch-cancel)
                    ( "i"        . god-mode-isearch-disable)
                    ( "<insert>" . god-mode-isearch-disable))))

(defun my/activate-control-mode ()
  "Activate Control Mode."
  (require 'control-mode)
  (add-hook 'after-change-major-mode-hook
            #'my/control-mode-set-cursor)
  (global-control-mode)
  (my/activate-god-mode-isearch))

(defun my/activate-keyfreq-mode ()
  "Activate KeyFreq Mode."
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(defun my/activate-modes ()
  "Activate a bunch of global modes."
  (cask-initialize)
  (pallet-mode)
  (my/activate-control-mode)
  (powerline-major-mode)
  (powerline-default-theme)
  (global-git-gutter-mode)
  (ido-mode)
  (ido-vertical-mode)
  (ido-ubiquitous-mode)
  (flx-ido-mode)
  (fancy-narrow-mode)
  (which-key-mode)
  (auto-insert-mode)
  (auto-compile-on-save-mode)
  (auto-compile-on-load-mode)
  (browse-kill-ring-default-keybindings)
  (easy-repeat-mode)
  (smart-region-on)
  (elpy-enable)
  (global-aggressive-indent-mode)
  (projectile-global-mode)
  (recentf-mode)
  (abbrev-mode)
  (my/activate-keyfreq-mode)
  (my/activate-god-mode-isearch)
  (my/activate-visual-regexp)
  (my/activate-yas))

;;;

(my/activate-modes)
(my/activate-global-keybindings)

(provide 'my-after-init)
;;; my-after-init.el ends here
