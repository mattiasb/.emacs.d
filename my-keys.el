;;; my-keys.el --- My global keybindings -*- lexical-binding: t; -*-

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

;; Version: 20170304
;; Keywords: local
;; Package-Requires: ()
;; URL: TBA
;; Doc URL: TBA
;; Compatibility: GNU Emacs: 24.x

;;; Commentary:

;;; Note:

;;; Code:

(defconst my/global-remap-bindings
  '((occur                    . my/occur-dwim)
    (isearch-forward          . my/isearch-forward-symbol-with-prefix)
    (isearch-backward         . my/isearch-backward-symbol-with-prefix)
    (execute-extended-command . smex)
    (delete-other-windows     . zygospore-toggle-delete-other-windows)
    (fill-paragraph           . my/fill-or-unfill)
    (kill-buffer              . kill-this-buffer)))

(defconst my/global-keymap-bindings
  '(;; Keymaps
    ( "C-z"            . my/default-keymap)

    ;; XRef
    ( "M-,"            . nil)
    ( "M-."            . nil)
    ( "M-?"            . nil)
    ( "C-M-."          . nil)
    ( "M-<left>"       . xref-pop-marker-stack)
    ( "C-<return>"     . xref-find-definitions)
    ( "M-?"            . xref-find-references)
    ( "C-x 4 <return>" . xref-find-definitions-other-window)
    ( "C-x 5 <return>" . xref-find-definitions-other-frame)

    ;; Control-mode
    ( "<escape>"       . my/control-mode-on)
    ( "<insert>"       . global-control-mode)

    ;; NAVIGATION

    ;; General
    ( "C-'"           . ace-jump-word-mode)
    ( "C-<next>"      . forward-page)
    ( "C-<prior>"     . backward-page)

    ;; Move buffers
    ( "<C-S-up>"      . buf-move-up)
    ( "<C-S-down>"    . buf-move-down)
    ( "<C-S-left>"    . buf-move-left)
    ( "<C-S-right>"   . buf-move-right)

    ;; TEXT MANIPULATION

    ;; General
    ( "M-<up>"        . md/move-lines-up)
    ( "M-<down>"      . md/move-lines-down)
    ( "C-\""          . cycle-quotes)
    ( "C-S-a"         . beginning-of-line)
    ( "C-S-e"         . end-of-line)
    ( "C-a"           . mwim-beginning-of-code-or-line)
    ( "C-e"           . mwim-end-of-code-or-line)))

(my/define-keymap my/windows-keymap
                  '(( "c"      . customize)
                    ( "d"      . todotxt)
                    ( "e"      . ielm)
                    ( "p"      . list-packages)
                    ( "r"      . my/restclient)
                    ( "t"      . ansi-term)))

;; My Help keymap
(my/define-keymap my/help-keymap
                  '(( "i"      . info-display-manual)
                    ( "m"      . woman)))

;; My Toggle keymap
(my/define-keymap my/toggle-keymap
                  '(( "a"      . aggressive-indent-mode)
                    ( "b"      . magit-blame)
                    ( "e l"    . electric-layout-mode)
                    ( "e p"    . electric-pair-mode)
                    ( "p"      . projectile-mode)
                    ( "w"      . whitespace-mode)))

;; My Magit keymap
(my/define-keymap my/magit-keymap
                  '(( "c"      . magit-commit)
                    ( "p"      . magit-push-matching)))

;; My Yas keymap
(my/define-keymap my/yas-keymap
                  '(( "i"      . yas-insert-snippet)
                    ( "c"      . yas-new-snippet)
                    ( "e"      . yas-visit-snippet-file)
                    ( "r"      . yas-reload-all)
                    ( "t"      . auto-insert)))

(my/define-keymap my/default-keymap
                  '(;; Keymaps
                    ( "h"      . my/help-keymap)
                    ( "m"      . my/magit-keymap)
                    ( "s"      . my/yas-keymap)
                    ( "t"      . my/toggle-keymap)
                    ( "w"      . my/windows-keymap)

                    ;; Text manipulation
                    ( "+"      . shift-number-up)
                    ( "-"      . shift-number-down)
                    ( "."      . align-by-current-symbol)
                    ( "<down>" . md/duplicate-down)
                    ( "<up>"   . md/duplicate-up)
                    ( "="      . my/calc-thing-at-point)
                    ( "S"      . my/ispell-word-then-abbrev)
                    ( "a"      . align-string)
                    ( "c"      . my/toggle-comment)
                    ( "p"      . projectile-command-map)
                    ( "q"      . vr/query-replace)
                    ( "r"      . vr/replace)
                    ( "u"      . insert-char)

                    ;; Other
                    ( "C-z"    . suspend-frame)
                    ( "D"      . diff-buffer-with-file)
                    ( "R"      . my/restart-emacs)
                    ( "b"      . browse-url-at-point)
                    ( "g"      . imenu)
                    ( "n"      . make-frame)
                    ( "o"      . my/open-with)))

(provide 'my-keys)
;;; my-keys.el ends here
