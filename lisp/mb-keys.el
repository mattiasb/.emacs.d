;;; mb-keys.el --- My global keybindings -*- lexical-binding: t; -*-

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

;; Version          : 20170304
;; Keywords         : local
;; Package-Requires : ((emacs "25.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 25.x

;;; Commentary:

;; My global keybindings

;;; Note:

;;; Code:

(require 'mb-f)
(require 'mb-cmd)

(defconst mb-keys--global-remaps
  '((occur                    . mb-cmd-occur-dwim)
    (isearch-forward          . mb-cmd-isearch-forward-symbol-with-prefix)
    (isearch-backward         . mb-cmd-isearch-backward-symbol-with-prefix)
    (execute-extended-command . smex)
    (delete-other-windows     . zygospore-toggle-delete-other-windows)
    (fill-paragraph           . mb-cmd-fill-or-unfill)
    (kill-buffer              . mb-cmd-kill-this-buffer)
    (split-window-right       . mb-cmd-split-window-right)
    (split-window-below       . mb-cmd-split-window-below)
    (delete-window            . mb-cmd-delete-window)))

(defconst mb-keys-global-bindings
  '(;; Keymaps
    ( "C-z"            . mb-keys-default-map)

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
    ( "<escape>"       . mb-cmd-control-mode-on)
    ( "<insert>"       . global-control-mode)

    ;; NAVIGATION

    ;; General
    ( "C-'"           . ace-jump-word-mode)
    ( "C-<next>"      . forward-page)
    ( "C-<prior>"     . backward-page)
    ( "M-g M-g  "     . goto-line-preview-goto-line)

    ;; Move buffers
    ( "<C-S-up>"      . buf-move-up)
    ( "<C-S-down>"    . buf-move-down)
    ( "<C-S-left>"    . buf-move-left)
    ( "<C-S-right>"   . buf-move-right)

    ;; Frames
    ( "C-<tab>"       . other-frame)

    ;; TEXT MANIPULATION

    ;; General
    ( "M-<up>"        . md-move-lines-up)
    ( "M-<down>"      . md-move-lines-down)
    ( "C-\""          . cycle-quotes)
    ( "C-S-a"         . beginning-of-line)
    ( "C-S-e"         . end-of-line)
    ( "C-a"           . mwim-beginning-of-code-or-line)
    ( "C-e"           . mwim-end-of-code-or-line)))

(mb-f-define-keymap mb-keys-exec-map
                    '(( "t"      . mb-cmd-exec-gnome-terminal)
                      ( "f"      . mb-cmd-exec-nautilus)))

(mb-f-define-keymap mb-keys-windows-map
                    '(( "c"      . customize)
                      ( "t"      . todotxt)
                      ( "T"      . mb-cmd-open-tasks)
                      ( "d"      . daemons)
                      ( "j"      . mb-cmd-open-journal)
                      ( "m"      . wl-other-frame)
                      ( "e"      . ielm)
                      ( "p"      . list-packages)
                      ( "r"      . mb-cmd-restclient)
                      ( "a"      . ansi-term)
                      ( "v"      . vterm)
                      ( "s"      . dired-sidebar-toggle-sidebar)))

;; My Help keymap
(mb-f-define-keymap mb-keys-help-map
                    '(( "i"      . info-display-manual)
                      ( "m"      . woman)))

;; My Toggle keymap
(mb-f-define-keymap mb-keys-toggle-map
                    '(( "a"      . aggressive-indent-mode)
                      ( "b"      . magit-blame)
                      ( "e l"    . electric-layout-mode)
                      ( "e p"    . electric-pair-mode)
                      ( "e o"    . electric-operator-mode)
                      ( "p"      . projectile-mode)
                      ( "r"      . rainbow-mode)
                      ( "s"      . flyspell-mode)
                      ( "w"      . whitespace-mode)
                      ( "y"      . yas-minor-mode)))

;; My Magit keymap
(mb-f-define-keymap mb-keys-magit-map
                    '(( "c"      . magit-commit)
                      ( "p"      . magit-push-matching)))

;; My Yas keymap
(mb-f-define-keymap mb-keys-yas-map
                    '(( "i"      . yas-insert-snippet)
                      ( "c"      . yas-new-snippet)
                      ( "e"      . yas-visit-snippet-file)
                      ( "r"      . yas-reload-all)
                      ( "t"      . auto-insert)))

(mb-f-define-keymap mb-keys-string-inflection-map
                    '(( "i"      . string-inflection-all-cycle)
                      ;; PascalCase
                      ( "p"      . string-inflection-camelcase)
                      ;; camelcase
                      ( "c"      . string-inflection-lower-camelcase)
                      ;; kebab-case / lisp-case
                      ( "k"      . string-inflection-kebab-case)
                      ( "l"      . string-inflection-kebab-case)
                      ( "-"      . string-inflection-kebab-case)
                      ;; snake_case
                      ( "s"      . string-inflection-underscore)
                      ( "_"      . string-inflection-underscore)
                      ( "<down>" . string-inflection-underscore)
                      ;; UPPER_SNAKE_CASE
                      ( "u"      . string-inflection-upcase)
                      ( "S"      . string-inflection-upcase)
                      ( "<up>"   . string-inflection-upcase)))

(mb-f-define-keymap mb-keys-tables-insert-map
                    '(( "c"        . table-insert-column)
                      ( "r"        . table-insert-row)))

(mb-f-define-keymap mb-keys-tables-delete-map
                    '(( "c"        . table-delete-column)
                      ( "r"        . table-delete-row)))

(mb-f-define-keymap mb-keys-tables-map
                    '(( "i"        . mb-keys-tables-insert-map)
                      ( "t"        . table-insert)
                      ( "c"        . table-capture)
                      ( "<return>" . table-recognize-table)
                      ( "g"        . table-unrecognize-table)
                      ( "k"        . mb-keys-tables-delete-map)
                      ( "g"        . table-unrecognize)
                      ( "<left>"   . table-narrow-cell)
                      ( "<right>"  . table-widen-cell)
                      ( "<up>"     . table-shorten-cell)
                      ( "<down>"   . table-heighten-cell)))

(mb-f-define-keymap mb-keys-default-map
                    '(;; Keymaps
                      ( "h"      . mb-keys-help-map)
                      ( "m"      . mb-keys-magit-map)
                      ( "s"      . mb-keys-yas-map)
                      ( "t"      . mb-keys-toggle-map)
                      ( "T"      . mb-keys-tables-map)
                      ( "w"      . mb-keys-windows-map)
                      ( "x"      . mb-keys-exec-map)

                      ;; Text manipulation
                      ( "+"      . shift-number-up)
                      ( "-"      . shift-number-down)
                      ( "."      . align-by-current-symbol)
                      ( "a"      . ialign)
                      ( "<down>" . md-duplicate-down)
                      ( "<up>"   . md-duplicate-up)
                      ( "="      . mb-cmd-calc-thing-at-point)
                      ( "S"      . mb-cmd-ispell-word-then-abbrev)
                      ( "c"      . mb-cmd-toggle-comment)
                      ( "p"      . projectile-command-map)
                      ( "q"      . vr/query-replace)
                      ( "r"      . vr/replace)
                      ( "u"      . insert-char)
                      ( "i"      . mb-keys-string-inflection-map)

                      ;; Other
                      ( "C-z"    . suspend-frame)
                      ( "D"      . diff-buffer-with-file)
                      ( "R"      . restart-emacs)
                      ( "b"      . browse-url-at-point)
                      ( "g"      . imenu)
                      ( "n"      . make-frame)
                      ( "o"      . mb-cmd-open-with)))

(defun mb-keys-activate ()
  "Activate keybinding."
  (mb-f-global-remap-keys mb-keys--global-remaps)
  (mb-f-global-define-keys mb-keys-global-bindings))

(provide 'mb-keys)
;;; mb-keys.el ends here
