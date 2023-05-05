;;; mb-keys.el --- My global keybindings -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2020, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20170304
;; Keywords         : local
;; Package-Requires : ((emacs "27.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 27.x

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
    ( "M-<left>"       . history-prev-history)
    ( "M-<right>"      . history-next-history)
    ( "C-<return>"     . xref-find-definitions)
    ( "C-S-<return>"   . xref-find-references)
    ( "C-x 4 <return>" . xref-find-definitions-other-window)
    ( "C-x 5 <return>" . xref-find-definitions-other-frame)

    ;; NAVIGATION

    ;; General
    ( "C-'"           . ace-jump-word-mode)
    ( "C-<next>"      . forward-page)
    ( "C-<prior>"     . backward-page)
    ( "M-g M-g  "     . goto-line-preview)

    ;; Move buffers
    ( "<C-S-up>"      . buf-move-up)
    ( "<C-S-down>"    . buf-move-down)
    ( "<C-S-left>"    . buf-move-left)
    ( "<C-S-right>"   . buf-move-right)

    ;; Frames
    ( "C-<tab>"       . tab-next)

    ;; TEXT MANIPULATION

    ;; General
    ( "M-<up>"        . move-dup-move-lines-up)
    ( "M-<down>"      . move-dup-move-lines-down)
    ( "C-S-a"         . beginning-of-line)
    ( "C-S-e"         . end-of-line)
    ( "C-a"           . mwim-beginning-of-code-or-line)
    ( "C-e"           . mwim-end-of-code-or-line)

    ;; Snippets
    ("<insert>"       . tempel-complete)))

(mb-f-define-keymap mb-keys-exec-map
                    '(( "t"      . mb-cmd-exec-terminal)
                      ( "f"      . mb-cmd-exec-nautilus)))

(mb-f-define-keymap mb-keys-windows-map
                    '(( "R"      . rfc-mode-browse)
                      ( "a"      . ansi-term)
                      ( "c"      . customize)
                      ( "d"      . devhelp)
                      ( "e"      . eat)
                      ( "i"      . ielm)
                      ( "p"      . list-packages)
                      ( "r"      . mb-cmd-restclient)
                      ( "s"      . dired-sidebar-toggle-sidebar)
                      ( "t"      . todotxt)
                      ( "v"      . vterm)))

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
                      ( "f"      . auto-fill-mode)
                      ( "p"      . projectile-mode)
                      ( "r"      . rainbow-mode)
                      ( "s"      . flyspell-mode)
                      ( "t"      . toggle-truncate-lines)
                      ( "w"      . whitespace-mode)))

;; My Magit keymap
(mb-f-define-keymap mb-keys-magit-map
                    '(( "c"      . magit-commit)
                      ( "f"      . magit-find-file)
                      ( "g"      . mb-cmd-git-get)
                      ( "l"      . mb-cmd-git-copy-url)
                      ( "b"      . mb-cmd-git-link-browse)
                      ( "p"      . magit-push-matching)))

;; My tempel keymap
(mb-f-define-keymap mb-keys-tempel-map
                    '(( "<tab>"  . tempel-complete)
                      ( "i"      . tempel-insert)
                      ( "e"      . mb-cmd-visit-templates)))

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

(mb-f-define-keymap mb-keys-zoom-map
                    '(( "+"        . default-text-scale-increase)
                      ( "-"        . default-text-scale-decrease)
                      ( "0"        . default-text-scale-reset)))

(mb-f-define-keymap mb-keys-default-map
                    '(;; Keymaps
                      ( "h"      . mb-keys-help-map)
                      ( "m"      . mb-keys-magit-map)
                      ( "s"      . mb-keys-tempel-map)
                      ( "t"      . mb-keys-toggle-map)
                      ( "T"      . mb-keys-tables-map)
                      ( "w"      . mb-keys-windows-map)
                      ( "x"      . mb-keys-exec-map)
                      ( "z"      . mb-keys-zoom-map)

                      ;; Text manipulation
                      ( "+"      . shift-number-up)
                      ( "-"      . shift-number-down)
                      ( "a"      . ialign)
                      ( "<down>" . move-dup-duplicate-down)
                      ( "<up>"   . move-dup-duplicate-up)
                      ( "="      . mb-cmd-calc-thing-at-point)
                      ( "'"      . cycle-quotes)
                      ( "c"      . mb-cmd-toggle-comment)
                      ( "p"      . projectile-command-map)
                      ( "q"      . vr/query-replace)
                      ( "r"      . vr/replace)
                      ( "u"      . insert-char)
                      ( "i"      . mb-keys-string-inflection-map)

                      ;; Other
                      ( "_"      . vundo)
                      ( "C-z"    . suspend-frame)
                      ( "D"      . diff-buffer-with-file)
                      ( "b"      . browse-url-at-point)
                      ( "g"      . imenu)
                      ( "n"      . mb-cmd-new-frame)
                      ( "o"      . mb-cmd-open-with)))

(defun mb-keys-activate ()
  "Activate keybinding."
  (mb-f-global-remap-keys mb-keys--global-remaps)
  (mb-f-global-define-keys mb-keys-global-bindings))

(provide 'mb-keys)
;;; mb-keys.el ends here
