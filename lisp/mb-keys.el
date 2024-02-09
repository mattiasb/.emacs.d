;;; mb-keys.el --- My global keybindings -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2020, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20170304
;; Keywords         : local
;; Package-Requires : ((emacs "29.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 29.x

;;; Commentary:

;; My global keybindings

;;; Note:

;;; Code:

(require 'mb-f)
(require 'mb-cmd)

(defconst mb-keys--global-remaps
  '((occur                         . mb-cmd-occur-dwim)
    (isearch-forward               . mb-cmd-isearch-forward-symbol-with-prefix)
    (isearch-backward              . mb-cmd-isearch-backward-symbol-with-prefix)
    (delete-other-windows          . zygospore-toggle-delete-other-windows)
    (eval-last-sexp                . mb-f-eval-or-inspect-last-sexp)
    (eval-expression               . mb-f-eval-or-inspect-expression)
    (fill-paragraph                . unfill-toggle)
    (kill-buffer                   . mb-cmd-kill-this-buffer)
    (split-window-right            . mb-cmd-split-window-right)
    (split-window-below            . mb-cmd-split-window-below)
    (delete-window                 . mb-cmd-delete-window)))

(defconst mb-keys-global-bindings
  '(;; Keymaps
    ( "C-z"                        . mb-keys-default-map)

    ;; XRef
    ( "C-<return>"                 . xref-find-definitions)
    ( "C-M-."                      . nil)
    ( "C-S-<return>"               . xref-find-references)
    ( "C-x 4 <return>"             . xref-find-definitions-other-window)
    ( "C-x 4 C-<return>"           . xref-find-definitions-other-window)
    ( "C-x 5 <return>"             . xref-find-definitions-other-frame)
    ( "C-x 5 C-<return>"           . xref-find-definitions-other-frame)
    ( "M-,"                        . nil)
    ( "M-."                        . nil)
    ( "M-<left>"                   . history-prev-history)
    ( "M-<right>"                  . history-next-history)
    ( "M-?"                        . nil)

    ;; NAVIGATION

    ;; General
    ( "C-'"                        . ace-jump-word-mode)
    ( "C-<next>"                   . forward-page)
    ( "C-<prior>"                  . backward-page)
    ( "M-g M-g  "                  . goto-line-preview)
    ( "M-n"                        . smartscan-symbol-go-forward)
    ( "M-p"                        . smartscan-symbol-go-backward)

    ;; Move buffers
    ( "<C-S-down>"                 . buf-move-down)
    ( "<C-S-left>"                 . buf-move-left)
    ( "<C-S-right>"                . buf-move-right)
    ( "<C-S-up>"                   . buf-move-up)

    ;; Frames
    ( "C-<tab>"                    . tab-next)

    ;; TEXT MANIPULATION

    ;; General
    ( "C-S-a"                      . beginning-of-line)
    ( "C-S-e"                      . end-of-line)
    ( "C-a"                        . mwim-beginning-of-code-or-line)
    ( "C-e"                        . mwim-end-of-code-or-line)
    ( "M-<down>"                   . move-dup-move-lines-down)
    ( "M-<up>"                     . move-dup-move-lines-up)

    ;; Snippets
    ("<insert>"                    . tempel-complete)))

(mb-f-define-keymap mb-keys-exec-map
                    '(( "t"        . mb-cmd-exec-terminal)
                      ( "f"        . mb-cmd-exec-nautilus)))

(mb-f-define-keymap mb-keys-windows-map
                    '(( "R"        . rfc-mode-browse)
                      ( "a"        . ansi-term)
                      ( "c"        . customize)
                      ( "d"        . devhelp)
                      ( "e"        . eat)
                      ( "i"        . ielm)
                      ( "p"        . list-packages)
                      ( "r"        . mb-cmd-restclient)
                      ( "s"        . dired-sidebar-toggle-sidebar)
                      ( "t"        . todotxt)
                      ( "v"        . vterm)))

;; My Help keymap
(mb-f-define-keymap mb-keys-help-map
                    '(( "i"        . info-display-manual)
                      ( "m"        . woman)))

;; My Toggle keymap
(mb-f-define-keymap mb-keys-toggle-map
                    '(( "a"        . aggressive-indent-mode)
                      ( "b"        . magit-blame)
                      ( "c"        . olivetti-mode)
                      ( "d"        . dedicated-mode)
                      ( "e l"      . electric-layout-mode)
                      ( "e o"      . electric-operator-mode)
                      ( "e p"      . electric-pair-mode)
                      ( "f"        . auto-fill-mode)
                      ( "l"        . display-line-numbers-mode)
                      ( "j"        . jinx-mode)
                      ( "p"        . projectile-mode)
                      ( "r"        . rainbow-mode)
                      ( "t"        . toggle-truncate-lines)
                      ( "w"        . whitespace-mode)))

;; My Magit keymap
(mb-f-define-keymap mb-keys-magit-map
                    '(( "b"        . mb-cmd-git-link-browse)
                      ( "c"        . magit-commit)
                      ( "f"        . magit-find-file)
                      ( "g"        . mb-cmd-git-get)
                      ( "l"        . mb-cmd-git-copy-url)
                      ( "p"        . magit-push-matching)))

;; My tempel keymap
(mb-f-define-keymap mb-keys-tempel-map
                    '(( "<tab>"    . tempel-complete)
                      ( "e"        . mb-cmd-visit-templates)
                      ( "i"        . tempel-insert)))

(mb-f-define-keymap mb-keys-highlight-map
                    '(( "<down>"   . hl-find-next-thing)
                      ( "<up>"     . hl-find-prev-thing)
                      ( "g"        . hl-unhighlight-all-local)
                      ( "l"        . hl-highlight-thingatpt-local)
                      ( "n"        . hl-find-next-thing)
                      ( "p"        . hl-find-prev-thing)
                      ( "u"        . hl-unhighlight-all-local)))

(mb-f-define-keymap mb-keys-string-inflection-map
                    '(( "-"        . string-inflection-kebab-case)
                      ( "<down>"   . string-inflection-underscore)
                      ( "<up>"     . string-inflection-upcase)
                      ( "S"        . string-inflection-upcase)
                      ( "_"        . string-inflection-underscore)
                      ( "c"        . string-inflection-lower-camelcase)
                      ( "i"        . string-inflection-all-cycle)
                      ( "k"        . string-inflection-kebab-case)
                      ( "l"        . string-inflection-kebab-case)
                      ( "p"        . string-inflection-camelcase)
                      ( "s"        . string-inflection-underscore)
                      ( "u"        . string-inflection-upcase)))

(mb-f-define-keymap mb-keys-tables-insert-map
                    '(( "c"        . table-insert-column)
                      ( "r"        . table-insert-row)))

(mb-f-define-keymap mb-keys-tables-delete-map
                    '(( "c"        . table-delete-column)
                      ( "r"        . table-delete-row)))

(mb-f-define-keymap mb-keys-tables-map
                    '(( "<down>"   . table-heighten-cell)
                      ( "<left>"   . table-narrow-cell)
                      ( "<return>" . table-recognize-table)
                      ( "<right>"  . table-widen-cell)
                      ( "<up>"     . table-shorten-cell)
                      ( "c"        . table-capture)
                      ( "g"        . table-unrecognize)
                      ( "g"        . table-unrecognize-table)
                      ( "i"        . mb-keys-tables-insert-map)
                      ( "k"        . mb-keys-tables-delete-map)
                      ( "t"        . table-insert)))

(mb-f-define-keymap mb-keys-zoom-map
                    '(( "+"        . default-text-scale-increase)
                      ( "-"        . default-text-scale-decrease)
                      ( "0"        . default-text-scale-reset)))

(mb-f-define-keymap mb-keys-debug-map
                    '(( "a"        . realgud-short-key-mode)
                      ( "d"        . mb-cmd-realgud)))

(mb-f-define-keymap mb-keys-flymake-map
                    '(( "<down>"   . flymake-goto-next-error)
                      ( "<up>"     . flymake-goto-prev-error)
                      ( "d"        . flymake-show-buffer-diagnostics)
                      ( "l"        . flymake-switch-to-log-buffer)
                      ( "n"        . flymake-goto-next-error)
                      ( "p"        . flymake-goto-prev-error)))

(mb-f-define-keymap mb-keys-default-map
                    '(;; Keymaps
                      ( "T"        . mb-keys-tables-map)
                      ( "d"        . mb-keys-debug-map)
                      ( "e"        . mb-keys-flymake-map)
                      ( "h"        . mb-keys-help-map)
                      ( "l"        . mb-keys-highlight-map)
                      ( "m"        . mb-keys-magit-map)
                      ( "s"        . mb-keys-tempel-map)
                      ( "t"        . mb-keys-toggle-map)
                      ( "w"        . mb-keys-windows-map)
                      ( "x"        . mb-keys-exec-map)
                      ( "z"        . mb-keys-zoom-map)

                      ;; Text manipulation
                      ( "'"        . cycle-quotes)
                      ( "+"        . shift-number-up)
                      ( "-"        . shift-number-down)
                      ( "<down>"   . move-dup-duplicate-down)
                      ( "<up>"     . move-dup-duplicate-up)
                      ( "="        . mb-cmd-calc-thing-at-point)
                      ( "a"        . ialign)
                      ( "c"        . mb-cmd-toggle-comment)
                      ( "i"        . mb-keys-string-inflection-map)
                      ( "p"        . projectile-command-map)
                      ( "q"        . vr/query-replace)
                      ( "r"        . vr/replace)
                      ( "u"        . insert-char)

                      ;; Other
                      ( "D"        . diff-buffer-with-file)
                      ( "_"        . vundo)
                      ( "g"        . imenu)
                      ( "n"        . mb-cmd-new-frame)
                      ( "o"        . mb-cmd-open-with)
                      ( "?"        . devdocs-lookup)))

(defun mb-keys-activate ()
  "Activate keybinding."
  (mb-f-global-remap-keys mb-keys--global-remaps)
  (mb-f-global-define-keys mb-keys-global-bindings))

(provide 'mb-keys)
;;; mb-keys.el ends here
