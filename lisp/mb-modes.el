;;; mb-modes.el --- My modes configurations -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2017-2022, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version	    : 20170308
;; Keywords	    : local
;; Package-Requires : ((emacs "29.1"))
;; URL		    : https://github.com/moonlite/.emacs.d
;; Doc URL	    : TBA
;; Compatibility    : GNU Emacs: 29.x

;;; Commentary:

;; TODO: Move the setup code to a package and move the configuration to
;; customize.

;;; Note:

;;; Code:

(require 'mb-f)

(defconst mb-modes--filepattern-to-mode-map
  '(("\/Cask$"             . emacs-lisp-mode)
    ("\/Pipfile$"          . toml-mode)
    ("\/evo"               . message-mode)
    ("\/hosts$"            . ini-mode)
    ("\/tmp\/bash-fc\."    . sh-mode)
    ("\\.bu$"              . yaml-mode)
    ("\\.eld$"             . lisp-data-mode)
    ("\\.y[a]ml.j2$"       . yaml-mode)
    ("\\.dec$"             . mtg-deck-mode)
    ("\\.geojson$"         . json-mode)
    ("\\.html$"            . html-mode)
    ("\\.inl\\'"           . c++-mode)
    ("\\.j2$"              . jinja2-mode)
    ("\\.js$"              . js2-mode)
    ("\\.jscsrc$"          . json-mode)
    ("\\.jshintrc$"        . js2-mode)
    ("\\.ks$"              . sh-mode)
    ("\\.m$"               . octave-mode)
    ("\\.mapcss$"          . css-mode)
    ("\\.mcss$"            . css-mode)
    ("\\.ui$"              . nxml-mode)
    ("\\/.config/tmux/.*"  . tmux-mode)
    ("\\.vala$"            . vala-mode)))

(defconst mb-modes--shortened-major-modes
  '((c++-mode        . "C++")
    (c-mode          . "C")
    (cmake-mode      . "CMake")
    (emacs-lisp-mode . "Elisp")
    (go-mode         . "Go")
    (haskell-mode    . "λ")
    (js2-mode        . "JS")
    (markdown-mode   . "M↓")
    (nxml-mode       . "XML")
    (sh-mode         . "Sh")))

(defconst mb-modes--shortened-minor-modes
  '((abbrev-mode                 . " A")
    (aggressive-indent-mode      . " ⇒")
    (all-the-icons-dired-mode    . "")
    (ansible-doc-mode            . "")
    (auto-dim-other-buffers-mode . "")
    (auto-revert-mode            . " ⎌")
    (auto-sudoedit-mode          . "")
    (eldoc-mode                  . " 📖")
    (electric-operator-mode      . " ⊙")
    (fancy-narrow-mode           . "")
    (flymake-mode                . " ✎")
    (git-gutter-mode             . "")
    (haskell-indentation-mode    . "")
    (history-mode                . "")
    (magit-auto-revert-mode      . "")
    (magit-filenotify-mode       . " Notify")
    (sqlup-mode                  . " ⇑")
    (rainbow-mode                . " 🌈")
    (which-key-mode              . "")
    (ws-butler-mode              . "")))

(defun mb-modes-activate ()
  "Activate mode configurations."
  (mb-f-auto-modes          mb-modes--filepattern-to-mode-map)
  (mb-f-shorten-major-modes mb-modes--shortened-major-modes)
  (mb-f-shorten-minor-modes mb-modes--shortened-minor-modes)
  (add-to-list 'magic-mode-alist '("SQLite format 3\x00" . mb-f-sqlite-magic)))

(provide 'mb-modes)
;;; mb-modes.el ends here
