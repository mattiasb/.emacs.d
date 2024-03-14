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
    ("\/Dockerfile$"       . dockerfile-ts-mode)
    ("\/Containerfile$"    . dockerfile-ts-mode)
    ("\/evo"               . message-mode)
    ("\/hosts$"            . ini-mode)
    ("\/tmp\/bash-fc\."    . sh-mode)
    ("\\.bu$"              . yaml-mode)
    ("\\.container$"       . systemd-mode)
    ("\\.volume$"          . systemd-mode)
    ("\\.eld$"             . lisp-data-mode)
    ("\\.y[a]ml.j2$"       . yaml-mode)
    ("\\.dec$"             . mtg-deck-mode)
    ("\\.geojson$"         . json-mode)
    ("\\.html$"            . html-mode)
    ("\\.inl\\'"           . c++-mode)
    ("\\.j2$"              . jinja2-mode)
    ("\\.jsx?$"            . js-ts-mode)
    ("\\.ts$"              . typescript-ts-mode)
    ("\\.tsx$"             . tsx-ts-mode)
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
  '((cmake-mode      . "CMake")
    (go-mode         . "Go")

    (js-ts-mode      . "")
    (js-mode         . "")
    (markdown-mode   . "")
    (nxml-mode       . "")
    (sh-mode         . "")
    (haskell-mode    . "")
    (c++-mode        . "")
    (c-mode          . "")
    (emacs-lisp-mode . "")))

(defconst mb-modes--shortened-minor-modes
  '(;; Togglables
    (aggressive-indent-mode      . " ⇒")
    (auto-fill-mode              . " ⏎")
    (electric-operator-mode      . " ⊙")
    (rainbow-mode                . " 🌈")
    (jinx-mode                   . " ✓")
    (olivetti-mode               . " ⟛")
    ;; TODO(mattiasb): Make these work!
    (electric-layout-mode        . " ⏎")
    (electric-pair-mode          . " ⫘")

    (abbrev-mode                 . "")
    (all-the-icons-dired-mode    . "")
    (ansible-doc-mode            . "")
    (ansible-mode                . "")
    (auto-dark-mode              . "")
    (auto-dim-other-buffers-mode . "")
    (auto-revert-mode            . "")
    (auto-sudoedit-mode          . "")
    (eldoc-mode                  . "")
    (fancy-narrow-mode           . "")
    (flymake-mode                . "")
    (flymake-popon-mode          . "")
    (git-gutter-mode             . "")
    (haskell-indentation-mode    . "")
    (history-mode                . "")
    (hl-highlight-mode           . "")
    (jinx-mode                   . "")
    (magit-auto-revert-mode      . "")
    (magit-blame-mode            . "")
    (magit-filenotify-mode       . "")
    (pandoc-mode                 . "")
    (sqlup-mode                  . "")
    (projectile-mode             . "")
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
