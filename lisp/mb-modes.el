;;; mb-modes.el --- My modes configurations -*- lexical-binding: t; -*-

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

;; Version	    : 20170308
;; Keywords	    : local
;; Package-Requires : ((emacs "25.1"))
;; URL		    : https://github.com/moonlite/.emacs.d
;; Doc URL	    : TBA
;; Compatibility    : GNU Emacs: 25.x

;;; Commentary:

;; TODO: Move the setup code to a package and move the configuration to
;; customize.

;;; Note:

;;; Code:

(require 'mb-f)

(defconst mb-modes--filepattern-to-mode-map
  '(("\\.inl\\'"           . c++-mode)
    ("\\.ui$"              . nxml-mode)
    ("\\.html$"            . html-mode)
    ("\\.j2$"              . jinja2-mode)
    ("\\.js$"              . js2-mode)
    ("\\.jshintrc$"        . js2-mode)
    ("\\.jscsrc$"          . json-mode)
    ("\\.ks$"              . sh-mode)
    ("\\.geojson$"         . json-mode)
    ("\\.vala$"            . vala-mode)
    ("\\.mapcss$"          . css-mode)
    ("\\.mcss$"            . css-mode)
    ("\\.m$"               . octave-mode)
    ("\\.dec$"             . mtg-deck-mode)
    ("\/hosts$"            . ini-mode)
    ("\/Cask$"             . emacs-lisp-mode)
    ("\/Pipfile$"          . toml-mode)
    ("\/evo"               . message-mode)))

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
    (sh-mode         . "Sh")
    (snippet-mode    . "Yas Snippet")))

(defconst mb-modes--shortened-minor-modes
  '((abbrev-mode                 . " A")
    (aggressive-indent-mode      . " ⇒")
    (all-the-icons-dired-mode    . "")
    (anaconda-mode               . " 🐍")
    (ansible-doc-mode            . "")
    (auto-dim-other-buffers-mode . "")
    (auto-revert-mode            . " ⎌")
    (auto-sudoedit-mode          . "")
    (company-mode                . " C")
    (control-mode                . "")
    (eldoc-mode                  . " 🕮")
    (electric-operator-mode      . " ⊙")
    (fancy-narrow-mode           . "")
    (flymake-mode                . " ✎")
    (flyspell-mode               . " ✎")
    (git-gutter-mode             . "")
    (haskell-indentation-mode    . "")
    (magit-auto-revert-mode      . "")
    (magit-filenotify-mode       . " Notify")
    (magit-gitflow-mode          . " Flow")
    (racer-mode                  . "")
    (sqlup-mode                  . " ⇑")
    (which-key-mode              . "")
    (ws-butler-mode              . " W")
    (yas-minor-mode              . " Y")))

(defun mb-modes-activate ()
  "Activate mode configurations."
  (mb-f-auto-modes          mb-modes--filepattern-to-mode-map)
  (mb-f-shorten-major-modes mb-modes--shortened-major-modes)
  (mb-f-shorten-minor-modes mb-modes--shortened-minor-modes))

(provide 'mb-modes)
;;; mb-modes.el ends here
