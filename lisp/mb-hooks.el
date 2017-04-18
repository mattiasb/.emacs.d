;;; mb-hooks.el --- My mode hooks -*- lexical-binding: t; -*-

;; Copyright ⓒ 2016 Mattias Bengtsson
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
(require 'mb-cmd)

;; After Save
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; AG
(defvar ag-mode-map)
(defun mb-hooks--ag-mode ()
  "My `ag' mode hook."
  (mb-f-define-keys ag-mode-map
                    '(( "W" . wgrep-change-to-wgrep-mode))))

(add-hook 'ag-mode-hook #'mb-hooks--ag-mode)

;; Backward Forward
(defun mb-hooks--backward-forward-mode ()
  "My `backward-forward' mode hook."
  (defvar backward-forward-mode-map)
  (mb-f-define-keys backward-forward-mode-map
                    '(("M-<left>"  . backward-forward-previous-location)
                      ("M-<right>" . backward-forward-next-location)
                      ("C-<left>"  . nil)
                      ("C-<right>" . nil))))

(add-hook 'backward-forward-mode-hook #'mb-hooks--backward-forward-mode)

;; Browse Kill Ring
(defvar browse-kill-ring-mode-map)
(defun mb-hooks--browse-kill-ring-mode ()
  "My `browse-kill-ring' mode hook."
  (mb-f-define-keys browse-kill-ring-mode-map
                    '(( "<down>"    . browse-kill-ring-forward)
                      ( "<tab>"     . browse-kill-ring-forward)
                      ( "<up>"      . browse-kill-ring-previous)
                      ( "<backtab>" . browse-kill-ring-previous)
                      ( "C-g"       . browse-kill-ring-quit))))

(add-hook 'browse-kill-ring-mode-hook #'mb-hooks--browse-kill-ring-mode)

;; C common
(defvar c-mode-base-map)
(defun mb-hooks--c-common ()
  "My `c-mode' mode hook."
  (unless (keymap-parent c-mode-base-map)
    (set-keymap-parent c-mode-base-map prog-mode-map)))

(add-hook 'c-mode-common-hook #'mb-hooks--c-common)

;; C / C++
(defvar rtags-completions-enabled)
(defvar company-backends)
(defvar flycheck-disabled-checkers)
(defvar flycheck-check-syntax-automatically)
(defvar flycheck-highlighting-mode)
(defvar projectile-command-map)
(autoload 'rtags-diagnostics "rtags" "" t nil)
(defun mb-hooks--c-mode ()
  "A mode hook for C and C++."
  (require 'rtags)

  (require 'flycheck-rtags)
  (setq-local flycheck-disabled-checkers '(c/c++-gcc c/c++-clang))
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil)

  (require 'company-rtags)
  (setq-local company-backends '(company-rtags))

  (backward-forward-mode -1)
  (mb-f-define-keys c-mode-base-map
                    '(( "M-<left>"       . rtags-location-stack-back)
                      ( "M-<right>"      . rtags-location-stack-forward)
                      ( "C-<return>"     . rtags-find-symbol-at-point)
                      ( "M-?"            . rtags-find-references)
                      ( "C-x 4 <return>" . rtags-show-target-in-other-window)
                      ( "C-z f r"        . rtags-rename-symbol)
                      ( "."              . mb-cmd-dot-and-complete)
                      ( ":"              . mb-cmd-double-colon-and-complete)
                      ( ">"              . mb-cmd-arrow-and-complete)))
  (mb-f-define-keys projectile-command-map
                    '(( "j"              . rtags-find-symbol)
                      ( "R"              . mb-cmd-projectile-regen-rtags))))

(add-hook 'c-mode-hook   #'mb-hooks--c-mode)
(add-hook 'c++-mode-hook #'mb-hooks--c-mode)

;; CMake
(defun mb-hooks--cmake-mode ()
  "My `cmake' mode hook."
  (setq-local company-backends '((company-cmake
                                  company-keywords
                                  company-files
                                  company-dabbrev-code))))

(add-hook 'cmake-mode-hook #'mb-hooks--prog-mode)
(add-hook 'cmake-mode-hook #'mb-hooks--cmake-mode)

;; Control
(defvar control-mode)
(defvar control-mode-keymap)
(defun mb-hooks--control-mode ()
  "My `control' mode hook."
  (mb-f-control-mode-set-cursor)
  (mb-f-define-keys control-mode-keymap
                    '(( "i"           . mb-cmd-control-mode-off)
                      ( "<escape>"    . ESC-prefix)
                      ( "x x"         . smex)
                      ( "x s"         . save-buffer)
                      ( "x S"         . save-some-buffers))))

(add-hook 'control-mode-keymap-generation-functions
          'control-mode-ctrlx-hacks)
(add-hook 'control-mode-hook #'mb-hooks--control-mode)

;; Company
(defvar company-active-map)
(defun mb-hooks--company-mode ()
  "My `company' mode hook."
  (company-quickhelp-mode)

  (mb-f-define-keys company-active-map
                    '(( "\C-n"    . company-select-next)
                      ( "\C-p"    . company-select-previous)
                      ( "<next>"  . mb-cmd-company-scroll-down)
                      ( "<prior>" . mb-cmd-company-scroll-up)
                      ( "\C-v"    . company-show-location)
                      ( "\C-g"    . company-abort))))

(add-hook 'company-mode-hook                 #'mb-hooks--company-mode)
(add-hook 'company-completion-started-hook   #'mb-f-fci-turn-off)
(add-hook 'company-completion-finished-hook  #'mb-f-fci-turn-on)
(add-hook 'company-completion-cancelled-hook #'mb-f-fci-turn-on)


;; Compilation Mode

(defvar compilation-filter-start)
(defun mb-hooks--compilation-filter ()
  "My `compilation-filter' mode hook."
  (require 'ansi-color)
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook #'mb-hooks--compilation-filter)

;; Cython
(defun mb-hooks--cython-mode ()
  "My `cython' mode hook."
  (require 'flycheck-cython))

(add-hook 'cython-mode-hook #'mb-hooks--cython-mode)

;; Dired
(defvar dired-mode-map)
(autoload 'dired-omit-mode "dired-x" "" t nil)
(autoload 'dired-hide-details-mode "dired" "" t nil)
(defun mb-hooks--dired-mode ()
  "My `dired' mode hook."
  (require 'dired-x)
  (require 'tramp)
  (auto-revert-mode)
  (hl-line-mode)
  (all-the-icons-dired-mode)
  (dired-omit-mode)
  (dired-hide-details-mode)
  (dired-hide-dotfiles-mode)
  (mb-f-define-keys dired-mode-map
                    '(( "W" . wdired-change-to-wdired-mode)
                      ( "F" . find-name-dired)
                      ( "c" . mb-cmd-find-file-default)
                      ( "." . dired-hide-dotfiles-mode)))
  (mb-f-remap-keys dired-mode-map
                   '(("s" . "C-s")
                     ("r" . "C-r"))))

(add-hook 'dired-mode-hook #'mb-hooks--dired-mode)

;; ELisp
(defun mb-hooks--emacs-lisp-mode ()
  "My `emacs-lisp' mode hook."
  (setq page-delimiter
        (rx bol ";;;" (not (any "#")) (* not-newline) "\n"
            (* (* blank) (opt ";" (* not-newline)) "\n"))))

(add-hook 'emacs-lisp-mode-hook #'lisp-extra-font-lock-mode)
(add-hook 'emacs-lisp-mode-hook #'mb-hooks--emacs-lisp-mode)

;; Flycheck
(defun mb-hooks--flycheck-mode ()
  "My `flycheck' mode hook."
  (flycheck-pos-tip-mode)
  (flycheck-status-emoji-mode)
  (flycheck-cask-setup)
  (flycheck-package-setup)
  (flycheck-rust-setup))

(add-hook 'flycheck-mode-hook #'mb-hooks--flycheck-mode)

;; Flyspell
(defvar flyspell-mode-map)
(defun mb-hooks--flyspell-mode ()
  "My `flyspell' mode hook."
  (require 'flyspell-correct-popup)
  (flyspell-buffer)
  (mb-f-define-keys flyspell-mode-map
                    '(("C-," . mb-cmd-flyspell-goto-previous-error)
                      ("C-." . flyspell-goto-next-error)
                      ("C-;" . flyspell-correct-previous-word-generic)
                      ("C-:" . flyspell-correct-next-word-generic))))

(add-hook 'flyspell-mode-hook #'mb-hooks--flyspell-mode)

;; Find-file
(add-hook 'find-file-not-found-functions #'mb-f-create-non-existent-directory)

;; Go
(defvar go-mode-map)
(defun mb-hooks--go-mode ()
  "My `go' mode hook."
  (go-eldoc-setup)

  (setq-local tab-width 4)
  (setq-local company-backends '(company-go
                                 company-keywords
                                 company-files))

  (mb-f-define-keys go-mode-map
                    '(( "C-z i a"    . go-import-add)
                      ( "C-z i r"    . go-remove-unused-imports)
                      ( "C-z i g"    . go-goto-imports)
                      ( "C-z d"      . godoc-at-point)
                      ( "C-<return>" . godef-jump)
                      ( "."          . mb-cmd-dot-and-complete))))

(add-hook 'go-mode-hook #'mb-hooks--go-mode)

;; Haskell
(defun mb-hooks--haskell-mode ()
  "My `haskell' mode hook."
  (haskell-indentation-mode))

(add-hook 'haskell-mode-hook #'mb-hooks--haskell-mode)

;; Help
(defvar help-mode-map)
(defun mb-hooks--help-mode ()
  "My `help' mode hook."
  (mb-f-define-keys help-mode-map
                    '(( "M-<left>"  . help-go-back)
                      ( "M-<right>" . help-go-forward)))
  (mb-f-remap-keys help-mode-map
                   '(("s" . "C-s")
                     ("r" . "C-r"))))

(add-hook 'help-mode-hook #'mb-hooks--help-mode)

;; Ido
(defvar ido-common-completion-map)
(defun mb-hooks--ido-setup ()
  "My `ido' mode hook."
  (mb-f-define-keys ido-common-completion-map
                    '(( "<tab"    . ido-complete)
                      ( "<next>"  . mb-cmd-ido-scroll-down)
                      ( "<prior>" . mb-cmd-ido-scroll-up))))

(add-hook 'ido-setup-hook #'mb-hooks--ido-setup)

;; IBuffer
(defvar ibuffer-sorting-mode)
(defun mb-hooks--ibuffer ()
  "My `ibuffer' mode hook."
  (ibuffer-projectile-set-filter-groups)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(add-hook 'ibuffer-hook 'mb-hooks--ibuffer)

;; Iedit
(defvar iedit-mode-keymap)
(defun mb-hooks--iedit-mode ()
  "My `iedit' mode hook."
  (mb-f-define-keys iedit-mode-keymap
                    '(("C-g"      . iedit-quit)
                      ("<return>" . iedit-quit))))

(add-hook 'iedit-mode-hook #'mb-hooks--iedit-mode)
(add-hook 'iedit-aborting-hook #'deactivate-mark)

;; IELM
(defvar ielm-map)
(defun mb-hooks--ielm-mode ()
  "My `ielm' mode hook."
  (company-mode)
  (mb-f-define-keys ielm-map
                    '(( "<tab>" . mb-cmd-snippet-or-complete))))

(add-hook 'ielm-mode-hook #'mb-hooks--ielm-mode)

;; Info
(defun mb-hooks--Info-mode ()
  "My `Info' mode hook."
  (mb-f-define-keys Info-mode-map
                    '(( "M-<left>"  . Info-history-back)
                      ( "M-<right>" . Info-history-forward)
                      ( "M-<up>"    . Info-up))))

(add-hook 'Info-mode-hook #'mb-hooks--Info-mode)
(add-hook 'Info-selection-hook #'niceify-info)

;; Java
(defun mb-hooks--java-mode ()
  "My `java' mode hook."
  (require 'ensime-company)
  (ensime)
  (setq-local company-backends '((ensime-company))))

(add-hook 'java-mode-hook #'mb-hooks--java-mode)

;; JS2
(defvar js2-mode-map)
(defvar flimenu-imenu-separator)
(autoload 'js2r-rename-var "js2-refactor" "" t nil)

(defun mb-hooks--js2-mode ()
  "My `js2' mode hook."
  (require 'js2-refactor)
  (js2-imenu-extras-mode)
  (setq flimenu-imenu-separator ".")
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)

  (mb-f-define-keys js2-mode-map
                    '(( "C-z f r"    . js2r-rename-var)))

  (setq-local company-backends '((company-dabbrev-code
                                  company-files
                                  company-keywords))))

(add-hook 'js2-mode-hook #'mb-hooks--js2-mode)

(defvar tern-mode-keymap)
(defun mb-hooks--tern-mode ()
  "My `tern' mode hook."
  (mb-f-define-keys tern-mode-keymap
                    '(( "C-<return>" . tern-find-definition)
                      ( "C-z f r"    . tern-rename-variable))))

(add-hook 'tern-mode-hook #'mb-hooks--tern-mode)

;; Todotxt
(autoload 'todotxt "todotxt" "" t nil)
(autoload 'todotxt-mode "todotxt" "" t nil)
(defvar todotxt-mode-map)
(defun mb-hooks--todotxt-mode ()
  "My `todotxt' mode hook."
  (mb-f-define-keys todotxt-mode-map
                    '(("j" . nil)
                      ("k" . todotxt-nuke-item)
                      ("_" . todotxt-undo)
                      ("u" . nil))))

(add-hook 'todotxt-mode-hook #'mb-hooks--todotxt-mode)

;; JSON
(defun mb-hooks--json-mode ()
  "My `json' mode hook."
  (highlight-numbers-mode -1))

(add-hook 'json-mode-hook #'mb-hooks--json-mode)

;; Lua
(defun mb-hooks--lua-mode ()
  "My `lua' mode hook."
  (setq-local company-backends '((company-dabbrev-code
                                  company-files
                                  company-keywords))))

(add-hook 'lua-mode-hook #'mb-hooks--lua-mode)

;; Magit
(autoload 'git-commit-turn-on-flyspell "git-commit" "" t nil)
(autoload 'git-commit-turn-on-auto-fill "git-commit" "" t nil)
(defun mb-hooks--git-commit-mode ()
  "My `git-commit' mode hook."
  (mb-cmd-control-mode-off)
  (setq-local fill-column 72)
  (git-commit-turn-on-flyspell)
  (git-commit-turn-on-auto-fill)
  (fci-mode 1))

(autoload 'turn-on-magit-gitflow "magit-gitflow" "" t nil)
(autoload 'git-gutter:update-all-windows "git-gutter" "" t nil)
(autoload 'magit-define-popup-action "magit-popup")

(defun mb-hooks--magit-mode ()
  "My `magit' mode hook."
  (require 'magit-gitflow)
  (turn-on-magit-gitflow)

  (magit-define-popup-action 'magit-run-popup
    ?g "Gitg" #'mb-cmd-projectile-gitg)

  (magit-define-popup-action 'magit-run-popup
    ?a "ansi-term" #'mb-cmd-projectile-ansi-term)

  (magit-define-popup-action 'magit-run-popup
    ?t "gnome-terminal" #'mb-cmd-projectile-gnome-terminal)

  (add-hook 'magit-post-refresh-hook
            #'git-gutter:update-all-windows))

(defvar magit-blame-mode-map)
(defun mb-hooks--magit-blame-mode ()
  "My `magit-blame' mode hook."
  (mb-f-define-keys magit-blame-mode-map
                    '(( "C-z t b"     .  magit-blame-quit))))

(add-hook 'magit-status-mode-hook #'magit-filenotify-mode)
(add-hook 'magit-blame-mode-hook  #'mb-hooks--magit-blame-mode)
(add-hook 'magit-mode-hook        #'mb-hooks--magit-mode)
(add-hook 'git-commit-mode-hook   #'mb-hooks--git-commit-mode)

;; Markdown
(defvar markdown-mode-map)
(defun mb-hooks--markdown-mode ()
  "My `markdown' mode hook."
  (flyspell-mode)
  (setq-local fill-column 80)
  (fci-mode)
  (auto-fill-mode)
  (setq-local indent-tabs-mode nil)
  (mb-f-define-keys markdown-mode-map
                    '(( "C-<return>" . markdown-jump)
                      ( "C-c C-c p"  . mb-cmd-open-with)
                      ( "M-<up>"     . nil)
                      ( "M-<down>"   . nil))))

(add-hook 'markdown-mode-hook #'mb-hooks--markdown-mode)

;; MTG deck mode
(defvar mtg-deck-mode-map)
(defun mb-hooks--mtg-deck-mode ()
  "My `mtg-deck' mode hook."
  (company-mode)
  (setq-local company-backends '(company-capf))
  (mb-f-define-keys mtg-deck-mode-map
                    '(( "<tab>" . mb-cmd-snippet-or-complete))))

(add-hook 'mtg-deck-mode-hook #'mb-hooks--mtg-deck-mode)

;; Multiple Cursors
(autoload 'control-mode-reload-bindings "control-mode" "" t nil)
(defun mb-hooks--multiple-cursors-mode-enabled ()
  "My `multiple-cursors' mode hook."
  (control-mode-reload-bindings))

(add-hook 'multiple-cursors-mode-enabled-hook
          #'mb-hooks--multiple-cursors-mode-enabled)

;; nXML
(defvar nxml-mode-map)
(defun mb-hooks--nxml-mode ()
  "My `nxml' mode hook."
  (setq-local company-backends '(company-nxml
                                 company-keywords
                                 company-files))
  (mb-f-define-keys nxml-mode-map
                    '(( "<tab>" . mb-cmd-snippet-or-complete))))

(add-hook 'nxml-mode-hook #'mb-hooks--nxml-mode)
(add-hook 'nxml-mode-hook #'mb-hooks--prog-mode)

;; Package Menu
(defun mb-hooks--package-menu-mode ()
  "My `package-menu' mode hook."
  (hl-line-mode)
  (mb-f-remap-keys package-menu-mode-map
                   '(("s" . "C-s")
                     ("l" . "C-l")
                     ("R" . "r")
                     ("r" . "C-r"))))

(add-hook 'package-menu-mode-hook #'mb-hooks--package-menu-mode)

;; Prog
(defun mb-hooks--prog-mode ()
  "My `prog-mode' hook."

  (setq-local fill-column 80)
  (unless (derived-mode-p 'makefile-mode)
    (setq-local indent-tabs-mode nil))

  (unless (derived-mode-p 'emacs-lisp-mode)
    (electric-operator-mode))

  (ws-butler-mode)
  (company-mode)
  (flycheck-mode)
  (fci-mode)
  (highlight-numbers-mode)
  (emr-initialize)
  (backward-forward-mode)

  (mb-f-define-keys prog-mode-map
                    '(( "<tab>"       . mb-cmd-snippet-or-complete)
                      ( "C-z f e"     . mb-cmd-iedit-in-defun)
                      ( "C-z f f"     . emr-show-refactor-menu)
                      ( "C-z d"       . nil)
                      ( "C-z d d"     . mb-cmd-realgud-debug)
                      ( "C-z d a"     . realgud-short-key-mode)))
  (mb-f-remap-keys  prog-mode-map
                    '(( "RET"         . "M-j"))))

(add-hook 'prog-mode-hook #'mb-hooks--prog-mode)

;; Projectile
(defvar projectile-known-projects)
(defun mb-hooks--projectile-mode ()
  "My `projectile' mode hook."

  (unless projectile-known-projects
    (mb-cmd-projectile-index-projects))

  (setq projectile-mode-line
        '(:eval (if (or (file-remote-p default-directory)
                        (string-match-p "/run/user/[0-9]+/gvfs/"
                                        default-directory))
                    " [?]"
                  (format " [%s]" (projectile-project-name)))))

  (projectile-register-project-type 'jhbuild
                                    (lambda () nil)
                                    :compile "jhbuild make"
                                    :test "make check"
                                    :run "jhbuild make && jhbuild run ${PWD##*/}")

  (mb-f-define-keys projectile-command-map
                    '(( "B"   . projectile-ibuffer)
                      ( "i"   . mb-cmd-projectile-index-projects)
                      ( "I"   . projectile-invalidate-cache)
                      ( "d"   . projectile-dired)
                      ( "V"   . mb-cmd-projectile-gitg)
                      ( "D"   . projectile-find-dir)
                      ( "s S" . mb-cmd-projectile-ag-regex)
                      ( "w a" . mb-cmd-projectile-ansi-term)
                      ( "w t" . mb-cmd-projectile-gnome-terminal)))

  (control-mode-reload-bindings)

  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired))

  (def-projectile-commander-method ?q
    "Go back to project selection."
    (projectile-switch-project))

  (def-projectile-commander-method ?a
    "Start an `ansi-term' session in the project root."
    (mb-cmd-projectile-ansi-term))

  (def-projectile-commander-method ?t
    "Spawn gnome-terminal in the project root."
    (mb-cmd-projectile-gnome-terminal))

  (def-projectile-commander-method ?V
    "Spawn gitg in the project root."
    (mb-cmd-projectile-gitg)))

(add-hook 'projectile-mode-hook #'mb-hooks--projectile-mode)

;; Python
(defvar anaconda-mode-map)
(defvar python-mode-map)
(defvar mb-cmd-realgud-debugger)
(defvar yas-indent-line)
(autoload 'realgud:ipdb "realgud" "" t nil)
(defun mb-hooks--python-mode ()
  "My `python' mode hook."
  (setq-local fill-column 79)           ; PEP0008 says lines should be 79 chars
  (setq-local mb-cmd-realgud-debugger #'realgud:ipdb)
  (setq-local company-backends '(company-anaconda))
  (anaconda-mode)
  (anaconda-eldoc-mode)
  (importmagic-mode)

  (add-to-list 'electric-layout-rules
               (cons ?: #'mb-f-python-electric-newline))
  ;; sort imports and conform to PEP0008 on save
  (add-hook 'before-save-hook 'py-isort-before-save)
  (py-autopep8-enable-on-save)

  (setq-local yas-indent-line 'fixed)
  (mb-f-define-keys python-mode-map
                    '(( "."         . mb-cmd-dot-and-complete)
                      ( "<tab>"     . mb-cmd-indent-snippet-or-complete))))

(add-hook 'python-mode-hook #'mb-hooks--python-mode)

;; Anaconda
;; TODO: Figure out bindings for this
(defun mb-hooks--anaconda-mode ()
  "My `anaconda' mode hook."
  (mb-f-define-keys anaconda-mode-map
                    '(( "C-<return>" . anaconda-mode-find-definitions)
                      ( "M-<return>" . anaconda-mode-find-assignments)
                      ( "C-z h d"    . anaconda-mode-show-doc)
                      ( "M-?"        . anaconda-mode-find-references))))

(add-hook 'anaconda-mode-hook #'mb-hooks--anaconda-mode)

;; Realgud Track
(defvar realgud-track-mode-map)
(defun mb-hooks--realgud-track-mode ()
  "My `realgud-track' mode hook."
  (mb-f-define-keys realgud-track-mode-map
                    '(( "."      . mb-cmd-dot-and-complete)
                      ( "<tab>"  . mb-cmd-snippet-or-complete))))

(add-hook 'realgud-track-mode-hook #'mb-hooks--realgud-track-mode)

;; REST Client
(defvar restclient-mode-map)
(defun mb-hooks--restclient-mode ()
  "My `restclient' mode hook."
  (company-mode)
  (setq-local company-backends '((company-restclient)))
  (mb-f-define-keys restclient-mode-map
                    '(( "<tab>" . mb-cmd-snippet-or-complete))))

(add-hook 'restclient-mode-hook #'mb-hooks--restclient-mode)

;; Rust
(defvar rust-mode-map)
(defun mb-hooks--rust-mode ()
  "My `rust' mode hook."
  (racer-mode)
  (mb-f-define-keys rust-mode-map
                    '(( "C-<return>" . racer-find-definition)
                      ( "."          . mb-cmd-dot-and-complete)
                      ( ":"          . mb-cmd-double-colon-and-complete))))

(add-hook 'rust-mode-hook #'mb-hooks--rust-mode)

;; Shell
(defvar term-raw-map)
(defvar yas-dont-activate-functions)
(defun mb-hooks--term-mode ()
  "My `term' mode hook."
  (setq yas-dont-activate-functions t)
  (mb-f-define-keys term-raw-map
                    '(( "M-x"       . smex)
                      ( "C-y"       . mb-cmd-term-paste)
                      ( "<escape>"  . ESC-prefix))))

(defun mb-hooks--term-exec ()
  "My `term' mode hook."
  (set-buffer-process-coding-system 'utf-8-unix
                                    'utf-8-unix))

(add-hook 'term-mode-hook #'mb-hooks--term-mode)
(add-hook 'term-exec-hook #'mb-hooks--term-exec)

;; Shell script
(defun mb-hooks--sh-mode ()
  "My `sh' mode hook."
  (setq-local defun-prompt-regexp
              (concat "^\\("
                      "\\(function[ \t]\\)?[ \t]*[[:alnum:]-_]+[ \t]*([ \t]*)"
                      "\\|"
                      "function[ \t]+[[:alnum:]-_]+[ \t]*\\(([ \t]*)\\)?"
                      "\\)[ \t]*"))
  (setq-local company-backends '((company-shell
                                  company-keywords
                                  company-files
                                  company-dabbrev-code)))
  (sh-extra-font-lock-activate))

(add-hook 'sh-mode-hook #'mb-hooks--sh-mode)


;; Sql
(defun mb-hooks--sql-mode ()
  "My `sql' mode hook."
  (sqlup-mode))

(add-hook 'sql-mode-hook #'mb-hooks--sql-mode)

;; Systemd
(defvar systemd-mode-map)
(defun mb-hooks-systemd-mode ()
  "My `systemd' mode hook."
  (company-mode)
  (setq-local company-backends '(company-capf))
  (mb-f-define-keys systemd-mode-map
                    '(( "<tab>" . mb-cmd-snippet-or-complete))))

(add-hook 'systemd-mode-hook #'mb-hooks-systemd-mode)

;; Vala
(defvar flycheck-checkers)
(defun mb-hooks--vala-mode ()
  "My `vala' mode hook."
  (require 'flycheck-vala)
  (add-to-list 'flycheck-checkers 'vala-valac))

(add-hook 'vala-mode-hook #'mb-hooks--prog-mode)
(add-hook 'vala-mode-hook #'mb-hooks--vala-mode)

;; Woman
(defvar woman-mode-map)
(defun mb-hooks--woman-mode ()
  "My `woman' mode hook."
  (mb-f-remap-keys woman-mode-map
                   '(("a" . "s")
                     ("s" . "C-s")
                     ("R" . "r")
                     ("r" . "C-r"))))

(add-hook 'woman-mode-hook #'mb-hooks--woman-mode)

;; Yaml
(defun mb-hooks--yaml-mode-hook ()
  "My `yaml' mode hook."
  (flycheck-yamllint-setup)
  (flycheck-mode))

(add-hook 'yaml-mode-hook #'mb-hooks--yaml-mode-hook)

(provide 'mb-hooks)
;;; mb-hooks.el ends here
