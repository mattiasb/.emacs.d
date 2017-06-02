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
(with-eval-after-load "ag"
  (defvar ag-mode-map)
  (mb-f-define-keys ag-mode-map
                    '(( "W" . wgrep-change-to-wgrep-mode))))

;; Backward Forward
(with-eval-after-load "backward-forward"
  (defvar backward-forward-mode-map)
  (mb-f-define-keys backward-forward-mode-map
                    '(("M-<left>"  . backward-forward-previous-location)
                      ("M-<right>" . backward-forward-next-location)
                      ("C-<left>"  . nil)
                      ("C-<right>" . nil))))

;; Browse Kill Ring
(with-eval-after-load "browse-kill-ring"
  (defvar browse-kill-ring-mode-map)
  (mb-f-define-keys browse-kill-ring-mode-map
                    '(( "<down>"    . browse-kill-ring-forward)
                      ( "<tab>"     . browse-kill-ring-forward)
                      ( "<up>"      . browse-kill-ring-previous)
                      ( "<backtab>" . browse-kill-ring-previous)
                      ( "C-g"       . browse-kill-ring-quit))))

;; C common
(defvar c-mode-base-map)
(defun mb-hooks--c-common ()
  "My `c-mode' mode hook."
  (unless (keymap-parent c-mode-base-map)
    (set-keymap-parent c-mode-base-map prog-mode-map)))

(add-hook 'c-mode-common-hook #'mb-hooks--c-common)

;; C / C++
(defun mb-hooks--c-mode ()
  "A mode hook for C and C++."
  (defvar company-backends)
  (defvar flycheck-disabled-checkers)
  (defvar flycheck-check-syntax-automatically)
  (defvar flycheck-highlighting-mode)

  (setq-local flycheck-disabled-checkers '(c/c++-gcc c/c++-clang))
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil)
  (setq-local company-backends '(company-rtags))

  ;; Use the RTags back-forward stuff instead
  (backward-forward-mode -1)

  (defvar projectile-command-map)
  (mb-f-define-keys projectile-command-map
                    '(( "j"              . rtags-find-symbol)
                      ( "R"              . mb-cmd-projectile-regen-rtags))))

(with-eval-after-load "cc-mode"
  (require 'rtags)
  (require 'flycheck-rtags)
  (require 'company-rtags)

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

  (add-hook 'c-mode-hook   #'mb-hooks--c-mode)
  (add-hook 'c++-mode-hook #'mb-hooks--c-mode))

;; CMake
(defun mb-hooks--cmake-mode ()
  "My `cmake' mode hook."
  (defvar company-backends)
  (setq-local company-backends '((company-cmake
                                  company-keywords
                                  company-files
                                  company-dabbrev-code))))
(with-eval-after-load "cmake"
  (add-hook 'cmake-mode-hook #'mb-hooks--prog-mode)
  (add-hook 'cmake-mode-hook #'mb-hooks--cmake-mode))

;; Control
(with-eval-after-load "control-mode"
  (require 'god-mode-isearch)
  (setq-default control-mode t)
  (global-control-mode)

  (defvar control-mode-keymap)
  (mb-f-define-keys control-mode-keymap
                    '(( "i"           . mb-cmd-control-mode-off)
                      ( "<escape>"    . ESC-prefix)
                      ( "x x"         . smex)
                      ( "x s"         . save-buffer)
                      ( "x S"         . save-some-buffers)))

  (mb-f-define-keys isearch-mode-map
                    '(( "<escape>" . god-mode-isearch-activate)
                      ( "<insert>" . god-mode-isearch-activate)))

  (defvar god-mode-isearch-map)
  (mb-f-define-keys god-mode-isearch-map
                    '(( "g"        . isearch-cancel)
                      ( "i"        . god-mode-isearch-disable)
                      ( "<insert>" . god-mode-isearch-disable)))


  (declare-function control-mode-ctrlx-hacks "control-mode.el")
  (add-hook 'control-mode-keymap-generation-functions
            #'control-mode-ctrlx-hacks)
  (add-hook 'control-mode-hook
            #'mb-f-control-mode-set-cursor))

;; Company
(with-eval-after-load "company"
  (company-quickhelp-mode)

  (defvar company-active-map)
  (mb-f-define-keys company-active-map
                    '(( "\C-n"    . company-select-next)
                      ( "\C-p"    . company-select-previous)
                      ( "<next>"  . mb-cmd-company-scroll-down)
                      ( "<prior>" . mb-cmd-company-scroll-up)
                      ( "\C-v"    . company-show-location)
                      ( "\C-g"    . company-abort)))

  (add-hook 'company-completion-started-hook   #'mb-f-fci-turn-off)
  (add-hook 'company-completion-finished-hook  #'mb-f-fci-turn-on)
  (add-hook 'company-completion-cancelled-hook #'mb-f-fci-turn-on))

;; Compilation Mode

(defvar compilation-filter-start)
(defun mb-hooks--compilation-filter ()
  "My `compilation-filter' mode hook."
  (when (eq major-mode 'compilation-mode)
    (require 'ansi-color)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(with-eval-after-load "compile"
  (add-hook 'compilation-filter-hook #'mb-hooks--compilation-filter))

;; Cython
(with-eval-after-load "cython-mode"
  (require 'flycheck-cython))

;; Dired
(defun mb-hooks--dired-mode ()
  "My `dired' mode hook."
  (declare-function dired-omit-mode "dired-x.el")
  (declare-function dired-hide-details-mode "dired.el")
  (auto-revert-mode)
  (hl-line-mode)
  (all-the-icons-dired-mode)
  (dired-omit-mode)
  (dired-hide-details-mode)
  (dired-hide-dotfiles-mode))

(with-eval-after-load "dired"
  (require 'dired-x)
  (require 'tramp)

  (defvar dired-mode-map)
  (mb-f-define-keys dired-mode-map
                    '(( "W" . wdired-change-to-wdired-mode)
                      ( "F" . find-name-dired)
                      ( "c" . mb-cmd-find-file-default)
                      ( "." . dired-hide-dotfiles-mode)))
  (mb-f-remap-keys dired-mode-map
                   '(("s" . "C-s")
                     ("r" . "C-r")))

  (add-hook 'dired-mode-hook #'mb-hooks--dired-mode))

;; ELisp
(defun mb-hooks--emacs-lisp-mode ()
  "My `emacs-lisp' mode hook."
  ;; This and the next imenu expression is based on code from Sebastian Wiesner
  ;; https://github.com/lunaryorn/my-old-.emacs.d/blob/master/lisp/lunaryorn-elisp.el#L51
  (add-to-list 'imenu-generic-expression
               `("Use Package" ,(rx "(use-package"
                                    (optional "-with-elapsed-timer")
                                    symbol-end
                                    (1+ (syntax whitespace))
                                    symbol-start
                                    (group-n 1 (1+ (or (syntax word)
                                                       (syntax symbol))))
                                    symbol-end)
                 1))
  (add-to-list 'imenu-generic-expression
               `("After Load" ,(rx "(with-eval-after-load"
                                   symbol-end
                                   (1+ whitespace)
                                   (syntax string-quote)
                                   (group-n 1 (1+ (or (syntax word)
                                                      (syntax symbol))))
                                   (syntax string-quote))
                 1))
  (setq page-delimiter
        (rx bol ";;;" (not (any "#")) (* not-newline) "\n"
            (* (* blank) (opt ";" (* not-newline)) "\n"))))

(with-eval-after-load "elisp-mode"
  (add-hook 'emacs-lisp-mode-hook #'lisp-extra-font-lock-mode)
  (add-hook 'emacs-lisp-mode-hook #'mb-hooks--emacs-lisp-mode))

;; Flycheck
(with-eval-after-load "flycheck"
  (flycheck-pos-tip-mode)
  (flycheck-status-emoji-mode)
  (flycheck-cask-setup)
  (flycheck-package-setup)
  (flycheck-rust-setup))

;; Flyspell
(with-eval-after-load "flyspell"
  (require 'flyspell-correct-popup)

  (defvar flyspell-mode-map)
  (mb-f-define-keys flyspell-mode-map
                    '(("C-," . mb-cmd-flyspell-goto-previous-error)
                      ("C-." . flyspell-goto-next-error)
                      ("C-;" . flyspell-correct-previous-word-generic)
                      ("C-:" . flyspell-correct-next-word-generic)))

  (add-hook 'flyspell-mode-hook #'flyspell-buffer))

;; Find-file
(add-hook 'find-file-not-found-functions #'mb-f-create-non-existent-directory)

;; Go
(defun mb-hooks--go-mode ()
  "My `go' mode hook."
  (go-eldoc-setup)

  (setq-local tab-width 4)
  (defvar company-backends)
  (setq-local company-backends '(company-go
                                 company-keywords
                                 company-files)))

(with-eval-after-load "go-mode"
  (defvar go-mode-map)

  (mb-f-define-keys go-mode-map
                    '(( "C-z i a"    . go-import-add)
                      ( "C-z i r"    . go-remove-unused-imports)
                      ( "C-z i g"    . go-goto-imports)
                      ( "C-z d"      . godoc-at-point)
                      ( "C-<return>" . godef-jump)
                      ( "."          . mb-cmd-dot-and-complete)))

  (add-hook 'go-mode-hook #'mb-hooks--go-mode))

;; Haskell
(with-eval-after-load "haskell-mode"
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode))

;; Help
(with-eval-after-load "help-mode"
  (defvar help-mode-map)
  (mb-f-define-keys help-mode-map
                    '(( "M-<left>"  . help-go-back)
                      ( "M-<right>" . help-go-forward)))
  (mb-f-remap-keys help-mode-map
                   '(("s" . "C-s")
                     ("r" . "C-r"))))

;; Ido
(with-eval-after-load "ido"
  (defvar ido-common-completion-map)
  (mb-f-define-keys ido-common-completion-map
                    '(( "<tab"    . ido-complete)
                      ( "<next>"  . mb-cmd-ido-scroll-down)
                      ( "<prior>" . mb-cmd-ido-scroll-up))))

;; IBuffer
(defun mb-hooks--ibuffer ()
  "My `ibuffer' mode hook."
  (ibuffer-projectile-set-filter-groups)

  (defvar ibuffer-sorting-mode)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(with-eval-after-load "ibuffer"
  (add-hook 'ibuffer-hook #'mb-hooks--ibuffer))

;; Iedit
(with-eval-after-load "iedit"
  (defvar iedit-mode-keymap)
  (mb-f-define-keys iedit-mode-keymap
                    '(("C-g"      . iedit-quit)
                      ("<return>" . iedit-quit)))

  (add-hook 'iedit-aborting-hook #'deactivate-mark))

;; IELM
(with-eval-after-load "ielm"
  (defvar ielm-map)
  (mb-f-define-keys ielm-map
                    '(( "<tab>" . mb-cmd-snippet-or-complete)))

  (add-hook 'ielm-mode-hook #'company-mode))

;; Info
(defun mb-hooks--Info-mode ()
  "My `Info' mode hook."
  (backward-forward-mode -1))

(with-eval-after-load "info"
  (mb-f-define-keys Info-mode-map
                    '(( "M-<left>"  . Info-history-back)
                      ( "M-<right>" . Info-history-forward)
                      ( "M-<up>"    . Info-up)))

  (add-hook 'Info-mode-hook #'mb-hooks--Info-mode)
  (add-hook 'Info-selection-hook #'niceify-info))

;; Java
(defun mb-hooks--java-mode ()
  "My `java' mode hook."
  (require 'ensime-company)
  (ensime)
  (defvar company-backends)
  (setq-local company-backends '((ensime-company))))

(with-eval-after-load "cc-mode"
  (add-hook 'java-mode-hook #'mb-hooks--java-mode))

;; JS2
(defun mb-hooks--js2-mode ()
  "My `js2' mode hook."
  (declare-function js2r-rename-var "js2-refactor.el")

  (defvar flimenu-imenu-separator)
  (setq-local flimenu-imenu-separator ".")

  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
  (js2-imenu-extras-mode)

  (defvar company-backends)
  (setq-local company-backends '((company-dabbrev-code
                                  company-files
                                  company-keywords))))

(with-eval-after-load "js2-mode"
  (require 'js2-refactor)

  (defvar js2-mode-map)
  (mb-f-define-keys js2-mode-map
                    '(( "C-z f r"    . js2r-rename-var)))

  (add-hook 'js2-mode-hook #'mb-hooks--js2-mode))

;; Todotxt
(autoload 'todotxt "todotxt" "" t nil)
(autoload 'todotxt-mode "todotxt" "" t nil)
(with-eval-after-load "todotxt"
  (defvar todotxt-mode-map)
  (mb-f-define-keys todotxt-mode-map
                    '(("j" . nil)
                      ("k" . todotxt-nuke-item)
                      ("_" . todotxt-undo)
                      ("u" . nil))))

;; JSON
(defun mb-hooks--json-mode ()
  "My `json' mode hook."
  (highlight-numbers-mode -1))

(with-eval-after-load "json-mode"
  (add-hook 'json-mode-hook #'mb-hooks--json-mode))

;; Lua
(defun mb-hooks--lua-mode ()
  "My `lua' mode hook."
  (defvar company-backends)
  (setq-local company-backends '((company-dabbrev-code
                                  company-files
                                  company-keywords))))

(with-eval-after-load "lua-mode"
  (add-hook 'lua-mode-hook #'mb-hooks--lua-mode))

;; Magit
(defun mb-hooks--git-commit-mode ()
  "My `git-commit' mode hook."
  (declare-function git-commit-turn-on-flyspell "git-commit.el")
  (declare-function git-commit-turn-on-auto-fill "git-commit.el")

  (mb-cmd-control-mode-off)
  (setq-local fill-column 72)
  (auto-fill-mode)
  (git-commit-turn-on-flyspell)
  (git-commit-turn-on-auto-fill)
  (git-commit-insert-issue-mode)
  (fci-mode 1))

(with-eval-after-load "magithub"
  (declare-function magithub-feature-autoinject "magithub.el")
  (magithub-feature-autoinject 't))

(with-eval-after-load "magit"
  (declare-function git-gutter:update-all-windows "git-gutter.el")
  (declare-function magit-define-popup-action "magit-popup.el")

  ;; magithub still seems a bit rough around the edges.
  ;; Let's try it out again later.
  ;;
  ;; (require 'magithub)

  (magit-define-popup-action 'magit-run-popup ?g "Gitg"
    #'mb-cmd-projectile-gitg)

  (magit-define-popup-action 'magit-run-popup ?a "ansi-term"
    #'mb-cmd-projectile-ansi-term)

  (magit-define-popup-action 'magit-run-popup ?t "gnome-terminal"
    #'mb-cmd-projectile-gnome-terminal)

  (defvar magit-blame-mode-map)
  (mb-f-define-keys magit-blame-mode-map
                    '(( "C-z t b" .  magit-blame-quit)))

  (defvar git-commit-mode-map)
  (mb-f-define-keys git-commit-mode-map
                    '(( "C-c C-f" . mb-cmd-git-commit-insert-issue-fix)))

  (add-hook 'magit-mode-hook         #'turn-on-magit-gitflow)
  (add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows)
  (add-hook 'magit-status-mode-hook  #'magit-filenotify-mode)
  (add-hook 'git-commit-mode-hook    #'mb-hooks--git-commit-mode))

;; Markdown
(defun mb-hooks--markdown-mode ()
  "My `markdown' mode hook."
  (setq-local fill-column 80)
  (setq-local indent-tabs-mode nil)

  (flyspell-mode)
  (fci-mode)
  (auto-fill-mode))

(with-eval-after-load "markdown-mode"
  (defvar markdown-mode-map)
  (mb-f-define-keys markdown-mode-map
                    '(( "C-<return>" . markdown-jump)
                      ( "C-c C-c p"  . mb-cmd-open-with)
                      ( "M-<up>"     . nil)
                      ( "M-<down>"   . nil)))

  (add-hook 'markdown-mode-hook #'mb-hooks--markdown-mode))

;; MTG deck mode
(defvar mtg-deck-mode-map)
(defun mb-hooks--mtg-deck-mode ()
  "My `mtg-deck' mode hook."
  (company-mode)
  (defvar company-backends)
  (setq-local company-backends '(company-capf)))

(with-eval-after-load "mtg-deck-mode"
  (mb-f-define-keys mtg-deck-mode-map
                    '(( "C-<return>" . mtg-deck-show-card-at-point)
                      ( "<tab>"      . mb-cmd-snippet-or-complete)))

  (add-hook 'mtg-deck-mode-hook #'mb-hooks--mtg-deck-mode))

;; Multiple Cursors
(defun mb-hooks--multiple-cursors-mode-enabled ()
  "My `multiple-cursors' mode hook."
  (declare-function control-mode-reload-bindings "control-mode.el")
  (control-mode-reload-bindings))

(with-eval-after-load "multiple-cursors"
  (add-hook 'multiple-cursors-mode-enabled-hook
            #'mb-hooks--multiple-cursors-mode-enabled))

;; nXML
(defun mb-hooks--nxml-mode ()
  "My `nxml' mode hook."
  (defvar company-backends)
  (setq-local company-backends '(company-nxml
                                 company-keywords
                                 company-files)))

(with-eval-after-load "nxml-mode"
  (defvar nxml-mode-map)
  (mb-f-define-keys nxml-mode-map
                    '(( "<tab>" . mb-cmd-snippet-or-complete)))

  (add-hook 'nxml-mode-hook #'mb-hooks--nxml-mode)
  (add-hook 'nxml-mode-hook #'mb-hooks--prog-mode))

;; Package Menu
(with-eval-after-load "package"
  (mb-f-remap-keys package-menu-mode-map
                   '(("s" . "C-s")
                     ("l" . "C-l")
                     ("R" . "r")
                     ("r" . "C-r")))

  (add-hook 'package-menu-mode-hook #'hl-line-mode))

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
  (backward-forward-mode))

(with-eval-after-load "prog-mode"
  (mb-f-define-keys prog-mode-map
                    '(( "<tab>"       . mb-cmd-snippet-or-complete)
                      ( "C-z f e"     . mb-cmd-iedit-in-defun)
                      ( "C-z f f"     . emr-show-refactor-menu)
                      ( "C-z d"       . nil)
                      ( "C-z d d"     . mb-cmd-realgud-debug)
                      ( "C-z d a"     . realgud-short-key-mode)))
  (mb-f-remap-keys  prog-mode-map
                    '(( "RET"         . "M-j")))

  (add-hook 'prog-mode-hook #'mb-hooks--prog-mode))

;; Projectile
(with-eval-after-load "projectile"
  (defvar projectile-known-projects)

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
  (projectile-register-project-type 'win-batch
                                    '("build.bat")
                                    :compile "cmd.exe \"/c build\"")
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
    (mb-cmd-projectile-gitg))

  (add-hook 'projectile-mode-hook #'control-mode-reload-bindings))

;; Python
(defun mb-hooks--python-mode ()
  "My `python' mode hook."
  (declare-function realgud:ipdb "realgud.el")

  (setq-local fill-column 79)           ; PEP0008 says lines should be 79 chars

  (defvar yas-indent-line)
  (setq-local yas-indent-line 'fixed)

  (defvar mb-cmd-realgud-debugger)
  (setq-local mb-cmd-realgud-debugger #'realgud:ipdb)

  (defvar company-backends)
  (setq-local company-backends '(company-anaconda))

  (anaconda-mode)
  (anaconda-eldoc-mode)
  (importmagic-mode)
  (aggressive-indent-mode -1)
  (indent-tools-minor-mode)
  (add-to-list 'electric-layout-rules
               (cons ?: #'mb-f-python-electric-newline))
  ;; sort imports and conform to PEP0008 on save
  (add-hook 'before-save-hook 'py-isort-before-save)
  (py-autopep8-enable-on-save))

(with-eval-after-load "python"
  (defvar python-mode-map)
  (mb-f-define-keys python-mode-map
                    '(( "."           . mb-cmd-dot-and-complete)
                      ( "<tab>"       . mb-cmd-indent-snippet-or-complete)
                      ( "C-z <left>"  . indent-tools-demote)
                      ( "C-z <right>" . indent-tools-indent)))

  (add-hook 'python-mode-hook #'mb-hooks--python-mode))

(with-eval-after-load "anaconda-mode"
  (defvar anaconda-mode-map)
  (mb-f-define-keys anaconda-mode-map
                    '(( "C-<return>" . anaconda-mode-find-definitions)
                      ( "M-<return>" . anaconda-mode-find-assignments)
                      ( "C-z h d"    . anaconda-mode-show-doc)
                      ( "M-?"        . anaconda-mode-find-references))))

;; Realgud Track
(with-eval-after-load "realgud"
  (defvar realgud-track-mode-map)
  (mb-f-define-keys realgud-track-mode-map
                    '(( "."      . mb-cmd-dot-and-complete)
                      ( "<tab>"  . mb-cmd-snippet-or-complete))))

;; REST Client
(defun mb-hooks--restclient-mode ()
  "My `restclient' mode hook."
  (company-mode)
  (defvar company-backends)
  (setq-local company-backends '((company-restclient))))

(with-eval-after-load "restclient"
  (defvar restclient-mode-map)
  (mb-f-define-keys restclient-mode-map
                    '(( "<tab>" . mb-cmd-snippet-or-complete)))

  (add-hook 'restclient-mode-hook #'mb-hooks--restclient-mode))

;; Rust
(with-eval-after-load "rust-mode"
  (defvar rust-mode-map)
  (mb-f-define-keys rust-mode-map
                    '(( "C-<return>" . racer-find-definition)
                      ( "."          . mb-cmd-dot-and-complete)
                      ( ":"          . mb-cmd-double-colon-and-complete)))

  (add-hook 'rust-mode-hook #'racer-mode))

;; Shell
(defun mb-hooks--term-mode ()
  "My `term' mode hook."
  (defvar yas-dont-activate-functions)
  (setq yas-dont-activate-functions t))

(defun mb-hooks--term-exec ()
  "My `term' mode hook."
  (set-buffer-process-coding-system 'utf-8-unix
                                    'utf-8-unix))

(with-eval-after-load "term"
  (defvar term-raw-map)
  (mb-f-define-keys term-raw-map
                    '(( "M-x"       . smex)
                      ( "C-y"       . mb-cmd-term-paste)
                      ( "<escape>"  . ESC-prefix)))

  (add-hook 'term-mode-hook #'mb-hooks--term-mode)
  (add-hook 'term-exec-hook #'mb-hooks--term-exec))

;; Shell script
(defun mb-hooks--sh-mode ()
  "My `sh' mode hook."

  (setq-local defun-prompt-regexp
              (concat "^\\("
                      "\\(function[ \t]\\)?[ \t]*[[:alnum:]-_]+[ \t]*([ \t]*)"
                      "\\|"
                      "function[ \t]+[[:alnum:]-_]+[ \t]*\\(([ \t]*)\\)?"
                      "\\)[ \t]*"))

  (sh-extra-font-lock-activate)
  (defvar company-backends)
  (setq-local company-backends '((company-shell
                                  company-keywords
                                  company-files
                                  company-dabbrev-code))))

(with-eval-after-load "sh-script"
  (require 'electric-operator)

  (declare-function electric-operator-add-rules-for-mode
                    "electric-operator.el")
  (electric-operator-add-rules-for-mode 'sh-mode
                                        (cons "=" nil))
  (add-hook 'sh-mode-hook #'mb-hooks--sh-mode))

;; Sql
(with-eval-after-load "sql"
  (add-hook 'sql-mode-hook #'sqlup-mode))

;; Systemd
(defun mb-hooks-systemd-mode ()
  "My `systemd' mode hook."
  (company-mode)
  (defvar company-backends)
  (setq-local company-backends '(company-capf)))


(with-eval-after-load "systemd"
  (add-hook 'systemd-mode-hook #'mb-hooks-systemd-mode)

  (defvar systemd-mode-map)
  (mb-f-define-keys systemd-mode-map
                    '(( "<tab>" . mb-cmd-snippet-or-complete))))

;; Vala
(defun mb-hooks--vala-mode ()
  "My `vala' mode hook."
  (defvar flycheck-checkers)
  (add-to-list 'flycheck-checkers 'vala-valac))

(with-eval-after-load "vala-mode"
  (require 'flycheck-vala)
  (add-hook 'vala-mode-hook #'mb-hooks--prog-mode)
  (add-hook 'vala-mode-hook #'mb-hooks--vala-mode))

;; Visual Regexp
(with-eval-after-load "visual-regexp-steroids"
  (mb-f-define-keys esc-map
                    '(( "C-r" . vr/isearch-backward)
                      ( "C-s" . vr/isearch-forward))))

;; Woman
(defun mb-hooks--woman-mode ()
  "My `woman' mode hook."
  (defvar woman-mode-map)
  (mb-f-remap-keys woman-mode-map
                   '(("a" . "s")
                     ("s" . "C-s")
                     ("R" . "r")
                     ("r" . "C-r"))))

(with-eval-after-load "woman"
  (add-hook 'woman-mode-hook #'mb-hooks--woman-mode))

;; Yaml
(defun mb-hooks--yaml-mode-hook ()
  "My `yaml' mode hook."
  (flycheck-yamllint-setup)
  (flycheck-mode)
  (indent-tools-minor-mode))

(with-eval-after-load "yaml-mode"
  (defvar yaml-mode-map)
  (mb-f-define-keys yaml-mode-map
                    '(( "C-z <left>"  . indent-tools-demote)
                      ( "C-z <right>" . indent-tools-indent)))

  (add-hook 'yaml-mode-hook #'mb-hooks--yaml-mode-hook))

(provide 'mb-hooks)
;;; mb-hooks.el ends here
