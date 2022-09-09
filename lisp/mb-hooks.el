;;; mb-hooks.el --- My mode hooks -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2022, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20160417
;; Keywords         : local
;; Package-Requires : ((emacs "27.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 27.x

;;; Commentary:

;; My mode hooks.

;;; Note:

;;; Code:

(require 'mb-f)
(require 'mb-cmd)

;;; Standard hooks

;; After Save
(add-hook 'after-save-hook
          #'mb-f-executable-make-buffer-file-executable-if-script-p)

;; Make new Frames use the dark theme variant and have focus
(add-hook 'after-make-frame-functions #'select-frame-set-input-focus)
(add-hook 'after-make-frame-functions #'mb-f-set-dark-wm-theme)

(defun mb-hooks--window-setup-hook ()
  "My window-setup hook.

Based on: http://www.whiz.se/2016/05/01/dark-theme-in-emacs/"
  (mb-f-set-dark-wm-theme (selected-frame))
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'mb-hooks--window-setup-hook)

;;; Packages

;; Ansible
(defun mb-hooks--ansible-hook ()
  "My `yaml' mode hook."
  ;; TODO: Wrap with `cape-company-to-capf'
  ;; (defvar company-backends)
  ;; (setq-local company-backends
  ;;             (when (and (boundp 'ansible) ansible)
  ;;               '(company-ansible)))
  ;; (company-mode)
  (mb-f-add-electric-pairs '((?\( . ?\))))
  (ansible-doc-mode)
  (when (ansible-vault--is-encrypted-vault-file)
    (ansible-vault-mode 1))
  (font-lock-flush))

(with-eval-after-load "ansible"
  (add-hook 'ansible-hook #'mb-hooks--ansible-hook))

;; Browse Kill Ring
(with-eval-after-load 'browse-kill-ring
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
  "A mode hook for C and C++.")

(with-eval-after-load 'cc-mode
  (add-hook 'c-mode-hook   #'mb-hooks--c-mode)
  (add-hook 'c++-mode-hook #'mb-hooks--c-mode))

;; CMake
(with-eval-after-load 'cmake-mode
  (add-hook 'cmake-mode-hook #'mb-hooks--prog-mode))

;; Corfu
;;   TODO:
;;    - Get icons working
(defalias 'eglot+keyword
  (cape-super-capf #'eglot #'cape-keyword))

(defun mb-hooks--corfu-mode ()
  "My `corfu' mode hook."
  (corfu-doc-mode))

(with-eval-after-load 'corfu
  (mb-f-define-keys corfu-map
                    '(( "\C-l"    . corfu-info-location)
                      ( "\C-h"    . corfu-info-documentation)))

  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)

  (add-hook 'corfu-mode-hook #'mb-hooks--corfu-mode))


;; Compilation Mode

(with-eval-after-load 'compile
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;; Cython
(with-eval-after-load 'cython-mode
  (require 'flycheck-cython))

;; Daemons
(with-eval-after-load 'daemons
  (defvar daemons-mode-map)
  (mb-f-define-keys daemons-mode-map
                    '(("a" . mwim-beginning-of-code-or-line)
                      ("e" . mwim-end-of-code-or-line)
                      ("S" . tabulated-list-sort)
                      ("k" . daemons-stop-at-point)     ; Stop (kill)
                      ("x" . daemons-start-at-point)    ; Start
                      ("l" . daemons-reload-at-point))) ; Reload

  (mb-f-remap-keys daemons-mode-map
                   '(("s" . "C-s")
                     ("r" . "C-r"))))

;; Dockerfile
(defun mb-hooks--dockerfile-mode ()
  "My `dockerfile' mode hook."
  ;; dockerfile-mode uses tab-width to check indentation. 😢
  (setq-local tab-width 4))

(with-eval-after-load 'dockerfile-mode
  (add-hook 'dockerfile-mode-hook #'mb-hooks--dockerfile-mode))

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

(with-eval-after-load 'dired
  (require 'dired-x)
  (require 'tramp)

  (defvar dired-mode-map)
  (mb-f-define-keys dired-mode-map
                    '(( "W" . wdired-change-to-wdired-mode)
                      ( "F" . find-name-dired)
                      ( "c" . mb-cmd-find-file-default)
                      ( "M-<up>" . dired-up-directory)
                      ( "." . dired-hide-dotfiles-mode)))
  (mb-f-remap-keys dired-mode-map
                   '(("s" . "C-s")
                     ("r" . "C-r")))

  (add-hook 'dired-mode-hook #'mb-hooks--dired-mode))

(with-eval-after-load 'dired-sidebar
  (add-hook 'dired-sidebar-mode-hook #'hide-mode-line-mode))

;; Eglot
(with-eval-after-load 'eglot
  (defvar eglot-mode-map)
  (defvar eglot-server-programs)

  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  (mb-f-define-keys eglot-mode-map
                    '(( "C-z f r" . eglot-rename)
                      ( "C-z f f" . eglot-format)
                      ( "C-z f a" . eglot-code-actions)
                      ( "C-z f i" . eglot-code-action-organize-imports))))

;; Electric operator
(with-eval-after-load 'electric-operator
  ;; TODO: Add support for toml-mode
  (electric-operator-add-rules-for-mode 'ini-mode
                                        (cons "-" nil))
  (electric-operator-add-rules-for-mode 'makefile-mode
                                        (cons "-" nil))
  (electric-operator-add-rules-for-mode 'makefile-gmake-mode
                                        (cons "-" nil))
  (electric-operator-add-rules-for-mode 'haskell-mode
                                        (cons "-" nil))
  (electric-operator-add-rules-for-mode 'ruby-mode
                                        (cons "=" nil))
  (electric-operator-add-rules-for-mode 'ruby-mode
                                        (cons ">" nil))
  (electric-operator-add-rules-for-mode 'sh-mode
                                        (cons "=" nil)))

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
                                   (or (syntax string-quote)
                                       (syntax expression-prefix))
                                   (group-n 1 (1+ (or (syntax word)
                                                      (syntax symbol))))
                                   (optional (syntax string-quote)))
                 1))
  (setq page-delimiter
        (rx bol ";;;" (not (any "#")) (* not-newline) "\n"
            (* (* blank) (opt ";" (* not-newline)) "\n")))
  (mb-f-define-keys emacs-lisp-mode-map
                    '(("M-q"     . mb-cmd-elisp-fill-function-arguments))))

(with-eval-after-load 'elisp-mode
  (mb-f-define-keys emacs-lisp-mode-map
                    '(("C-z d" . mb-cmd-describe-symbol)))

  (add-hook 'emacs-lisp-mode-hook #'lisp-extra-font-lock-mode)
  (add-hook 'emacs-lisp-mode-hook #'mb-hooks--emacs-lisp-mode))

;; Enriched Text mode
;; Workaround security bug
;; https://lists.gnu.org/archive/html/info-gnu/2017-09/msg00006.html
(eval-after-load 'enriched
  '(defun enriched-decode-display-prop (start end &optional _)
     (list start end)))

;; Flymake
(with-eval-after-load 'flymake
  (require 'flymake-diagnostic-at-point)
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;; Flycheck
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode)
  (flycheck-status-emoji-mode)
  (flycheck-cask-setup)
  (flycheck-package-setup)
  (flycheck-rust-setup))

;; Flyspell
(with-eval-after-load 'flyspell
  (defvar flyspell-mode-map)
  (mb-f-define-keys flyspell-mode-map
                    '(("C-," . mb-cmd-flyspell-goto-previous-error)
                      ("C-." . flyspell-goto-next-error)
                      ("C-;" . flyspell-correct-previous-word-generic)
                      ("C-:" . flyspell-correct-next-word-generic)))

  (add-hook 'flyspell-mode-hook #'flyspell-buffer))

;; Find-file
(add-hook 'find-file-not-found-functions #'mb-f-create-non-existent-directory)

;; Git Gutter
(with-eval-after-load 'git-gutter
  (run-at-time 0 5 #'git-gutter:update-all-windows))

;; Go
(defun mb-hooks--go-mode ()
  "My `go' mode hook."
  (eglot-ensure)

  (setq-local tab-width 4))

(defun mb-hooks--go-before-save ()
  ;; This fails on just a single import
  (with-demoted-errors "%S"
    (call-interactively 'eglot-code-action-organize-imports))
  (eglot-format-buffer))

(with-eval-after-load 'go-mode
  (defvar go-mode-map)

  (eglot-ensure)
  (mb-f-define-keys go-mode-map
                    '(( "C-z i a"    . go-import-add)
                      ( "C-z i r"    . go-remove-unused-imports)
                      ( "C-z i g"    . go-goto-imports)
                      ( "C-z d"      . godoc-at-point)))
  (add-hook 'before-save-hook #'mb-hooks--go-before-save)
  (add-hook 'go-mode-hook #'mb-hooks--go-mode))

;; Haskell
(defun mb-hooks--haskell-mode ()
  "My `haskell' mode hook."
  (haskell-indentation-mode)
  (electric-layout-mode -1))

(add-hook 'haskell-mode-hook #'mb-hooks--haskell-mode)

;; Help
(defun mb-hooks--help-mode ()
  "My `help' mode hook.")

(with-eval-after-load 'help-mode
  (defvar help-mode-map)
  (mb-f-define-keys help-mode-map
                    '(( "M-<left>"  . help-go-back)
                      ( "M-<right>" . help-go-forward)))
  (mb-f-remap-keys help-mode-map
                   '(("s" . "C-s")
                     ("r" . "C-r")))

  (add-hook 'help-mode-hook #'mb-hooks--help-mode))

;; History

(with-eval-after-load 'history
  (set-face-attribute 'history-current-history
                      nil
                      :foreground
                      (face-attribute 'default :background)))

;; IBuffer
(defun mb-hooks--ibuffer ()
  "My `ibuffer' mode hook."
  (ibuffer-projectile-set-filter-groups)

  (defvar ibuffer-sorting-mode)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(with-eval-after-load 'ibuffer
  (add-hook 'ibuffer-hook #'mb-hooks--ibuffer))

;; Iedit
(with-eval-after-load 'iedit
  (defvar iedit-mode-keymap)
  (mb-f-define-keys iedit-mode-keymap
                    '(("C-g"      . iedit-mode)
                      ("<return>" . iedit-mode)))

  (add-hook 'iedit-aborting-hook #'deactivate-mark))

;; Info
(defun mb-hooks--Info-mode ()
  "My `Info' mode hook.")

(with-eval-after-load 'info
  (mb-f-define-keys Info-mode-map
                    '(( "M-<left>"  . Info-history-back)
                      ( "M-<right>" . Info-history-forward)
                      ( "M-<up>"    . Info-up)))

  (add-hook 'Info-mode-hook #'mb-hooks--Info-mode)
  (add-hook 'Info-selection-hook #'niceify-info))

;; Jinja2
(defun mb-hooks--jinja2-mode ()
  "My `jinja2' mode hook.")

(with-eval-after-load 'jinja2
  (add-hook 'jinja2-mode-hook #'mb-hooks--jinja2-mode))

;; JS2
(defun mb-hooks--js2-mode ()
  "My `js2' mode hook."
  (declare-function js2r-rename-var "js2-refactor.el")

  (defvar flimenu-imenu-separator)
  (setq-local flimenu-imenu-separator ".")
  (flycheck-disable-checker 'javascript-jscs)
  (flycheck-disable-checker 'javascript-jshint)
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
  (js2-imenu-extras-mode)
  (add-node-modules-path)
  (when (flycheck-eslint-config-exists-p)
    (js-auto-format-mode)))

(with-eval-after-load 'js2-mode
  (require 'js2-refactor)

  (defvar js2-mode-map)
  (mb-f-define-keys js2-mode-map
                    '(( "C-z f r"    . js2r-rename-var)))

  (add-hook 'js2-mode-hook #'mb-hooks--js2-mode))

;; JSON
(defun mb-hooks--json-mode ()
  "My `json' mode hook."
  (highlight-numbers-mode -1))

(with-eval-after-load 'json-mode
  (add-hook 'json-mode-hook #'mb-hooks--json-mode))

;; Ligature
(defun mb-hooks--ligature-mode ()
  "My `ligature' mode hook.")

(with-eval-after-load 'ligature
  (ligature-set-ligatures 'prog-mode
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||="
                            "||>" ":::" "::=" "=:=" "===" "==>" "=!=" "=>>"
                            "=<<" "=/=" "!==" "!!." ">=>" ">>=" ">>>" ">>-"
                            ">->" "->>" "-->" "---" "-<<" "<~~" "<~>" "<*>"
                            "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->" "<--"
                            "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_("
                            "..<" "..." "+++" "/==" "///" "_|_" "www" "&&" "^="
                            "~~" "~@" "~=" "~>" "~-" "**" "*>" "*/" "||" "|}"
                            "|]" "|=" "|>" "|-" "{|" "[|" "]#" "::" ":=" ":>"
                            ":<" "$>" "==" "=>" "!=" "!!" ">:" ">=" ">>" ">-"
                            "-~" "-|" "->" "--" "<:" "<$" "<=" "<>" "<-" "<<"
                            "<+" "</" "#{" "#[" "#:" "#=" "#!" "##" "#(" "#?"
                            "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:" "?="
                            "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*"
                            "*)" "\\\\" "://"))
  (add-hook 'ligature-mode-hook #'mb-hooks--ligature-mode))


;; LSP
(defun mb-hooks--lsp-mode ()
  "My `lsp' mode hook.")

(with-eval-after-load 'lsp-mode)

;; Lastpass
(with-eval-after-load 'lastpass
  (lastpass-auth-source-enable)
  (add-hook 'auth-source-backend-parser-functions
            #'lastpass-auth-source-backend-parse))

;; Magit
(defun mb-hooks--git-commit-setup ()
  "My `git-commit' mode hook."
  (setq fill-column 72)
  (display-fill-column-indicator-mode))

(with-eval-after-load 'magit
  (require 'forge)
  (defvar ghub-insecure-hosts)
  (defvar forge-alist)
  (declare-function git-gutter:update-all-windows "git-gutter.el")

  ;; Support insecure forges
  (defclass forge-gitlab-http-repository (forge-gitlab-repository)
    ((issues-url-format         :initform "http://%h/%o/%n/issues")
     (issue-url-format          :initform "http://%h/%o/%n/issues/%i")
     (issue-post-url-format     :initform "http://%h/%o/%n/issues/%i#note_%I")
     (pullreqs-url-format       :initform "http://%h/%o/%n/merge_requests")
     (pullreq-url-format        :initform "http://%h/%o/%n/merge_requests/%i")
     (pullreq-post-url-format   :initform "http://%h/%o/%n/merge_requests/%i#note_%I")
     (commit-url-format         :initform "http://%h/%o/%n/commit/%r")
     (branch-url-format         :initform "http://%h/%o/%n/commits/%r")
     (remote-url-format         :initform "http://%h/%o/%n")
     (create-issue-url-format   :initform "http://%h/%o/%n/issues/new")
     (create-pullreq-url-format :initform "http://%h/%o/%n/merge_requests/new")
     (pullreq-refspec           :initform "+refs/merge-requests/*/head:refs/pullreqs/*")))

  (add-to-list 'ghub-insecure-hosts "git.smarteye.se/api/v4")

  (transient-define-suffix magit-submodule-populate-all ()
    "Update *all* submodules"
    :description "Populate All   git submodule update --init --recursive"
    (interactive)
    (magit-with-toplevel
      (magit-run-git-async "submodule" "update" "--init" "--recursive")))

  (transient-append-suffix 'magit-submodule "p"
    '("P" magit-submodule-populate-all))

  (transient-append-suffix 'magit-run "!"
    '("g" "Gitg" mb-cmd-projectile-gitg))

  (transient-append-suffix 'magit-run "!"
    '("a" "ansi-term" mb-cmd-projectile-ansi-term))

  (transient-append-suffix 'magit-run "!"
    '("v" "vterm" projectile-run-vterm))

  (transient-append-suffix 'magit-run "!"
    '("t" "terminal" mb-cmd-projectile-terminal))

  (magit-todos-mode)

  (defvar magit-blame-mode-map)
  (mb-f-define-keys magit-blame-mode-map
                    '(( "C-z t b" .  magit-blame-quit)))

  (add-hook 'after-save-hook         #'magit-after-save-refresh-status)
  (add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows)
  (add-hook 'git-commit-setup-hook   #'mb-hooks--git-commit-setup))

;; Markdown
(defun mb-hooks--markdown-mode ()
  "My `markdown' mode hook."
  (auto-fill-mode)
  (pandoc-mode)
  (pandoc-load-default-settings))

(with-eval-after-load 'markdown-mode
  (defvar markdown-mode-map)

  (mb-f-define-keys markdown-mode-map
                    '(( "C-<return>" . mb-cmd-markdown-jump)
                      ( "C-c p"      . pandoc-main-hydra/body)
                      ( "M-<up>"     . nil)
                      ( "M-<down>"   . nil)))

  (add-hook 'markdown-mode-hook #'mb-hooks--markdown-mode))

;; MTG deck mode
(with-eval-after-load 'mtg-deck-mode
  (mb-f-define-keys mtg-deck-mode-map
                    '(( "C-<return>" . mtg-deck-show-card-at-point)
                      ( "C-c C-s"    . mtg-deck-sideboard-toggle))))

(with-eval-after-load 'nxml-mode
  (add-hook 'nxml-mode-hook #'mb-hooks--prog-mode))

;; Package Menu
(with-eval-after-load 'package
  (mb-f-remap-keys package-menu-mode-map
                   '(("s" . "C-s")
                     ("l" . "C-l")
                     ("R" . "r")
                     ("r" . "C-r")))

  (add-hook 'package-menu-mode-hook #'hl-line-mode))

;; Prog
(defun mb-hooks--prog-mode ()
  "My `prog-mode' hook."

  (defvar aggressive-indent-excluded-modes)
  (setq-local fill-column 80)
  (unless (derived-mode-p 'makefile-mode)
    (setq-local indent-tabs-mode nil))

  (unless (derived-mode-p 'lisp-mode
                          'emacs-lisp-mode
                          'dockerfile-mode)
    ;; https://github.com/davidshepherd7/electric-operator/pull/71
    (require 'electric-operator)
    (electric-operator-mode))

  (unless (derived-mode-p 'lisp-mode
                          'emacs-lisp-mode)
    (mb-f-add-electric-pairs '((?' . ?')
                               (?< . ?>))))

  ;; This logic is taken from aggressive-indent-mode
  (require 'aggressive-indent)
  (unless (or (cl-member-if #'derived-mode-p aggressive-indent-excluded-modes)
              (equal indent-line-function #'indent-relative)
              buffer-read-only)
    (aggressive-indent-mode))
  (ws-butler-mode)
  (flymake-mode)
  (display-fill-column-indicator-mode)
  (highlight-numbers-mode)
  (ligature-mode))

(with-eval-after-load 'prog-mode
  (mb-f-define-keys prog-mode-map
                    '(( "C-z f e"     . mb-cmd-iedit-in-defun)
                      ( "C-z d"       . nil)
                      ( "M-q"         . fill-function-arguments-dwim)))

  (add-hook 'prog-mode-hook #'mb-hooks--prog-mode))

;; Projectile
(with-eval-after-load 'projectile
  (defvar projectile-known-projects)
  (defvar projectile-mode-line)

  (unless projectile-known-projects
    (mb-cmd-projectile-index-projects))

  (setq projectile-mode-line
        '(:eval (if (or (file-remote-p default-directory)
                        (string-match-p "/run/user/[0-9]+/gvfs/"
                                        default-directory))
                    " [?]"
                  (format " [%s]" (projectile-project-name)))))
  (projectile-register-project-type 'win-batch
                                    '("build.bat")
                                    :compile "cmd.exe \"/c build\"")
  (mb-f-define-keys projectile-command-map
                    '(( "B"   . projectile-ibuffer)
                      ( "i"   . mb-cmd-projectile-index-projects)
                      ( "I"   . projectile-invalidate-cache)
                      ( "d"   . projectile-dired)
                      ( "D"   . projectile-find-dir)
                      ( "4 d" . projectile-dired-other-window)
                      ( "4 D" . projectile-find-dir-other-window)
                      ( "5 d" . projectile-dired-other-frame)
                      ( "5 D" . projectile-find-dir-other-frame)
                      ( "V"   . mb-cmd-projectile-gitg)
                      ( "s s" . projectile-ripgrep)
                      ( "s S" . mb-cmd-projectile-ripgrep-regex)
                      ( "x a" . mb-cmd-projectile-ansi-term)
                      ( "x t" . mb-cmd-projectile-terminal)))

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
    "Spawn terminal in the project root."
    (mb-cmd-projectile-terminal))

  (def-projectile-commander-method ?V
    "Spawn gitg in the project root."
    (mb-cmd-projectile-gitg))

  (add-hook 'find-file-hook #'mb-f-projectile-relative-buf-name))

;; Python
(defun mb-hooks--python-mode ()
  "My `python' mode hook."

  (setq-local fill-column 79)           ; PEP0008 says lines should be 79 chars

  (defvar yas-indent-line)
  (setq-local yas-indent-line 'fixed)

  ;; (importmagic-mode)
  (pipenv-mode)
  (eglot-ensure)

  (aggressive-indent-mode -1)
  (setq-local electric-layout-rules '((?: . mb-f-python-electric-newline))))

(with-eval-after-load 'python
  (defvar python-mode-map)
  (add-hook 'python-mode-hook #'mb-hooks--python-mode))

;; RipGrep
(with-eval-after-load 'ripgrep
  (defvar ripgrep-search-mode-map)
  (mb-f-define-keys ripgrep-search-mode-map
                    '(( "W" . wgrep-change-to-wgrep-mode))))

;; Rust
(with-eval-after-load 'rust-mode)

;; Table
(with-eval-after-load "table"
  (defvar table-command-remap-alist)
  (push '(delete-forward-char . *table--cell-delete-char)
        table-command-remap-alist))

;; Shell
(defun mb-hooks--term-mode ()
  "My `term' mode hook."
  (defvar yas-dont-activate-functions)
  (setq scroll-margin 0)
  (setq yas-dont-activate-functions t))

(defun mb-hooks--term-exec ()
  "My `term' mode hook."
  (set-buffer-process-coding-system 'utf-8-unix
                                    'utf-8-unix))

(with-eval-after-load 'term
  (defvar term-raw-map)
  (mb-f-define-keys term-raw-map
                    '(( "M-x"       . execute-extended-command)
                      ( "C-y"       . mb-cmd-term-paste)
                      ( "<escape>"  . ESC-prefix)))

  (add-hook 'term-mode-hook #'mb-hooks--term-mode)
  (add-hook 'term-exec-hook #'mb-hooks--term-exec))

;; Text
(defun mb-hooks--text-mode ()
  "My `text' mode hook."
  (setq-local fill-column 80)
  (setq-local indent-tabs-mode nil)

  (display-fill-column-indicator-mode)

  ;;;; Disable flyspell for now
  ;;
  ;; (unless (derived-mode-p 'yaml-mode 'jinja2-mode)
  ;;   (guess-language-mode)
  ;;   (guess-language)
  ;;   (flyspell-mode)
  ;;   (auto-fill-mode))
  )

;; Text mode doesn't have a (provide)
(add-hook 'text-mode-hook #'mb-hooks--text-mode)

;; Todotxt
(defun mb-hooks--todotxt-mode ()
  "My `todotxt' mode hook."
  (todotxt-show-incomplete)
  (set-window-dedicated-p (selected-window) t))

(with-eval-after-load 'todotxt
  (defvar todotxt-mode-map)
  (mb-f-define-keys todotxt-mode-map
                    '(("<return>" . todotxt-edit-item)
                      ("e"        . nil)
                      ("j"        . nil)
                      ("k"        . todotxt-nuke-item)
                      ("N"        . nil)
                      ("+"        . todotxt-add-item)
                      ("a"        . todotxt-archive)
                      ("A"        . nil)
                      ("_"        . todotxt-undo)
                      ("u"        . nil)
                      ("f"        . todotxt-filter-for)
                      ("/"        . nil)
                      ("F"        . todotxt-filter-out)
                      ("\\"       . nil)
                      ("h"        . describe-mode)))

  (add-hook 'todotxt-mode-hook #'mb-hooks--todotxt-mode))

;; Toml
(defun mb-hooks--toml-mode ()
  "My `toml' mode hook."
  (require 'electric-operator))

(with-eval-after-load 'toml
  (defvar toml-mode-map)
  (add-hook 'toml-mode-hook #'mb-hooks--toml-mode))

;; Smerge
(defun mb-hooks--smerge-mode ()
  "My `smerge' mode hook."
  (smerge-refine))

(with-eval-after-load 'smerge-mode
  (add-hook 'smerge-mode-hook #'mb-hooks--smerge-mode))


;; Shell script
(defun mb-hooks--sh-mode ()
  "My `sh' mode hook."
  (eglot-ensure)
  (setq-local defun-prompt-regexp
              (concat "^\\("
                      "\\(function[ \t]\\)?[ \t]*[[:alnum:]-_]+[ \t]*([ \t]*)"
                      "\\|"
                      "function[ \t]+[[:alnum:]-_]+[ \t]*\\(([ \t]*)\\)?"
                      "\\)[ \t]*"))
  (sh-extra-font-lock-activate)

  (defvar fill-function-arguments-first-argument-same-line)
  (defvar fill-function-arguments-second-argument-same-line)
  (defvar fill-function-arguments-last-argument-same-line)
  (defvar fill-function-arguments-argument-separator)

  (setq-local fill-function-arguments-second-argument-same-line nil)
  (setq-local fill-function-arguments-first-argument-same-line t)
  (setq-local fill-function-arguments-last-argument-same-line t)
  (setq-local fill-function-arguments-argument-separator " "))


(with-eval-after-load 'sh-script
  (add-hook 'sh-mode-hook #'mb-hooks--sh-mode))

;; Sql
(with-eval-after-load 'sql
  (add-hook 'sql-mode-hook #'sqlup-mode))

;; Vala
(defun mb-hooks--vala-mode ()
  "My `vala' mode hook."
  (defvar flycheck-checkers)
  (add-to-list 'flycheck-checkers 'vala-valac))

(with-eval-after-load 'vala-mode
  (require 'flycheck-vala)
  (add-hook 'vala-mode-hook #'mb-hooks--prog-mode)
  (add-hook 'vala-mode-hook #'mb-hooks--vala-mode))

;; Vertico
(defun mb-hooks--vertico-mode ()
  "My `vertico' mode hook.")

(with-eval-after-load 'vertico
  (mb-f-define-keys vertico-map
                    '(("<return>"    . vertico-directory-enter)
                      ("<backspace>" . vertico-directory-delete-char)
                      ("M-<delete>"  . vertico-directory-delete-word)))

  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'vertico-mode-hook #'mb-hooks--vertico-mode))

;; Whitespace
(defun mb-hooks--whitespace-mode ()
  "My `whitespace' mode hook."
  (redisplay))

(with-eval-after-load 'whitespace
  (add-hook 'whitespace-mode-hook #'mb-hooks--whitespace-mode))

;; Visual Regexp
(with-eval-after-load 'visual-regexp-steroids
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

(with-eval-after-load 'woman
  (add-hook 'woman-mode-hook #'mb-hooks--woman-mode))

;; Yaml
(defun mb-hooks--yaml-mode-hook ()
  "My `yaml' mode hook."
  (setq-local fill-column 80)
  (display-fill-column-indicator-mode)
  (flymake-mode)
  (flymake-yamllint-setup)
  (when (locate-dominating-file default-directory "ansible.cfg")
    (ansible 1)))

(with-eval-after-load 'yaml-mode
  (defvar yaml-mode-map)
  (mb-f-define-keys yaml-mode-map
                    '(( "C-z t A"     . ansible)))

  (add-hook 'yaml-mode-hook #'mb-hooks--yaml-mode-hook))

;; Yas
(defun mb-hooks--yas-mode ()
  "My `yas' mode hook.")

(with-eval-after-load 'yasnippet
  (mb-f-define-keys yas-keymap
                    '(( "C-g" . yas-exit-snippet)))
  (add-hook 'yas-minor-mode-hook #'mb-hooks--yas-mode))

(provide 'mb-hooks)
;;; mb-hooks.el ends here
