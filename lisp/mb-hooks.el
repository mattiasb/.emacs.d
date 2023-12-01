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

(require 'mb-loadpaths)
(require 'mb-f)
(require 'mb-cmd)

;;; Standard hooks

;; After Save
(add-hook 'after-save-hook
          #'mb-f-executable-make-buffer-file-executable-if-script-p)

;; After make frame
(defun mb-hooks--after-make-frame-functions-hook (frame &optional _)
  "My after-make-frame-functions hook."
  (select-frame-set-input-focus frame)
  (mb-f-doom-modeline-set-height))

(defun mb-hooks--window-setup-hook ()
  "My window-setup hook.

Based on: http://www.whiz.se/2016/05/01/dark-theme-in-emacs/"
  (let ((frame (selected-frame)))
    (mb-hooks--after-make-frame-functions-hook frame)
    (unless (display-graphic-p frame)
      (set-face-background 'default "unspecified-bg" frame))))

(add-hook 'after-make-frame-functions
          #'mb-hooks--after-make-frame-functions-hook)
(add-hook 'window-setup-hook
          #'mb-hooks--window-setup-hook)

;;; Packages

;; Ansible
(defun mb-hooks--ansible-hook ()
  "My `yaml' mode hook."
  (mb-f-req 'ansible)
  (mb-f-req 'ansible-doc)
  (mb-f-req 'ansible-vault)
  (mb-f-req 'cape)
  (mb-f-req 'company-ansible)
  (mb-f-req 'mb-keys)
  (if ansible
      (mb-f-set-capfs (mb-f-company-to-capf #'company-ansible))
    ;; NOTE: This is repeated in the `yaml-mode' hook.
    (mb-f-set-capfs #'cape-dabbrev))

  (mb-f-electric-pairs '((?\( . ?\))) ansible)
  (ansible-doc-mode (if ansible 1 -1))
  (when (ansible-vault--is-encrypted-vault-file)
    (ansible-vault-mode ansible))
  (font-lock-flush))

(with-eval-after-load "ansible"
  ;; TODO: Make this macro work. Ignores the prefix currently.
  ;; (cl-flet ((binding (key) (format "%s %s"
  ;;                                  (mb-f-get-keybinding-to 'mb-keys-tempel-map)
  ;;                                  key)))
  ;;   (eval `(tempel-key ,(binding "t") task ansible-key-map))
  ;;   (eval `(tempel-key ,(binding "p") play ansible-key-map)))
  (tempel-key "C-z s t" task ansible-key-map)
  (tempel-key "C-z s p" play ansible-key-map)
  (mb-f-define-keys ansible-key-map
                    '(( "{"    . mb-cmd-jinja2-{ )
                      ( "C-{"  . mb-cmd-jinja2-C-{ )))

  (add-hook 'ansible-hook #'mb-hooks--ansible-hook))

;; Auto Dark

(defun mb-hooks--auto-dark-dark-mode ()
  (enable-theme 'mb-madhat2r))

(defun mb-hooks--auto-dark-light-mode ()
  (enable-theme 'mb-leuven))

(defun mb-hooks--auto-dark-mode ()
  "My `auto-dark' mode hook."
  (mb-f-req 'auto-dark)
  (if auto-dark-mode
      (progn
        (add-hook 'auto-dark-dark-mode-hook #'mb-hooks--auto-dark-dark-mode)
        (add-hook 'auto-dark-light-mode-hook #'mb-hooks--auto-dark-light-mode)
        (auto-dark--set-theme (if (auto-dark--is-dark-mode) 'dark 'light)))
    (remove-hook 'auto-dark-dark-mode-hook #'mb-hooks--auto-dark-dark-mode)
    (remove-hook 'auto-dark-light-mode-hook #'mb-hooks--auto-dark-light-mode)))

(with-eval-after-load 'auto-dark
  (mb-f-req 'auto-dark)
  (add-hook 'auto-dark-mode-hook #'mb-hooks--auto-dark-mode))

;; Browse Kill Ring
(with-eval-after-load 'browse-kill-ring
  (mb-f-req 'browse-kill-ring)
  (mb-f-define-keys browse-kill-ring-mode-map
                    '(( "<down>"    . browse-kill-ring-forward)
                      ( "<tab>"     . browse-kill-ring-forward)
                      ( "<up>"      . browse-kill-ring-previous)
                      ( "<backtab>" . browse-kill-ring-previous)
                      ( "C-g"       . browse-kill-ring-quit))))

;; C common
(defun mb-hooks--c-common ()
  "My `c-mode' mode hook."
  (mb-f-req 'cc-mode)
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

(defun mb-hooks--corfu-mode ()
  "My `corfu' mode hook."
  (mb-f-req 'corfu-popupinfo)
  (corfu-popupinfo-mode))

(with-eval-after-load 'corfu
  (mb-f-define-keys corfu-map
                    '(( "C-<return>" . corfu-info-location)
                      ( "C-h"        . corfu-info-documentation)))

  (add-hook 'corfu-mode-hook #'mb-hooks--corfu-mode))


;; Compilation Mode

(with-eval-after-load 'compile
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;; Conf mode

(defun mb-hooks--conf-mode-hook ()
  "My `conf-mode' hook."
  (mb-f-req 'electric-operator)
  (electric-indent-local-mode)
  (electric-operator-mode)
  (mb-f-set-capfs #'cape-dabbrev))

(with-eval-after-load 'conf-mode
  (add-hook 'conf-mode-hook #'mb-hooks--conf-mode-hook))

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
  (mb-f-req 'dired-x)
  (mb-f-req 'dired-hide-dotfiles)
  (mb-f-req 'all-the-icons-dired)
  (auto-revert-mode)
  (hl-line-mode)
  (all-the-icons-dired-mode)
  (dired-omit-mode)
  (dired-hide-details-mode)
  (dired-hide-dotfiles-mode))

(with-eval-after-load 'dired
  (require 'dired-x)
  (require 'tramp)

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
  (mb-f-req 'hide-mode-line)
  (add-hook 'dired-sidebar-mode-hook #'hide-mode-line-mode))

;; Eglot
(defun mb-hooks--eglot-managed-mode ()
  "My `eglot' mode hook-"
  (mb-f-req 'eglot)
  (mb-f-req 'cape)
  (mb-f-req 'cape-keyword)
  (if (eglot-managed-p)
      (mb-f-set-capfs #'eglot-completion-at-point
                      #'cape-keyword)
    ;; NOTE: This is repeated in the `prog-mode' hook.
    (mb-f-set-capfs #'tags-completion-at-point-function
                    #'cape-keyword)))

(with-eval-after-load 'eglot
  (mb-f-req 'eglot-tempel)
  (eglot-tempel-mode 1)

  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("terraform-ls" "serve")))
  (mb-f-define-keys eglot-mode-map
                    '(( "C-z f r" . eglot-rename)
                      ( "C-z f f" . eglot-format)
                      ( "C-z f a" . eglot-code-actions)
                      ( "C-z f i" . eglot-code-action-organize-imports)))
  (add-hook 'eglot-managed-mode-hook #'mb-hooks--eglot-managed-mode))

;; Electric Layout
(defun mb-hooks--electric-layout-mode ()
  "My `electric-layout' mode hook."
  (when (seq-some #'derived-mode-p
                  '(js2-mode
                    c-mode
                    c++-mode
                    rust-mode
                    js-ts-mode
                    c-ts-mode
                    c++-ts-mode
                    rust-ts-mode))
    (add-to-list 'electric-layout-rules '(?\; . after)))

  (when (seq-some #'derived-mode-p
                  '(js2-mode
                    c-mode
                    c++-mode
                    rust-mode
                    sh-mode
                    js-ts-mode
                    c-ts-mode
                    c++-ts-mode
                    rust-ts-mode
                    bash-ts-mode))
    (add-to-list 'electric-layout-rules '(?{  . after))
    (add-to-list 'electric-layout-rules '(?}  . before)))

  (when (seq-some #'derived-mode-p
                  '(python-mode python-ts-mode))
    (add-to-list 'electric-layout-rules
                 '(?: . mb-f-python-electric-newline))))

(with-eval-after-load 'electric-layout
  (add-hook 'electric-layout-mode-hook #'mb-hooks--electric-layout-mode))

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
  (electric-operator-add-rules-for-mode 'lisp-data-mode
                                        (cons "-" nil))
  (electric-operator-add-rules-for-mode 'ruby-mode
                                        (cons "=" nil))
  (electric-operator-add-rules-for-mode 'ruby-mode
                                        (cons ">" nil))
  (electric-operator-add-rules-for-mode 'ruby-mode
                                        (cons "%" nil))
  (electric-operator-add-rules-for-mode 'sh-mode
                                        (cons "=" nil))
  (electric-operator-add-rules-for-mode 'bash-ts-mode
                                        (cons "=" nil))
  (electric-operator-add-rules-for-mode 'bash-ts-mode
                                        (cons "-" nil))
  (electric-operator-add-rules-for-mode 'tmux-mode
                                        (cons "-" nil))
  (electric-operator-add-rules-for-mode 'yaml-mode
                                        (cons ":" ": "))
  (electric-operator-add-rules-for-mode 'yaml-mode
                                        (cons "-" nil)))

;; ELisp
(defun mb-hooks--emacs-lisp-mode ()
  "My `emacs-lisp' mode hook."
  (mb-f-set-capfs #'elisp-completion-at-point
                  #'cape-keyword)

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
                    '(("M-q"     . mb-cmd-lisp-fill-function-arguments))))

(with-eval-after-load 'elisp-mode
  (mb-f-req 'lisp-extra-font-lock)
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
  (mb-f-req 'flymake-diagnostic-at-point)
  (require 'flymake-diagnostic-at-point)
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;; Flyspell
(with-eval-after-load 'flyspell
  (mb-f-req 'flyspell)
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
  (mb-f-req 'git-gutter)
  (run-at-time 0 5 #'git-gutter:update-all-windows))

;; Go
(defun mb-hooks--go-mode ()
  "My `go' mode hook."
  (mb-f-req 'go-mode)
  (eglot-ensure)
  (add-hook 'before-save-hook #'mb-f-eglot-format-all 0 t)
  (setq-local tab-width 4))

(with-eval-after-load 'go-mode
  (mb-f-req 'go-mode)
  (eglot-ensure)
  (mb-f-define-keys go-mode-map
                    '(( "C-z i a"    . go-import-add)
                      ( "C-z i r"    . go-remove-unused-imports)
                      ( "C-z i g"    . go-goto-imports)
                      ( "C-z d"      . godoc-at-point)))
  
  (add-hook 'go-mode-hook #'mb-hooks--go-mode))

;; Haskell
(defun mb-hooks--haskell-mode ()
  "My `haskell' mode hook."
  (mb-f-req 'haskell-indentation)
  (haskell-indentation-mode))

(add-hook 'haskell-mode-hook #'mb-hooks--haskell-mode)

;; Help
(defun mb-hooks--help-mode ()
  "My `help' mode hook.")

(with-eval-after-load 'help-mode
  (mb-f-define-keys help-mode-map
                    '(( "M-<left>"  . help-go-back)
                      ( "M-<right>" . help-go-forward)))
  (mb-f-remap-keys help-mode-map
                   '(("s" . "C-s")
                     ("r" . "C-r")))

  (add-hook 'help-mode-hook #'mb-hooks--help-mode))

;; IBuffer
(defun mb-hooks--ibuffer ()
  "My `ibuffer' mode hook."
  (mb-f-req 'ibuffer-projectile)
  (mb-f-req 'all-the-icons-ibuffer)
  (mb-f-req 'all-the-icons-ibuffer)

  (ibuffer-projectile-set-filter-groups)
  (all-the-icons-ibuffer-mode)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(with-eval-after-load 'ibuffer
  (add-hook 'ibuffer-hook #'mb-hooks--ibuffer))

;; Iedit
(with-eval-after-load 'iedit
  (mb-f-req 'iedit)
  (mb-f-define-keys iedit-mode-keymap
                    '(("C-g"      . iedit-mode)
                      ("<return>" . iedit-mode)))

  (add-hook 'iedit-aborting-hook #'deactivate-mark))

;; Info
(defun mb-hooks--Info-mode ()
  "My `Info' mode hook.")

(with-eval-after-load 'info
  (mb-f-req 'niceify-info)
  (mb-f-req 'info)
  (mb-f-define-keys Info-mode-map
                    '(( "M-<left>"  . Info-history-back)
                      ( "M-<right>" . Info-history-forward)
                      ( "M-<up>"    . Info-up)))

  (add-hook 'Info-mode-hook #'mb-hooks--Info-mode)
  (add-hook 'Info-selection-hook #'niceify-info))

;; Jinja2
(defun mb-hooks--jinja2-mode ()
  "My `jinja2' mode hook.")

(with-eval-after-load 'jinja2-mode
  (mb-f-req 'jinja2-mode)
  (mb-f-define-keys jinja2-mode-map
                    '(( "{"    . mb-cmd-jinja2-{ )
                      ( "C-{"  . mb-cmd-jinja2-C-{ )))
  (add-hook 'jinja2-mode-hook #'mb-hooks--jinja2-mode))

;; JS2
(defun mb-hooks--js2-mode ()
  "My `js2' mode hook."
  (mb-f-req 'js2-imenu-extras)
  (js2-imenu-extras-mode))

(with-eval-after-load 'js2-mode
  (add-hook 'js2-mode-hook #'mb-hooks--js2-mode))

;; JSON
(defun mb-hooks--json-mode ()
  "My `json' mode hook."
  (mb-f-req 'highlight-numbers)
  (highlight-numbers-mode -1))

(with-eval-after-load 'json-mode
  (add-hook 'json-mode-hook #'mb-hooks--json-mode))

;; Lisp Data
(defun mb-hooks--lisp-data-mode ()
  "My `lisp-data' mode hook."
  ;; Getting this to work with the `rx' construct was anything but trivial.
  ;; Specifically to generalize `= 0' below to 0..9.
  ;;   (rx line-start
  ;;       (= 0 (1+ (or (syntax word) (syntax symbol))) space)
  ;;       symbol-start
  ;;       (group-n 1 (1+ (or (syntax word)
  ;;                          (syntax symbol))))
  ;;       symbol-end)
  (mb-f-req 'mb-f)
  (mb-f-req 'tempel)
  (when (mb-f-path= (buffer-file-name) tempel-path)
    (dolist (n (number-sequence 0 9))
      (let ((regex (concat "^\\(?:\\(?:\\sw\\|\\s_\\)+[[:space:]]"
                           "\\)\\{"
                           (number-to-string n)
                           "\\}\\_<\\(?1:\\(?:\\sw\\|\\s_\\)+\\)\\_>")))
        (add-to-list 'imenu-generic-expression (list nil regex 1)))))

  (mb-f-define-keys lisp-data-mode-map
                    '(("M-q"     . mb-cmd-lisp-fill-function-arguments))))

(with-eval-after-load 'lisp-mode
  (add-hook 'lisp-data-mode-hook #'mb-hooks--lisp-data-mode))

;; Ligature
(defun mb-hooks--ligature-mode ()
  "My `ligature' mode hook.")

(with-eval-after-load 'ligature
  (mb-f-req 'ligature)
  (ligature-set-ligatures 'prog-mode
                          '("|||>" "<|||" "<==>" "<!--" "~~>" "***" "||="
                            "||>" ":::" "::=" "=:=" "===" "==>" "=!=" "=>>"
                            "=<<" "=/=" "!==" "!!." ">=>" ">>=" ">>>" ">>-"
                            ">->" "->>" "-->" "---" "-<<" "<~~" "<~>" "<*>"
                            "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->" "<--"
                            "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "#_("
                            "..<" "..." "+++" "/==" "///" "_|_" "www" "&&" "^="
                            "~~" "~@" "~=" "~>" "~-" "**" "*>" "*/" "||" "|}"
                            "|]" "|=" "|>" "|-" "{|" "[|" "]#" "::" ":=" ":>"
                            ":<" "$>" "==" "=>" "!=" "!!" ">:" ">=" ">>" ">-"
                            "-~" "-|" "->" "--" "<:" "<$" "<=" "<>" "<-" "<<"
                            "<+" "</" "#{" "#[" "#:" "#=" "#!" "#(" "#?"
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
  (mb-f-req 'lastpass)
  (lastpass-auth-source-enable)
  (add-hook 'auth-source-backend-parser-functions
            #'lastpass-auth-source-backend-parse))

;; Magit
(defun mb-hooks--git-commit-setup ()
  "My `git-commit' mode hook."
  (setq fill-column 72)
  (display-fill-column-indicator-mode))

(with-eval-after-load 'magit
  (mb-f-req 'magit-todos)
  (mb-f-req 'forge)
  (mb-f-req 'ghub)

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

  (mb-f-define-keys magit-blame-mode-map
                    '(( "C-z t b" .  magit-blame-quit)))

  (add-hook 'after-save-hook         #'magit-after-save-refresh-status)
  (add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows)
  (add-hook 'git-commit-setup-hook   #'mb-hooks--git-commit-setup))

;; Markdown
(defun mb-hooks--markdown-mode ()
  "My `markdown' mode hook."
  (mb-f-req 'pandoc-mode)
  (auto-fill-mode)
  (pandoc-mode)
  (pandoc-load-default-settings))

(with-eval-after-load 'markdown-mode
  (mb-f-define-keys markdown-mode-map
                    '(( "C-<return>" . mb-cmd-markdown-jump)
                      ( "C-c p"      . pandoc-main-hydra/body)
                      ( "M-<up>"     . nil)
                      ( "M-<down>"   . nil)))

  (tempel-key "C-z s 1"        h1       markdown-mode-map)
  (tempel-key "C-z s 2"        h2       markdown-mode-map)
  (tempel-key "C-z s 3"        h3       markdown-mode-map)
  (tempel-key "C-z s 4"        h4       markdown-mode-map)
  (tempel-key "C-z s 5"        h5       markdown-mode-map)
  (tempel-key "C-z s 6"        h6       markdown-mode-map)
  (tempel-key "C-z s b"        **       markdown-mode-map)
  (tempel-key "C-z s *"        *        markdown-mode-map)
  (tempel-key "M-["            rlink    markdown-mode-map)
  (tempel-key "C-z s c"        code     markdown-mode-map)
  (tempel-key "C-M-<return>"   checkbox markdown-mode-map)
  (tempel-key "C-z s <return>" br       markdown-mode-map)

  (add-hook 'markdown-mode-hook #'mb-hooks--markdown-mode))

;; MTG deck mode
(with-eval-after-load 'mtg-deck-mode
  (mb-f-req 'mtg-deck-mode)
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
  (mb-f-req 'aggressive-indent)
  (mb-f-req 'ws-butler)

  (setq-local fill-column 80)
  (unless (derived-mode-p 'makefile-mode)
    (setq-local indent-tabs-mode nil))

  (unless (derived-mode-p 'lisp-mode
                          'lisp-data-mode
                          'emacs-lisp-mode
                          'dockerfile-mode)
    (electric-operator-mode))

  (unless (derived-mode-p 'lisp-mode
                          'lisp-data-mode
                          'emacs-lisp-mode)
    (mb-f-electric-pairs '((?' . ?')
                           (?< . ?>))))

  (if (seq-some #'derived-mode-p aggressive-indent-excluded-modes)
      (electric-indent-local-mode)
    (aggressive-indent-mode))

  (which-function-mode)
  (ws-butler-mode)
  (flymake-mode)
  (display-fill-column-indicator-mode)
  (highlight-numbers-mode)
  (ligature-mode)

  ;; NOTE: This is repeated in `mb-hooks--eglot-managed-mode'.
  (mb-f-set-capfs #'tags-completion-at-point-function
                  #'cape-keyword))

(with-eval-after-load 'prog-mode
  (mb-f-define-keys prog-mode-map
                    '(( "C-z f e"     . mb-cmd-iedit-in-defun)
                      ( "C-z d"       . nil)
                      ( "M-q"         . fill-function-arguments-dwim)))

  (add-hook 'prog-mode-hook #'mb-hooks--prog-mode))

;; Projectile
(with-eval-after-load 'projectile
  (mb-f-req 'projectile)
  (unless projectile-known-projects
    (mb-cmd-projectile-index-projects))

  (defvar projectile-mode-line
    '(:eval (if (or (file-remote-p default-directory)
                    (string-match-p "/run/user/[0-9]+/gvfs/"
                                    default-directory))
                " [?]"
              (format " [%s]" (projectile-project-name)))))
  (projectile-register-project-type 'win-batch
                                    '("build.bat")
                                    :compile "cmd.exe \"/c build\"")
  (mb-f-define-keys projectile-command-map
                    '(( "B"         . projectile-ibuffer)
                      ( "i"         . mb-cmd-projectile-index-projects)
                      ( "I"         . projectile-invalidate-cache)
                      ( "d"         . projectile-dired)
                      ( "D"         . projectile-find-dir)
                      ( "4 d"       . projectile-dired-other-window)
                      ( "4 D"       . projectile-find-dir-other-window)
                      ( "5 d"       . projectile-dired-other-frame)
                      ( "5 D"       . projectile-find-dir-other-frame)
                      ( "V"         . mb-cmd-projectile-gitg)
                      ( "s s"       . projectile-ripgrep)
                      ( "s S"       . mb-cmd-projectile-ripgrep-regex)
                      ( "x a"       . mb-cmd-projectile-ansi-term)
                      ( "x e"       . mb-cmd-projectile-eat)
                      ( "x 4 e"     . mb-cmd-projectile-eat-other-window)
                      ( "x t"       . mb-cmd-projectile-terminal)))

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
  (eglot-ensure)
  (add-hook 'before-save-hook #'mb-f-eglot-format-all 0 t))

(with-eval-after-load 'python
  (mb-f-req 'python)
  (mb-f-define-keymap mb-python-pytest-map
                      '(( "1"           . pytest-one)
                        ( "a"           . pytest-all)
                        ( "m"           . pytest-module)
                        ( "r"           . pytest-run)))

  (mb-f-define-keys python-ts-mode-map
                    '(( "C-c t"         . mb-python-pytest-map)
                      ( "C-c C-t"       . mb-python-pytest-map)
                      ( "C-c <left>"    . python-indent-shift-left)
                      ( "C-c <right>"   . python-indent-shift-right)))

  (add-hook 'python-ts-mode-hook #'mb-hooks--python-mode))

;; Re Builder
(defun mb-hooks--reb-mode ()
  "My `re-builder' mode hook."
  (mb-f-define-keys reb-mode-map
                    '(( "C-x k"      . reb-quit)
                      ( "C-c C-k"    . reb-quit)
                      ( "C-c <down>" . reb-next-match)
                      ( "C-c n"      . reb-next-match)
                      ( "C-c <up>"   . reb-prev-match)
                      ( "C-c p"      . reb-prev-match)
                      )))

(with-eval-after-load 're-builder
  (add-hook 'reb-mode-hook #'mb-hooks--reb-mode))

;; RipGrep
(with-eval-after-load 'ripgrep
  (mb-f-req 'ripgrep)
  (mb-f-define-keys ripgrep-search-mode-map
                    '(( "W" . wgrep-change-to-wgrep-mode))))

;; Rust
(with-eval-after-load 'rust-mode)

;; Ruby
(defun mb-hooks--ruby-mode ()
  "My `ruby' mode hook."
  (mb-f-req 'ruby-mode)
  (remove-hook 'flymake-diagnostic-functions
               #'ruby-flymake-auto
               'local)
  (eglot-ensure))

(with-eval-after-load 'ruby-mode
  (add-hook 'ruby-mode-hook #'mb-hooks--ruby-mode))

;; Table
(with-eval-after-load 'table
  (mb-f-req 'table)
  (push '(delete-forward-char . *table--cell-delete-char)
        table-command-remap-alist))

;; Shell
(defun mb-hooks--term-mode ()
  "My `term' mode hook."
  (setq scroll-margin 0))

(defun mb-hooks--term-exec ()
  "My `term' mode hook."
  (set-process-coding-system 'utf-8-unix
                             'utf-8-unix))

(with-eval-after-load 'term
  (mb-f-req 'term)
  (mb-f-define-keys term-raw-map
                    '(( "M-x"       . execute-extended-command)
                      ( "C-y"       . mb-cmd-term-paste)
                      ( "<escape>"  . ESC-prefix)))

  (add-hook 'term-mode-hook #'mb-hooks--term-mode)
  (add-hook 'term-exec-hook #'mb-hooks--term-exec))

;; Terraform
(defun mb-hooks--terraform-mode ()
  "My `terraform' mode hook."
  (mb-f-req 'terraform-mode)
  (eglot-ensure)
  (add-hook 'before-save-hook #'mb-f-eglot-format-all 0 t))

(with-eval-after-load 'terraform-mode
  (add-hook 'terraform-mode-hook #'mb-hooks--terraform-mode))

;; Text
(defun mb-hooks--text-mode ()
  "My `text' mode hook."
  (mb-f-req 'corfu)
  (mb-f-req 'jinx)

  (setq-local fill-column 80)
  (setq-local indent-tabs-mode nil)

  (display-fill-column-indicator-mode)
  (electric-indent-local-mode)
  (electric-operator-mode)
  (mb-f-set-capfs #'cape-dict)
  (unless (derived-mode-p 'yaml-mode 'yaml-ts-mode 'jinja2-mode)
    (jinx-mode))
  (setq-local corfu-auto nil))

;; Text mode doesn't have a (provide)
(add-hook 'text-mode-hook #'mb-hooks--text-mode)

;; Todotxt
(defun mb-hooks--todotxt-mode ()
  "My `todotxt' mode hook."
  (mb-f-req 'todotxt)
  (todotxt-show-incomplete)
  (set-window-dedicated-p (selected-window) t))

(with-eval-after-load 'todotxt
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
  (add-hook 'toml-mode-hook #'mb-hooks--toml-mode))

;; Treesit Auto
(defun mb-hooks--treesit-auto-mode ()
  "My `treesit-auto' mode hook.")

(with-eval-after-load 'treesit-auto
  (setq treesit-language-source-alist
        '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake      "https://github.com/uyha/tree-sitter-cmake")
          (css        "https://github.com/tree-sitter/tree-sitter-css")
          (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
          (go         "https://github.com/tree-sitter/tree-sitter-go")
          (html       "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                      "master"
                      "src")
          (json       "https://github.com/tree-sitter/tree-sitter-json")
          (make       "https://github.com/alemuller/tree-sitter-make")
          (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
          (python     "https://github.com/tree-sitter/tree-sitter-python")
          (toml       "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx        "https://github.com/tree-sitter/tree-sitter-typescript"
                      "master"
                      "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                      "master"
                      "typescript/src")
          (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))
  (add-hook 'treesit-auto-mode-hook #'mb-hooks--treesit-auto-mode))

;; Smerge
(defun mb-hooks--smerge-mode ()
  "My `smerge' mode hook."
  (smerge-refine))

(with-eval-after-load 'smerge-mode
  (add-hook 'smerge-mode-hook #'mb-hooks--smerge-mode))


;; Shell script
(defun mb-hooks--sh-mode ()
  "My `sh' mode hook."
  (mb-f-req 'sh-extra-font-lock)
  (eglot-ensure)
  (setq-local defun-prompt-regexp
              (concat "^\\("
                      "\\(function[ \t]\\)?[ \t]*[[:alnum:]-_]+[ \t]*([ \t]*)"
                      "\\|"
                      "function[ \t]+[[:alnum:]-_]+[ \t]*\\(([ \t]*)\\)?"
                      "\\)[ \t]*"))
  (sh-extra-font-lock-activate)

  (setq-local fill-function-arguments-second-argument-same-line nil)
  (setq-local fill-function-arguments-first-argument-same-line t)
  (setq-local fill-function-arguments-last-argument-same-line t)
  (setq-local fill-function-arguments-argument-separator " "))


(with-eval-after-load 'sh-script
  (mb-f-req 'sh-script)
  (mb-f-define-keys sh-mode-map
                    '(( "$"      . mb-cmd-sh-$ )
                      ( "C-$"    . mb-cmd-sh-C-$ )))
  (add-hook 'sh-base-mode-hook #'mb-hooks--sh-mode))

;; Sql
(with-eval-after-load 'sql
  (mb-f-req 'sqlup-mode)
  (add-hook 'sql-mode-hook #'sqlup-mode))

;; Systemd
(defun mb-hooks--systemd-mode ()
  "My `systemd' mode hook."
  (mb-f-req 'systemd)
  (mb-f-set-capfs #'systemd-complete-at-point))

(with-eval-after-load 'systemd
  (add-hook 'systemd-mode-hook #'mb-hooks--systemd-mode))

;; Tempel
(defun mb-hooks--tempel-mode ()
  "My `tempel' mode hook.")

(with-eval-after-load 'tempel
  (mb-f-req 'tempel)
  (mb-f-req 'mb-keys)
  (mb-f-define-keys tempel-map
                    '(("<tab>"     . tempel-next)
                      ("<backtab>" . tempel-previous)
                      ("C-g"       . tempel-abort)))

  (add-to-list 'tempel-user-elements #'mb-f-tempel-include)

  (add-hook 'tempel-mode-hook #'mb-hooks--tempel-mode))

;; Tmux
(defun mb-hooks--tmux-mode ()
  "My `tmux-mode' mode hook."
  ;; tmux-mode-completion-at-point
  (mb-f-set-capfs #'tmux-mode-completion-at-point))

(with-eval-after-load 'tmux-mode
  (add-hook 'tmux-mode-hook #'mb-hooks--tmux-mode))

;; Vala
(defun mb-hooks--vala-mode ()
  "My `vala' mode hook.")

(with-eval-after-load 'vala-mode
  (add-hook 'vala-mode-hook #'mb-hooks--prog-mode)
  (add-hook 'vala-mode-hook #'mb-hooks--vala-mode))

;; Vertico
(defun mb-hooks--vertico-mode ()
  "My `vertico' mode hook.")

(with-eval-after-load 'vertico
  (mb-f-req 'vertico)
  (mb-f-req 'vertico-directory)
  (mb-f-define-keys vertico-map
                    '(("<return>"    . vertico-directory-enter)
                      ("<backspace>" . vertico-directory-delete-char)
                      ("M-<delete>"  . vertico-directory-delete-word)))

  ;; See: https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index start)
                (setq cand (funcall orig cand prefix suffix index start))
                (concat
                 (if (and (boundp 'vertico--index) (= vertico--index index))
                     (propertize " → " 'face 'vertico-current)
                   "   ")
                 cand)))

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
  (mb-f-req 'woman)
  (mb-f-remap-keys woman-mode-map
                   '(("a" . "s")
                     ("s" . "C-s")
                     ("R" . "r")
                     ("r" . "C-r"))))

(with-eval-after-load 'woman
  (add-hook 'woman-mode-hook #'mb-hooks--woman-mode))


;; Xref
(with-eval-after-load 'xref
  (mb-f-define-keys xref--xref-buffer-mode-map
                    '(("<tab>"     . xref-next-group)
                      ("<backtab>" . xref-prev-group)
                      ("<down>"    . xref-next-line)
                      ("<up>"      . xref-prev-line)
                      ("C-g"       . xref-quit-and-pop-marker-stack)
                      ("q"         . xref-quit-and-pop-marker-stack))))


;; Yaml
(defun mb-hooks--yaml-mode-hook ()
  "My `yaml' mode hook."
  (mb-f-req 'flymake-yamllint)
  (setq-local fill-column 80)
  ;; NOTE: This is repeated in the `yaml-mode' hook.
  (mb-f-set-capfs #'cape-dabbrev)
  (display-fill-column-indicator-mode)
  (flymake-mode)
  (flymake-yamllint-setup)
  (when (locate-dominating-file default-directory "ansible.cfg")
    (ansible 1)))

(with-eval-after-load 'yaml-mode
  (mb-f-req 'yaml-mode)
  (mb-f-define-keys yaml-mode-map
                    '(( "C-z t A"     . ansible)))

  (add-hook 'yaml-mode-hook #'mb-hooks--yaml-mode-hook))

(provide 'mb-hooks)
;;; mb-hooks.el ends here
