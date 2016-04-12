;;; init.el --- My init file
;; Copyright (C) 2013, 2014 Mattias Bengtsson

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20141020
;; Keywords         : init
;; Package-Requires : ()
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 24.x

;;; The MIT License:

;; http://en.wikipedia.org/wiki/MIT_License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;;; Note:

;;; Code:


;;; Settings

;; Unset these early to remove at least some of the inital flicker.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path
(defvar load-prefer-newer)
(setq load-prefer-newer t)
(require 'funcs "~/.emacs.d/lisp/funcs.el")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;;; Early init code

;; Maximize on start
(my/maximize)


;;; Modes – General

(my/auto-modes  '(("\\.inl\\'"    . c++-mode)
                  ("\\.ui$"       . nxml-mode)
                  ("\\.js$"       . js2-mode)
                  ("\\.jshintrc$" . js2-mode)
                  ("\\.jscsrc$"   . json-mode)
                  ("\\.geojson$"  . json-mode)
                  ("\\.vala$"     . vala-mode)
                  ("\\.mapcss$"   . css-mode)
                  ("\\.mcss$"     . css-mode)
                  ("\\.m$"        . octave-mode)
                  ("\\.dec$"      . mtg-deck-mode)
                  ("\/Cask$"      . emacs-lisp-mode)))

(my/shorten-major-modes '((markdown-mode   . "M↓")
                          (js2-mode        . "JS")
                          (nxml-mode       . "XML")
                          (c-mode          . "C")
                          (c++-mode        . "C++")
                          (cmake-mode      . "CMake")
                          (emacs-lisp-mode . "Elisp")
                          (go-mode         . "Go")
                          (haskell-mode    . "λ")
                          (snippet-mode    . "Yas")))

(my/shorten-minor-modes '((company-mode             . " C")
                          (abbrev-mode              . " A")
                          (ws-butler-mode           . " W")
                          (control-mode             . "")
                          (git-gutter-mode          . "")
                          (magit-gitflow-mode       . " Flow")
                          (magit-filenotify-mode    . " Notify")
                          (yas-minor-mode           . "")
                          (fancy-narrow-mode        . "")
                          (haskell-indentation-mode . "")
                          (racer-mode               . "")
                          (aggressive-indent-mode   . " ⇒")
                          (which-key-mode           . "")
                          (magit-auto-revert-mode   . "")
                          (abbrev-mode              . "")))


;;; Modes – Specific

;; Browse Kill Ring
(defvar browse-kill-ring-mode-map)
(defun my/browse-kill-ring-mode-hook ()
  "My `browse-kill-ring' mode hook."
  (my/define-keys browse-kill-ring-mode-map
                  '(( "<down>"    . browse-kill-ring-forward)
                    ( "<tab>"     . browse-kill-ring-forward)
                    ( "<up>"      . browse-kill-ring-previous)
                    ( "<backtab>" . browse-kill-ring-previous)
                    ( "C-g"       . browse-kill-ring-quit))))

(add-hook 'browse-kill-ring-mode-hook #'my/browse-kill-ring-mode-hook)

;; C common
(defvar c-mode-base-map)
(defun my/c-mode-common-hook ()
  "My `c-mode' mode hook."
  (unless (keymap-parent c-mode-base-map)
    (set-keymap-parent c-mode-base-map prog-mode-map)))

(add-hook 'c-mode-common-hook #'my/c-mode-common-hook)

;; C / C++
(defvar rtags-completions-enabled)
(defvar company-backends)
(defvar projectile-command-map)
(defun my/c-mode-hook ()
  "A mode hook for C and C++."
  (require 'rtags)
  (require 'company-rtags)
  (rtags-start-process-unless-running)
  (setq-local rtags-completions-enabled t)
  (rtags-enable-standard-keybindings c-mode-base-map)
  (setq-local company-backends '((company-rtags)))

  ;; Work around bug where c-mode-base-map doesn't inherit from
  ;; prog-mode-map
  (my/define-keys c-mode-base-map
                  '(( "C-<return>" . rtags-find-symbol-at-point)
                    ( "M-<left>"   . rtags-location-stack-back)
                    ( "M-<right>"  . rtags-location-stack-forward)
                    ( "C-z f r"    . rtags-rename-symbol)
                    ( "."          . my/dot-and-complete)
                    ( ":"          . my/double-colon-and-complete)
                    ( ">"          . my/arrow-and-complete)))
  (my/define-keys projectile-command-map
                  '(( "j"         . rtags-find-symbol))))

(add-hook 'c-mode-hook   #'my/c-mode-hook)
(add-hook 'c++-mode-hook #'my/c-mode-hook)

;; CMake
(defun my/cmake-mode-hook ()
  "My `cmake' mode hook."
  (setq-local company-backends
              '((company-cmake
                 company-files
                 company-dabbrev-code))))

(add-hook 'cmake-mode-hook #'my/prog-mode)
(add-hook 'cmake-mode-hook #'my/cmake-mode-hook)

;; Control
(defvar control-mode)
(defvar control-mode-keymap)
(defun my/control-mode-hook ()
  "My `control' mode hook."
  (my/control-mode-set-cursor)
  (my/define-keys control-mode-keymap
                  '(( "i"           . my/control-mode-off)
                    ( "<escape>"    . ESC-prefix)
                    ( "x x"         . smex)
                    ( "x s"         . save-buffer)
                    ( "x S"         . save-some-buffers))))

(add-hook 'control-mode-keymap-generation-functions
          'control-mode-ctrlx-hacks)
(add-hook 'control-mode-hook #'my/control-mode-hook)

;; Company
(defvar company-active-map)
(defun my/company-mode-hook ()
  "My `company' mode hook."
  (company-quickhelp-mode)
  (my/define-keys company-active-map
                  '(( "\C-n"    . company-select-next)
                    ( "\C-p"    . company-select-previous)
                    ( "<next>"  . my/company-scroll-down)
                    ( "<prior>" . my/company-scroll-up)
                    ( "\C-v"    . company-show-location)
                    ( "\C-g"    . company-abort))))

(add-hook 'company-mode-hook                 #'my/company-mode-hook)
(add-hook 'company-completion-started-hook   #'my/fci-turn-off)
(add-hook 'company-completion-finished-hook  #'my/fci-turn-on)
(add-hook 'company-completion-cancelled-hook #'my/fci-turn-on)

;; Cython
(defun my/cython-mode-hook ()
  "My `cython' mode hook."
  (require 'flycheck-cython))

(add-hook 'cython-mode-hook #'my/cython-mode-hook)

;; Dired
(defvar dired-mode-map)
(defun my/dired-mode-hook ()
  "My `dired' mode hook."
  (dired-hide-details-mode)
  (my/define-keys dired-mode-map
                  '(( "W" . wdired-change-to-wdired-mode)
                    ( "F" . find-name-dired)
                    ( "c" . find-file)))
  (my/remap-keys dired-mode-map
                 '(("s" . "C-s")
                   ("r" . "C-r"))))

(add-hook 'dired-mode-hook #'my/dired-mode-hook)

;; ELisp
(defun my/emacs-lisp-mode-hook ()
  "My `emacs-lisp' mode hook."
  (my/set-imenu-create-index-function nil "/")
  (setq page-delimiter
        (rx bol ";;;" (not (any "#")) (* not-newline) "\n"
            (* (* blank) (opt ";" (* not-newline)) "\n"))))

(add-hook 'emacs-lisp-mode-hook #'lisp-extra-font-lock-mode)
(add-hook 'emacs-lisp-mode-hook #'my/emacs-lisp-mode-hook)

;; Flycheck
(defun my/flycheck-mode-hook ()
  "My `flycheck' mode hook."
  (flycheck-pos-tip-mode)
  (flycheck-status-emoji-mode)
  (flycheck-cask-setup)
  (flycheck-package-setup))

(add-hook 'flycheck-mode-hook #'my/flycheck-mode-hook)

;; Find-file
(add-hook 'find-file-not-found-functions #'my/create-non-existent-directory)

;; Go
(defvar go-mode-map)
(defun my/go-mode-hook ()
  "My `go' mode hook."
  (go-eldoc-setup)

  (setq-local tab-width 4)
  (setq-local company-backends '(company-go))

  (my/define-keys go-mode-map
                  '(( "C-z i a"    . go-import-add)
                    ( "C-z i r"    . go-remove-unused-imports)
                    ( "C-z i g"    . go-goto-imports)
                    ( "C-z d"      . godoc-at-point)
                    ( "C-<return>" . godef-jump)
                    ( "."          . my/dot-and-complete))))

(add-hook 'go-mode-hook #'my/go-mode-hook)

;; Haskell
(defun my/haskell-mode-hook ()
  "My `haskell' mode hook."
  (setq-local electric-indent-mode nil)
  (turn-on-haskell-indentation))

(add-hook 'haskell-mode-hook #'my/haskell-mode-hook)

;; Help
(defvar help-mode-map)
(defun my/help-mode-hook ()
  "My `help' mode hook."
  (my/define-keys help-mode-map
                  '(( "M-<left>"  . help-go-back)
                    ( "M-<right>" . help-go-forward))))

(add-hook 'help-mode-hook #'my/help-mode-hook)

;; Ido
(defvar ido-common-completion-map)
(defun my/ido-setup-hook ()
  "My `ido' mode hook."
  (my/define-keys ido-common-completion-map
                  '(( "<tab"    . ido-complete)
                    ( "<next>"  . my/ido-scroll-down)
                    ( "<prior>" . my/ido-scroll-up))))

(add-hook 'ido-setup-hook #'my/ido-setup-hook)

;; IBuffer
(defvar ibuffer-sorting-mode)
(defun my/ibuffer-hook ()
  "My `ibuffer' mode hook."
  (ibuffer-projectile-set-filter-groups)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(add-hook 'ibuffer-hook 'my/ibuffer-hook)

;; IELM
(defvar ielm-map)
(defun my/ielm-mode-hook ()
  "My `ielm' mode hook."
  (company-mode)
  (my/define-keys ielm-map
                  '(( "<tab>" . my/indent-snippet-or-complete))))

(add-hook 'ielm-mode-hook #'my/ielm-mode-hook)

;; Info
(defun my/Info-mode-hook ()
  "My `Info' mode hook."
  (my/define-keys Info-mode-map
                  '(( "M-<left>"  . Info-history-back)
                    ( "M-<right>" . Info-history-forward)
                    ( "M-<up>"    . Info-up))))

(add-hook 'Info-mode-hook #'my/Info-mode-hook)

;; JS2
(defvar js2-mode-map)
(autoload 'js2r-rename-var "js2-refactor" "" t nil)
(defun my/js2-mode-hook ()
  "My `js2' mode hook."
  (js2-imenu-extras-mode)
  (my/set-imenu-create-index-function #'js2-mode-create-imenu-index
                                      ".")
  (require 'js2-refactor)
  (define-key js2-mode-map (kbd "C-z f r") #'js2r-rename-var)
  (setq-local company-backends '((company-dabbrev-code
                                  company-files
                                  company-keywords))))

(add-hook 'js2-mode-hook #'my/js2-mode-hook)

(defvar tern-mode-keymap)
(defun my/tern-mode-hook ()
  "My `tern' mode hook."
  (my/define-keys tern-mode-keymap
                  '(( "M-<left>"   . tern-pop-find-definition)
                    ( "C-<return>" . tern-find-definition)
                    ( "C-z f r"    . tern-rename-variable))))

(add-hook 'tern-mode-hook #'my/tern-mode-hook)

;; Lua
(defun my/lua-mode-hook ()
  "My `lua' mode hook."
  (setq-local company-backends '((company-dabbrev-code
                                  company-files
                                  company-keywords))))

(add-hook 'lua-mode-hook #'my/lua-mode-hook)

;; Magit
(defun my/git-commit-mode-hook ()
  "My `git-commit' mode hook."
  (my/control-mode-off)
  (flyspell-mode)
  (auto-fill-mode)
  (fci-mode 1)
  (setq-local fill-column 72))

(autoload 'turn-on-magit-gitflow "magit-gitflow" "" t nil)
(defun my/magit-mode-hook ()
  "My `magit' mode hook."
  (require 'magit-gitflow)
  (turn-on-magit-gitflow))

(defvar magit-blame-mode-map)
(defun my/magit-blame-mode-hook ()
  "My `magit-blame' mode hook."
  (my/define-keys magit-blame-mode-map
                  '(( "C-z t b"     .  magit-blame-quit))))

(add-hook 'magit-status-mode-hook #'magit-filenotify-mode)
(add-hook 'magit-blame-mode-hook  #'my/magit-blame-mode-hook)
(add-hook 'magit-mode-hook        #'my/magit-mode-hook)
(add-hook 'git-commit-mode-hook   #'my/git-commit-mode-hook)

;; Markdown
(defvar markdown-mode-map)
(defun my/markdown-mode-hook ()
  "My `markdown' mode hook."
  (setq-local fill-column 80)
  (fci-mode)
  (auto-fill-mode)
  (setq-local indent-tabs-mode nil)
  (my/define-keys markdown-mode-map
                  '(( "C-<return>" . markdown-jump)
                    ( "C-c C-c p"  . my/open-with)
                    ( "M-<up>"     . nil)
                    ( "M-<down>"   . nil))))

(add-hook 'markdown-mode-hook #'my/markdown-mode-hook)

;; MTG deck mode
(defvar mtg-deck-mode-map)
(defun my/mtg-deck-mode-hook ()
  "My `mtg-deck' mode hook."
  (company-mode)
  (setq-local company-backends '(company-capf))
  (my/define-keys mtg-deck-mode-map
                  '(( "<tab>" . my/snippet-or-complete))))

(add-hook 'mtg-deck-mode-hook #'my/mtg-deck-mode-hook)

;; nXML
(defvar nxml-mode-map)
(defun my/nxml-mode-hook ()
  "My `nxml' mode hook."
  (setq-local company-backends '(company-nxml))
  (my/define-keys nxml-mode-map
                  '(( "<tab>" . my/indent-snippet-or-complete))))

(add-hook 'nxml-mode-hook #'my/nxml-mode-hook)
(add-hook 'nxml-mode-hook #'my/prog-mode)

;; Package Menu
(defun my/package-menu-mode-hook ()
  "My `package-menu' mode hook."
  (my/remap-keys package-menu-mode-map
                 '(("s" . "C-s")
                   ("R" . "r")
                   ("r" . "C-r"))))

(add-hook 'package-menu-mode-hook #'my/package-menu-mode-hook)

;; Prog
(defvar flyspell-prog-text-faces)
(defun my/prog-mode ()
  "My `prog-mode' hook."

  (setq-local fill-column 80)
  (unless (derived-mode-p 'makefile-mode)
    (setq-local indent-tabs-mode nil))

  (ws-butler-mode)
  (company-mode)
  (flycheck-mode)
  (setq flyspell-prog-text-faces
        '(font-lock-comment-face font-lock-doc-face))
  (flyspell-prog-mode)
  (fci-mode)
  (highlight-numbers-mode)
  (emr-initialize)

  (my/define-keys prog-mode-map
                  '(( "<tab>"       . my/indent-snippet-or-complete)
                    ( "C-z f f"     . emr-show-refactor-menu)
                    ( "C-<return>"  . find-tag)))
  (my/remap-keys  prog-mode-map
                  '(( "RET"         . "M-j"))))

(add-hook 'prog-mode-hook #'my/prog-mode)

;; Projectile
(defun my/projectile-mode-hook ()
  "My `projectile' mode hook."
  (setq projectile-mode-line
        '(:eval (format " [%s]" (projectile-project-name))))

  (projectile-register-project-type 'jhbuild
                                    (lambda () nil)
                                    "jhbuild make"
                                    "make check"
                                    "jhbuild run ${PWD##*/}")

  (my/define-keys projectile-command-map
                  '(( "s p" . projectile-pt)
                    ( "B"   . projectile-ibuffer)))

  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired))

  (def-projectile-commander-method ?q
    "Go back to project selection."
    (projectile-switch-project))

  ;; TODO: fix this!
  (def-projectile-commander-method ?t
    "Open a terminal in the project root."
    (ansi-term (format "*ansi-term [%s]*" (projectile-project-name)))))

(add-hook 'projectile-mode-hook #'my/projectile-mode-hook)

;; PT
(defvar pt-search-mode-map)
(defun my/pt-search-mode-hook ()
  "My `pt-search' mode hook."
  (my/define-keys pt-search-mode-map
                  '(( "W" . wgrep-change-to-wgrep-mode))))

(add-hook 'pt-search-mode-hook #'my/pt-search-mode-hook)

;; Python
(defvar python-mode-map)
(defun my/python-mode-hook ()
  "My `python' mode hook."
  (setq-local fill-column 79)           ; PEP0008 says lines should be 79 chars
  (my/define-keys python-mode-map
                  '(( "C-<return>" . elpy-goto-definition)
                    ( "."          . my/dot-and-complete))))

(add-hook 'python-mode-hook #'my/python-mode-hook)

;; REST Client
(defvar restclient-mode-map)
(defun my/restclient-mode-hook ()
  "My `restclient' mode hook."
  (company-mode)
  (setq-local company-backends '((company-restclient)))
  (my/define-keys restclient-mode-map
                  '(( "<tab>" . my/snippet-or-complete))))

(add-hook 'restclient-mode-hook #'my/restclient-mode-hook)

;; Rust
(defvar rust-mode-map)
(defun my/rust-mode-hook ()
  "My `rust' mode hook."
  (racer-mode)
  (my/define-keys rust-mode-map
                  '(( "C-<return>" . racer-find-definition)
                    ( "."          . my/dot-and-complete)
                    ( ":"          . my/double-colon-and-complete))))

(add-hook 'rust-mode-hook #'my/rust-mode-hook)

;; Shell
(defvar term-raw-map)
(defvar yas-dont-activate)
(defun my/term-mode-hook ()
  "My `term' mode hook."
  (setq yas-dont-activate t)
  (my/define-keys term-raw-map
                  '(( "M-x"       . smex)
                    ( "C-y"       . my/term-paste)
                    ( "<escape>"  . ESC-prefix))))

(defun my/term-exec-hook ()
  "My `term' mode hook."
  (set-buffer-process-coding-system 'utf-8-unix
                                    'utf-8-unix))

(add-hook 'term-mode-hook #'my/term-mode-hook)
(add-hook 'term-exec-hook #'my/term-exec-hook)

;; Shell script
(defun my/sh-mode-hook ()
  "My `sh' mode hook."
  (setq-local company-backends '((company-shell
                                  company-dabbrev-code
                                  company-files
                                  company-keywords)))
  (sh-extra-font-lock-activate))

(add-hook 'sh-mode-hook #'my/sh-mode-hook)

;; Vala
(defvar flycheck-checkers)
(defun my/vala-mode-hook ()
  "My `vala' mode hook."
  (require 'flycheck-vala)
  (add-to-list 'flycheck-checkers 'vala-valac))

(add-hook 'vala-mode-hook #'my/prog-mode)
(add-hook 'vala-mode-hook #'my/vala-mode-hook)

;; Woman
(defvar woman-mode-map)
(defun my/woman-mode-hook ()
  "My `woman' mode hook."
  (my/remap-keys woman-mode-map
                 '(("a" . "s")
                   ("s" . "C-s")
                   ("r" . "R")
                   ("r" . "C-r"))))

(add-hook 'woman-mode-hook #'my/woman-mode-hook)

;;; Project specific settings

;; JHBuild
(dir-locals-set-class-variables
 'gnome-code
 '((nil . ((projectile-project-type . jhbuild)))))

(dir-locals-set-directory-class "~/Code/gnome/src/" 'gnome-code)


;;; Post-init code

(defun my/activate-global-keybindings ()
  "Activate global keybindings."
  (my/global-remap-keys
   '((occur                    . my/occur-dwim)
     (isearch-forward          . my/isearch-forward-symbol-with-prefix)
     (isearch-backward         . my/isearch-backward-symbol-with-prefix)
     (execute-extended-command . smex)
     (delete-other-windows     . zygospore-toggle-delete-other-windows)))

  (my/global-define-keys
   '(
     ;; Global overrides
     ( "M-."         .  nil)
     ( "C-M-."       .  nil)

     ;; Windows
     ( "C-z"         .  nil)
     ( "C-z w c"     .  customize)
     ( "C-z w p"     .  list-packages)
     ( "C-z w t"     .  ansi-term)
     ( "C-z w r"     .  my/restclient)
     ( "C-z w e"     .  ielm)

     ;; Documentation
     ( "C-z h i"     .  info-display-manual)
     ( "C-z h m"     .  woman)

     ;; Toggle modes
     ( "C-z t w"     .  whitespace-mode)
     ( "C-z t a"     .  aggressive-indent-mode)
     ( "C-z t b"     .  magit-blame)
     ( "<escape>"    .  my/control-mode-on)
     ( "<insert>"    .  my-global-control-mode)

     ;; Other
     ( "C-z d"       .  diff-buffer-with-file)
     ( "C-z R"       .  restart-emacs)

     ;; NAVIGATION

     ;; General
     ( "C-'"         .  ace-jump-word-mode)
     ( "C-z g"       .  imenu)
     ( "C-z b"       .  browse-url-at-point)
     ( "C-z o"       .  my/open-with)
     ( "C-z n"       .  make-frame)
     ( "C-<next>"    .  forward-page)
     ( "C-<prior>"   .  backward-page)
     ( "M-<left>"    .  pop-tag-mark)
     ( "M-<right>"   .  nil)

     ;; Move buffers
     ( "<C-S-up>"    .  buf-move-up)
     ( "<C-S-down>"  .  buf-move-down)
     ( "<C-S-left>"  .  buf-move-left)
     ( "<C-S-right>" .  buf-move-right)


     ;; TEXT MANIPULATION

     ;; General
     ( "M-<up>"      .  move-text-up)
     ( "M-<down>"    .  move-text-down)
     ( "C-z a"       .  align-string)
     ( "C-z ."       .  align-by-current-symbol)
     ( "C-z c"       .  my/toggle-comment)
     ( "C-z <up>"    .  my/toggle-programming-case-word-at-point)
     ( "C-z <down>"  .  my/toggle-programming-case-word-at-point-reverse)
     ( "C-z u"       .  insert-char)
     ( "C-z ="       .  my/calc-thing-at-point)
     ( "C-z +"       .  my/increment-number-decimal)
     ( "C-z -"       .  my/decrement-number-decimal)
     ( "C-z i"       .  my/ispell-word-then-abbrev)
     ( "C-a"         .  mwim-beginning-of-code-or-line)
     ( "C-e"         .  mwim-end-of-code-or-line)

     ;; Replace
     ( "C-z r"       .  vr/replace)
     ( "C-z q"       .  vr/query-replace)

     ;; YAS
     ( "C-z s i"     .  yas-insert-snippet)
     ( "C-z s c"     .  yas-new-snippet)
     ( "C-z s e"     .  yas-visit-snippet-file)
     ( "C-z s r"     .  yas-reload-all)
     ( "C-z s t"     .  auto-insert)))

  (windmove-default-keybindings))

(defun my/activate-visual-regexp ()
  "Activate visual-regexp."
  (require 'visual-regexp-steroids)
  (my/define-keys esc-map
                  '(( "C-r" . vr/isearch-backward)
                    ( "C-s" . vr/isearch-forward))))

(defun my/activate-yas ()
  "Activate YASnippet."
  ;; This needs to be set here, or customize will bork.
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode))

(defvar god-mode-isearch-map)
(defun my/activate-god-mode-isearch ()
  "Activate `god-mode-isearch'."
  (require 'god-mode-isearch)
  (my/define-keys isearch-mode-map
                  '(( "<escape>" . god-mode-isearch-activate)
                    ( "<insert>" . god-mode-isearch-activate)))
  (my/define-keys god-mode-isearch-map
                  '(( "g"        . isearch-cancel)
                    ( "i"        . god-mode-isearch-disable)
                    ( "<insert>" . god-mode-isearch-disable))))

(defvar my-global-control-mode)
(defun my/activate-control-mode ()
  "Activate Control Mode."
  (require 'control-mode)

  (define-globalized-minor-mode my-global-control-mode ;TODO: get this upstream
    control-mode
    my/maybe-toggle-control-mode)

  (add-hook 'after-change-major-mode-hook
            #'my/control-mode-set-cursor)
  (my-global-control-mode)
  (my/activate-god-mode-isearch))

(defun my/activate-keyfreq-mode ()
  "Activate KeyFreq Mode."
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(defun my/activate-modes ()
  "Activate a bunch of global modes."
  (cask-initialize)
  (pallet-mode)
  (my/activate-control-mode)
  (powerline-major-mode)
  (powerline-default-theme)
  (global-git-gutter-mode)
  (ido-mode)
  (ido-vertical-mode)
  (ido-ubiquitous-mode)
  (flx-ido-mode)
  (fancy-narrow-mode)
  (which-key-mode)
  (auto-insert-mode)
  (auto-compile-on-save-mode)
  (auto-compile-on-load-mode)
  (browse-kill-ring-default-keybindings)
  (easy-repeat-mode)
  (smart-region-on)
  (elpy-enable)
  (global-aggressive-indent-mode)
  (projectile-global-mode)
  (recentf-mode)
  (abbrev-mode)
  (my/activate-keyfreq-mode)
  (my/activate-god-mode-isearch)
  (my/activate-visual-regexp)
  (my/activate-yas))

(add-hook 'after-init-hook (lambda ()
                             (require 'cask  "~/.emacs.d/lisp/cask/cask.el")
                             (my/activate-modes)
                             (my/activate-global-keybindings)))


;;; Advice

(advice-add #'isearch-forward-symbol-at-point  :after  #'god-mode-isearch-activate)
(advice-add #'isearch-backward-symbol-at-point :after  #'god-mode-isearch-activate)
(advice-add #'popup-create                     :before #'my/fci-turn-off)
(advice-add #'popup-delete                     :after  #'my/fci-turn-on)

(advice-add #'ido-find-file                    :after  #'my/reopen-file-as-root)

(advice-add #'backward-page                    :after  #'recenter)
(advice-add #'forward-page                     :after  #'recenter)

(advice-add #'diff-buffer-with-file            :after  (lambda (&rest _) (other-window 1)))
(advice-add #'delete-window                    :after  (lambda (&rest _) (balance-windows)))
(advice-add #'split-window-right               :after  #'balance-windows)
(advice-add #'split-window-below               :after  #'balance-windows)
(advice-add #'split-window-right               :after  (lambda () (other-window 1)))
(advice-add #'split-window-below               :after  (lambda () (other-window 1)))

;; Kill terminal buffer when the terminal process exits
(advice-add #'term-sentinel
            :after (lambda (proc _)
                     (when (memq (process-status proc) '(signal exit))
                       (kill-buffer (process-buffer proc)))))

(advice-add #'ansi-term
            :before (lambda (_)
                      (interactive (list "/bin/bash"))))

(advice-add #'custom-save-all
            :around (lambda (func &rest args)
                      (let ((print-quoted t))
                        (apply func args))))

(advice-add #'save-buffers-kill-emacs
            :around (lambda (func &rest args)
                      (cl-flet ((process-list ()))
                        (apply func args))))

(advice-add #'flycheck-pos-tip-error-messages
            :around (lambda (func &rest args)
                      (let ((x-gtk-use-system-tooltips nil))
                        (apply func args))))

(mapc #'my/advice-describe-func '(package-menu-describe-package
                                  describe-variable
                                  describe-mode
                                  describe-function
                                  describe-bindings))
(advice-add #'keyfreq-show
            :after (lambda (&rest _)
                     (my/focus-buffer-dwim "*KeyFreq*")
                     (tabulated-list-mode)))

(provide 'init)
;;; init.el ends here
