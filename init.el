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
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/cask/")
(require 'funcs)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;;; Early init code

;; Maximize on start
(my/maximize)


;;; Keybindings
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "M-."))
(global-set-key [remap kill-line]       (my/bol-with-prefix kill-line))
(global-set-key [remap occur]           #'my/occur-dwim)
(global-set-key [remap isearch-forward] #'my/isearch-symbol-with-prefix)

(my/global-set-keys
 '(
   ;; Global overrides
   ( "C-x 1"       .  zygospore-toggle-delete-other-windows)
   ( "C-x C-x"     .  smex)
   ( "M-x"         .  smex)

   ;; Windows
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
   ( "<insert>"    .  my/control-mode-toggle)

   ;; Other
   ( "C-z d"       .  diff-buffer-with-file)
   ( "C-z R"       .  restart-emacs)

   ;; NAVIGATION

   ;; General
   ( "C-'"         .  ace-jump-word-mode)
   ( "C-z o"       .  browse-url-at-point)
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
   ( "C-<tab>"     .  my/yas-insert-or-expand)
   ( "C-z s c"     .  yas-new-snippet)
   ( "C-z s e"     .  yas-visit-snippet-file)
   ( "C-z s r"     .  yas-reload-all)
   ( "C-z s t"     .  auto-insert)))

(windmove-default-keybindings)


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
                          (git-gutter-mode          . "")
                          (magit-gitflow-mode       . " Flow")
                          (magit-filenotify-mode    . " Notify")
                          (yas-minor-mode           . "")
                          (fancy-narrow-mode        . "")
                          (haskell-indentation-mode . "")
                          (racer-mode               . "")
                          (aggressive-indent-mode   . " ⇒")
                          (which-key-mode           . "")
                          (magit-auto-revert-mode   . "")))


;;; Modes – Specific

;; Browse Kill Ring
(add-hook 'browse-kill-ring-mode-hook
          (lambda ()
            (my/define-keys browse-kill-ring-mode-map
                            '(( "<down>"    . browse-kill-ring-forward)
                              ( "<tab>"     . browse-kill-ring-forward)
                              ( "<up>"      . browse-kill-ring-previous)
                              ( "<backtab>" . browse-kill-ring-previous)
                              ( "C-g"       . browse-kill-ring-quit)))))

;; C common
(defvar c-mode-base-map)
(add-hook 'c-mode-common-hook
          (lambda ()
            (unless (keymap-parent c-mode-base-map)
              (set-keymap-parent c-mode-base-map prog-mode-map))
            ))
;; C / C++
(defvar rtags-completions-enabled)
(defun my/c-mode-hook ()
  "A mode hook for C and C++."
  (require 'rtags)
  (require 'company-rtags)
  (my/rtags-start)
  (setq-local rtags-completions-enabled t)
  (rtags-enable-standard-keybindings c-mode-base-map)
  (setq-local company-backends '((company-rtags)))

  ;; Work around bug where c-mode-base-map doesn't inherit from
  ;; prog-mode-map
  (my/define-keys c-mode-base-map
                  '(("C-<return>" . rtags-find-symbol-at-point)
                    ("M-<left>"   . rtags-location-stack-back)
                    ("M-<right>"  . rtags-location-stack-forward)
                    ("C-z f r"    . rtags-rename-symbol)
                    ("."          . my/dot-and-complete)
                    (":"          . my/double-colon-and-complete)
                    (">"          . my/arrow-and-complete)))
  (my/define-keys projectile-command-map
                  '(( "j"         . rtags-find-symbol))))

(add-hook 'c-mode-hook   'my/c-mode-hook)
(add-hook 'c++-mode-hook 'my/c-mode-hook)

;; CMake
(add-hook 'cmake-mode-hook 'my/prog-mode)
(add-hook 'cmake-mode-hook
          (lambda ()
            (setq-local company-backends
                        '((company-cmake
                           company-files
                           company-dabbrev-code)))))

;; Control
(add-hook 'control-mode-keymap-generation-functions
          'control-mode-ctrlx-hacks)
(add-hook 'control-mode-hook
          (lambda ()
            (setq cursor-type (if control-mode
                                  'box
                                '(bar . 5)))
            (my/define-keys control-mode-keymap
                            '(("i"           . my/control-mode-off)
                              ("<escape>"    . ESC-prefix)
                              ("x s"         . save-buffer)
                              ("x S"         . save-some-buffers)
                              ))))

;; Company
(add-hook 'company-mode-hook
          (lambda ()
            (company-quickhelp-mode)
            (my/define-keys company-active-map
                            '(("\C-n"    . company-select-next)
                              ("\C-p"    . company-select-previous)
                              ("<next>"  . my/company-select-next-nine)
                              ("<prior>" . my/company-select-previous-nine)
                              ("\C-v"    . company-show-location)
                              ("\C-g"    . company-abort)))))

(add-hook 'company-completion-started-hook   'my/fci-turn-off)
(add-hook 'company-completion-finished-hook  'my/fci-turn-on)
(add-hook 'company-completion-cancelled-hook 'my/fci-turn-on)

;; Dired
(defvar dired-mode-map)
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)
            (my/control-mode-off)
            (my/define-keys dired-mode-map
                            '(("W" . wdired-change-to-wdired-mode)
                              ("F" . find-name-dired)
                              ("c" . find-file)))))

;; ELisp
(add-hook 'emacs-lisp-mode-hook #'lisp-extra-font-lock-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq page-delimiter
                  (rx bol ";;;" (not (any "#")) (* not-newline) "\n"
                      (* (* blank) (opt ";" (* not-newline)) "\n")))
            ))

;; Flycheck
(add-hook 'flycheck-mode-hook
          (lambda()
            ;; Re-add this when it works correctly
            ;; (require 'flycheck-jscs)
            ;; (add-to-list 'flycheck-checkers 'javascript-jscs)
            (flycheck-pos-tip-mode)
            (flycheck-status-emoji-mode)
            (flycheck-cask-setup)
            (flycheck-package-setup)))

;; Find-file
(add-hook 'find-file-not-found-functions #'my/create-non-existent-directory)

;; Go
(defvar go-mode-map)
(add-hook 'go-mode-hook
          (lambda ()
            (go-eldoc-setup)

            (setq-local tab-width 4)
            (setq-local company-backends '(company-go))

            (my/define-keys go-mode-map
                            '(("C-z i a"    . go-import-add)
                              ("C-z i r"    . go-remove-unused-imports)
                              ("C-z i g"    . go-goto-imports)
                              ("C-z d"      . godoc-at-point)
                              ("C-<return>" . godef-jump)
                              ("."          . my/dot-and-complete)))))

;; Haskell
(add-hook 'haskell-mode-hook (lambda ()
                               (setq-local electric-indent-mode nil)
                               (turn-on-haskell-indentation)))
;; Help
(add-hook 'help-mode-hook (lambda ()
                            (my/define-keys help-mode-map
                                            '(("M-<left>"  . help-go-back)
                                              ("M-<right>" . help-go-forward)))))

;; Ido
(add-hook 'ido-setup-hook
          (lambda ()
            (my/define-keys ido-common-completion-map
                            '(( "<tab"    . ido-complete)
                              ( "<next>"  . my/ido-select-next-nine)
                              ( "<prior>" . my/ido-select-prev-nine)))))

;; IBuffer
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; IELM
(defvar ielm-map)
(add-hook 'ielm-mode-hook
          (lambda()
            (company-mode)
            (my/define-keys ielm-map
                            '(("<tab>" . my/indent-snippet-or-complete)))))

;; Info
(add-hook 'Info-mode-hook
          (lambda ()
            (my/define-keys Info-mode-map
                            '(("M-<left>"  . Info-history-back)
                              ("M-<right>" . Info-history-forward)
                              ("M-<up>"    . Info-up)))))

;; JS2
(defvar js2-mode-map)
(autoload 'js2r-rename-var "js2-refactor" "" t nil)
(add-hook 'js2-mode-hook
          (lambda ()
            (require 'js2-refactor)
            (define-key js2-mode-map (kbd "C-z f r") #'js2r-rename-var)
            (setq-local company-backends '((company-dabbrev-code
                                            company-files
                                            company-keywords)))))

;; Lua
(add-hook 'lua-mode-hook
          (lambda ()
            (setq-local company-backends '((company-dabbrev-code
                                            company-files
                                            company-keywords)))))

;; Magit
(add-hook 'magit-status-mode-hook
          #'magit-filenotify-mode)

(add-hook 'git-commit-mode-hook
          (lambda ()
            (my/control-mode-off)
            (flyspell-mode)
            (fci-mode 1)
            (setq-local fill-column 72)))

(autoload 'turn-on-magit-gitflow "magit-gitflow" "" t nil)
(add-hook 'magit-mode-hook
          (lambda ()
            (require 'magit-gitflow)
            (turn-on-magit-gitflow)))

(defvar magit-blame-mode-map)
(add-hook 'magit-blame-mode-hook
          (lambda ()
            (my/define-keys magit-blame-mode-map
                            '(( "C-z t b"     .  magit-blame-quit)))))

;; Markdown
(defvar markdown-mode-map)
(add-hook 'markdown-mode-hook
          (lambda ()
            (setq-local fill-column 80)
            (fci-mode)
            (my/define-keys markdown-mode-map
                            '(("C-<return>" . markdown-jump)
                              ("M-<up>"     . nil)
                              ("M-<down>"   . nil)))))
;; MTG deck mode
(defvar mtg-deck-mode-map)
(add-hook 'mtg-deck-mode-hook
          (lambda ()
            (company-mode)
            (setq-local company-backends '(company-capf))
            (my/define-keys mtg-deck-mode-map
                            '(("<tab>" . my/snippet-or-complete)))
            ))

;; nXML
(defvar nxml-mode-map)
(add-hook 'nxml-mode-hook
          (lambda ()
            (setq-local company-backends '(company-nxml))
            (my/define-keys nxml-mode-map
                            '(("<tab>" . my/indent-snippet-or-complete)))))

(add-hook 'nxml-mode-hook #'my/prog-mode)

;; Package
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; Prog
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
                  '(("<tab>"       . my/indent-snippet-or-complete)
                    ("C-z f f"     . emr-show-refactor-menu)
                    ("C-<return>"  . find-tag))))

(add-hook 'prog-mode-hook #'my/prog-mode)

;; Projectile
(add-hook 'projectile-mode-hook
          (lambda ()
            (setq projectile-mode-line
                  '(:eval (format " [%s]" (projectile-project-name))))

            (projectile-register-project-type 'jhbuild
                                              (lambda () nil)
                                              "jhbuild make"
                                              "make check"
                                              "jhbuild run ${PWD##*/}")

            (my/define-keys projectile-command-map
                            '(("s p" . projectile-pt)
                              ("B"   . projectile-ibuffer)))
            ))

;; PT
(defvar pt-search-mode-map)
(add-hook 'pt-search-mode-hook
          (lambda ()
            (my/define-keys pt-search-mode-map
                            '(("W" . wgrep-change-to-wgrep-mode)))))

;; Python
(add-hook 'python-mode-hook
          (lambda ()
            (my/define-keys python-mode-map
                            '(("C-<return>" . elpy-goto-definition)
                              ("."          . my/dot-and-complete))
                            )))

;; REST Client
(defvar restclient-mode-map)
(add-hook 'restclient-mode-hook
          (lambda ()
            (company-mode)
            (setq-local company-backends '((company-restclient)))
            (my/define-keys restclient-mode-map
                            '(("<tab>" . my/snippet-or-complete)))))

;; Rust
(defvar rust-mode-map)
(add-hook 'rust-mode-hook
          (lambda ()
            (racer-mode)
            (my/define-keys rust-mode-map
                            '(("C-<return>" . racer-find-definition)
                              ("."          . my/dot-and-complete)
                              (":"          . my/double-colon-and-complete)
                              ))
            ))

;; Shell
(add-hook 'shell-dynamic-complete-functions #'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions #'bash-completion-dynamic-complete)
(defvar term-raw-map)
(add-hook 'term-mode-hook (lambda ()
                            (my/control-mode-off)
                            (define-key term-raw-map   (kbd "<tab>")
                              (lookup-key term-raw-map (kbd "C-M-i")))
                            (my/define-keys term-raw-map
                                            '(("M-x" . smex)
                                              ("C-y" . my/term-paste)
                                              ))))

(add-hook 'term-exec-hook (lambda ()
                            (set-buffer-process-coding-system 'utf-8-unix
                                                              'utf-8-unix)))

;; Shell script
(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local company-backends '((company-shell
                                            company-dabbrev-code
                                            company-files
                                            company-keywords)))
            (sh-extra-font-lock-activate)))

;; Vala
(add-hook 'vala-mode-hook #'my/prog-mode)
(add-hook 'vala-mode-hook
          (lambda ()
            (require 'flycheck-vala)
            (add-to-list 'flycheck-checkers 'vala-valac)))


;;; Project specific settings

;; JHBuild
(dir-locals-set-class-variables
 'gnome-code
 '((nil . ((projectile-project-type . jhbuild)))))

(dir-locals-set-directory-class "~/Code/gnome/" 'gnome-code)


;;; Post-init code

(defun my/activate-visual-regexp ()
  "Activate visual-regexp."
  (require 'visual-regexp-steroids)
  (my/define-keys esc-map
                  '(("C-r" . vr/isearch-backward)
                    ("C-s" . vr/isearch-forward))))

(defun my/activate-yas ()
  "Activate YASnippet."
  ;; This needs to be set here, or customize will bork.
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode))

(defun my/activate-god-mode-isearch ()
  "Activate `god-mode-isearch'."
  (require 'god-mode-isearch)
  (my/define-keys isearch-mode-map
                  '(("<escape>" . god-mode-isearch-activate)
                    ("<insert>" . god-mode-isearch-activate)))
  (my/define-keys god-mode-isearch-map
                  '(("g"        . isearch-cancel)
                    ("i"        . god-mode-isearch-disable)
                    ("<insert>" . god-mode-isearch-disable))))

(defun my/activate-control-mode ()
  "Activate Control Mode."
  (global-control-mode)
  (add-hook #'projectile-mode-hook
            #'control-mode-reload-bindings))

(defun my/activate-modes ()
  "Activate a bunch of global modes."
  (cask-initialize)
  (pallet-mode)
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
  (my/activate-control-mode)
  (my/activate-god-mode-isearch)
  (my/activate-visual-regexp)
  (my/activate-yas))

(add-hook 'after-init-hook (lambda ()
                             (require 'cask)
                             (my/activate-modes)))


;;; Advices

(advice-add #'isearch-forward-symbol-at-point :after #'god-mode-isearch-activate)
(advice-add #'popup-create                    :before #'my/fci-turn-off)
(advice-add #'popup-delete                    :after  #'my/fci-turn-on)

(advice-add #'ido-find-file                   :after  #'my/reopen-file-as-root)

(advice-add #'backward-page                   :after  #'recenter)
(advice-add #'forward-page                    :after  #'recenter)

(advice-add #'delete-window                   :after (lambda (_) (balance-windows)))
(advice-add #'split-window-right              :after  #'balance-windows)
(advice-add #'split-window-below              :after  #'balance-windows)
(advice-add #'split-window-right              :after (lambda () (other-window 1)))
(advice-add #'split-window-below              :after (lambda () (other-window 1)))

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


(provide 'init)
;;; init.el ends here
