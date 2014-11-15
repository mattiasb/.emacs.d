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

;; Unset these early to remove at least some of the inital flicker.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path
(setq load-prefer-newer t)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/cask/")
(require 'funcs)

;; Maximize on start
(my/maximize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;;;; Keybindings ;;;;
(my/global-set-keys
 '(
   ;; Global overrides
   ( "C-x 1"       .  zygospore-toggle-delete-other-windows)
   ( "M-x"         .  smex)

   ;; Windows
   ( "C-c w c"     .  customize)
   ( "C-c w p"     .  list-packages)
   ( "C-c w t"     .  ansi-term)
   ( "C-c w m"     .  magit-status)
   ( "C-c w h"     .  info-display-manual)

   ;; Magit
   ( "C-c m c"     .  magit-commit)

   ;; Marks / Highlights
   ( "C-§"         .  er/expand-region)
   ( "C-c h"       .  highlight-symbol-at-point)

   ;; Toggle modes
   ( "C-c t w"     .  whitespace-mode)
   ( "C-c t f"     .  fullscreen-mode-fullscreen-toggle)
   ( "C-c t a"     .  aggressive-indent-mode)


   ;; NAVIGATION

   ;; General
   ( "C-'"         .  ace-jump-word-mode)
   ( "C-c o"       .  browse-url-at-point)
   ( "C-c n"       .  make-frame)

   ;; Move buffers
   ( "<C-S-up>"    .  buf-move-up)
   ( "<C-S-down>"  .  buf-move-down)
   ( "<C-S-left>"  .  buf-move-left)
   ( "<C-S-right>" .  buf-move-right)


   ;; TEXT MANIPULATION

   ;; General
   ( "<tab>"       .  my/tab-indent-or-complete)
   ( "M-<up>"      .  move-text-up)
   ( "M-<down>"    .  move-text-down)
   ( "C-c a"       .  align-regexp)
   ( "C-c c"       .  my/toggle-comment)
   ( "C-c C-c"     .  my/toggle-programming-case-word-at-point)
   ( "C-c i"       .  insert-char)
   ( "C-c d"       .  my/insert-date) ;; TODO: Replace with snippet

   ;; Replace
   ( "C-c r"       .  replace-string)
   ( "C-c C-r"     .  replace-regexp)
   ( "C-c q"       .  query-replace)
   ( "C-c C-q"     .  query-replace-regexp)

   ;; YAS
   ( "C-c s s"     .  yas-insert-snippet)
   ( "C-c s n"     .  yas-new-snippet)
   ( "C-c s e"     .  yas-visit-snippet-file)
   ( "C-c s r"     .  yas-reload-all)
   ))

(global-unset-key (kbd "C-z"))
(global-set-key [remap kill-line]   (my/bol-with-prefix kill-line))

(windmove-default-keybindings)

;;;; Modes ;;;;

;; Auto Modes
(my/auto-modes '(("\\.inl\\'" . c++-mode)
                 ("\\.ui$"    . nxml-mode)
                 ("\\.js$"    . js2-mode)
                 ))

(my/shorten-major-modes '((markdown-mode   . "Md")
                          (js2-mode        . "JS2")
                          (nxml-mode       . "X")
                          (c-mode          . "C")
                          (c++-mode        . "C++")
                          (cmake-mode      . "Cm")
                          (emacs-lisp-mode . "El")
                          (go-mode         . "Go")
                          (haskell-mode    . "Hs")
                          (snippet-mode    . "S")
                          ))

(my/shorten-minor-modes '((company-mode           . "Co")
                          (abbrev-mode            . "A")
                          (flycheck-mode          . "Fc")
                          (git-gutter-mode        . "")
                          (yas-minor-mode         . "")
                          (aggressive-indent-mode . " ⇒")
                          (magit-auto-revert-mode . "")
                          ))

;;;; Specific modes

;; C common
(add-hook 'c-mode-common-hook
          (lambda ()
            (require 'rtags)
            (my/rtags-start)
            (setq-local rtags-completions-enabled t)
            (my/define-keys c-mode-base-map
                            '(("C-<return>" . rtags-find-symbol-at-point)
                              ("M-<left>"   . rtags-location-stack-back)
                              ("M-<right>"  . rtags-location-stack-forward)
                              ("C-c f r"    . rtags-rename-symbol)
                              ("C-c o"      . ff-find-other-file)))
            ))

;; CMake
(add-hook 'cmake-mode-hook 'my/prog-mode)
(add-hook 'cmake-mode-hook
          (lambda ()
            (setq-local company-backends
                        '((company-cmake
                           company-files
                           company-dabbrev-code)))))

;; Company
(add-hook 'company-mode-hook
          (lambda ()
            (my/define-keys company-active-map
                            '(("\C-n"    . company-select-next)
                              ("\C-p"    . company-select-previous)
                              ("<next>"  . my/company-select-next-five)
                              ("<prior>" . my/company-select-previous-five)
                              ("\C-p"    . company-select-previouss)
                              ("\C-d"    . company-show-doc-buffer)
                              ("\C-v"    . company-show-location)
                              ("\C-g"    . company-abort)
                              ))))

;; Workaround for fci-mode with Company
;; https://github.com/company-mode/company-mode/issues/180#issuecomment-55047120
(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  "Turn off fci-mode. (IGNORE any parameters)."
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  "Turn on fci-mode. (IGNORE any parameters)."
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook   'company-turn-off-fci)
(add-hook 'company-completion-finished-hook  'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

;; ELisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (my/define-keys emacs-lisp-mode-map
                            '(("C-<tab>" . company-complete)
                              ("<tab>"   . my/tab-indent-or-complete)))))

;; Go
(add-hook 'go-mode-hook
          (lambda ()
            (go-eldoc-setup)

            (setq-local tab-width 4)
            (setq-local company-backends '(company-go))

            (my/define-keys go-mode-map
                            '(("C-c i a"    . go-import-add)
                              ("C-c i r"    . go-remove-unused-imports)
                              ("C-c i g"    . go-goto-imports)
                              ("C-c d"      . godoc-at-point)
                              ("C-<return>" . godef-jump)))))

;; Magit
(add-hook 'git-commit-mode-hook
          (lambda ()
            (company-mode 1)
            (fci-mode 1)
            ))

;; Haskell
(add-hook 'haskell-mode-hook (lambda ()
                               (setq-local electric-indent-mode nil)))

;; Ido
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [tab] 'ido-complete)
            ))

;; JS2
(add-hook 'js2-mode-hook
          (lambda ()
            (require 'js2-refactor)
            (define-key js2-mode-map (kbd "C-c f r") 'js2r-rename-var)
            (setq-local company-backends '((company-dabbrev-code
                                            company-files
                                            company-keywords)))))

;; nXML
(add-hook 'nxml-mode-hook (lambda ()
                            (setq-local company-backends '(company-nxml))
                            (aggressive-indent-mode -1)
                            ))
(add-hook 'nxml-mode-hook 'my/prog-mode)

;; Package
(add-hook 'package-menu-mode-hook 'hl-line-mode)

;; Prog
(defun my/prog-mode ()
  "My `prog-mode' hook."

  (setq-local fill-column      80)
  (setq-local indent-tabs-mode nil)

  (company-mode)
  (flycheck-mode)
  (fci-mode)
  (highlight-numbers-mode)
  (aggressive-indent-mode))

(add-hook 'prog-mode-hook 'my/prog-mode)

;; Shell
(add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions 'bash-completion-dynamic-complete)
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map   (kbd "<tab>")
                              (lookup-key term-raw-map (kbd "C-M-i")))
                            (define-key term-raw-map   (kbd "M-x") 'smex)))

;;;; Project specific settings ;;;;

;; JHBuild
(dir-locals-set-class-variables
 'gnome-code
 '((nil . ((eval . (progn
                     (puthash (projectile-project-root)
                              "jhbuild make"
                              projectile-compilation-cmd-map)
                     (puthash (projectile-project-root)
                              "make check"
                              projectile-test-cmd-map)
                     ))))))
(dir-locals-set-directory-class "~/Code/gnome/" 'gnome-code)

;;;; Post-init code ;;;;

(defun my/after-init ()
  "My after init hook."
  (require 'cask)
  (cask-initialize)
  (require 'pallet)

  ;; customizations that for various reasons can't be in the customize block.
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq projectile-mode-line
        '(:eval (format " P[%s]" (projectile-project-name))))
  (when (and (string= (window-system) "w32"))
    (setq-default projectile-indexing-method 'native))

  ;; Activate a bunch of global modes
  (yas-global-mode)
  (global-git-gutter-mode)
  (projectile-global-mode)
  (ido-mode)
  (ido-vertical-mode)
  (ido-ubiquitous-mode)
  (flx-ido-mode)
  (fancy-narrow-mode))

(add-hook 'after-init-hook 'my/after-init)

;; Advices

(defadvice split-window-right (after rebalance-windows activate)
  "Balance windows after splitting."
  (balance-windows)
  (other-window 1))
(defadvice split-window-below (after rebalance-windows activate)
  "Balance windows after splitting."
  (balance-windows)
  (other-window 1))
(defadvice delete-window (after rebalance-windows activate)
  "Balance windows after deletion."
  (balance-windows))

(defadvice magit-status (around magit-fullscreen activate)
  "Save window configuration when running magit-fullscreen."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))
(defadvice magit-mode-quit-window (after magit-restore-screen activate)
  "Restore previous window configuration after quitting magit-fullscreen."
  (jump-to-register :magit-fullscreen))

;;;; Other settings ;;;;

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)

(provide 'init)
;;; init.el ends here
