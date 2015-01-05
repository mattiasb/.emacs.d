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



;;;; Settings ;;;;

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



;;;; Early init code

;;Maximize on start
(my/maximize)



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
   ( "C-c w r"     .  my/restclient)

   ;; Documentation
   ( "C-c h i"     .  info-display-manual)
   ( "C-c h m"     .  woman)

   ;; Marks / Highlights
   ( "C-§"         .  er/expand-region)

   ;; Toggle modes
   ( "C-c t w"     .  whitespace-mode)
   ( "C-c t f"     .  fullscreen-mode-fullscreen-toggle)
   ( "C-c t a"     .  aggressive-indent-mode)


   ;; NAVIGATION

   ;; General
   ( "C-'"         .  ace-jump-word-mode)
   ( "C-c o"       .  browse-url-at-point)
   ( "C-c n"       .  make-frame)
   ( "C-<next>"    .  forward-page)
   ( "C-<prior>"   .  backward-page)

   ;; Move buffers
   ( "<C-S-up>"    .  buf-move-up)
   ( "<C-S-down>"  .  buf-move-down)
   ( "<C-S-left>"  .  buf-move-left)
   ( "<C-S-right>" .  buf-move-right)


   ;; TEXT MANIPULATION

   ;; General
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
   ( "C-c s s"     .  company-yasnippet)
   ( "C-c s n"     .  yas-new-snippet)
   ( "C-c s e"     .  yas-visit-snippet-file)
   ( "C-c s r"     .  yas-reload-all)
   ( "C-c s t"     .  auto-insert)
   ))

(global-unset-key (kbd "C-z"))
(global-set-key [remap kill-line]   (my/bol-with-prefix kill-line))

(windmove-default-keybindings)



;;;; Modes ;;;;

;; General stuff
(my/auto-modes  '(("\\.inl\\'"    . c++-mode)
                  ("\\.ui$"       . nxml-mode)
                  ("\\.js$"       . js2-mode)
                  ("\\.jshintrc$" . js2-mode)
                  ("\\.jscsrc$"   . json-mode)
                  ("\\.vala$"     . vala-mode)
                  ("\/Cask$"      . emacs-lisp-mode)
                  ))

(my/shorten-major-modes '((markdown-mode   . "Markdown")
                          (js2-mode        . "JS2")
                          (nxml-mode       . "XML")
                          (c-mode          . "C")
                          (c++-mode        . "C++")
                          (cmake-mode      . "CMake")
                          (emacs-lisp-mode . "Elisp")
                          (go-mode         . "Go")
                          (haskell-mode    . "λ")
                          (snippet-mode    . "Yasnippet")
                          ))

(my/shorten-minor-modes '((company-mode             . " C")
                          (abbrev-mode              . " A")
                          (flycheck-mode            . " F")
                          (form-feed-mode           . "")
                          (git-gutter-mode          . "")
                          (magit-gitflow-mode       . " Flow")
                          (yas-minor-mode           . "")
                          (fancy-narrow-mode        . "")
                          (haskell-indentation-mode . "")
                          (aggressive-indent-mode   . " ⇒")
                          (magit-auto-revert-mode   . "")
                          ))

;;;; Specific modes

;; C common
(add-hook 'c-mode-common-hook
          (lambda ()
            (require 'rtags)
            (my/rtags-start)
            (setq-local rtags-completions-enabled t)
            (rtags-enable-standard-keybindings c-mode-base-map)
            (setq-local company-backends '((company-rtags)))

            ;; Work around bug where c-mode-base-map doesn't inherit from
            ;; prog-mode-map
            (unless (keymap-parent c-mode-base-map)
              (set-keymap-parent c-mode-base-map prog-mode-map))
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
                              ("<next>"  . my/company-select-next-nine)
                              ("<prior>" . my/company-select-previous-nine)
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
            (form-feed-mode)
            (my/define-keys emacs-lisp-mode-map
                            '(("C-<tab>" . company-complete)
                              ("<tab>"   . my/indent-snippet-or-complete)))))

;; Flycheck
(add-hook 'flycheck-mode-hook
          (lambda()
            (require 'flycheck-jscs)
            (add-to-list 'flycheck-checkers 'javascript-jscs)))

;; Go
(defvar go-mode-map)
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
            (setq fill-column 72)))

(add-hook 'magit-mode-hook
          (lambda ()
            (require 'magit-gitflow)
            (when (fboundp 'turn-on-magit-gitflow)
              (turn-on-magit-gitflow))
            ))

;; Haskell
(add-hook 'haskell-mode-hook (lambda ()
                               (aggressive-indent-mode -1)
                               (setq-local electric-indent-mode nil)
                               (turn-on-haskell-indentation)
                               ))

;; Ido
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [tab] 'ido-complete)
            ))

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
                            '(("<tab>" . my/indent-snippet-or-complete)))
            ))

;; Info
(add-hook 'Info-mode-hook
          (lambda ()
            (my/define-keys Info-mode-map
                            '(("M-<left>"  . Info-history-back)
                              ("M-<right>" . Info-history-forward)
                              ("M-<up>"    . Info-up)
                              ))))

;; JS2
(defvar js2-mode-map)
(add-hook 'js2-mode-hook
          (lambda ()
            (require 'js2-refactor)
            (define-key js2-mode-map (kbd "C-c f r") 'js2r-rename-var)
            (setq-local company-backends '((company-dabbrev-code
                                            company-files
                                            company-keywords)))))

;; Markdown
(defvar markdown-mode-map)
(add-hook 'markdown-mode-hook
          (lambda ()
            (setq-local fill-column 80)
            (fci-mode)
            (my/define-keys markdown-mode-map
                            '(("C-<return>" . markdown-jump)
                              ))
            ))

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
  (aggressive-indent-mode)
  (emr-initialize)
  (my/define-keys prog-mode-map '(("<tab>"      . my/indent-snippet-or-complete)
                                  ("C-<tab>"    . company-complete)
                                  ("<menu>"     . company-complete)
                                  ("M-<return>" . emr-show-refactor-menu)
                                  ))
  )

(add-hook 'prog-mode-hook 'my/prog-mode)

;; Python
(add-hook 'python-mode-hook (lambda () (aggressive-indent-mode -1)))

;; REST Client
(defvar restclient-mode-map)
(add-hook 'restclient-mode-hook
          (lambda ()
            (company-mode)
            (setq-local company-backends '((company-restclient)))
            (my/define-keys restclient-mode-map
                            '(("<tab>" . my/snippet-or-complete)))
            ))

;; Shell
(add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions 'bash-completion-dynamic-complete)
(defvar term-raw-map)
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
  (pallet-mode t)

  ;; customizations that for various reasons can't be in the customize block.
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq projectile-mode-line
        '(:eval (format " [%s]" (projectile-project-name))))
  (when (and (string= (window-system) "w32"))
    (setq-default projectile-indexing-method 'native))

  ;; Activate a bunch of global modes
  (powerline-major-mode)
  (powerline-default-theme)
  (yas-global-mode)
  (global-git-gutter-mode)
  (projectile-global-mode)
  (ido-mode)
  (ido-vertical-mode)
  (ido-ubiquitous-mode)
  (flx-ido-mode)
  (fancy-narrow-mode)
  (auto-insert-mode)
  (auto-compile-on-save-mode)
  (auto-compile-on-load-mode)

  ;; Some more keys
  (my/define-keys projectile-command-map '(("s p" . projectile-pt))))

(add-hook 'after-init-hook 'my/after-init)



;;;; Advices ;;;;

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

(defadvice custom-save-all (around custom-save-all-around)
  "Use abbreviated quotes for customize."
  (let ((print-quoted t))
    ad-do-it))


(provide 'init)
;;; init.el ends here
