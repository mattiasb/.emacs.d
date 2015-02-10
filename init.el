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

;; Maximize on start
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

   ;; Other
   ( "C-c d"       .  diff-buffer-with-file)

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
   ( "C-c a"       .  align-string)
   ( "C-c ."       .  align-by-current-symbol)
   ( "C-c c"       .  my/toggle-comment)
   ( "C-c C-c"     .  my/toggle-programming-case-word-at-point)
   ( "C-c i"       .  insert-char)
   ( "C-a"         .  mwim-beginning-of-code-or-line)
   ( "C-e"         .  mwim-end-of-code-or-line)

   ;; Replace
   ( "C-c r"       .  vr/replace)
   ( "C-c q"       .  vr/query-replace)

   ;; YAS
   ( "C-<tab>"     .  yas-insert-snippet)
   ( "C-c s c"     .  yas-new-snippet)
   ( "C-c s e"     .  yas-visit-snippet-file)
   ( "C-c s r"     .  yas-reload-all)
   ( "C-c s t"     .  auto-insert)))

(global-unset-key (kbd "C-z"))
(global-set-key [remap kill-line]   (my/bol-with-prefix kill-line))
(global-set-key [remap occur]       'my/occur-dwim)

(windmove-default-keybindings)


;;;; Modes – General;;;;

(my/auto-modes  '(("\\.inl\\'"    . c++-mode)
                  ("\\.ui$"       . nxml-mode)
                  ("\\.js$"       . js2-mode)
                  ("\\.jshintrc$" . js2-mode)
                  ("\\.jscsrc$"   . json-mode)
                  ("\\.vala$"     . vala-mode)
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
                          (flycheck-mode            . " F")
                          (form-feed-mode           . "")
                          (git-gutter-mode          . "")
                          (magit-gitflow-mode       . " Flow")
                          (yas-minor-mode           . "")
                          (fancy-narrow-mode        . "")
                          (haskell-indentation-mode . "")
                          (aggressive-indent-mode   . " ⇒")
                          (magit-auto-revert-mode   . "")))



;;;; Modes – Specific ;;;;

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
(defvar rtags-completions-enabled)
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
                              ("."          . my/dot-and-complete)))))

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
                              ("\C-g"    . company-abort)))))

(add-hook 'company-completion-started-hook   'my/fci-turn-off)
(add-hook 'company-completion-finished-hook  'my/fci-turn-on)
(add-hook 'company-completion-cancelled-hook 'my/fci-turn-on)

;; Dired
(defvar dired-mode-map)
(add-hook 'dired-mode-hook
          (lambda ()
            (my/define-keys dired-mode-map
                            '(("W" . wdired-change-to-wdired-mode)
                              ("F" . find-name-dired)))))

;; ELisp
(add-hook 'emacs-lisp-mode-hook #'form-feed-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (my/define-keys emacs-lisp-mode-map
                            '(("/" . my/slash-and-complete)
                              ("-" . my/dash-and-complete)))))

;; Flycheck
(add-hook 'flycheck-mode-hook
          (lambda()
            ;; Re-add this when it works correctly
            ;; (require 'flycheck-jscs)
            ;; (add-to-list 'flycheck-checkers 'javascript-jscs)

            (flycheck-cask-setup)
            (flycheck-package-setup)))



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
                              ("C-<return>" . godef-jump)
                              ("."          . my/dot-and-complete)))))

;; Magit
(add-hook 'git-commit-mode-hook
          (lambda ()
            (fci-mode 1)
            (setq fill-column 72)))

(add-hook 'magit-mode-hook
          (lambda ()
            (require 'magit-gitflow)
            (when (fboundp 'turn-on-magit-gitflow)
              (turn-on-magit-gitflow))))

;; Haskell
(add-hook 'haskell-mode-hook (lambda ()
                               (aggressive-indent-mode -1)
                               (setq-local electric-indent-mode nil)
                               (turn-on-haskell-indentation)))

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
(add-hook 'js2-mode-hook
          (lambda ()
            (require 'js2-refactor)
            (define-key js2-mode-map (kbd "C-c f r") 'js2r-rename-var)
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
          (lambda ()
            (my/define-keys magit-status-mode-map
                            '(("q" . my/magit-mode-quit)))))

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

;; nXML
(add-hook 'nxml-mode-hook (lambda ()
                            (setq-local company-backends '(company-nxml))
                            (aggressive-indent-mode -1)))
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

  (my/define-keys prog-mode-map
                  '(("<tab>"      . my/indent-snippet-or-complete)
                    ("M-<return>" . emr-show-refactor-menu))))

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
                            '(("<tab>" . my/snippet-or-complete)))))

;; Shell
(add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions 'bash-completion-dynamic-complete)
(defvar term-raw-map)
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map   (kbd "<tab>")
                              (lookup-key term-raw-map (kbd "C-M-i")))
                            (define-key term-raw-map   (kbd "M-x") 'smex)))

;; Vala
(add-hook 'vala-mode-hook 'my/prog-mode)
(add-hook 'vala-mode-hook
          (lambda ()
            (require 'flycheck-vala)
            (add-to-list 'flycheck-checkers 'vala-valac)))


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
                              projectile-test-cmd-map)))))))
(dir-locals-set-directory-class "~/Code/gnome/" 'gnome-code)



;;;; Post-init code ;;;;

(defun my/activate-projectile ()
  "Activate projectile."
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " [%s]" (projectile-project-name))))
  (when (and (string= (window-system) "w32"))
    (setq-default projectile-indexing-method 'native))

  (my/define-keys projectile-command-map
                  '(("s p" . projectile-pt))))

(defun my/activate-visual-regexp ()
  "Activate visual-regexp."
  (require 'visual-regexp-steroids)
  (my/define-keys esc-map
                  '(("C-r" . vr/isearch-backward)
                    ("C-s" . vr/isearch-forward))))

(defun my/activate-yas ()
  "Activate YASnippet."
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode))

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
  (auto-insert-mode)
  (auto-compile-on-save-mode)
  (auto-compile-on-load-mode)
  (browse-kill-ring-default-keybindings)

  (my/activate-projectile)
  (my/activate-visual-regexp)
  (my/activate-yas))

(add-hook 'after-init-hook (lambda ()
                             (require 'cask)
                             (my/activate-modes)))



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

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible."
  (my/fci-turn-off))
(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed."
  (when (null popup-instances)
    (my/fci-turn-on)))



(provide 'init)
;;; init.el ends here
