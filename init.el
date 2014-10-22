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
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'funcs)
(require 'package++)

;;;; Keybindings ;;;;

(global-set-keys
 '(
   ;; Windows
   ( "<f9>"        .  customize)
   ( "<f12>"       .  list-packages)
   ( "M-x"         .  smex)
   ( "C-c e"       .  ansi-term)

   ;; Magit
   ( "C-c m s"     .  magit-status)
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
   ( "C-x 1"       .  zygospore-toggle-delete-other-windows)
   ;; Move buffers
   ( "<C-S-up>"    .  buf-move-up)
   ( "<C-S-down>"  .  buf-move-down)
   ( "<C-S-left>"  .  buf-move-left)
   ( "<C-S-right>" .  buf-move-right)


   ;; TEXT MANIPULATION

   ;; General
   ( "<tab>"       .  tab-indent-or-complete)
   ( "M-<up>"      .  move-text-up)
   ( "M-<down>"    .  move-text-down)
   ( "C-c a"       .  align-regexp)
   ( "C-c c"       .  comment-or-uncomment-region-or-line)
   ( "C-c d"       .  insert-date) ;; TODO: Replace with snippet
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
(global-set-key [remap kill-line]   (bol-with-prefix kill-line))

(windmove-default-keybindings)

;;;; Modes ;;;;

;; Abbrev
(add-hook 'abbrev-mode-hook (lambda() (diminish 'abbrev-mode "A")))

;; C common
(add-hook 'c-mode-common-hook
          (lambda ()
            (require 'rtags)
            (rtags-start-process)
            (setq-local rtags-completions-enabled t)
            (define-keys c-mode-base-map
              '(("C-<return>" . rtags-find-symbol-at-point)
                ("M-<left>"   . rtags-location-stack-back)
                ("M-<right>"  . rtags-location-stack-forward)
                ("C-c f r"    . rtags-rename-symbol)
                ("C-c o"      . ff-find-other-file)))
            ))

;; C
(add-hook 'c-mode-hook (lambda () (setq-local mode-name "C") ))

;; C++
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'"  . c++-mode))
(add-hook 'c++-mode-hook (lambda () (setq-local mode-name "C++")))

;; CMake
(add-hook 'cmake-mode-hook 'my-prog-mode)
(add-hook 'cmake-mode-hook
          (lambda ()
            (setq-local mode-name "Cm")
            (setq-local company-backends
                        '((company-cmake
                           company-files
                           company-dabbrev-code)))))

;; Company
(add-hook 'company-mode-hook
          (lambda ()
            (diminish 'company-mode "Co")
            (define-keys company-active-map
              '(("\C-n"    . company-select-next)
                ("\C-p"    . company-select-previous)
                ("<next>"  . company-select-next-five)
                ("<prior>" . company-select-previous-five)
                ("\C-p"    . company-select-previouss)
                ("\C-d"    . company-show-doc-buffer)
                ("\C-v"    . company-show-location)
                ("\C-g"    . (lambda ()
                               (interactive)
                               (company-abort)))))))

;; ELisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local mode-name "El")
            (define-keys emacs-lisp-mode-map
              '(("C-<tab>" . company-complete)
                ("<tab>"   . tab-indent-or-complete)))))

;; Flycheck
(add-hook 'flycheck-mode-hook (lambda () (diminish 'flycheck-mode "Fc")))

;; GitGutter
(add-hook 'git-gutter-mode-on-hook (lambda () (diminish 'git-gutter-mode "GG")))

;; Go
(add-hook 'go-mode-hook
          (lambda ()
            (go-eldoc-setup)
            (setq-local mode-name "go")

            (setq-local tab-width 4)
            (setq-local company-backends '(company-go))

            (define-keys go-mode-map '(("C-c i a"    . go-import-add)
                                       ("C-c i r"    . go-remove-unused-imports)
                                       ("C-c i g"    . go-goto-imports)
                                       ("C-c d"      . godoc-at-point)
                                       ("C-<return>" . godef-jump)))))

;; Haskell
(add-hook 'haskell-mode-hook (lambda ()
                               (setq-local mode-name "Hs")
                               (setq-local electric-indent-mode nil)))

;; Ido
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [tab] 'ido-complete)
            ))

;; JS2
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (setq-local mode-name "JS2")
            (require 'js2-refactor)
            (define-key js2-mode-map (kbd "C-c f r") 'js2r-rename-var)
            (setq-local company-backends '((company-dabbrev-code
                                            company-files
                                            company-keywords)))))

;; Markdown
(add-hook 'markdown-mode-hook (lambda () (setq-local mode-name "Md")))

;; nXML
(add-to-list 'auto-mode-alist '("\\.ui$" . nxml-mode))
(add-hook 'nxml-mode-hook (lambda ()
                            (setq-local mode-name "X")
                            (setq-local company-backends '(company-nxml))
                            (aggressive-indent-mode -1)
                            ))
(add-hook 'nxml-mode-hook 'my-prog-mode)

;; Package
(add-hook 'package-menu-mode-hook 'hl-line-mode)

;; Prog
(defun my-prog-mode ()
  "My `prog-mode' hook."

  (setq-local fill-column      80)
  (setq-local indent-tabs-mode nil)

  (company-mode)
  (flycheck-mode)
  (fci-mode)
  (highlight-numbers-mode)
  (aggressive-indent-mode))

(add-hook 'prog-mode-hook 'my-prog-mode)

;; Shell
(add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions 'bash-completion-dynamic-complete)
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map   (kbd "<tab>")
                              (lookup-key term-raw-map (kbd "C-M-i")))
                            (define-key term-raw-map   (kbd "M-x") 'smex)))

;; Snippet
(add-hook 'snippet-mode-hook (lambda () (setq-local mode-name "S")))

;; YAS
(add-hook 'yas-minor-mode-hook (lambda () (diminish 'yas-minor-mode " Y")))

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

(defun my-after-init-hook ()
  "My after init hook."
  (package-sync)

  (require 'uniquify)

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

(add-hook 'after-init-hook 'my-after-init-hook)

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

;;;; Custom mode ;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-auto-complete t)
 '(company-auto-complete-chars (quote (32 46)))
 '(company-backends (quote (company-elisp company-nxml company-css company-eclim company-semantic company-rtags company-clang company-xcode company-ropemacs company-cmake (company-gtags company-etags company-dabbrev-code company-keywords) company-oddmuse company-files company-dabbrev company-yasnippet)))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case t)
 '(company-idle-delay 0.5)
 '(company-tooltip-align-annotations t)
 '(compilation-scroll-output t)
 '(cppcm-build-dirname "build")
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(delete-selection-mode t)
 '(electric-indent-mode t)
 '(electric-layout-mode t)
 '(electric-pair-mode t)
 '(fci-rule-color "gray21")
 '(flycheck-completion-system (quote ido))
 '(flycheck-highlighting-mode (quote symbols))
 '(git-commit-summary-max-length 40)
 '(global-company-mode t)
 '(global-git-gutter-mode t)
 '(haskell-font-lock-symbols (quote unicode))
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)) t)
 '(highlight-symbol-idle-delay 1.0)
 '(ido-completion-buffer nil)
 '(ido-vertical t)
 '(ido-vertical-define-keys (quote C-n-C-p-up-down-left-right))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js2-allow-keywords-as-property-names t)
 '(js2-auto-indent-p t)
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p nil)
 '(js2-cleanup-whitespace t)
 '(js2-concat-multiline-strings t)
 '(js2-enter-indents-newline t)
 '(js2-global-externs (quote ("imports" "require" "module" "exports" "console" "process" "setTimeout" "clearTimeout" "log")))
 '(js2-include-browser-externs t)
 '(js2-indent-on-enter-key t)
 '(js2-mirror-mode t)
 '(js2-mode-indent-ignore-first-tab t)
 '(js2-skip-preprocessor-directives t)
 '(menu-bar-mode nil)
 '(nxml-auto-insert-xml-declaration-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(package-manifest (quote ("zygospore" "aggressive-indent" "highlight-numbers" "vala-mode" "fill-column-indicator" "rtags" "sass-mode" "dummy-h-mode" "bash-completion" "git-commit-training-wheels-mode" "fullscreen-mode" "ace-jump-mode" "gitignore-mode" "company-go" "go-eldoc" "go-mode" "highlight-symbol" "flycheck" "git-gutter" "cpputils-cmake" "cmake-mode" "buffer-move" "js2-refactor" "lua-mode" "fancy-narrow" "ack-and-a-half" "diminish" "gitconfig-mode" "ido-ubiquitous" "epl" "projectile" "flx-ido" "smex" "expand-region" "haskell-mode" "js2-mode" "json-mode" "magit" "markdown-mode" "editorconfig" "yasnippet" "move-text" "company" "popup" "ido-vertical-mode")))
 '(projectile-keymap-prefix (kbd "C-p"))
 '(rng-schema-locating-files (quote ("/home/mattiasb/.emacs.d/schemas.xml" "/usr/share/emacs/24.3/etc/schema/schemas.xml" "schemas.xml")))
 '(rtags-autostart-diagnostics t)
 '(rtags-completions-enabled t)
 '(safe-local-variable-values (quote ((eval progn (puthash (projectile-project-root) "jhbuild make" projectile-compilation-cmd-map) (puthash (projectile-project-root) "make check" projectile-test-cmd-map)))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-width 8)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(use-file-dialog nil)
 '(user-mail-address "mattias.jc.bengtsson@gmail.com")
 '(yas-also-auto-indent-first-line t)
 '(yas-expand-only-for-last-commands nil)
 '(yas-prompt-functions (quote (yas-popup-isearch-prompt)))
 '(yas-trigger-key nil)
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:underline (:color "tomato3" :style wave)))))
 '(flycheck-error-list-info ((t (:foreground "olive drab"))))
 '(flycheck-error-list-warning ((t (:foreground "goldenrod"))))
 '(flycheck-fringe-error ((t (:foreground "tomato3"))))
 '(flycheck-fringe-info ((t (:foreground "olive drab"))))
 '(flycheck-fringe-warning ((t (:foreground "goldenrod"))))
 '(git-gutter:added ((t (:foreground "olive drab" :weight bold))))
 '(git-gutter:deleted ((t (:foreground "tomato3" :weight bold))))
 '(git-gutter:modified ((t (:foreground "goldenrod" :weight bold))))
 '(highlight-symbol-face ((t (:background "gray21"))))
 '(hl-line ((t (:background "gray21")))))

(provide 'init)
;;; init.el ends here
