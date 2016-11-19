;;; custom.el --- My custom settings

;; Copyright ⓒ 2013-2016 Mattias Bengtsson

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20141108
;; Keywords         : init
;; Package-Requires : ((emacs "25.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 25.x

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

;;; Commentary:

;;; Note:

;;; Code:

;;;; Custom mode ;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-insert 'other)
 '(auto-insert-alist
   '((("\\.el\\'" . "Emacs Lisp")
      .
      ["default.el" my/autoinsert-yas-expand])
     (("\\.lua\\'" . "Lua")
      .
      ["default.lua" my/autoinsert-yas-expand])))
 '(auto-insert-directory "~/.emacs.d/templates/")
 '(blink-cursor-blinks -1)
 '(browse-kill-ring-current-entry-face 'bold)
 '(browse-kill-ring-display-duplicates nil)
 '(browse-kill-ring-highlight-current-entry t)
 '(browse-kill-ring-highlight-inserted-item nil)
 '(browse-kill-ring-no-duplicates t)
 '(browse-kill-ring-quit-action 'kill-and-delete-window)
 '(browse-kill-ring-resize-window t)
 '(browse-kill-ring-separator
   "——————————————————————————————————————————————————————————")
 '(browse-kill-ring-show-preview nil)
 '(browse-url-browser-function 'browse-url-default-browser)
 '(column-number-mode t)
 '(company-auto-complete t)
 '(company-auto-complete-chars '(32 40 41 46))
 '(company-backends
   '(company-elisp company-nxml company-css company-cmake company-capf
                   (company-dabbrev-code company-keywords)
                   company-files company-dabbrev))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case t)
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 2)
 '(company-quickhelp-delay 1.2)
 '(company-quickhelp-mode t)
 '(company-selection-wrap-around nil)
 '(company-show-numbers nil)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-minimum-width 40)
 '(compilation-read-command nil)
 '(compilation-scroll-output t)
 '(custom-buffer-done-kill t)
 '(custom-enabled-themes '(wombat))
 '(custom-safe-themes t)
 '(delete-selection-mode t)
 '(dired-listing-switches "-laGh1v --group-directories-first")
 '(easy-repeat-command-list
   '(shift-number-up shift-number-down md/duplicate-up md/duplicate-down flycheck-next-error flycheck-previous-error my/toggle-programming-case-word-at-point-reverse my/toggle-programming-case-word-at-point other-window next-buffer scroll-other-window recenter-top-bottom kill-buffer backward-page forward-page previous-error next-error scroll-up-command scroll-down-command beginning-of-defun end-of-defun org-previous-visible-heading org-next-visible-heading org-forward-heading-same-level org-backward-heading-same-level outline-up-heading outline-next-visible-heading outline-previous-visible-heading outline-forward-same-level outline-backward-same-level git-gutter:previous-hunk git-gutter:next-hunk paredit-forward paredit-backward paredit-backward-up))
 '(edconf-exec-path "")
 '(edconf-get-properties-function 'editorconfig-core-get-properties-hash)
 '(electric-indent-mode t)
 '(electric-layout-mode t)
 '(electric-pair-mode t)
 '(ensime-completion-style nil)
 '(expand-region-contract-fast-key "S-SPC")
 '(expand-region-preferred-python-mode 'elpy-mode)
 '(fci-rule-color "gray21")
 '(flycheck-completion-system 'ido)
 '(flycheck-cython-executable "cython3")
 '(flycheck-highlighting-mode 'symbols)
 '(flycheck-keymap-prefix "e")
 '(gc-cons-threshold 20000000)
 '(git-commit-summary-max-length 40)
 '(global-company-mode nil)
 '(global-control-mode-exceptions
   '(special-mode dired-mode term-mode magit-popup-mode woman-mode customize-mode compilation-mode))
 '(global-git-gutter-mode t)
 '(haskell-font-lock-symbols 'unicode)
 '(haskell-mode-hook '(turn-on-haskell-indentation) t)
 '(highlight-symbol-idle-delay 1.0)
 '(ido-completion-buffer nil)
 '(ido-cr+-max-items 50000)
 '(ido-ubiquitous-max-items 50000)
 '(ido-vertical t)
 '(ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
 '(imenu-after-jump-hook '(recenter))
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 750000)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message nil)
 '(js2-allow-keywords-as-property-names t)
 '(js2-auto-indent-p t)
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p nil)
 '(js2-cleanup-whitespace t)
 '(js2-concat-multiline-strings t)
 '(js2-enter-indents-newline t)
 '(js2-global-externs
   '("imports" "require" "module" "exports" "console" "process" "setTimeout" "clearTimeout" "log"))
 '(js2-imenu-other-functions-ns "?")
 '(js2-imenu-show-other-functions t)
 '(js2-include-browser-externs t)
 '(js2-indent-on-enter-key t)
 '(js2-mirror-mode t)
 '(js2-mode-indent-ignore-first-tab t)
 '(js2-skip-preprocessor-directives t)
 '(keyfreq-autosave-mode t)
 '(keyfreq-buffer "*KeyFreq*")
 '(keyfreq-mode t)
 '(large-file-warning-threshold 100000000)
 '(lua-indent-level 2)
 '(lua-prefix-key "C-z")
 '(magit-auto-revert-mode-lighter "")
 '(magit-completing-read-function 'magit-ido-completing-read)
 '(magit-no-confirm '(stage-all-changes unstage-all-changes))
 '(magit-push-always-verify nil)
 '(magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
 '(magit-set-upstream-on-push t)
 '(magit-stage-all-confirm nil)
 '(magit-unstage-all-confirm nil)
 '(markdown-asymmetric-header nil)
 '(markdown-command
   "pandoc -s -f markdown_github -c file:///home/mattiasb/.emacs.d/github-pandoc.css")
 '(markdown-open-command "/usr/bin/firefox")
 '(markdown-reference-location 'end)
 '(markdown-unordered-list-item-prefix "- ")
 '(menu-bar-mode nil)
 '(nxml-auto-insert-xml-declaration-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(package-archive-priorities '(("melpa" . 10) ("gnu" . 5) ("melpa-stable" . 0)))
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")))
 '(package-pinned-packages '((ensime . "melpa-stable")))
 '(package-selected-packages
   '(toml-mode yatemplate company-anaconda anaconda-mode realgud ensime flyspell-correct flyspell-correct-popup mtg-deck-mode gobgen ripgrep fancy-narrow flimenu tmux-keys sh-extra-font-lock flycheck-vala align-string align-by-current-symbol evil-esc-mode ace-jump-mode ag aggressive-indent ascii-art-to-unicode auto-compile browse-kill-ring buffer-move cmake-mode company-go company-quickhelp company-restclient company-shell control-mode cycle-quotes cython-mode diminish easy-repeat editorconfig emr fill-column-indicator flx-ido flycheck-cask flycheck-cython flycheck-package flycheck-pos-tip flycheck-rust flycheck-status-emoji git-gutter gitconfig-mode gitignore-mode go-eldoc god-mode haskell-mode highlight-numbers html5-schema ibuffer-projectile ido-ubiquitous ido-vertical-mode js2-refactor json-mode lisp-extra-font-lock lua-mode magit-filenotify magit-gitflow markdown-mode move-dup mwim niceify-info php-mode powerline racer restart-emacs rtags sass-mode shift-number smart-region smex sqlup-mode vala-mode vala-snippets visual-regexp-steroids wgrep-ag which-key ws-butler xref-js2 yaml-mode zygospore))
 '(powerline-default-separator 'bar)
 '(projectile-globally-ignored-file-suffixes '("~" "#"))
 '(projectile-keymap-prefix (kbd "C-z p"))
 '(projectile-switch-project-action 'projectile-commander)
 '(racer-cmd "/home/mattiasb/.local/bin/racer")
 '(racer-rust-src-path "/home/mattiasb/Code/rust-lang/rust/src/")
 '(ring-bell-function 'ignore)
 '(rng-schema-locating-files
   '("~/.emacs.d/schemas/schemas.xml" "/usr/share/emacs/24.3/etc/schema/schemas.xml" "schemas.xml"))
 '(rtags-autostart-diagnostics t)
 '(rtags-completions-enabled t)
 '(rust-indent-method-chain t)
 '(safe-local-variable-values
   '((c-mode . c++)
     (mtg-deck-format . vintage)
     (mtg-deck-format . standard)
     (mtg-deck-format . legacy)
     (mtg-deck-format . modern)
     (projectile-project-type . jhbuild)))
 '(save-abbrevs 'silently)
 '(scroll-bar-mode nil)
 '(scroll-margin 5)
 '(scroll-step 1)
 '(show-paren-mode t)
 '(tab-width 8)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(use-file-dialog nil)
 '(user-mail-address "mattias.jc.bengtsson@gmail.com")
 '(vc-handled-backends nil)
 '(vr/auto-show-help nil)
 '(vr/match-separator-string " → ")
 '(vr/match-separator-use-custom-face t)
 '(wdired-allow-to-change-permissions 'advanced)
 '(wdired-allow-to-redirect-links nil)
 '(wgrep-auto-save-buffer t)
 '(which-key-add-column-padding 1)
 '(which-key-idle-delay 0.7)
 '(which-key-mode t)
 '(which-key-popup-type 'side-window)
 '(which-key-show-prefix nil)
 '(which-key-side-window-location 'bottom)
 '(which-key-sort t)
 '(which-key-special-keys nil)
 '(woman-fill-column 80)
 '(woman-fill-frame t)
 '(yaml-indent-offset 2)
 '(yas-also-auto-indent-first-line t)
 '(yas-expand-only-for-last-commands nil)
 '(yas-prompt-functions '(yas-popup-isearch-prompt))
 '(yas-trigger-key nil)
 '(yas-trigger-symbol " ⇒")
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t))

;; Real ugly, but apparently can't be set through customize
(defvar magit-last-seen-setup-instructions)
(setq magit-last-seen-setup-instructions "1.4.0")

(custom-theme-set-faces
 'wombat
 '(company-scrollbar-bg ((t (:background "#303030"))))
 '(company-scrollbar-fg ((t (:background "#4a4a4a"))))
 '(company-template-field ((t (:background "#4a4a4a"))))
 '(company-tooltip ((t (:inherit default :background "#3d3d3d"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip))))
 '(company-tooltip-common ((t (:inherit (font-lock-constant-face company-tooltip)))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground "red"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face :background "#353535"))))
 '(highlight-symbol-face ((t (:background "gray21"))))
 '(hl-line ((t (:background "gray21"))))
 '(popup-face ((t (:inherit default :background "#3d3d3d"))))
 '(popup-isearch-match ((t (:inherit (font-lock-constant-face popup-face)))))
 '(popup-menu-mouse-face ((t nil)))
 '(popup-menu-selection-face ((t (:inherit font-lock-function-name-face :background "#353535"))))
 '(popup-scroll-bar-background-face ((t (:background "#303030"))))
 '(popup-scroll-bar-foreground-face ((t (:background "#4a4a4a"))))
 '(popup-tip-face ((t (:background "#4a4a4a"))))
 '(powerline-active2 ((t (:inherit mode-line :background "gray30"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "grey18"))))
 '(vr/match-0 ((t (:background "gray21" :foreground "AntiqueWhite4"))))
 '(vr/match-1 ((t (:background "gray21" :foreground "AntiqueWhite4"))))
 )


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "tomato3"))))
 '(flycheck-error ((t (:underline (:color "tomato3" :style wave)))))
 '(flycheck-error-list-info ((t (:foreground "olive drab"))))
 '(flycheck-error-list-warning ((t (:foreground "goldenrod"))))
 '(flycheck-fringe-error ((t (:foreground "tomato3"))))
 '(flycheck-fringe-info ((t (:foreground "olive drab"))))
 '(flycheck-fringe-warning ((t (:foreground "goldenrod"))))
 '(git-gutter:added ((t (:foreground "olive drab" :weight bold))))
 '(git-gutter:deleted ((t (:foreground "tomato3" :weight bold))))
 '(git-gutter:modified ((t (:foreground "goldenrod" :weight bold))))
 '(iedit-occurrence ((t (:inherit region))))
 '(js2-highlight-vars-face ((t (:background "gray24"))))
 '(lisp-extra-font-lock-quoted ((t (:inherit shadow))))
 '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :weight bold))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.1))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face)))))

(provide 'custom)
;;; custom.el ends here
