;;; custom.el --- My custom settings

;; Copyright ⓒ 2013-2020 Mattias Bengtsson
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

;; Version          : 20141108
;; Keywords         : local
;; Package-Requires : ((emacs "27.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 27.x

;;; Commentary:

;; My custom settings.

;;; Note:

;;; Code:

;;;; Custom mode ;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aggressive-indent-dont-electric-modes t)
 '(aggressive-indent-excluded-modes
   '(inf-ruby-mode makefile-mode makefile-gmake-mode python-mode text-mode yaml-mode haskell-mode dockerfile-mode))
 '(auto-insert 'other)
 '(auto-insert-directory (concat user-emacs-directory "templates/"))
 '(auto-save-no-message t)
 '(backup-directory-alist '(("." . "~/.cache/emacs-backup/")))
 '(blink-cursor-blinks -1)
 '(browse-kill-ring-current-entry-face 'bold)
 '(browse-kill-ring-display-duplicates nil)
 '(browse-kill-ring-highlight-current-entry t)
 '(browse-kill-ring-highlight-inserted-item nil)
 '(browse-kill-ring-no-duplicates t)
 '(browse-kill-ring-quit-action 'save-and-restore)
 '(browse-kill-ring-resize-window t)
 '(browse-kill-ring-separator
   "——————————————————————————————————————————————————————————")
 '(browse-kill-ring-show-preview nil)
 '(browse-url-browser-function 'browse-url-default-browser)
 '(column-number-mode t)
 '(comment-auto-fill-only-comments t)
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
 '(custom-file (concat user-emacs-directory "custom.el"))
 '(custom-safe-themes t)
 '(delete-selection-mode t)
 '(dired-listing-switches "-laGh1v --group-directories-first")
 '(dired-sidebar-width 20)
 '(display-fill-column-indicator-character 9474)
 '(easy-repeat-command-list
   '(table-narrow-cell table-widen-cell table-shorten-cell table-heighten-cell string-inflection-all-cycle shift-number-up shift-number-down md/duplicate-up md/duplicate-down flycheck-next-error flycheck-previous-error mb-cmd-toggle-programming-case-word-at-point-reverse mb-cmd-toggle-programming-case-word-at-point other-window next-buffer scroll-other-window recenter-top-bottom kill-buffer backward-page forward-page previous-error next-error scroll-up-command scroll-down-command beginning-of-defun end-of-defun org-previous-visible-heading org-next-visible-heading org-forward-heading-same-level org-backward-heading-same-level outline-up-heading outline-next-visible-heading outline-previous-visible-heading outline-forward-same-level outline-backward-same-level git-gutter:previous-hunk git-gutter:next-hunk paredit-forward paredit-backward paredit-backward-up))
 '(edconf-exec-path "")
 '(edconf-get-properties-function 'editorconfig-core-get-properties-hash)
 '(electric-indent-mode nil)
 '(electric-layout-mode t)
 '(electric-operator-c-pointer-type-style 'type)
 '(electric-pair-mode t)
 '(electric-pair-pairs
   '((34 . 34)
     (8216 . 8217)
     (8220 . 8221)
     (123 . 125)
     (91 . 93)
     (96 . 96)))
 '(elmo-folder-update-threshold 1001)
 '(elmo-imap4-default-authenticate-type 'clear)
 '(elmo-imap4-default-port 993)
 '(elmo-imap4-default-server "imap.gmail.com")
 '(elmo-imap4-default-stream-type 'ssl)
 '(elmo-imap4-default-user "mattias.jc.bengtsson@gmail.com")
 '(elmo-lang "en")
 '(elmo-message-fetch-threshold nil)
 '(emojify-download-emojis-p t)
 '(emojify-emoji-styles '(unicode))
 '(expand-region-contract-fast-key "S-SPC")
 '(fill-function-arguments-first-argument-same-line t)
 '(fill-function-arguments-last-argument-same-line t)
 '(fill-function-arguments-second-argument-same-line nil)
 '(fill-function-arguments-trailing-separator t)
 '(flycheck-completion-system 'ido)
 '(flycheck-cython-executable "cython3")
 '(flycheck-display-errors-function 'ignore)
 '(flycheck-emacs-lisp-load-path 'inherit)
 '(flycheck-highlighting-mode 'symbols)
 '(flycheck-keymap-prefix "e")
 '(flycheck-mypy\.ini "setup.cfg")
 '(flycheck-pos-tip-max-width 60)
 '(flycheck-python-flake8-executable "flake8")
 '(flycheck-python-mypy-config "setup.cfg")
 '(flycheck-ruby-rubocop-executable "bundle exec rubocop")
 '(flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face) t)
 '(forge-alist
   '(("github.com" "api.github.com" "github.com" forge-github-repository)
     ("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository)
     ("salsa.debian.org" "salsa.debian.org/api/v4" "salsa.debian.org" forge-gitlab-repository)
     ("git.smarteye.se" "http://git.smarteye.se/api/v4" "git.smarteye.se" forge-gitlab-repository)
     ("gitlab.gnome.org" "gitlab.gnome.org/api/v4" "gitlab.gnome.org" forge-gitlab-repository)
     ("teahub.io" "teahub.io/api/v1" "teahub.io" forge-gitea-repository)
     ("code.orgmode.org" "code.orgmode.org/api/v1" "code.orgmode.org" forge-gogs-repository)
     ("bitbucket.org" "api.bitbucket.org/2.0" "bitbucket.org" forge-bitbucket-repository)
     ("git.savannah.gnu.org" nil "git.savannah.gnu.org" forge-cgit*-repository)
     ("git.kernel.org" nil "git.kernel.org" forge-cgit-repository)
     ("repo.or.cz" nil "repo.or.cz" forge-repoorcz-repository)
     ("git.suckless.org" nil "git.suckless.org" forge-stagit-repository)
     ("git.sr.ht" nil "git.sr.ht" forge-srht-repository)))
 '(fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist) t)
 '(gc-cons-threshold 20000000)
 '(git-commit-fill-column 72)
 '(git-commit-setup-hook
   '(git-commit-save-message git-commit-setup-changelog-support git-commit-turn-on-auto-fill git-commit-propertize-diff with-editor-usage-message))
 '(git-commit-summary-max-length 50)
 '(git-link-commit-remote-alist
   '(("git.sr.ht" git-link-commit-github)
     ("github" git-link-commit-github)
     ("bitbucket" git-link-commit-bitbucket)
     ("gitorious" git-link-commit-gitorious)
     ("gitlab" git-link-commit-github)
     ("visualstudio\\|azure" git-link-commit-azure)
     ("sourcegraph" git-link-commit-sourcegraph)
     ("git.smarteye.se" git-link-commit-github)))
 '(git-link-remote-alist
   '(("git.sr.ht" git-link-github)
     ("github" git-link-github)
     ("bitbucket" git-link-bitbucket)
     ("gitorious" git-link-gitorious)
     ("gitlab" git-link-gitlab)
     ("visualstudio\\|azure" git-link-azure)
     ("sourcegraph" git-link-sourcegraph)
     ("git.smarteye.se" git-link-gitlab)))
 '(git-link-use-single-line-number nil)
 '(global-company-mode nil)
 '(global-control-mode-exceptions
   '(ansible-doc-module-mode mime-view-mode todotxt-mode daemons-mode xref--xref-buffer-mode ibuffer-mode package-menu-mode ag-mode tabulated-list-mode Info-mode help-mode special-mode view-mode dired-mode term-mode magit-status-mode magit-refs-mode magit-process-mode magit-cherry-mode magit-log-mode magit-diff-mode woman-mode customize-mode compilation-mode vterm-mode))
 '(global-git-gutter-mode t)
 '(guess-language-langcodes '((en "en_US" "English") (sv "sv_SE" "Swedish")))
 '(guess-language-languages '(en sv))
 '(haskell-font-lock-symbols 'unicode)
 '(haskell-mode-hook '(turn-on-haskell-indentation) t)
 '(highlight-symbol-idle-delay 1.0)
 '(history-advised-before-functions
   '(imenu isearch-mode beginning-of-buffer end-of-buffer xref-find-definitions xref-find-references xref-find-definitions-other-window xref-find-definitions-other-frame mb-cmd-markdown-jump anaconda-mode-find-definitions anaconda-mode-find-assignments))
 '(ido-completion-buffer nil)
 '(ido-cr+-max-items 50000)
 '(ido-ubiquitous-command-overrides
   '((enable prefix "xref-")
     (disable exact "execute-extended-command")
     (enable prefix "wl-")
     (enable-old prefix "Info-")
     (enable exact "webjump")
     (enable regexp "\\`\\(find\\|load\\|locate\\)-library\\'")
     (disable prefix "tmm-")
     (enable regexp "\\`\\(load\\|enable\\|disable\\|describe\\|custom-theme-visit\\)-theme\\'")
     (enable-old prefix "bbdb-")
     (enable-old exact "where-is")
     (disable exact "todo-add-category")
     (enable exact "find-tag")
     (enable prefix "etags-select-")))
 '(ido-ubiquitous-max-items 50000)
 '(ido-vertical t)
 '(ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
 '(imenu-after-jump-hook '(recenter))
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 750000)
 '(importmagic-python-interpreter "python3")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message nil)
 '(isearch-lazy-count t)
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
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(js2-skip-preprocessor-directives t)
 '(keyfreq-autosave-mode t)
 '(keyfreq-buffer "*KeyFreq*")
 '(keyfreq-mode t)
 '(large-file-warning-threshold 100000000)
 '(lastpass-user "mattias.jc.bengtsson@gmail.com")
 '(load-prefer-newer t)
 '(lsp-ui-sideline-enable nil)
 '(lua-indent-level 2)
 '(lua-prefix-key "C-z")
 '(magit-auto-revert-mode-lighter "")
 '(magit-completing-read-function 'magit-ido-completing-read)
 '(magit-log-arguments '("-n1000" "--graph" "--decorate"))
 '(magit-no-confirm '(stage-all-changes unstage-all-changes))
 '(magit-push-always-verify nil)
 '(magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
 '(magit-set-upstream-on-push t)
 '(magit-stage-all-confirm nil)
 '(magit-tag-arguments '("--annotate"))
 '(magit-unstage-all-confirm nil)
 '(magithub-api-timeout 3)
 '(markdown-asymmetric-header nil)
 '(markdown-command
   "pandoc --metadata pagetitle=*markdown* -s -f gfm -c file:///home/mattiasb/.config/emacs/github-pandoc.css")
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-open-command "/usr/bin/firefox")
 '(markdown-reference-location 'end)
 '(markdown-unordered-list-item-prefix "- ")
 '(markdown-use-pandoc-style-yaml-metadata t)
 '(mime-view-type-subtype-score-alist
   '(((text . enriched)
      . 3)
     ((text . richtext)
      . 1)
     ((text . plain)
      . 4)
     ((text . html)
      . 2)
     (multipart . mime-view-multipart-entity-score)))
 '(nxml-auto-insert-xml-declaration-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(package-archive-priorities '(("melpa" . 10) ("gnu" . 5) ("melpa-stable" . 0)))
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")))
 '(package-quickstart t)
 '(package-selected-packages
   '(powershell ace-jump-mode add-node-modules-path aggressive-indent ahk-mode align-by-current-symbol align-string all-the-icons-dired anaconda-mode ansible ansible-doc ansible-vault ascii-art-to-unicode attrap auto-compile auto-dim-other-buffers auto-sudoedit bmx-mode browse-kill-ring buffer-move cmake-mode company-anaconda company-ansible company-go company-lsp company-nginx company-quickhelp company-restclient company-shell control-mode csharp-mode csv-mode cycle-quotes cython-mode daemons debian-el diminish dired-hide-dotfiles dired-imenu dired-sidebar docker-compose-mode dockerfile-mode easy-repeat editorconfig electric-operator emojify evil-esc-mode fancy-narrow fill-function-arguments fit-text-scale flimenu flx-ido flycheck-cask flycheck-cython flycheck-package flycheck-pos-tip flycheck-rust flycheck-status-emoji flycheck-vala flycheck-yamllint flyspell-correct forge git-gutter git-link gitconfig-mode gitignore-mode gnu-elpa-keyring-update go-eldoc god-mode goto-line-preview groovy-mode guess-language haskell-mode hide-mode-line highlight-numbers history html5-schema ialign ibuffer-projectile ido-completing-read+ ido-vertical-mode iedit importmagic ini-mode jinja2-mode js-auto-format-mode js2-refactor json-mode lastpass lisp-extra-font-lock list-unicode-display lsp-mode lsp-ui lua-mode madhat2r-theme magit magit-filenotify markdown-mode meson-mode move-dup mtg-deck-mode mwim nginx-mode niceify-info pandoc-mode php-mode pipenv powerline projectile py-autopep8 py-isort python-black rainbow-mode realgud ripgrep sass-mode sh-extra-font-lock shift-number smart-region smex spdx sqlup-mode string-inflection switch-buffer-functions systemd tmux-keys todotxt toml-mode vala-mode vala-snippets visual-regexp-steroids vterm wgrep which-key ws-butler xref-js2 yaml-mode yatemplate))
 '(powerline-default-separator 'bar)
 '(projectile-globally-ignored-file-suffixes '("~" "#"))
 '(projectile-keymap-prefix "p")
 '(projectile-mode-line-prefix " P")
 '(projectile-switch-project-action 'projectile-commander)
 '(projectile-tags-backend 'xref)
 '(ring-bell-function 'ignore)
 '(rng-schema-locating-files
   '((concat user-emacs-directory "schemas/schemas.xml")
     "/usr/share/emacs/25.2/etc/schema/schemas.xml" "schemas.xml"))
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
 '(sgml-basic-offset 4)
 '(sh-imenu-generic-expression
   '((sh
      (nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
      (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1))))
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(tab-bar-close-button-show nil)
 '(tab-bar-new-button-show nil)
 '(tab-bar-show 1)
 '(tab-width 8)
 '(table-command-prefix [])
 '(todotxt-file "~/Dropbox/pim/todo/todo.txt" t)
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
 '(yas-snippet-dirs (list (concat user-emacs-directory "snippets/")))
 '(yas-trigger-key nil)
 '(yas-trigger-symbol " ⇒")
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t))

;; Real ugly, but apparently these can't be set through customize

(defvar magit-last-seen-setup-instructions)
(setq magit-last-seen-setup-instructions "1.4.0")
(defvar todotxt-file)
(setq todotxt-file "~/Dropbox/todo.txt")

(when (custom-theme-enabled-p 'wombat)
  (custom-theme-set-faces
   'wombat
   '(cursor ((t (:background "tomato3"))))
   '(company-scrollbar-bg ((t (:background "#303030"))))
   '(company-scrollbar-fg ((t (:background "#4a4a4a"))))
   '(company-template-field ((t (:background "#4a4a4a"))))
   '(company-tooltip ((t (:inherit default :background "#3d3d3d"))))
   '(company-tooltip-annotation ((t (:inherit company-tooltip))))
   '(company-tooltip-common ((t (:inherit (font-lock-constant-face company-tooltip)))))
   '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground "red"))))
   '(company-tooltip-selection ((t (:inherit font-lock-function-name-face :background "#353535"))))
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
   '(hl-line ((t (:background "gray21"))))
   '(iedit-occurrence ((t (:inherit region))))
   '(lisp-extra-font-lock-quoted ((t (:inherit shadow))))
   '(powerline-active2 ((t (:inherit mode-line :background "gray30"))))
   '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "grey18"))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "gray11"))))
 '(fill-column-indicator ((t (:foreground "gray21"))))
 '(hl-line ((t (:background "gray19"))))
 '(iedit-occurrence ((t (:inherit region))))
 '(lsp-face-highlight-textual ((t (:background "gray17"))))
 '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :weight bold))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.1))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face))))
 '(table-cell ((t (:background "gray18"))))
 '(vr/match-0 ((t (:inherit region))))
 '(vr/match-1 ((t (:inherit region)))))

;;; Non-customizable settings

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; Project specific settings

;; TODO: Make this JHBuild project type work again
;; Dir Locals
;; (dir-locals-set-class-variables
;;  'gnome-code
;;  '((nil . ((projectile-project-type . jhbuild)))))

;; (dir-locals-set-directory-class "~/Code/gnome/src/" 'gnome-code)


(provide 'custom)
;;; custom.el ends here
