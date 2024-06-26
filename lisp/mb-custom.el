;;; mb-custom.el --- My custom settings -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2013-2022, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson
;; Version          : 20141108
;; Keywords         : local
;; Package-Requires : ((emacs "29.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 29.x

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
 '(ace-window-posframe-mode nil nil (ace-window))
 '(aggressive-indent-dont-electric-modes t)
 '(aggressive-indent-excluded-modes
   '(inf-ruby-mode makefile-mode makefile-gmake-mode python-mode text-mode yaml-mode haskell-mode dockerfile-mode python-ts-mode yaml-ts-mode))
 '(ansi-color-for-compilation-mode 'filter)
 '(auto-dark-dark-theme 'madhat2r)
 '(auto-dark-detection-method 'dbus)
 '(auto-dark-light-theme 'leuven)
 '(auto-insert 'other)
 '(auto-insert-alist '((".*" . mb-cmd-auto-insert)))
 '(auto-insert-directory (concat user-emacs-directory "templates/"))
 '(auto-insert-query nil)
 '(auto-save-file-name-transforms
   '(("\\`/.*/\\([^/]+\\)\\'" "~/.cache/emacs/autosave/\\1" t)))
 '(auto-save-list-file-prefix "~/.cache/emacs/auto-save-list/.saves-")
 '(auto-save-no-message t)
 '(aw-display-mode-overlay nil)
 '(aw-scope 'frame)
 '(backup-directory-alist '(("." . "~/.cache/emacs/backup/")))
 '(blink-cursor-blinks -1)
 '(browse-kill-ring-current-entry-face 'mb-browse-kill-ring-current-entry-face)
 '(browse-kill-ring-display-duplicates nil)
 '(browse-kill-ring-highlight-current-entry t)
 '(browse-kill-ring-highlight-inserted-item nil)
 '(browse-kill-ring-no-duplicates t)
 '(browse-kill-ring-quit-action 'save-and-restore)
 '(browse-kill-ring-resize-window t)
 '(browse-kill-ring-separator
   "————————————————————————————————————————————————————————————————————————————————")
 '(browse-kill-ring-separator-face 'fill-column-indicator)
 '(browse-kill-ring-show-preview nil)
 '(browse-url-browser-function 'browse-url-default-browser)
 '(column-number-mode t)
 '(comment-auto-fill-only-comments t)
 '(compilation-read-command t)
 '(compilation-scroll-output t)
 '(completion-category-overrides
   '((xref-location
      (styles substring flex))
     (buffer
      (styles substring flex))
     (project-file
      (styles substring flex))
     (symbol-help
      (styles substring flex))
     (eglot
      (styles substring flex))))
 '(completion-styles '(substring flex))
 '(corfu-auto nil)
 '(corfu-bar-width 0.5)
 '(corfu-cycle t)
 '(corfu-echo-documentation nil)
 '(corfu-min-width 25)
 '(corfu-popupinfo-max-height 30)
 '(corfu-popupinfo-mode t)
 '(corfu-scroll-margin 2)
 '(csv-align-min-width 1)
 '(csv-align-padding 4)
 '(custom-buffer-done-kill t)
 '(custom-file (concat user-emacs-directory "lisp/mb-custom.el"))
 '(custom-safe-themes t)
 '(custom-unlispify-tag-names nil)
 '(default-frame-alist '((vertical-scroll-bars) (fullscreen . maximized)))
 '(delete-selection-mode t)
 '(devdocs-data-dir "/home/mattiasb/.cache/emacs/devdocs")
 '(dired-dwim-target 'dired-dwim-target-next)
 '(dired-listing-switches "-laGh1v --group-directories-first")
 '(dired-omit-files "^\\.")
 '(dired-omit-verbose nil)
 '(dired-sidebar-width 40)
 '(display-fill-column-indicator-character 9474)
 '(doom-modeline-buffer-encoding 'nondefault)
 '(doom-modeline-buffer-file-name-style 'relative-from-project)
 '(doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode text-mode))
 '(doom-modeline-enable-word-count t)
 '(doom-modeline-height 35)
 '(doom-modeline-hud t)
 '(doom-modeline-minor-modes t)
 '(doom-modeline-modal-icon nil)
 '(doom-modeline-percent-position nil)
 '(easy-repeat-command-list
   '(backward-page beginning-of-defun default-text-scale-decrease default-text-scale-increase default-text-scale-reset end-of-defun flymake-goto-next-error flymake-goto-prev-error forward-page git-gutter:next-hunk git-gutter:previous-hunk hl-find-next-thing hl-find-prev-thing kill-buffer mb-cmd-split-window-below mb-cmd-split-window-right mb-cmd-toggle-programming-case-word-at-point mb-cmd-toggle-programming-case-word-at-point-reverse md/duplicate-down md/duplicate-up next-buffer next-error org-backward-heading-same-level org-forward-heading-same-level org-next-visible-heading org-previous-visible-heading other-window outline-backward-same-level outline-forward-same-level outline-next-visible-heading outline-previous-visible-heading outline-up-heading paredit-backward paredit-backward-up paredit-forward previous-error python-indent-shift-left python-indent-shift-right reb-next-match reb-prev-match recenter-top-bottom scroll-down-command scroll-other-window scroll-up-command shift-number-down shift-number-up string-inflection-all-cycle table-heighten-cell table-narrow-cell table-shorten-cell table-widen-cell))
 '(eat-enable-shell-prompt-annotation nil)
 '(eat-kill-buffer-on-exit t)
 '(eat-semi-char-non-bound-keys
   '([24]
     [28]
     [17]
     [7]
     [8]
     [27 3]
     [21]
     [27 120]
     [27 58]
     [27 33]
     [27 38]
     [C-insert]
     [M-insert]
     [S-insert]
     [C-M-insert]
     [C-S-insert]
     [M-S-insert]
     [C-M-S-insert]
     [M-delete]
     [S-delete]
     [C-M-delete]
     [C-S-delete]
     [M-S-delete]
     [C-M-S-delete]
     [C-deletechar]
     [M-deletechar]
     [S-deletechar]
     [C-M-deletechar]
     [C-S-deletechar]
     [M-S-deletechar]
     [C-M-S-deletechar]
     [C-up]
     [C-down]
     [M-up]
     [M-down]
     [M-right]
     [M-left]
     [S-up]
     [S-down]
     [S-right]
     [S-left]
     [C-M-up]
     [C-M-down]
     [C-M-right]
     [C-M-left]
     [C-S-up]
     [C-S-down]
     [C-S-right]
     [C-S-left]
     [M-S-up]
     [M-S-down]
     [M-S-right]
     [M-S-left]
     [C-M-S-up]
     [C-M-S-down]
     [C-M-S-right]
     [C-M-S-left]
     [C-home]
     [M-home]
     [S-home]
     [C-M-home]
     [C-S-home]
     [M-S-home]
     [C-M-S-home]
     [C-end]
     [M-end]
     [S-end]
     [C-M-end]
     [C-S-end]
     [M-S-end]
     [C-M-S-end]
     [C-prior]
     [M-prior]
     [S-prior]
     [C-M-prior]
     [C-S-prior]
     [M-S-prior]
     [C-M-S-prior]
     [C-next]
     [M-next]
     [S-next]
     [C-M-next]
     [C-S-next]
     [M-S-next]
     [C-M-S-next]))
 '(eat-term-scrollback-size 1200000)
 '(edconf-exec-path "")
 '(edconf-get-properties-function 'editorconfig-core-get-properties-hash)
 '(eglot-autoshutdown t)
 '(eglot-confirm-server-initiated-edits nil)
 '(eldoc-idle-delay 3600)
 '(electric-pair-pairs
   '((34 . 34)
     (8216 . 8217)
     (8220 . 8221)
     (123 . 125)
     (91 . 93)
     (96 . 96)))
 '(emojify-download-emojis-p t)
 '(emojify-emoji-styles '(unicode))
 '(emojify-emojis-dir "~/.cache/emacs/emojis")
 '(envrc-error-lighter '(" " (:propertize "E" face envrc-mode-line-error-face)))
 '(envrc-global-mode t)
 '(envrc-none-lighter nil)
 '(envrc-on-lighter '(" " (:propertize "E" face envrc-mode-line-on-face)))
 '(eww-header-line-format "")
 '(expand-region-contract-fast-key "S-SPC")
 '(fill-function-arguments-first-argument-same-line t)
 '(fill-function-arguments-last-argument-same-line t)
 '(fill-function-arguments-second-argument-same-line nil)
 '(fill-function-arguments-trailing-separator t)
 '(flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))
 '(font-use-system-font t)
 '(forge-alist
   '(("github.com" "api.github.com" "github.com" forge-github-repository)
     ("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository)
     ("salsa.debian.org" "salsa.debian.org/api/v4" "salsa.debian.org" forge-gitlab-repository)
     ("git.smarteye.se" "git.smarteye.se/api/v4" "git.smarteye.se" forge-gitlab-http-repository)
     ("gitlab.gnome.org" "gitlab.gnome.org/api/v4" "gitlab.gnome.org" forge-gitlab-repository)
     ("teahub.io" "teahub.io/api/v1" "teahub.io" forge-gitea-repository)
     ("code.orgmode.org" "code.orgmode.org/api/v1" "code.orgmode.org" forge-gogs-repository)
     ("bitbucket.org" "api.bitbucket.org/2.0" "bitbucket.org" forge-bitbucket-repository)
     ("git.savannah.gnu.org" nil "git.savannah.gnu.org" forge-cgit*-repository)
     ("git.kernel.org" nil "git.kernel.org" forge-cgit-repository)
     ("repo.or.cz" nil "repo.or.cz" forge-repoorcz-repository)
     ("git.suckless.org" nil "git.suckless.org" forge-stagit-repository)
     ("git.sr.ht" nil "git.sr.ht" forge-srht-repository)))
 '(forge-database-file "/home/mattiasb/.cache/emacs/forge-database.sqlite")
 '(forge-topic-list-limit '(60 . 0))
 '(fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist) t)
 '(gc-cons-threshold 20000000)
 '(git-commit-fill-column 72)
 '(git-commit-known-pseudo-headers
   '("Signed-off-by" "Acked-by" "Modified-by" "Cc" "Suggested-by" "Reported-by" "Tested-by" "Reviewed-by" "Co-authored-by" "Fixes"))
 '(git-commit-major-mode 'markdown-mode)
 '(git-commit-setup-hook
   '(git-commit-turn-on-auto-fill with-editor-usage-message mb-hooks--git-commit-setup forge-bug-reference-setup git-commit-save-message git-commit-setup-changelog-support git-commit-turn-on-auto-fill git-commit-propertize-diff with-editor-usage-message))
 '(git-commit-style-convention-checks '(non-empty-second-line overlong-summary-line))
 '(git-commit-summary-max-length 50)
 '(git-commit-trailer
   '("Signed-off-by" "Acked-by" "Modified-by" "Cc" "Suggested-by" "Reported-by" "Tested-by" "Reviewed-by" "Co-authored-by" "Fixes"))
 '(git-commit-trailers
   '("Signed-off-by" "Acked-by" "Modified-by" "Cc" "Suggested-by" "Reported-by" "Tested-by" "Reviewed-by" "Co-authored-by" "Fixes"))
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
 '(global-corfu-mode t)
 '(global-git-gutter-mode t)
 '(haskell-font-lock-symbols 'unicode)
 '(haskell-mode-hook '(turn-on-haskell-indentation) t)
 '(help-enable-symbol-autoload t)
 '(highlight-symbol-idle-delay 1.0)
 '(history-advised-before-functions
   '(beginning-of-buffer end-of-buffer imenu isearch-mode magit-diff-visit-file mb-cmd-markdown-jump xref-find-definitions xref-find-definitions-other-frame xref-find-definitions-other-window xref-find-references))
 '(ialign-initial-regexp "(\\s+)")
 '(ialign-initial-repeat t)
 '(ialign-pcre-mode t)
 '(iedit-toggle-key-default nil)
 '(imenu-after-jump-hook '((lambda nil (recenter 0))))
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 750000)
 '(importmagic-python-interpreter "python3")
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message "mattiasb")
 '(inhibit-startup-screen t)
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message "")
 '(isearch-lazy-count t)
 '(jinx-exclude-regexps
   '(((emacs-lisp-mode "Package-Requires *:.*$" "Keywords:.*$" "[a-zA-Z0-9_-]+\\.el")
      (t "[A-Z]+\\>" "-+\\>" "\\w*?[0-9]\\w*\\>" "[a-z]+://\\S-+" "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?" "\\(?:Local Variables\\|End\\):\\s-*$" "jinx-\\(?:languages\\|local-words\\):\\s-+.*$"))))
 '(large-file-warning-threshold 100000000)
 '(lastpass-user "mattias.jc.bengtsson@gmail.com")
 '(load-prefer-newer t)
 '(lock-file-name-transforms '(("\\`/.*/\\([^/]+\\)\\'" "~/.cache/emacs/lock/\\1" t)))
 '(lua-indent-level 2)
 '(lua-prefix-key "C-z")
 '(magit-auto-revert-mode-lighter "")
 '(magit-diff-refine-hunk 'all)
 '(magit-no-confirm '(stage-all-changes unstage-all-changes))
 '(magit-repository-directories '(("~/Code/" . 5)))
 '(magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
 '(magit-save-repository-buffers 'dontask)
 '(magit-todos-auto-group-items 'always)
 '(magit-todos-exclude-globs '(".git/" "snippets/prog-mode/todo"))
 '(magit-todos-group-by '(magit-todos-item-filename))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(major-mode-remap-alist
   '((bash-mode . bash-ts-mode)
     (css-mode . css-ts-mode)
     (js2-mode . js-ts-mode)
     (json-mode . json-ts-mode)
     (python-mode . python-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (yaml-ts-mode . yaml-mode)))
 '(markdown-asymmetric-header nil)
 '(markdown-command
   "pandoc --metadata pagetitle=*markdown* -s -f gfm -c file:///home/mattiasb/.config/emacs/github-pandoc.css")
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-open-command "/usr/bin/firefox")
 '(markdown-reference-location 'end)
 '(markdown-unordered-list-item-prefix "- ")
 '(markdown-use-pandoc-style-yaml-metadata t)
 '(mc/always-run-for-all t)
 '(mc/list-file "~/.cache/emacs/.mc-lists.el")
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
 '(mode-line-percent-position nil)
 '(nxml-auto-insert-xml-declaration-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(package-archive-priorities
   '(("gnu" . 80)
     ("nongnu" . 40)
     ("melpa-stable" . 20)
     ("melpa" . 10)))
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
 '(package-gnupghome-dir "~/.cache/emacs/elpa/gnupg/")
 '(package-install-upgrade-built-in t)
 '(package-native-compile t)
 '(package-quickstart t)
 '(package-quickstart-file "~/.cache/emacs/package-quickstart.el")
 '(package-selected-packages
   '(ace-jump-mode ace-window aggressive-indent all-the-icons-completion all-the-icons-dired all-the-icons-ibuffer ansible ansible-doc ansible-vault ascii-art-to-unicode auto-compile auto-dark auto-dim-other-buffers bind-key browse-kill-ring bury-successful-compilation cape caps-lock cl-generic cl-lib cmake-mode company-ansible consult corfu csharp-mode csv-mode cycle-quotes dedicated default-text-scale devdocs devhelp diminish dired-imenu dired-sidebar docker-compose-mode doom-modeline easy-repeat eat editorconfig eglot eglot-tempel eldoc emacsql-sqlite-module envrc erc expand-region external-completion faceup fill-function-arguments fit-text-scale flymake flymake-eldev flymake-popon flymake-yamllint forge git-gutter git-link git-modes go-mode goto-line-preview groovy-mode gsettings haskell-mode hide-mode-line highlight-numbers history hl-anything html5-schema ialign ibuffer-projectile iedit ini-mode inspector jinja2-mode jinx jsonrpc lastpass let-alist ligature lisp-extra-font-lock lua-mode madhat2r-theme magit magit-filenotify magit-todos map markdown-mode meson-mode move-dup mtg-deck-mode multiple-cursors mwim nadvice nginx-mode niceify-info ntlm olivetti org pandoc-mode pcre2el php-mode powershell project projectile pydoc pytest pytest-realgud python rainbow-mode realgud restclient rfc-mode ripgrep sass-mode seq sh-extra-font-lock shift-number smart-region so-long soap-client spdx sql-indent sqlup-mode string-inflection su svg systemd tempel terraform-mode tmux-keys tmux-mode todotxt toml-mode tramp tree-inspector treesit-auto unfill use-package verilog-mode vertico visual-fill-column visual-regexp-steroids vundo wgrep which-key ws-butler xref yaml-mode))
 '(package-user-dir "~/.cache/emacs/elpa")
 '(project-list-file "~/.cache/emacs/projects")
 '(projectile-cache-file "/home/mattiasb/.cache/emacs/projectile.cache")
 '(projectile-globally-ignored-file-suffixes '("~" "#"))
 '(projectile-keymap-prefix "\32p")
 '(projectile-known-projects-file "/home/mattiasb/.cache/emacs/projectile-bookmarks.eld")
 '(projectile-mode-line-prefix " P")
 '(projectile-switch-project-action 'projectile-commander)
 '(projectile-tags-backend 'xref)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(reb-re-syntax 'pcre)
 '(recentf-save-file "~/.cache/emacs/recentf")
 '(rfc-mode-directory "/home/mattiasb/Documents/rfc/")
 '(ring-bell-function 'ignore)
 '(rust-indent-method-chain t)
 '(safe-local-variable-values
   '((checkdoc-package-keywords-flag)
     (mtg-deck-format . vintage)
     (mtg-deck-format . standard)
     (mtg-deck-format . legacy)
     (mtg-deck-format . modern)))
 '(save-abbrevs 'silently)
 '(savehist-additional-variables '(projectile-project-command-history command-history))
 '(savehist-file "~/.cache/emacs/history")
 '(scroll-bar-mode nil)
 '(scroll-margin 5)
 '(scroll-step 1)
 '(sentence-end-double-space nil)
 '(sgml-basic-offset 4)
 '(sh-imenu-generic-expression
   '((sh
      (nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
      (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1))))
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(shr-bullet "• ")
 '(shr-discard-aria-hidden nil)
 '(shr-hr-line 8212)
 '(shr-image-animate nil)
 '(shr-inhibit-images t)
 '(shr-max-width 80)
 '(shr-use-colors nil)
 '(shr-use-fonts t)
 '(shr-width 80)
 '(spdx-copyright-holder 'user)
 '(spdx-copyright-sign 'none)
 '(spdx-ignore-deprecated t)
 '(spdx-project-detection 'projectile)
 '(switch-to-buffer-in-dedicated-window 'pop)
 '(switch-to-buffer-obey-display-actions t)
 '(tab-always-indent 'complete)
 '(tab-bar-close-button-show nil)
 '(tab-bar-new-button-show nil)
 '(tab-width 8)
 '(table-command-prefix [])
 '(tempel-path "/home/mattiasb/.config/emacs/templates.eld")
 '(terraform-format-on-save nil)
 '(tooltip-mode nil)
 '(tramp-persistency-file-name "/home/mattiasb/.cache/emacs/tramp")
 '(transient-history-file "~/.cache/emacs/transient/history.el")
 '(treesit-auto-install t)
 '(treesit-auto-langs
   '(awk bash bibtex c c-sharp clojure cmake commonlisp cpp css dart dockerfile elixir go gomod heex html java javascript json julia kotlin latex lua make markdown proto python r ruby rust toml tsx typescript typst verilog vhdl))
 '(typescript-ts-mode-indent-offset 4)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(url-cache-directory "~/.cache/emacs/url/files/")
 '(url-configuration-directory "~/.cache/emacs/url/")
 '(url-cookie-file "~/.cache/emacs/url/cookies")
 '(url-history-file "/home/mattiasb/.cache/emacs/url/history")
 '(use-file-dialog nil)
 '(use-short-answers t)
 '(user-mail-address "mattias.jc.bengtsson@gmail.com")
 '(vc-handled-backends nil)
 '(vertico-resize nil)
 '(vertico-sort-function 'vertico-sort-history-alpha)
 '(vr/auto-show-help nil)
 '(vr/match-separator-string " → ")
 '(vr/match-separator-use-custom-face t)
 '(vundo-glyph-alist
   '((selected-node . 9679)
     (node . 9675)
     (horizontal-stem . 9472)
     (vertical-stem . 9474)
     (branch . 9500)
     (last-branch . 9492)))
 '(vundo-window-max-height 10)
 '(warning-suppress-log-types '((magit-todos) (magit-todos) (comp)))
 '(warning-suppress-types '((magit-todos) (comp)))
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
 '(windmove-wrap-around nil)
 '(woman-default-indent 4)
 '(woman-fill-column 80)
 '(woman-fill-frame nil)
 '(woman-fontify t)
 '(woman-imenu t)
 '(xref-search-program 'ripgrep)
 '(xref-show-definitions-function 'xref-show-definitions-buffer-at-bottom)
 '(xref-show-xrefs-function 'xref-show-definitions-buffer-at-bottom)
 '(yaml-indent-offset 2)
 '(yank-excluded-properties
   '(category field follow-link fontified font-lock-face help-echo intangible invisible keymap local-map mouse-face read-only yank-handler face)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:background "gainsboro" :foreground "firebrick" :box (:line-width (2 . 2) :color "grey75" :style released-button) :height 5.0 :width extra-expanded))))
 '(corfu-popupinfo ((t (:inherit corfu-default))))
 '(doom-modeline-buffer-minor-mode ((t (:weight normal :slant normal :inherit doom-modeline))))
 '(envrc-mode-line-none-face ((t (:inherit doom-modeline-buffer-minor-mode))))
 '(flymake-error ((t (:underline (:color "tomato3" :style wave)))))
 '(flymake-note ((t (:underline (:color "olive drab" :style wave)))))
 '(flymake-warning ((t (:underline (:color "goldenrod" :style wave)))))
 '(iedit-occurrence ((t (:inherit region))))
 '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :weight bold))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.1))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face))))
 '(shr-abbreviation ((t (:inherit underline :underline (:color foreground-color :style wave)))))
 '(shr-h1 ((t (:inherit markdown-header-face-1))))
 '(shr-h2 ((t (:inherit markdown-header-face-2))))
 '(shr-h3 ((t (:inherit markdown-header-face-3))))
 '(shr-h4 ((t (:inherit markdown-header-face-4))) t)
 '(shr-h5 ((t (:inherit markdown-header-face-5))) t)
 '(shr-h6 ((t (:inherit markdown-header-face-6))) t)
 '(variable-pitch ((t (:height 1.1 :family "Sans Serif"))))
 '(vr/match-0 ((t (:inherit region))))
 '(vr/match-1 ((t (:inherit region)))))

(provide 'mb-custom)
;;; mb-custom.el ends here
