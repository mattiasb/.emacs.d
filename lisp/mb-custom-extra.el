;;; mb-custom-extra.el --- Extra configuration not run through customize -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2023, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20230921
;; Keywords         : local
;; Package-Requires : ((emacs 28.2))
;; URL              : https://github.com/mattiasb/.emacs.d
;; Compatibility    : GNU Emacs: 28.x

;;; Commentary:

;;; Note:

;;; Code:


;;; Non-customizable settings

(defalias 'list-buffers 'ibuffer)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defvar magit-last-seen-setup-instructions)
(setq magit-last-seen-setup-instructions "1.4.0")
(defvar todotxt-file)
(setq todotxt-file "~/Dropbox/todo/todo.txt")
(setq completion-ignore-case t)

;; Themes

(defface mb-browse-kill-ring-current-entry-face
  '((t (:inherit magit-section-highlight :weight bold)))
  "Face for browse-kill-ring-current-entry."
  :version "2023-09-21"
  :group 'browse-kill-ring)

(with-eval-after-load 'madhat2r-theme
  (deftheme mb-madhat2r)
  (custom-theme-set-faces
   'mb-madhat2r
   '(auto-dim-other-buffers-face ((t (:background "gray11"))) t)
   '(fill-column-indicator ((t (:foreground "gray21"))) t)
   '(hl-line ((t (:background "gray19"))) t)
   '(popup-tip-face ((t (:background "gray19"))))
   '(tab-bar ((t (:underline "gray35"))) t)
   '(tab-bar-tab-inactive ((t nil)) t)
   '(table-cell ((t (:background "gray18"))) t)
   '(vertico-current ((t (:foreground "#ebc844" :extend t :inherit bold))) t)))

(with-eval-after-load 'leuven-theme
  (deftheme mb-leuven)
  (custom-theme-set-faces
   'mb-leuven
   '(auto-dim-other-buffers-face ((t (:background "gray98"))) t)
   '(popup-tip-face ((t (:inherit corfu-default))))))

(with-eval-after-load 'wombat
  (custom-theme-set-faces
   'wombat
   '(cursor ((t (:background "tomato3"))))
   '(git-gutter:added ((t (:foreground "olive drab" :weight bold))))
   '(git-gutter:deleted ((t (:foreground "tomato3" :weight bold))))
   '(git-gutter:modified ((t (:foreground "goldenrod" :weight bold))))
   '(highlight-symbol-face ((t (:background "gray21"))))
   '(hl-line ((t (:background "gray21"))))
   '(lisp-extra-font-lock-quoted ((t (:inherit shadow))))
   '(powerline-active2 ((t (:inherit mode-line :background "gray30"))))
   '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "grey18"))))))

(provide 'mb-custom-extra)
;;; mb-custom-extra.el ends here
