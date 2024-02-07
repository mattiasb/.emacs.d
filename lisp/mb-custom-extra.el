;;; mb-custom-extra.el --- Extra configuration not run through customize -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2023, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20230921
;; Keywords         : local
;; Package-Requires : ((emacs "29.1"))
;; URL              : https://github.com/mattiasb/.emacs.d
;; Compatibility    : GNU Emacs: 29.x

;;; Commentary:

;;; Note:

;;; Code:


;;; Non-customizable settings

(defalias 'list-buffers 'ibuffer)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

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
   '(diff-refine-added ((t (:background "#416d41" :foreground "#cceecc"))))
   '(diff-refine-removed ((t (:background "#664040" :foreground "#eecccc"))))
   '(fill-column-indicator ((t (:foreground "gray21"))) t)
   '(history-current-history ((t (:foreground "#131516"))))
   '(history-current-temp-history ((t (:foreground "#131516"))))
   '(history-other-history ((t (:foreground "#131516"))))
   '(history-prompt ((t (:foreground "#131516"))))
   '(history-temp-history ((t (:foreground "#131516"))))
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
   '(doom-modeline-project-dir ((t (:inherit (doom-modeline
                                              doom-modeline-warning
                                              bold)))))
   '(fill-column-indicator ((t (:foreground "#D2D2D2"))) t)
   '(git-gutter:added ((t (:foreground "#3A993A" :weight bold))))
   '(git-gutter:deleted ((t (:foreground "#CC3333" :weight bold))))
   '(git-gutter:modified ((t (:foreground "#FEA500" :weight bold))))
   '(history-current-history ((t (:foreground "#000000"))))
   '(history-current-temp-history ((t (:foreground "#FFFFFF"))))
   '(history-other-history ((t (:foreground "#FFFFFF"))))
   '(history-prompt ((t (:foreground "#FFFFFF"))))
   '(history-temp-history ((t (:foreground "#FFFFFF"))))
   '(popup-tip-face ((t (:inherit popup-summary-face))))))

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

;; Define a read-only directory class
(dir-locals-set-class-variables 'read-only
                                '((nil . ((buffer-read-only . t)))))
(dir-locals-set-directory-class (file-truename "~/.cache/emacs/elpa/")
                                'read-only)

(provide 'mb-custom-extra)
;;; mb-custom-extra.el ends here
