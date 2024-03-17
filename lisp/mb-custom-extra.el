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

(eval-when-compile
  (defvar mb-cache-directory))

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
   '(flymake-popon ((t (:background "gray19"))))
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
   '(flymake-popon ((t (:box (:line-width (5 . 5) :color "white" :style flat-button) :inherit default))))
   '(history-temp-history ((t (:foreground "#FFFFFF"))))))

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
(dir-locals-set-directory-class (file-name-concat mb-cache-directory "elpa")
                                'read-only)
(dir-locals-set-directory-class "/usr/share/emacs/"
                                'read-only)

;; Display Buffer AList
(setq display-buffer-alist
      `((,(rx "*Async Shell Command*")
         (display-buffer-no-window))

        ((or (derived-mode . help-mode)
             (derived-mode . ripgrep-search-mode))
         (display-buffer-reuse-mode-window display-buffer-use-some-window)
         (body-function . select-window))

        ((or (derived-mode . inferior-python-mode)
             (derived-mode . eat-mode)
             (derived-mode . inferior-emacs-lisp-mode))
         (display-buffer-reuse-window display-buffer-below-selected)
         (window-height . 0.3)
         (dedicated . t))

        ((or (derived-mode . flymake-diagnostics-buffer-mode)
             (derived-mode . occur-mode))
         (display-buffer-below-selected)
         (window-height . shrink-window-if-larger-than-buffer)
         (dedicated . t)
         (body-function . select-window))

        ;; ;; Magit
        ;; ((derived-mode . magit-status-mode)
        ;;  (display-buffer-in-side-window)
        ;;  (dedicated . t)
        ;;  (preserve-size . (t . t))
        ;;  (side . right)
        ;;  (slot . 0)
        ;;  (window-width . 100))
        ;; ((derived-mode . magit-revision-mode)
        ;;  (display-buffer-in-side-window)
        ;;  (dedicated . t)
        ;;  (side . right)
        ;;  (slot . 1)
        ;;  (window-width . 100))

        ))

(provide 'mb-custom-extra)
;;; mb-custom-extra.el ends here
