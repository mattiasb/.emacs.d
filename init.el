;; Load path
(add-to-list 'load-path "~/.emacs.d/lisp/")
(autoload 'package++ "package++.el" nil t)

;;;; Keybindings ;;;;

(global-set-key (kbd "C-§")         'er/expand-region)
(global-set-key (kbd "C-c w")       'whitespace-mode)
(global-set-key (kbd "C-c c")       'comment-or-uncomment-region-or-line)
(global-set-key (kbd "<f9>")        'magit-status)
(global-set-key (kbd "<f11>")       'list-packages)
(global-set-key (kbd "<f12>")       'customize)
(global-set-key (kbd "C-x C-n")     'make-frame)
(global-set-key (kbd "M-<up>")      'move-text-up)
(global-set-key (kbd "M-<down>")    'move-text-down)
(global-set-key (kbd "C-c a")       'align-regexp)
(global-set-key (kbd "<tab>")       'indent-for-tab-command)
(global-set-key (kbd "M-x")         'smex)
(global-set-key (kbd "C-c e")       'ansi-term)
(global-set-key (kbd "C-c h")       'highlight-symbol-at-point)

(global-set-key (kbd "C-c r")       'replace-string)
(global-set-key (kbd "C-c C-r")     'replace-regexp)
(global-set-key (kbd "C-c q")       'query-replace)
(global-set-key (kbd "C-c C-q")     'query-replace-regexp)

(global-set-key (kbd "C-c s s")     'yas-insert-snippet)
(global-set-key (kbd "C-c s n")     'yas-new-snippet)
(global-set-key (kbd "C-c s e")     'yas-visit-snippet-file)
(global-set-key (kbd "C-c s r")     'yas-reload-all)

(global-set-key (kbd "<C-S-up>")    'buf-move-up)
(global-set-key (kbd "<C-S-down>")  'buf-move-down)
(global-set-key (kbd "<C-S-left>")  'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

(windmove-default-keybindings)

;;;; Modes ;;;;

;; Ido
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [tab] 'ido-complete)
            ))

;; GitGutter

(add-hook 'git-gutter-mode-on-hook
          (lambda ()
            (when (fboundp 'diminish) (diminish 'git-gutter-mode "GG"))))

;; Snippet

(add-hook 'snippet-mode-hook (lambda () (setq mode-name "S")))

;; Markdown

(add-hook 'markdown-mode-hook (lambda () (setq mode-name "Md")))

;; Abbrev

(add-hook 'abbrev-mode-hook (lambda() (when (fboundp 'diminish) (diminish 'abbrev-mode "A"))))

;; Company

(add-hook 'company-mode-hook
          (lambda ()
            (when (fboundp 'diminish) (diminish 'company-mode "Co"))
            (define-key company-active-map (kbd "\C-n")    'company-select-next)
            (define-key company-active-map (kbd "\C-p")    'company-select-previous)
            (define-key company-active-map (kbd "<next>")  'company-select-next-five)
            (define-key company-active-map (kbd "<prior>") 'company-select-previous-five)
            (define-key company-active-map (kbd "\C-p")    'company-select-previouss)
            (define-key company-active-map (kbd "\C-d")    'company-show-doc-buffer)
            (define-key company-active-map (kbd "\C-v")    'company-show-location)
            (define-key company-active-map (kbd "\C-g")    '(lambda ()
                                                              (interactive)
                                                              (company-abort)))
            ))

;; Flycheck
(add-hook 'flycheck-mode-hook
          (lambda ()
            (when (fboundp 'diminish) (diminish 'flycheck-mode "Fc"))))

;; GGTags
(add-hook 'ggtags-mode-hook
          (lambda ()
            (setq ggtags-mode-line-project-name nil)
            (define-key ggtags-mode-map (kbd "C-<return>")  'ggtags-find-tag-dwim)
            (define-key ggtags-mode-map (kbd "M-<left>")    'ggtags-prev-mark)
            (define-key ggtags-mode-map (kbd "M-<right>")   'ggtags-next-mark)
            ))


;; ELisp
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "El")))

;; Haskell
(add-hook 'haskell-mode-hook (lambda ()
                               (setq mode-name "Hs")
                               (setq-local electric-indent-mode nil)))

;; JS2
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (setq mode-name "JS2")
            (require 'js2-refactor)
            (define-key js2-mode-map (kbd "C-c f r") 'js2r-rename-var)
            (set (make-local-variable 'company-backends)
                 '((company-dabbrev-code
                    company-files
                    company-keywords
                    )))
            ))

;; C common

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (cppcm-reload-all)
              (ggtags-mode)
              )
            (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)
            ))

;; C
(add-hook 'c-mode-hook (lambda () (setq mode-name "C") ))

;; C++
(add-to-list 'auto-mode-alist '("\\cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\cc\\'"  . c++-mode))
(add-hook 'c++-mode-hook (lambda () (setq mode-name "C++")))


;; CMake

(add-hook 'cmake-mode-hook
  (lambda ()
    (when (fboundp 'company-mode) (company-mode))
    (setq mode-name "Cm")
    (set (make-local-variable 'company-backends)
      '((company-cmake company-files company-dabbrev-code)))
    ))

;; PROG
(add-hook 'prog-mode-hook
          (lambda ()
            (when (fboundp 'company-mode)  (company-mode))
            (when (fboundp 'flycheck-mode) (flycheck-mode))
            (setq-default indent-tabs-mode nil)
            ))

(add-hook 'yas-minor-mode-hook (lambda () (when (fboundp 'diminish) (diminish 'yas-minor-mode " Y"))))


;;;; Post-init code ;;;;

(defun my-after-init-hook ()
  ;; Set here since customize fubar's otherwise
  (setq yas-snippet-dirs (quote ("~/.emacs.d/snippets")))

  (require 'uniquify)
  (require 'package++)
  (package-sync)
  (require 'popup)
  (yas-global-mode)
  (global-git-gutter-mode)
  (setq projectile-mode-line-lighter "P")
  (projectile-global-mode)
  (ido-mode)
  (ido-vertical-mode)
  (ido-ubiquitous-mode)
  (flx-ido-mode)
  (fancy-narrow-mode)
  )
(add-hook 'after-init-hook 'my-after-init-hook)

;;;; Functions and Macros ;;;;

(defun company-select-next-five ()
  "A bit more eager company-select"
  (interactive)
  (dotimes (number 5 nil) (company-select-next))
  )

(defun company-select-previous-five ()
  "A bit more eager company-select"
  (interactive)
  (dotimes (number 5 nil) (company-select-previous))
  )

;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "->") t
;;           (if (looking-at "::") t
;;             nil
;;             ))
;;         ))))

;; (defun do-yas-expand ()
;;   (let ((yas-fallback-behavior 'return-nil))
;;     (yas-expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;     (minibuffer-complete)
;;     (let ((old-indent (current-indentation)))
;;       (indent-for-tab-command)
;;       (if (= old-indent (current-indentation))
;;         (if (or (not yas-minor-mode)
;;               (null (do-yas-expand)))
;;           (if (check-expansion)
;;             (company-complete-common)
;;             )))
;;       )))

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  "Use popup.el for yasnippet."
  (popup-menu*
   (mapcar
    (lambda (choice)
      (popup-make-item
       (or (and display-fn (funcall display-fn choice))
           choice)
       :value choice))
    choices)
   :prompt prompt
   ;; start isearch mode immediately
   :isearch t
   ))

(defun wrap-in-comment (str)
  (format "%s%s%s" comment-start str comment-end))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments current region or line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun rename-current-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun package-list-installed-packages ()
  "Like `package-list-packages', but shows only installed
optional packages."
  (interactive)
  (package-initialize)
  (package-show-package-list
   (cl-remove-if-not (lambda (x) (and (not (package-built-in-p x))
                                      (package-installed-p x)))
                     (mapcar 'car package-archive-contents))))

(defun rename-modeline (hook name)
  (add-hook 'hook (lambda () (setq mode-name name))))

;; Set proxy from environment
(defun set-proxy ()
  (interactive)
  (if (and (getenv "HTTP_PROXY") (getenv "HTTPS_PROXY"))
      (setq url-proxy-services '(("http"  . (getenv "HTTP_PROXY"))
                                 ("https" . (getenv "HTTPS_PROXY"))
                                 ))))

;; Advices

(defadvice split-window-right (after rebalance-windows activate)
  (balance-windows)
  (other-window 1))
(defadvice split-window-below (after rebalance-windows activate)
  (balance-windows)
  (other-window 1))
(defadvice delete-window (after rebalance-windows activate)
  (balance-windows))

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))
(defadvice magit-mode-quit-window (after magit-restore-screen activate)
  (jump-to-register :magit-fullscreen))

;; Yank and indent-stuff
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode       clojure-mode
                                     scheme-mode     haskell-mode    ruby-mode
                                     rspec-mode      python-mode     cmake-mode
                                     c-mode          c++-mode        objc-mode
                                     latex-mode      plain-tex-mode  js2-mode
                                     js-mode         json-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))


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
 '(company-backends (quote (company-elisp company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs company-cmake (company-gtags company-etags company-dabbrev-code company-keywords) company-oddmuse company-files company-dabbrev company-yasnippet)))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case t)
 '(company-idle-delay t)
 '(company-tooltip-align-annotations t)
 '(cppcm-build-dirname "build")
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(delete-selection-mode t)
 '(electric-indent-mode t)
 '(electric-layout-mode nil)
 '(electric-pair-mode t)
 '(flycheck-completion-system (quote ido))
 '(flycheck-highlighting-mode (quote symbols))
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
 '(js2-basic-offset 4 t)
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
 '(nxml-slash-auto-complete-flag t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(package-manifest (quote ("highlight-symbol" "flycheck" "git-gutter" "cpputils-cmake" "cmake-mode" "buffer-move" "ggtags" "js2-refactor" "lua-mode" "fancy-narrow" "ack-and-a-half" "diminish" "gitconfig-mode" "ido-ubiquitous" "epl" "projectile" "flx-ido" "smex" "expand-region" "haskell-mode" "js2-mode" "json-mode" "magit" "markdown-mode" "editorconfig" "yasnippet" "move-text" "company" "popup" "ido-vertical-mode")))
 '(projectile-keymap-prefix (kbd "C-p"))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-width 8)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
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
 '(highlight-symbol-face ((t (:background "gray21")))))
