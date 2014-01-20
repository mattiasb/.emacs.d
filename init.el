;; Load path
(add-to-list 'load-path "~/.local/share/emacs/lisp/")

;;;; Modes ;;;;

;; JS2
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; PROG 
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil)
            ))

;; ;; yasnippet
;; (defun yas-my-hook ()
;;   (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
;;   (yas-reload-all)
;;   (remove-hook 'yas-minor-mode-hook
;;                'yas-my-hook))

;; (add-hook 'yas-minor-mode-hook 'yas-my-hook)


;;;; Keybindings ;;;;

(global-set-key (kbd "C-§")      'er/expand-region)
(global-set-key (kbd "C-x w")    'whitespace-mode)
(global-set-key (kbd "C-c C-c")  'comment-or-uncomment-region-or-line)
(global-set-key (kbd "<f9>")     'magit-status)
(global-set-key (kbd "<f11>")    'list-packages)
(global-set-key (kbd "<f12>")    'customize)
(global-set-key (kbd "C-x C-n")  'make-frame)
(global-set-key (kbd "<C-tab>")  'other-window)
(global-set-key (kbd "M-<up>")   'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

(global-set-key (kbd "C-c s s")  'yas-insert-snippet)
(global-set-key (kbd "C-c s n")  'yas-new-snippet)
(global-set-key (kbd "C-c s v")  'yas-visit-snippet-file)
(global-set-key (kbd "C-c s r")  'yas-reload-all)

(global-set-key (kbd "C-c a")     'align-region)

(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;;;; Functions and Macros ;;;;

(defun align-region (begin end)
  "Align region to some common separators"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)[=|:]" 1 1))

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

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

;; Advices

(defadvice split-window-right (after rebalance-windows activate)
  (balance-windows))
(defadvice split-window-below (after rebalance-windows activate)
  (balance-windows))
(defadvice delete-window (after rebalance-windows activate)
  (balance-windows))

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))
(defadvice magit-mode-quit-window (after magit-restore-screen activate)
  (jump-to-register :magit-fullscreen))


;;;; Other settings ;;;;

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "emacs-lisp-mode" emacs-lisp-mode "elisp")
(defalias 'yes-or-no-p 'y-or-n-p)


;;;; Post-init code ;;;;

;; (add-hook 'after-init-hook 'package-sync)
(add-hook 'after-init-hook
          (lambda ()
            ;; (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
            (yas-global-mode)
            ))

;;;; Custom mode ;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(delete-selection-mode t)
 '(electric-indent-mode t)
 '(electric-layout-mode nil)
 '(electric-pair-mode t)
 '(haskell-font-lock-symbols (quote unicode))
 '(haskell-mode-hook (quote (turn-on-haskell-indent)))
 '(ido-vertical-define-keys (quote C-n-C-p-up-down-left-right))
 '(ido-vertical-mode t)
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
 '(nxml-slash-auto-complete-flag t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(package-manifest (quote ("package+" "expand-region" "haskell-mode" "js2-mode" "json-mode" "magit" "markdown-mode" "editorconfig" "yasnippet")))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(user-mail-address "mattias.jc.bengtsson@gmail.com")
 '(yas-also-auto-indent-first-line t)
 '(yas-snippet-dirs (quote ("~/.emacs.d/snippets")))
 '(yas-wrap-around-region t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
