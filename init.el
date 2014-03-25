;; Load path
(add-to-list 'load-path "~/.emacs.d/lisp/")
(autoload 'package++ "package++.el" nil t)

;;;; Keybindings ;;;;

(global-set-key (kbd "C-§")      'er/expand-region)
(global-set-key (kbd "C-x w")    'whitespace-mode)
(global-set-key (kbd "C-c c")    'comment-or-uncomment-region-or-line)
(global-set-key (kbd "<f9>")     'magit-status)
(global-set-key (kbd "<f11>")    'list-packages)
(global-set-key (kbd "<f12>")    'customize)
(global-set-key (kbd "C-x C-n")  'make-frame)
(global-set-key (kbd "M-<up>")   'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "C-c a")    'align-region)
(global-set-key (kbd "<insert>") 'dabbrev-expand)
(global-set-key (kbd "<tab>")    'tab-indent-or-complete)
(global-set-key (kbd "M-x")      'smex)
(global-set-key (kbd "C-y")      'yank-and-indent)

(global-set-key (kbd "C-c s s")    'yas-insert-snippet)
(global-set-key (kbd "C-c s n")    'yas-new-snippet)
(global-set-key (kbd "C-c s e")    'yas-visit-snippet-file)
(global-set-key (kbd "C-c s r")    'yas-reload-all)
(global-set-key (kbd "C-<return>") 'yas-expand)


;;;; Modes ;;;;


;; Company

(add-hook 'company-mode-hook 
	  (lambda ()
	    (define-key company-active-map (kbd "\C-n")    'company-select-next)
	    (define-key company-active-map (kbd "\C-p")    'company-select-previous)
            (define-key company-active-map (kbd "<next>")  'company-select-next-five)
            (define-key company-active-map (kbd "<prior>") 'company-select-previous-five)
	    (define-key company-active-map (kbd "\C-p")    'company-select-previous)
	    (define-key company-active-map (kbd "\C-d")    'company-show-doc-buffer)
	    (define-key company-active-map (kbd "\C-v")    'company-show-location)
	    (define-key company-active-map (kbd "<tab>")   'company-complete)
	    (define-key company-active-map (kbd "\C-g")    '(lambda ()
                                                              (interactive)
                                                              (company-abort)))
	    ))


;; JS2
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends)
		 '((company-dabbrev-code company-files)))
	    ))

(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-clang company-files)))
            ))

;; PROG 
(add-hook 'prog-mode-hook
          (lambda ()
	    (when (fboundp 'company-mode) (company-mode))
            (setq-default indent-tabs-mode nil)
            ))

;;;; Post-init code ;;;;

(add-hook 'after-init-hook
          (lambda ()
            (require 'uniquify)
	    (require 'package++)
	    (package-sync)
            (require 'popup)
            (yas-global-mode)
            (projectile-global-mode)
            (ido-mode)
            (ido-vertical-mode)
            (ido-ubiquitous-mode)
            (flx-ido-mode)
            ))

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

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (let ((old-indent (current-indentation)))
      (indent-for-tab-command)
      (if (= old-indent (current-indentation))
          (if (or (not yas-minor-mode)
                  (null (do-yas-expand)))
              (if (check-expansion)
                  (company-complete-common)
                )))
      )))

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

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
(rename-modeline "projectile-mode" projectile-mode "P")
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
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(delete-selection-mode t)
 '(electric-indent-mode t)
 '(electric-layout-mode nil)
 '(electric-pair-mode t)
 '(global-company-mode t)
 '(haskell-font-lock-symbols (quote unicode))
 '(haskell-mode-hook (quote (turn-on-haskell-indent)))
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
 '(package-manifest (quote ("gitconfig-mode" "ido-ubiquitous" "epl" "projectile" "flx-ido" "smex" "expand-region" "haskell-mode" "js2-mode" "json-mode" "magit" "markdown-mode" "editorconfig" "yasnippet" "move-text" "company" "popup" "ido-vertical-mode")))
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
 '(yas-snippet-dirs (quote ("~/.emacs.d/snippets")))
 '(yas-trigger-key nil)
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
