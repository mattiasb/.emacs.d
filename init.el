;;; init.el --- My init file

;; Copyright ⓒ 2013-2016 Mattias Bengtsson

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20141020
;; Keywords         : init
;; Package-Requires : ()
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 24.x

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


;;; Settings

;; Unset these early to remove at least some of the inital flicker.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path
(defvar load-prefer-newer)
(setq load-prefer-newer t)
(require 'funcs "~/.emacs.d/lisp/funcs.el")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;;; Early init code

;; Maximize on start
(my/maximize)


;;; Modes – General

(my/auto-modes  '(("\\.inl\\'"    . c++-mode)
                  ("\\.ui$"       . nxml-mode)
                  ("\\.js$"       . js2-mode)
                  ("\\.jshintrc$" . js2-mode)
                  ("\\.jscsrc$"   . json-mode)
                  ("\\.geojson$"  . json-mode)
                  ("\\.vala$"     . vala-mode)
                  ("\\.mapcss$"   . css-mode)
                  ("\\.mcss$"     . css-mode)
                  ("\\.m$"        . octave-mode)
                  ("\\.dec$"      . mtg-deck-mode)
                  ("\/Cask$"      . emacs-lisp-mode)))

(my/shorten-major-modes '((markdown-mode   . "M↓")
                          (js2-mode        . "JS")
                          (nxml-mode       . "XML")
                          (c-mode          . "C")
                          (c++-mode        . "C++")
                          (cmake-mode      . "CMake")
                          (emacs-lisp-mode . "Elisp")
                          (go-mode         . "Go")
                          (haskell-mode    . "λ")
                          (snippet-mode    . "Yas")))

(my/shorten-minor-modes '((company-mode             . " C")
                          (abbrev-mode              . " A")
                          (ws-butler-mode           . " W")
                          (control-mode             . "")
                          (git-gutter-mode          . "")
                          (magit-gitflow-mode       . " Flow")
                          (magit-filenotify-mode    . " Notify")
                          (yas-minor-mode           . "")
                          (fancy-narrow-mode        . "")
                          (haskell-indentation-mode . "")
                          (racer-mode               . "")
                          (aggressive-indent-mode   . " ⇒")
                          (which-key-mode           . "")
                          (magit-auto-revert-mode   . "")
                          (abbrev-mode              . "")))


;;; Project specific settings

;; Styles

(c-add-style "smarteye"
             '("stroustrup"
               (c-basic-offset . 2)
               (c-offsets-alist . ((innamespace . -)))))

;; Dir Locals
(dir-locals-set-class-variables
 'gnome-code
 '((nil . ((projectile-project-type . jhbuild)))))

(dir-locals-set-directory-class "~/Code/gnome/src/" 'gnome-code)

(dir-locals-set-class-variables
 'smarteye-code
 '((c-mode . ((c-file-style . "smarteye")))))

(dir-locals-set-directory-class "~/Code/git.smarteye.se/" 'smarteye-code)

;;; Post-init code

(add-hook 'after-init-hook
          (lambda ()
            (load "~/.emacs.d/my-after-init.el")
            (load "~/.emacs.d/my-hooks.el")))

;;; Advice

(advice-add #'isearch-forward-symbol-at-point  :after  #'god-mode-isearch-activate)
(advice-add #'isearch-backward-symbol-at-point :after  #'god-mode-isearch-activate)
(advice-add #'popup-create                     :before #'my/fci-turn-off)
(advice-add #'popup-delete                     :after  #'my/fci-turn-on)

(advice-add #'ido-find-file                    :after  #'my/reopen-file-as-root)

(advice-add #'backward-page                    :after  #'recenter)
(advice-add #'forward-page                     :after  #'recenter)

(advice-add #'diff-buffer-with-file            :after  (lambda (&rest _) (other-window 1)))
(advice-add #'delete-window                    :after  (lambda (&rest _) (balance-windows)))
(advice-add #'split-window-right               :after  #'balance-windows)
(advice-add #'split-window-below               :after  #'balance-windows)
(advice-add #'split-window-right               :after  (lambda () (other-window 1)))
(advice-add #'split-window-below               :after  (lambda () (other-window 1)))

;; Kill terminal buffer when the terminal process exits
(advice-add #'term-sentinel
            :after (lambda (proc _)
                     (when (memq (process-status proc) '(signal exit))
                       (kill-buffer (process-buffer proc)))))

(advice-add #'ansi-term
            :before (lambda (&rest _)
                      (interactive (list "/bin/bash"))))

(advice-add #'custom-save-all
            :around (lambda (func &rest args)
                      (let ((print-quoted t))
                        (apply func args))))

(advice-add #'save-buffers-kill-emacs
            :around (lambda (func &rest args)
                      (cl-flet ((process-list ()))
                        (apply func args))))

(advice-add #'flycheck-pos-tip-error-messages
            :around (lambda (func &rest args)
                      (let ((x-gtk-use-system-tooltips nil))
                        (apply func args))))

(mapc #'my/advice-describe-func '(package-menu-describe-package
                                  describe-variable
                                  describe-mode
                                  describe-function
                                  describe-bindings))
(advice-add #'keyfreq-show
            :after (lambda (&rest _)
                     (my/focus-buffer-dwim "*KeyFreq*")
                     (tabulated-list-mode)))

(provide 'init)
;;; init.el ends here
