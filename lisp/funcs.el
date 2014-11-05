;;; funcs.el --- Some functions and macros I use.

;; Copyright (C) 2013, 2014 Mattias Bengtsson

;; Author: Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version: 20141020
;; Keywords: extensions, tools
;; Package-Requires: ()
;; URL: TBA
;; Doc URL: TBA
;; Compatibility: GNU Emacs: 24.x

;;; The MIT License:

;; http://en.wikipedia.org/wiki/MIT_License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;;; Note:

;;; Code:

(defun my/byte-compile ()
  "Byte compile my configs."
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))

(defun my/auto-modes (modes)
  "Add many MODES to `auto-mode-alist'."
  (setq auto-mode-alist (append modes auto-mode-alist))
  )

(defun my/global-set-keys (keybindings)
  "Set a bunch of global KEYBINDINGS at the same time."
  (dolist (binding keybindings)
    (let* ((key  (car binding))
           (func (cdr binding)))
      (global-set-key (kbd key) func))))

;;;###autoload
(defun my/mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but apply FN-HEAD to CAR and FN-REST to CDR of LIST."
  (if list (cons (funcall fn-head (car list))
                 (mapcar fn-rest (cdr list)))))

;;;###autoload
(defun my/split-name (s)
  "Split S by name."
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

;;;###autoload
(defun my/lower-camel-case (s)
  "Camel case S."
  (interactive)
  (let ((names (my/split-name s)))
    (concat (downcase (car names))
            (mapconcat 'capitalize (cdr names) ""))
    ))

;;;###autoload
(defun my/camel-case (s)
  "Camel case S."
  (mapconcat 'capitalize (my/split-name s) ""))

;;;###autoload
(defun my/snake-case (s)
  "Snake case S."
  (mapconcat 'downcase (my/split-name s) "_"))

;;;###autoload
(defun my/dash-case (s)
  "Dash case S."
  (mapconcat 'downcase (my/split-name s) "-"))

;;;###autoload
(defun my/toggle-programming-case (s)
  "Toggle programming style casing of S."
  (cond ((string-match-p "\\(?:[a-z]+_\\)+[a-z]+" s) (my/dash-case        s))
        ((string-match-p "\\(?:[a-z]+-\\)+[a-z]+" s) (my/camel-case       s))
        ((string-match-p "^\\(?:[A-Z][a-z]+\\)+"  s) (my/lower-camel-case s))
        (t                                           (my/snake-case       s)) ))

;;;###autoload
(defun my/toggle-programming-case-word-at-point ()
  "Toggle programming style casing of word a point."
  (interactive)
  (let* ((case-fold-search nil)
         (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
         (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
         (txt (buffer-substring beg end))
         (cml (my/toggle-programming-case txt)) )
    (if cml (progn (delete-region beg end) (insert cml))) ))

;;;###autoload
(defun my/define-keys (mode-map keybindings)
  "Set a bunch of MODE-MAP specific KEYBINDINGS at the same time."
  (dolist (binding keybindings)
    (let* ((key  (car binding))
           (func (cdr binding)))
      (define-key mode-map (kbd key) func))))

(defun my/insert-date (prefix)
  "Insert the current date in ISO extended format.
With PREFIX = 4, use ISO basic format.
With PREFIX = 16, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%Y%m%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "en_US"))
    (insert (format-time-string format))))

;;;###autoload
(defmacro my/bol-with-prefix (function)
  "Define a new function which call FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument.  The FUNCTION still receives the prefix argument."
  (let ((name (intern (format "my/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to BOL when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

;;;###autoload
(defun my/rtags-start ()
  "Start rdm if it isn't running."
  (interactive)
  (unless rtags-process (rtags-restart-process)))

;;;###autoload
(defun my/company-select-next-five ()
  "A bit more eager `company-select-next'."
  (interactive)
  (dotimes (number 5 nil) (company-select-next)))

;;;###autoload
(defun my/company-select-previous-five ()
  "A bit more eager `company-select-previous'."
  (interactive)
  (dotimes (number 5 nil) (company-select-previous)))

;;;###autoload
(defun my/yas-expand-nil ()
  "Perform a `yas-expand' but return nil if failure."
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

;;;###autoload
(defun my/tab-indent-or-complete ()
  "Tab indent or complete (using `company-mode') depending on context."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (let ((old-indent (current-indentation)))
      (indent-for-tab-command)
      (if (= old-indent (current-indentation))
          (if (or (not yas-minor-mode)
                  (null (my/yas-expand-nil)))
              (company-complete-common)
            ))
      )))

;;;###autoload
(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  "Use popup.el for yasnippet.  (PROMPT, CHOICES, DISPLAY-FN)."
  (require 'popup)
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

;;;###autoload
(defun my/wrap-in-comment (string)
  "Wrap STRING inside comment."
  (format "%s%s%s" comment-start string comment-end))

;;;###autoload
(defun my/toggle-comment ()
  "Comments or uncomments current region or line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;;;###autoload
(defun my/rename-current-buffer-and-file ()
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

;;;###autoload
(defun my/list-installed-packages ()
  "Like `package-list-packages', but show only installed optional packages."
  (interactive)
  (package-initialize)
  (package-show-package-list
   (cl-remove-if-not (lambda (x) (and (not (package-built-in-p x))
                                      (package-installed-p x)))
                     (mapcar 'car package-archive-contents))))

;;;###autoload
(defun my/set-proxy ()
  "Automatically set HTTP proxy in Emacs based on system environment."
  (interactive)
  (if (and (getenv "HTTP_PROXY") (getenv "HTTPS_PROXY"))
      (setq-default url-proxy-services '(("http"  . (getenv "HTTP_PROXY"))
                                         ("https" . (getenv "HTTPS_PROXY"))
                                         ))))

(provide 'funcs)
;;; funcs.el ends here
