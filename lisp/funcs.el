;;; funcs.el --- Some functions and macros I use.    -*- lexical-binding: t; -*-

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

;;;###autoload
(defun my/maximize ()
  "Maximize Emacs."
  (interactive)
  (when (display-graphic-p)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))))

;;;###autoload
(defun my/shorten-minor-modes (modes)
  "Shorten the displayed name for MODES in the modeline."
  (dolist (mode-and-line modes)
    (let ((line (cdr mode-and-line))
          (mode (car mode-and-line)))
      (my/shorten-minor-mode mode line))))

;;;###autoload
(defun my/shorten-minor-mode (mode line)
  "Replace the displayed name for MODE by LINE."
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook (lambda () (diminish mode line)))))

;;;###autoload
(defun my/shorten-major-modes (modes)
  "Shorten the displayed name for MODES in the modeline."
  (dolist (mode-and-line modes)
    (let ((line (cdr mode-and-line))
          (mode (car mode-and-line)))
      (my/shorten-major-mode mode line))))

;;;###autoload
(defun my/shorten-major-mode (mode line)
  "Replace the displayed name for MODE by LINE."
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook (lambda () (setq-local mode-name line)))))

;;;###autoload
(defun my/byte-compile ()
  "Byte compile my configs."
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))

;;;###autoload
(defun my/auto-modes (modes)
  "Add many MODES to `auto-mode-alist'."
  (setq auto-mode-alist (append modes auto-mode-alist))
  )

;;;###autoload
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
(defun my/preceding-char-match-p (pattern)
  "Match preceding char with PATTERN."
  (let ((str (string (preceding-char))))
    (string-match-p pattern str)))

;;;###autoload
(defun my/following-char-match-p (pattern)
  "Match following char with PATTERN."
  (let ((str (string (following-char))))
    (string-match-p pattern str)))

;;;###autoload
(defun my/define-keys (mode-map keybindings)
  "Set a bunch of MODE-MAP specific KEYBINDINGS at the same time."
  (dolist (binding keybindings)
    (let* ((key  (car binding))
           (func (cdr binding)))
      (define-key mode-map (kbd key) func))))

(defvar my/time-formats '("%Y%m%d" "%Y-%m-%d" "%A, %d. %B %Y"))

(defun my/get-date (format)
  "Get the current date in FORMAT."
  (let ((system-time-locale "en_US"))
    (format-time-string format)))

(defun my/get-year ()
  "Get the curret year."
  (my/get-date "%Y"))

(defun my/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(defun my/yas-choose-license ()
  "Choose a license to expand."
  (yas-choose-value
   (directory-files "~/.emacs.d/licenses/"
                    nil
                    "^[A-Za-z0-9-+_][A-Za-z0-9-+_.]*$")))

(defun my/get-user-mail-address ()
  "Get variable `user-mail-address' with fallback."
  (if (boundp 'user-mail-address)
      user-mail-address
    "user@example.com"))

(defun my/get-user-full-name ()
  "Get variable `user-full-name' with fallback."
  (if (boundp 'user-full-name)
      user-full-name
    "Full Name"))

(defun my/dot-and-complete ()
  "Quicker auto-complete on objects and structs."
  (interactive)
  (my/char-and-complete "."))

(defun my/slash-and-complete ()
  "Quicker auto-complete in lisp-code."
  (interactive)
  (my/char-and-complete "/"))

(defun my/dash-and-complete ()
  "Quicker auto-complete in lisp-code."
  (interactive)
  (my/char-and-complete "-"))


(defun my/char-and-complete (char)
  "Insert CHAR and complete."
  (interactive)
  (progn
    (insert char)
    (company-complete-common)))

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
(defun my/ido-select-next-nine ()
  "A bit more eager `ido-next-match'."
  (interactive)
  (dotimes (_i 9 nil) (ido-next-match)))

;;;###autoload
(defun my/ido-select-prev-nine ()
  "A bit more eager `ido-prev-match'."
  (interactive)
  (dotimes (_i 9 nil) (ido-prev-match)))

;;;###autoload
(defun my/company-select-next-nine ()
  "A bit more eager `company-select-next'."
  (interactive)
  (dotimes (_i 9 nil) (company-select-next)))

;;;###autoload
(defun my/company-select-previous-nine ()
  "A bit more eager `company-select-previous'."
  (interactive)
  (dotimes (_i 9 nil) (company-select-previous)))

;;;###autoload
(defun my/yas-expand ()
  "Perform a `yas-expand' but return nil on failure."
  (if (not (yas-minor-mode)) nil
    (let ((yas-fallback-behavior 'return-nil))
      (yas-expand))))

(defun my/yas-insert-or-expand ()
  "Insert snippet from menu or expand the snippet at point."
  (interactive)
  (unless (my/yas-expand) (yas-insert-snippet)))

;;;###autoload
(defun my/indent-snippet-or-complete ()
  "Tab indent, insert snippet or complete (using `company-mode')
depending on context."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (let ((old-indent (current-indentation)))
      (indent-for-tab-command)
      (if (and (= old-indent (current-indentation))
               (my/preceding-char-match-p "[a-zA-Z\-\.\>\_\/\:]")
               (null (my/yas-expand)))
          (company-complete-common)))))

;;;###autoload
(defun my/indent-or-complete ()
  "Auto indent or complete (using `company-mode') depending on context."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (let ((old-indent (current-indentation)))
      (indent-for-tab-command)
      (if (and (= old-indent (current-indentation))
               (my/preceding-char-match-p "[a-zA-Z\-\.\>\_\/\:]"))
          (company-complete-common)))))


;;;###autoload
(defun my/snippet-or-complete ()
  "Insert snippet or complete (using `company-mode') depending on context."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (when (null (my/yas-expand))
      (company-complete-common))))

(defun my/complete ()
  "Complete (using `company-mode')."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (company-complete-common)))

;;;###autoload
(defun my/fci-turn-off (&rest _)
  "Turn off `fci-mode'."
  (when (boundp 'fci-mode)
    (turn-off-fci-mode)))

;;;###autoload
(defun my/fci-turn-on (&rest _)
  "Turn on `fci-mode'."
  (when (boundp 'fci-mode)
    (turn-on-fci-mode)))

;;;###autoload
(defun my/increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'ARG'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

;;;###autoload
(defun my/decrement-number-decimal (&optional arg)
  "Decrement the number forward from point by 'ARG'."
  (interactive "p*")
  (my/increment-number-decimal (if arg (- arg) -1)))

;;;###autoload
(defun my/restclient ()
  "Create a `restclient-mode' buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*REST*"))
  (restclient-mode)
  (insert "# -*- restclient -*-\n\n"))

;;;###autoload
(defun my/magit-mode-quit ()
  "Quit and kill magit-status window and frame."
  (interactive)
  (magit-mode-quit-window 4))

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
(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region BEG to END."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

;;;###autoload
(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

;;;###autoload
(defun my/occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (thing-at-point 'symbol))
        regexp-history)
  (call-interactively 'occur))

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
