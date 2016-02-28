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
  "Shorten the displayed name for MODES in the mode line."
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
  (setq auto-mode-alist (append modes auto-mode-alist)))

;;;###autoload
(defun my/global-set-keys (keybindings)
  "Set a bunch of global KEYBINDINGS at the same time."
  (dolist (binding keybindings)
    (let* ((key    (kbd (car binding)))
           (func   (cdr binding)))
      (if func (global-set-key key func)
        (global-unset-key key)))))

(defun my/global-remap-keys (mappings)
  "Remap a bunch of global keybindings defined in MAPPINGS."
  (dolist (mapping mappings)
    (let* ((old-func (car mapping))
           (new-func (cdr mapping)))
      (global-set-key [remap old-func] new-func))))

;;;###autoload
(defun my/mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but apply FN-HEAD to CAR and FN-REST to CDR of LIST."
  (cons (funcall fn-head (car list))
        (mapcar fn-rest (cdr list))))

(defun my/mapconcat-head (fn-head fn-rest list sep)
  "Like `mapconcat', but apply FN-HEAD to CAR and FN-REST to CDR of LIST.
Just like `mapconcat' the last argument (SEP) is used as separator."
  (mapconcat #'identity
             (my/mapcar-head fn-head fn-rest list)
             sep))

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
  (my/mapconcat-head 'downcase
                     'capitalize
                     (my/split-name s)
                     ""))

;;;###autoload
(defun my/camel-case (s)
  "Camel case S."
  (mapconcat #'capitalize (my/split-name s) ""))

;;;###autoload
(defun my/snake-case (s)
  "Snake case S."
  (mapconcat #'downcase (my/split-name s) "_"))

;;;###autoload
(defun my/dash-case (s)
  "Dash case S."
  (mapconcat #'downcase (my/split-name s) "-"))

;;;###autoload
(defun my/is-dash-case (s)
  "Return T if S is in dash-case."
  (let ((case-fold-search nil))
    (string-match-p "[a-z]+\\(?:-[a-z]+\\)+" s)))

;;;###autoload
(defun my/is-camel-case (s)
  "Return T if S is in camel-case."
  (let ((case-fold-search nil))
    (string-match-p "^\\(?:[A-Z][a-z]+\\)+"  s)))

;;;###autoload
(defun my/is-lower-camel-case (s)
  "Return T if S is in lower-camel-case."
  (let ((case-fold-search nil))
    (string-match-p "^[a-z]+\\(?:[A-Z][a-z]+\\)+"  s)))

;;;###autoload
  (defun my/is-snake-case (s)
    "Return T if S is in snake-case."
    (let ((case-fold-search nil))
      (string-match-p "^[a-z]+\\(?:_[a-z]+\\)+" s)))

;;;###autoload
(defun my/toggle-programming-case (s) ;; UP
  "Toggle programming style casing of S."
  (cond ((my/is-snake-case       s) (my/dash-case        s))
        ((my/is-dash-case        s) (my/camel-case       s))
        ((my/is-camel-case       s) (my/lower-camel-case s))
        ((my/is-lower-camel-case s) (my/snake-case       s))))

;;;###autoload
(defun my/toggle-programming-case-reverse (s)
  "Toggle programming style casing of S in reverse."
  (cond ((my/is-dash-case        s) (my/snake-case       s))
        ((my/is-snake-case       s) (my/lower-camel-case s))
        ((my/is-lower-camel-case s) (my/camel-case       s))
        ((my/is-camel-case       s) (my/dash-case        s))))

(defun my/toggle-programming-case-word-at-point ()
  "Toggle programming style casing of word a point."
  (interactive)
  (my/operate-on-thing-or-region 'symbol #'my/toggle-programming-case))

(defun my/toggle-programming-case-word-at-point-reverse ()
  "Toggle programming style casing of word a point.
In reverse."
  (interactive)
  (my/operate-on-thing-or-region 'symbol #'my/toggle-programming-case-reverse))

;;;###autoload
(defun my/operate-on-thing-or-region (thing fn)
  "Replace THING or region with the value of the function FN."
  (let (pos1 pos2 meat excerpt)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point thing))
            pos2 (cdr (bounds-of-thing-at-point thing))))
    (setq excerpt (buffer-substring-no-properties pos1 pos2))
    (setq meat (funcall fn excerpt))
    (delete-region pos1 pos2)
    (insert  meat)))

(defun my/calc-thing-at-point ()
  "Replace math expression at point or in region with it's value."
  (interactive)
  (my/operate-on-thing-or-region 'symbol #'calc-eval))

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

;;;###autoload
(defun my/dot-and-complete ()
  "Quicker auto-complete on objects and structs."
  (interactive)
  (my/char-and-complete ?.))

;;;###autoload
(defun my/double-colon-and-complete ()
  "Quicker auto-complete on namespaces and modules."
  (interactive)
  (my/char-and-complete ?: ?:))

;;;###autoload
(defun my/arrow-and-complete ()
  "Quicker auto-complete on namespaces and modules."
  (interactive)
  (my/char-and-complete ?> ?-))

;;;###autoload
(defun my/slash-and-complete ()
  "Quicker auto-complete in lisp-code."
  (interactive)
  (my/char-and-complete ?/))

;;;###autoload
(defun my/dash-and-complete ()
  "Quicker auto-complete in lisp-code."
  (interactive)
  (my/char-and-complete ?-))

;;;###autoload
(defun my/char-and-complete (char &optional prev)
  "Insert CHAR and complete if `preceding-char' is equal to PREV."
  (interactive)
  (let ((do-complete (if prev (char-equal prev (preceding-char)) t)))
    (progn
      (insert char)
      (when do-complete
        (company-complete-common)))))

(defun my/isearch-symbol-with-prefix (p)
  "Like isearch, unless prefix argument is provided.
With a prefix argument P, isearch for the symbol at point."
  (interactive "P")
  (let ((current-prefix-arg nil))
    (call-interactively
     (if p #'isearch-forward-symbol-at-point
       #'isearch-forward))))

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

(defun my/ido-visible-prospects ()
  "The number of visible prospects."
  (let* ((prospects-len (length ido-matches))
         (available-lines (1- (ffloor (* max-mini-window-height (frame-height)))))
         (dot-dot (< available-lines (max ido-max-prospects prospects-len))))
    (1- available-lines)))

;;;###autoload
(defun my/ido-scroll-down ()
  "A bit more eager `ido-next-match'."
  (interactive)
  (dotimes (_ (my/ido-visible-prospects) nil) (ido-next-match)))

;;;###autoload
(defun my/ido-scroll-up ()
  "A bit more eager `ido-prev-match'."
  (interactive)
  (dotimes (_ (my/ido-visible-prospects) nil) (ido-prev-match)))

(defun my/company-scroll-down ()
  "A bit more eager `company-select-next'."
  (interactive)
  (dotimes (_ (- company-tooltip-limit 1) nil) (company-select-next)))

;;;###autoload
(defun my/company-scroll-up ()
  "A bit more eager `company-select-previous'."
  (interactive)
  (dotimes (_ (- company-tooltip-limit 1) nil) (company-select-previous)))

;;;###autoload
(defun my/yas-expand ()
  "Perform a `yas-expand' but return nil on failure."
  (when (yas-minor-mode)
    (let ((yas-fallback-behavior 'return-nil))
      (yas-expand))))

;;;###autoload
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

;;;###autoload
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
(defun my/reopen-file-as-root ()
  "Re-open file the current buffer is visiting as root."
  (interactive)
  (when buffer-file-name
    (unless (file-writable-p buffer-file-name)
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))))

;;;###autoload
(defun my/decrement-number-decimal (&optional arg)
  "Decrement the number forward from point by 'ARG'."
  (interactive "p*")
  (my/increment-number-decimal (if arg (- arg) -1)))

;;;###autoload
(defun my/create-non-existent-directory ()
  "Offer to create parent directory for current buffer if it doesn't exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                                 parent-directory)))
      (make-directory parent-directory t))))

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
(defun my/aim-new-block (mode control-stmts &optional char-tokens)
  "Does this line suggest a new block in MODE.
CONTROL-STMTS is a list of new block introducing control statements.
The optional parameter CHAR-TOKENS is a list of block introducing char tokens."
  (let* ((control-stmt-regex (concat "\\b\\("
                                     (mapconcat #'identity control-stmts "\\|")
                                     "\\)\\b"))
         (char-tokens (or char-tokens "[;{}]"))
         (complete-regex (concat "\\("
                                 "[" char-tokens "]"
                                 "\\|"
                                 control-stmt-regex
                                 "\\)")))
    (and (derived-mode-p mode)
         (null (string-match complete-regex (thing-at-point 'line))))))

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
  (call-interactively #'occur))

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
(defun my/control-mode-off ()
  "Turn off `control-mode'."
  (interactive)
  (control-mode 0))

;;;###autoload
(defun my/control-mode-on ()
  "Turn on `control-mode'."
  (interactive)
  (control-mode 1))

;;;###autoload
(defvar control-mode)
(defun my/control-mode-toggle ()
  "Toggle `control-mode'."
  (interactive)
  (if control-mode
      (control-mode 0)
    (control-mode 1)))

;;;###autoload
(defun my/focus-buffer-dwim (buffer)
  "Switch to BUFFER in other window unless it's currently in view."
  (unless (string-equal buffer (buffer-name (current-buffer)))
    (switch-to-buffer-other-window buffer)))

(defun my/advice-describe-func (function)
  "Advice FUNCTION to switch to the *Help* buffer after popping it up."
  (advice-add function :after (lambda (&rest _) (my/focus-buffer-dwim "*Help*"))))

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
                     (mapcar #'car package-archive-contents))))

;;;###autoload
(defun my/set-proxy ()
  "Automatically set HTTP proxy in Emacs based on system environment."
  (interactive)
  (if (and (getenv "HTTP_PROXY") (getenv "HTTPS_PROXY"))
      (setq-default url-proxy-services '(("http"  . (getenv "HTTP_PROXY"))
                                         ("https" . (getenv "HTTPS_PROXY"))
                                         ))))
;;;###autoload
(defun my/term-paste (&optional string)
  "Paste STRING into a term-buffer."
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

;; Taken from here:
;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
;;;###autoload
(defun my/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev.  Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer.  You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (thing-at-point 'word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word))
      (setq aft (thing-at-point 'word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq-default abbrev-mode t)

(provide 'funcs)
;;; funcs.el ends here
