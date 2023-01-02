;;; mb-cmd.el --- My commands -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2022, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20170305
;; Keywords         : tools
;; Package-Requires : ((emacs "27.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 27.x

;;; Commentary:

;; My commands.

;;; Note:

;;; Code:

(require 'mb-loadpaths)
(require 'mb-f)

;;;###autoload
(defun mb-cmd-byte-compile ()
  "Byte compile my configs."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

;;;###autoload
(defun mb-cmd-calc-thing-at-point ()
  "Replace math expression at point or in region with it's value."
  (interactive)
  (mb-f-operate-on-thing-or-region 'symbol #'calc-eval))

;;;###autoload
(defun mb-cmd-isearch-forward-symbol-with-prefix (p)
  "Like function `isearch-forward', unless prefix argument is provided.
With a prefix argument P, isearch for the symbol at point."
  (interactive "P")
  (let ((current-prefix-arg nil))
    (call-interactively
     (if p #'isearch-forward-symbol-at-point
       #'isearch-forward))))

;;;###autoload
(defun mb-cmd-isearch-backward-symbol (&optional _not-symbol no-recursive-edit)
  "Do incremental search forward for a symbol.
The prefix argument is currently unused.
Like ordinary incremental search except that your input is treated
as a symbol surrounded by symbol boundary constructs \\_< and \\_>.
See the command `isearch-forward' for more information.

Disable rescursive edit when NO-RECURSIVE-EDIT is nil."
  (interactive "P\np")
  (isearch-mode nil nil nil (not no-recursive-edit) 'isearch-symbol-regexp))

;;;###autoload
(defun mb-cmd-isearch-backward-symbol-at-point (&rest _)
  "Do incremental search backward for a symbol found near point.
Like ordinary incremental search except that the symbol found at point
is added to the search string initially as a regexp surrounded
by symbol boundary constructs \\_< and \\_>.
See the command `isearch-backward-symbol' for more information."
  (interactive)
  (mb-cmd-isearch-backward-symbol nil 1)
  (let ((bounds (find-tag-default-bounds)))
    (cond
     (bounds
      (when (< (car bounds) (point))
        (goto-char (car bounds)))
      (isearch-yank-string
       (buffer-substring-no-properties (car bounds) (cdr bounds))))
     (t
      (setq isearch-error "No symbol at point")
      (isearch-update)))))

;;;###autoload
(defun mb-cmd-isearch-backward-symbol-with-prefix (p)
  "Like function `isearch-backward', unless prefix argument is provided.
With a prefix argument P, isearch for the symbol at point."
  (interactive "P")
  (let ((current-prefix-arg nil))
    (call-interactively
     (if p #'mb-cmd-isearch-backward-symbol-at-point
       #'isearch-backward))))

;;;###autoload
(defun mb-cmd-restclient ()
  "Create a `restclient-mode' buffer."
  (interactive)
  (mb-f-req 'restclient)
  (switch-to-buffer (get-buffer-create "*REST*"))
  (restclient-mode)
  (insert "# -*- restclient -*-\n\n"))

;;;###autoload
(defun mb-cmd-uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region BEG to END."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

;;;###autoload
(defun mb-cmd-uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (mb-cmd-uniquify-region-lines (point-min) (point-max)))

;;;###autoload
(defun mb-cmd-occur-dwim ()
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
(defun mb-cmd-toggle-comment ()
  "Comments or uncomments current region or line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;;;###autoload
(defun mb-cmd-rename-current-buffer-and-file ()
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
(defun mb-cmd-term-paste (&optional string)
  "Paste STRING into a term-buffer."
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

;;;###autoload
(defun mb-cmd-shell-command-dwim (command
                                  &optional
                                  output-buffer
                                  replace
                                  error-buffer
                                  display-error-buffer)
  "Like `shell-command-on-region' but infer START and END from context.
COMMAND, OUTPUT-BUFFER, REPLACE, ERROR-BUFFER and DISPLAY-ERROR-BUFFER are just
passed on unchanged."
  (interactive "sShell command: ")
  (shell-command-on-region (if (region-active-p) (region-beginning) (point-min))
                           (if (region-active-p) (region-end)       (point-max))
                           command
                           output-buffer
                           replace
                           error-buffer
                           display-error-buffer))

;;;###autoload
(defun mb-cmd-yaml2json-dwim ()
  "Convert the current region or all to json from yaml."
  (interactive)
  (mb-cmd-shell-command-dwim "yaml2json --preserve-key-order --indent-json 4"
                             nil t))

;;;###autoload
(defun mb-cmd-json2yaml-dwim ()
  "Convert the current region or all to yaml from json."
  (interactive)
  (mb-cmd-shell-command-dwim "json2yaml --preserve-key-order"
                             nil t))

;; TODO: Save URL in kill-ring
;;;###autoload
(defun mb-cmd-fpaste-dwim ()
  "Push the current region or buffer to paste.fedoraproject.org."
  (interactive)
  (mb-cmd-shell-command-dwim (format "fpaste -n %s" (user-login-name))))

;; TODO: Make this work
;;;###autoload
(defun mb-cmd-fpaste-insert (url)
  "Fetch an fpaste from URL."
  (interactive "sFPaste URL: ")
  (let* ((url (format "%s/raw" url))
         (cmd (format "curl \"%s\" 2>/dev/null" url))
         (err-buf "*Shell Command Error*"))
    (shell-command cmd t err-buf)))

;;;###autoload
(defun mb-cmd-open-with (arg)
  ;; Taken from Prelude
  "Open visited file in default external program.
With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (let ((is-osx    (eq system-type 'darwin))
          (is-linux (member system-type '(gnu gnu/linux gnu/kfreebsd))))
      (shell-command (concat
                      (cond
                       ((and (not arg) is-osx)   "open")
                       ((and (not arg) is-linux) "xdg-open")
                       (t (read-shell-command "Open current file with: ")))
                      " "
                      (shell-quote-argument buffer-file-name))))))

;;;###autoload
;; From: http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun mb-cmd-fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'mb-cmd-fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

;;;###autoload
(defun mb-cmd-projectile-ripgrep-regex (search-term)
  "Regex search for SEARCH-TERM with ripgrep."
  (interactive
   (list (projectile--read-search-string-with-default
          (format "Ripgrep %ssearch for" (if current-prefix-arg "regexp " "")))
         '(4)))
  (mb-f-req 'projectile)
  (apply #'projectile-ripgrep search-term '(4)))

;;;###autoload
(defun mb-cmd-projectile-index-projects ()
  "Index my project directories."
  (interactive)

  (mapc #'projectile-add-known-project
        (mb-f-find-git-projects "~/" 5))

  (projectile-cleanup-known-projects))

;;;###autoload
(defun mb-cmd-projectile-gitg ()
  "Run gitg at root of project."
  (interactive)
  (mb-cmd-projectile-spawn "/bin/gitg"))

;;;###autoload
(defun mb-cmd-projectile-terminal ()
  "Run gitg at root of project."
  (interactive)
  (mb-cmd-projectile-spawn "/usr/bin/kgx"))

;;;###autoload
(defun mb-cmd-projectile-spawn (command)
  "Start an external COMMAND w/ `call-process' at the project root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-process command nil 0)))

;;;###autoload
(defun mb-cmd-projectile-ansi-term ()
  "Start `ansi-term' in the project root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (ansi-term (getenv "SHELL")
               (format "*ansi-term [%s]*" (projectile-project-name)))))

;;;###autoload
(defun mb-cmd-flyspell-goto-previous-error ()
  "Go to ARG previous spelling error."
  (interactive)
  (mb-f-req 'flyspell)
  (let ((arg 1))
    (while (not (= 0 arg))
      (let ((pos (point))
            (min (point-min)))
        (if (and (eq (current-buffer) flyspell-old-buffer-error)
                 (eq pos flyspell-old-pos-error))
            (progn
              (if (= flyspell-old-pos-error min)
                  ;; goto beginning of buffer
                  (progn
                    (message "Restarting from end of buffer")
                    (goto-char (point-max)))
                (backward-word 1))
              (setq pos (point))))
        ;; seek the next error
        (while (and (> pos min)
                    (let ((ovs (overlays-at pos))
                          (r '()))
                      (while (and (not r) (consp ovs))
                        (if (flyspell-overlay-p (car ovs))
                            (setq r t)
                          (setq ovs (cdr ovs))))
                      (not r)))
          (backward-word 1)
          (setq pos (point)))
        ;; save the current location for next invocation
        (setq arg (1- arg))
        (setq flyspell-old-pos-error pos)
        (setq flyspell-old-buffer-error (current-buffer))
        (goto-char pos)
        (if (= pos min)
            (progn
              (message "No more miss-spelled word!")
              (setq arg 0)))))))

;;;###autoload
(defun mb-cmd-iedit-in-defun ()
  "`iedit' restricted to current `defun'."
  (interactive)
  (mb-f-req 'iedit)
  (progn (iedit-mode)
         (iedit-restrict-function)))

;;;###autoload
(defun mb-cmd-find-file-default (&rest args)
  "Wrapped `find-file' that avoids ido and passes on ARGS."
  (interactive (advice-eval-interactive-spec
                (cadr (interactive-form #'find-file))))
  (apply #'find-file args))

;;;###autoload
(defun mb-cmd-split-window-right ()
  "Split window right, rebalance and switch."
  (interactive)
  (split-window-right)
  (balance-windows)
  (mb-f-other-window))

;;;###autoload
(defun mb-cmd-split-window-below ()
  "Split window below, rebalance and switch."
  (interactive)
  (split-window-below)
  (balance-windows)
  (mb-f-other-window))

;;;###autoload
(defun mb-cmd-delete-window ()
  "Delete window and rebalance."
  (interactive)
  (delete-window)
  (balance-windows))

;;;###autoload
(defun mb-cmd-kill-this-buffer ()
  "Kill this buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;;;###autoload
(defun mb-cmd-exec-terminal ()
  "Open my tasks."
  (interactive)
  (call-process-shell-command "kgx&" nil 0))

;;;###autoload
(defun mb-cmd-exec-nautilus ()
  "Open my tasks."
  (interactive)
  (call-process-shell-command "nautilus . &" nil 0))

;;;###autoload
(defun mb-cmd-elisp-fill-function-arguments ()
  "Wrap `fill-function-arguments-dwim' with ELisp special casing."
  (interactive)
  (mb-f-req 'fill-function-arguments)
  (let* ((tap-defun (if (function-called-at-point) t nil))
         (fill-function-arguments-second-argument-same-line tap-defun)
         (fill-function-arguments-first-argument-same-line t)
         (fill-function-arguments-last-argument-same-line t)
         (fill-function-arguments-argument-separator " "))
    (call-interactively #'fill-function-arguments-dwim)))

;;;###autoload
(defun mb-cmd-git-copy-url ()
  "Open current line in a browser if origin is at a known forge."
  (interactive)
  (mb-f-req 'git-link)
  (let* ((git-link-open-in-browser nil))
    (call-interactively #'git-link)))

;;;###autoload
(defun mb-cmd-new-frame ()
  "Create a new frame opening the *scratch* buffer."
  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))

;;;###autoload
(defun mb-cmd-git-link-browse ()
  "Open current line in a browser if origin is at a known forge."
  (interactive)
  (let* ((git-link-open-in-browser t))
    (call-interactively #'git-link)))

;;;###autoload
(defun mb-cmd-markdown-jump ()
  "Markdown jump based on `markdown-do'.

Jumps between reference links and definitions; between footnote
markers and footnote text."
  (interactive)
  (mb-f-req 'markdown-mode)
  (require 'xref)

  (cond
   ;; Footnote definition
   ((markdown-footnote-text-positions)
    (markdown-footnote-return))
   ;; Footnote marker
   ((markdown-footnote-marker-positions)
    (markdown-footnote-goto-text))
   ;; Reference link
   ((thing-at-point-looking-at markdown-regex-link-reference)
    (markdown-reference-goto-definition))
   ;; Reference definition
   ((thing-at-point-looking-at markdown-regex-reference-definition)
    (markdown-reference-goto-link (match-string-no-properties 2)))
   (t nil)))


(defvar mb-cmd-forges '(("GitHub"   . "github")
                        ("GitLab"   . "gitlab")
                        ("SmartEye" . "smarteye")
                        ("GNOME"    . "gnome"))
  "Git forges.")

(defun mb-cmd--git-get-post-hook (buffer msg)
  "Reload projectile index on successful clone."
  (when (and (equal 'compilation-mode major-mode)
	     (string-match "*git-get*" (buffer-name buffer)))
    (remove-hook 'compilation-finish-functions
                 #'mb-cmd--git-get-post-hook)
    (when (and (string-match "finished" msg)
	       (not (search-forward "warning" nil t)))
      (call-interactively #'mb-cmd-projectile-index-projects))))

;;;###autoload
(defun mb-cmd-git-get (forge repository)
  "Use git-get to clone a REPOSITORY from FORGE."
  (interactive
   (list (cdr (assoc (completing-read "Forge: "
                                      mb-cmd-forges
                                      nil t)
                     mb-cmd-forges))
         (read-string "Repository: ")))
  (let ((compilation-buffer-name-function (lambda (_) "*git-get*"))
        (compilation-save-buffers-predicate (lambda () nil)))
    (ignore compilation-save-buffers-predicate)
    (add-hook 'compilation-finish-functions #'mb-cmd--git-get-post-hook)
    (compile (format "git get %s:%s" forge repository))
    (select-window (get-buffer-window "*git-get*"))))

(defun mb-cmd-describe-symbol ()
  "Describe symbol at point."
  (interactive)
  (describe-symbol (or (symbol-at-point)
                       (error "No symbol-at-point"))))

(defun mb-cmd-spdx-reuse-lint ()
  "Check project for SPDX license issues."
  (interactive)
  (let* ((buf-name "*spdx-reuse-lint*")
         (compilation-buffer-name-function (lambda (_) buf-name))
         (default-directory (projectile-project-root)))
    (compile "reuse lint")
    (select-window (get-buffer-window buf-name))))

(defun mb-cmd-spdx-reuse-download-all ()
  "Check project for SPDX license issues."
  (interactive)
  (let* ((buf-name "*spdx-reuse-download-all*")
         (compilation-buffer-name-function (lambda (_) buf-name))
         (default-directory (projectile-project-root)))
    (compile "reuse download --all")
    (select-window (get-buffer-window buf-name))))

;;;###autoload
(defun mb-cmd-visit-templates ()
  "Visit tempel templates file."
  (interactive)
  (find-file (format "%s/templates.eld" user-emacs-directory)))

(provide 'mb-cmd)
;;; mb-cmd.el ends here
