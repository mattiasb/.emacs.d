;;; mb-cmd.el --- My commands -*- lexical-binding: t; -*-

;; Copyright ⓒ 2017 Mattias Bengtsson
;;
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
;;
;; Author: Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Version          : 20170305
;; Keywords         : tools
;; Package-Requires : ((emacs "25.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 25.x

;;; Commentary:

;; My commands.

;;; Note:

;;; Code:

(require 'mb-f)
(require 'projectile)
(require 'ido)

(declare-function company-complete        "company.el")
(declare-function company-select-next     "company.el")
(declare-function company-select-previous "company.el")
(declare-function company-complete-common "company.el")

(declare-function iedit-restrict-function "iedit.el")
(declare-function flyspell-overlay-p      "flyspell.el")

;;;###autoload
(defun mb-cmd-byte-compile ()
  "Byte compile my configs."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

;;;###autoload
(defun mb-cmd-toggle-programming-case-word-at-point (&rest _)
  "Toggle programming style casing of word a point."
  (interactive)
  (mb-f-operate-on-thing-or-region 'symbol #'mb-f-toggle-programming-case))

;;;###autoload
(defun mb-cmd-toggle-programming-case-word-at-point-reverse (&rest _)
  "Toggle programming style casing of word a point.
In reverse."
  (interactive)
  (mb-f-operate-on-thing-or-region 'symbol #'mb-f-toggle-programming-case-reverse))

;;;###autoload
(defun mb-cmd-calc-thing-at-point ()
  "Replace math expression at point or in region with it's value."
  (interactive)
  (mb-f-operate-on-thing-or-region 'symbol #'calc-eval))

;;;###autoload
(defun mb-cmd-dot-and-complete ()
  "Quicker auto-complete on objects and structs."
  (interactive)
  (mb-cmd-char-and-complete ?.))

;;;###autoload
(defun mb-cmd-double-colon-and-complete ()
  "Quicker auto-complete on namespaces and modules."
  (interactive)
  (mb-cmd-char-and-complete ?: ?:))

;;;###autoload
(defun mb-cmd-arrow-and-complete ()
  "Quicker auto-complete on namespaces and modules."
  (interactive)
  (mb-cmd-char-and-complete ?> ?-))

;;;###autoload
(defun mb-cmd-slash-and-complete ()
  "Quicker auto-complete in lisp-code."
  (interactive)
  (mb-cmd-char-and-complete ?/))

;;;###autoload
(defun mb-cmd-dash-and-complete ()
  "Quicker auto-complete in lisp-code."
  (interactive)
  (mb-cmd-char-and-complete ?-))

;;;###autoload
(defun mb-cmd-char-and-complete (char &optional prev)
  "Insert CHAR and complete if `preceding-char' is equal to PREV."
  (interactive)
  (let ((do-complete (if prev (char-equal prev (preceding-char)) t)))
    (progn
      (insert char)
      (when do-complete
        (company-complete)))))

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
(defun mb-cmd-ido-scroll-down ()
  "A bit more eager `ido-next-match'."
  (interactive)
  (dotimes (_ (mb-f-ido-visible-prospects) nil) (ido-next-match)))

;;;###autoload
(defun mb-cmd-ido-scroll-up ()
  "A bit more eager `ido-prev-match'."
  (interactive)
  (dotimes (_ (mb-f-ido-visible-prospects) nil) (ido-prev-match)))

(defvar company-tooltip-limit)
;;;###autoload
(defun mb-cmd-company-scroll-down ()
  "A bit more eager `company-select-next'."
  (interactive)
  (dotimes (_ (- company-tooltip-limit 1) nil) (company-select-next)))

;;;###autoload
(defun mb-cmd-company-scroll-up ()
  "A bit more eager `company-select-previous'."
  (interactive)
  (dotimes (_ (- company-tooltip-limit 1) nil) (company-select-previous)))

;;;###autoload
(defun mb-cmd-snippet-complete-or-indent ()
  "Insert snippet, complete (using `company-mode') or indent.
Useful in Python and YAML files."
  (interactive)
  (unless (or (yas-expand)
              (and (mb-f-preceding-char-match-p "[a-zA-Z\-\.\>\_\/\:]")
                   (company-complete)))
    (call-interactively #'indent-for-tab-command)))

;;;###autoload
(defun mb-cmd-indent-snippet-or-complete ()
  "Tab indent, insert snippet or complete (using `company-mode')
depending on context."
  (interactive)
  (let ((old-indent (current-indentation)))
    (call-interactively #'indent-for-tab-command)
    (when (and (= old-indent (current-indentation))
               (mb-f-preceding-char-match-p "[a-zA-Z\-\.\>\_\/\:]")
               (null (yas-expand)))
      (company-complete))))

;;;###autoload
(defun mb-cmd-indent-or-complete ()
  "Auto indent or complete (using `company-mode') depending on context."
  (interactive)
  (let ((old-indent (current-indentation)))
    (call-interactively #'indent-for-tab-command)
    (when (and (= old-indent (current-indentation))
               (mb-f-preceding-char-match-p "[a-zA-Z\-\.\>\_\/\:]"))
      (company-complete))))

;;;###autoload
(defun mb-cmd-snippet-or-complete ()
  "Insert snippet or complete (using `company-mode') depending on context."
  (interactive)
  (when (and (mb-f-preceding-char-match-p "[a-zA-Z\-\.\>\_\/\:]")
             (null (yas-expand)))
    (company-complete)))

;;;###autoload
(defun mb-cmd-restclient ()
  "Create a `restclient-mode' buffer."
  (interactive)
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
(defun mb-cmd-control-mode-off ()
  "Turn off `control-mode'."
  (interactive)
  (global-control-mode 0))

;;;###autoload
(defun mb-cmd-control-mode-on ()
  "Turn on `control-mode'."
  (interactive)
  (global-control-mode 1))

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

;; Taken from here:
;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
;;;###autoload
(defun mb-cmd-ispell-word-then-abbrev (p)
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

;;;###autoload
(defun mb-cmd-shell-command-dwim (command &optional
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
(defun mb-cmd-guess-cc-mode ()
  "Guess whether to activate `c-mode' or `c++-mode' for a .h-file."
  (interactive)
  (let ((c-file (concat (substring (buffer-file-name) 0 -1) "c")))
    (if (file-exists-p c-file)
        (c-mode)
      (c++-mode))))

;;;###autoload
;; From: http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun mb-cmd-fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

;;;###autoload
(defun mb-cmd-projectile-ag-regex (search-term)
  "Regex search for SEARCH-TERM with ag."
  (interactive
   (list (projectile--read-search-string-with-default
          "Ag regexp search for")))
  (apply #'projectile-ag search-term '(4)))

;;;###autoload
(defun mb-cmd-projectile-regen-rtags ()
  "Update rtags for current project."
  (interactive)
  (if (eq (projectile-project-type) 'jhbuild)
      (mb-f-projectile-regen-rtags-jhbuild)
    (cond ((mb-f-projectile-meson-p)
           (mb-f-projectile-regen-rtags-meson))
          ((mb-f-projectile-cmake-p)
           (mb-f-projectile-regen-rtags-cmake))
          (t (message "Unsupported build system!")))))

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
(defun mb-cmd-projectile-gnome-terminal ()
  "Run gitg at root of project."
  (interactive)
  (mb-cmd-projectile-spawn "/usr/bin/gnome-terminal"))

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

(defvar flyspell-old-buffer-error)
(defvar flyspell-old-pos-error)
;;;###autoload
(defun mb-cmd-flyspell-goto-previous-error ()
  "Go to ARG previous spelling error."
  (interactive)
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

(defvar mb-cmd-realgud-debugger
  (lambda ()
    (interactive)
    (error "No debugger for this mode")))

;;;###autoload
(defun mb-cmd-realgud-debug ()
  "Run a `realgud' debugger."
  (interactive)
  (require 'realgud)
  (call-interactively mb-cmd-realgud-debugger))

;;;###autoload
(defun mb-cmd-iedit-in-defun ()
  "`iedit' restricted to current `defun'."
  (interactive)
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
(defun mb-cmd-git-commit-insert-issue-fix ()
  "Insert an issue fix into a git commit message."
  (interactive)
  (insert "Fixes: ")
  (call-interactively #'git-commit-insert-issue-ask-issues))

;;;###autoload
(defun mb-cmd-new-frame-with-scratch ()
  "Make a new frame with a new scratch buffer."
  (interactive)
  (defvar mb-cmd--scratch-counter 0)
  (incf mb-cmd--scratch-counter)
  (switch-to-buffer-other-frame (format "*scratch %s*"
                                        mb-cmd--scratch-counter)))

;;;###autoload
(defun mb-cmd-persp-clean ()
  "Create a new persp and remove all others."
  (interactive)
  (let ((p-new "P")
        (p-other (persp-names)))
    (persp-activate (persp-new p-new))
    (ignore-errors
      (mapcar #'persp-kill p-other))))

;;;###autoload
(defun mb-cmd-kill-this-buffer ()
  "Kill this buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;;;###autoload
(defun mb-cmd-open-journal ()
  "Open my journal."
  (interactive)
  (find-file-existing "~/Dropbox/pim/journal.txt")
  (table-recognize))

;;;###autoload
(defun mb-cmd-open-tasks ()
  "Open my tasks."
  (interactive)
  (find-file-existing "~/Dropbox/pim/todo/Tasks.md"))

(provide 'mb-cmd)
;;; mb-cmd.el ends here
