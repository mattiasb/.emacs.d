;;; mb-f.el --- Some functions and macros I use.    -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2022, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20170305
;; Keywords         : extensions, tools
;; Package-Requires : ((emacs "27.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 27.x

;;; Commentary:

;; My functions

;;; Note:

;;; Code:

(require 'mb-loadpaths)
(require 'package)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defmacro mb-f-req (s)
  `(eval-and-compile (require ,s)))

(defun mb-f-shorten-minor-modes (modes)
  "Shorten the displayed name for MODES in the modeline."
  (dolist (mode-and-line modes)
    (let ((line (cdr mode-and-line))
          (mode (car mode-and-line)))
      (mb-f-shorten-minor-mode mode line))))

(defun mb-f-shorten-minor-mode (mode line)
  "Replace the displayed name for MODE by LINE."
  (mb-f-req 'diminish)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook (lambda () (diminish mode line)))))

(defun mb-f-shorten-major-modes (modes)
  "Shorten the displayed name for MODES in the mode line."
  (dolist (mode-and-line modes)
    (let ((line (cdr mode-and-line))
          (mode (car mode-and-line)))
      (mb-f-shorten-major-mode mode line))))

(defun mb-f-shorten-major-mode (mode line)
  "Replace the displayed name for MODE by LINE."
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook (lambda () (setq-local mode-name line)))))

(defun mb-f-auto-modes (modes)
  "Add many MODES to `auto-mode-alist'."
  (setq auto-mode-alist (append modes auto-mode-alist)))

(defun mb-f-global-define-keys (keybindings)
  "Set a bunch of global KEYBINDINGS at the same time."
  (mb-f-define-keys (current-global-map)
                    keybindings))

(defun mb-f-global-remap-keys (mappings)
  "Remap a bunch of global keybindings defined in MAPPINGS."
  (mb-f-remap-keys (current-global-map) mappings))

(defun mb-f-define-keys (mode-map keybindings)
  "Set a bunch of MODE-MAP specific KEYBINDINGS at the same time."
  (dolist (binding keybindings)
    (let* ((key   (kbd (car binding)))
           (def   (cdr binding))
           (def-v (when (boundp def) (symbol-value def))))
      (if (or (fboundp def)
              (not (keymapp def-v)))
          (define-key mode-map key def)
        (define-key mode-map key (quote def-v))))))

(defun mb-f-remap-keys (mode-map mappings)
  "Remap a bunch of MODE-MAP keybindings defined in MAPPINGS."
  (dolist (mapping mappings)
    (let* ((key        (car mapping))
           (value      (cdr mapping))
           (func-remap (and (functionp key) (functionp value)))
           (key-remap  (and (stringp key) (stringp value))))
      (cond (key-remap (define-key mode-map
                         (kbd key)
                         (key-binding (kbd value))))
            (func-remap (substitute-key-definition (car mapping)
                                                   (cdr mapping)
                                                   mode-map))))))

(defmacro mb-f-define-keymap (keymap bindings)
  "Define a new KEYMAP and add a bunch of BINDINGS."
  `(progn (defvar ,keymap
            (let ((map (make-sparse-keymap)))
              (mb-f-define-keys map ,bindings)
              map))
          (fset (quote ,keymap) ,keymap)))

(defun mb-f-mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but apply FN-HEAD to CAR and FN-REST to CDR of LIST."
  (cons (funcall fn-head (car list))
        (mapcar fn-rest (cdr list))))

(defun mb-f-mapconcat-head (fn-head fn-rest list sep)
  "Like `mapconcat', but apply FN-HEAD to CAR and FN-REST to CDR of LIST.
Just like `mapconcat' the last argument (SEP) is used as separator."
  (mapconcat #'identity
             (mb-f-mapcar-head fn-head fn-rest list)
             sep))

(defun mb-f-split-name (s)
  "Split S by name."
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun mb-f-operate-on-thing-or-region (thing fn)
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

(defun mb-f-preceding-char-match-p (pattern)
  "Match preceding char with PATTERN."
  (let ((str (string (preceding-char))))
    (string-match-p pattern str)))

(defun mb-f-following-char-match-p (pattern)
  "Match following char with PATTERN."
  (let ((str (string (following-char))))
    (string-match-p pattern str)))

(defvar mb-f-time-formats '("%Y%m%d" "%Y-%m-%d" "%A, %d. %B %Y"))

(defun mb-f-get-date (format)
  "Get the current date in FORMAT."
  (let ((system-time-locale "en_US"))
    (format-time-string format)))

(defun mb-f-get-year ()
  "Get the curret year."
  (mb-f-get-date "%Y"))

(defun mb-f-get-user-mail-address ()
  "Get variable `user-mail-address' with fallback."
  (if (boundp 'user-mail-address)
      user-mail-address
    "user@example.com"))

(defun mb-f-get-user-full-name ()
  "Get variable `user-full-name' with fallback."
  (if (boundp 'user-full-name)
      user-full-name
    "Full Name"))

(defun mb-f-get-user-mailbox ()
  "Get RFC5322 formatted mailbox of user."
  (format "%s <%s>"
          (mb-f-get-user-full-name)
          (mb-f-get-user-mail-address)))

(defun mb-f-create-non-existent-directory ()
  "Offer to create parent directory for current buffer if it doesn't exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it? "
                                 parent-directory)))
      (make-directory parent-directory t))))

(defun mb-f-wrap-in-comment (string)
  "Wrap STRING inside comment."
  (format "%s%s%s" comment-start string comment-end))

(defun mb-f-comment-start ()
  "Return comment-start."
  (cond ((or (derived-mode-p 'lisp-mode)
             (derived-mode-p 'lisp-data-mode))
         ";; ")
        ((null comment-start) "")
        (t comment-start)))

(defun mb-f-comment-end ()
  "Return comment-end."
  (cond ((null comment-end) "")
        (t comment-end)))

(defun mb-f-get-monitor-dpi (monitor-attributes)
  "Calculate DPI from a MONITOR-ATTRIBUTES structure."
  (let ((pixel-width (nth 3 (alist-get 'geometry monitor-attributes)))
        (mm-width (nth 1 (alist-get 'mm-size monitor-attributes))))
    (if (and pixel-width mm-width)
        (round (/ pixel-width (/ mm-width 25.4)))
      ;; Default to 90 when we don't have any monitors
      90)))

(defun mb-f-get-monitor-dpis ()
  "Get DPI for all monitors."
  (mapcar #'mb-f-get-monitor-dpi (display-monitor-attributes-list)))

(defun mb-f-hidpi-p (dpi)
  "Return non-nil if DPI is ≥ 213."
  (>= dpi 213))

(defun mb-f-other-window (&rest _args)
  "Like `(other-window 1)' but skip all arguments."
  (other-window 1))

(defun mb-f-doom-modeline-set-height ()
  "Set doon-modeline height based on DPI."
  (mb-f-req 'doom-modeline)
  (if (seq-every-p #'mb-f-hidpi-p (mb-f-get-monitor-dpis))
      (setq doom-modeline-height 65)
    (setq doom-modeline-height 25)))

(defun mb-f-reset-terminal-cursors ()
  "Reset the terminal cursor."
  (send-string-to-terminal "\033]12;white\007\e[1 q"))

(defun mb-f-set-terminal-cursors ()
  "Set the terminal cursor."
  (send-string-to-terminal (concat "\033]12;"
                                   (face-background 'cursor)
                                   "\007")))

;; This doesn't seem to be needed anymore. But keep it as a reference if it
;; turns out I'm wrong and just using (kill-emacs) isn't enough.
(defun mb-f-kill-server ()
  "Kill the server daemon as gracefully as possible."
  (save-some-buffers t)

  ;; Maybe this will make quitting Emacs work better?
  (dotimes (_ 5) (keyboard-quit))

  (cl-flet ((process-list ()))
    (kill-emacs)))

(defun mb-f-set-terminal-window-separator ()
  "Set a unicode terminal window separator character."
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (let ((display-table (or buffer-display-table
                                       standard-display-table)))
                (set-display-table-slot display-table 5 ?│)))))


;; Taken from: https://emacs.stackexchange.com/a/38760
(defun mb-f-projectile-relative-buf-name ()
  "Rename buffers to include relative path to project.

Usage: (add-hook 'find-file-hook #'mb-f-projectile-relative-buf-name)"
  (mb-f-req 'projectile)
  (ignore-errors
    (rename-buffer
     (file-relative-name buffer-file-name (projectile-project-root)))))

(defun mb-f-projectile-autotools-p (&optional only)
  "Predicate that determines if current project is an autotools project.
Optionally return t ONLY if this project also isn't a Meson or CMake project."
  (let ((autotools (file-exists-p
                    (concat (projectile-project-root) "configure.ac"))))
    (or (and (not only) autotools)
        (and only autotools
             (not (mb-f-projectile-meson-p))
             (not (mb-f-projectile-cmake-p))))))

(defun mb-f-projectile-meson-p ()
  "Predicate that determines if current project is a Meson project."
  (file-exists-p (concat (projectile-project-root) "meson.build")))

(defun mb-f-projectile-cmake-p ()
  "Predicate that determines if current project is a CMake project."
  (file-exists-p (concat (projectile-project-root) "CMakeLists.txt")))

(defun mb-f-find-git-projects (dir &optional depth)
  "Find all git projects under DIR.
Optionally only search as deep as DEPTH."
  (let* ((cmd (format "find %s %s %s -or %s"
                      dir
                      (if depth (format "-maxdepth %d" depth) "")
                      "-name '.git' -type d"
                      "-type f -name '.projectile'"))
         (result (split-string (shell-command-to-string cmd))))
    (mapcar #'file-name-directory result)))

(defun mb-f-enclosing-paren ()
  "Return the opening paren type we're currently enclosed by or nil."
  (let ((ppss (syntax-ppss)))
    (when (nth 1 ppss)
      (char-after (nth 1 ppss)))))

(defun mb-f-elisp-get-namespace ()
  "Get the namespace of the current module."
  (if (and (derived-mode-p 'emacs-lisp-mode)
           (string= (file-name-extension (buffer-name)) "el"))
      (let ((base (file-name-base (buffer-name))))
        (if (string-suffix-p "-mode" base)
            (substring base 0 -5)
          base))
    ""))

(defun mb-f-python-electric-newline ()
  "Electric newline for Python."
  (let ((paren (mb-f-enclosing-paren)))
    (if (not (or (eq paren ?\{)
                 (eq paren ?\[)
                 (eq paren ?\()
                 (looking-back "\\blambda\\b.*" nil nil)))
        'after
      nil)))

(defun mb-f-executable-make-buffer-file-executable-if-script-p ()
  "Make file executable according to umask.

Version of `executable-make-buffer-file-executable-if-script-p'
that checks that the hash-bang seems to involve a path."
  (when (save-restriction
          (widen)
          (string= "#!/" (buffer-substring (point-min)
                                           (+ 3 (point-min)))))
    (executable-make-buffer-file-executable-if-script-p)))

(defun mb-f-electric-pairs (pairs &optional remove)
  "Add Electric Pair Mode PAIRS for current buffer."
  (if remove (mapc #'kill-local-variable
                   '(electric-pair-pairs electric-pair-text-pairs))
    (setq-local electric-pair-pairs (append electric-pair-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs)))

(defun mb-f-company-to-capf (capf)
  (mb-f-req 'cape)
  (let ((name (intern (format "%s-capf" (symbol-name capf)))))
    (defalias name (cape-company-to-capf capf))
    name))

(defun mb-f-super-capf (&rest capfs)
  (mb-f-req 'cape)
  (mb-f-req 'tempel)
  (let ((name (intern (format "mb-capf:%s"
                              (mapconcat #'symbol-name capfs "+")))))
    (defalias name (apply #'cape-super-capf capfs))
    name))

(defun mb-f-set-capfs (&rest capfs)
  "Create a completion-at-point-functions."
  (require 'cape)

  (setq-local completion-at-point-functions (list #'tempel-expand
                                                  (apply #'mb-f-super-capf capfs)
                                                  #'cape-file)))

(defun mb-f-set-dark-wm-theme (frame)
  "Set the dark theme variant for FRAME.

Based on: http://www.whiz.se/2016/05/01/dark-theme-in-emacs/"
  (select-frame frame)
  (when (and (display-graphic-p)
             (file-exists-p "/usr/bin/xprop"))
    (call-process "xprop"
                  nil
                  nil
                  nil
                  "-f"
                  "_GTK_THEME_VARIANT"
                  "8u"
                  "-set"
                  "_GTK_THEME_VARIANT"
                  "dark"
                  "-id"
                  (frame-parameter frame 'outer-window-id))))

(defun mb-f-no-confirm (fun &rest args)
  "Apply FUN to ARGS, skipping user confirmations."
  (cl-flet ((always-yes (&rest _) t))
    (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
              ((symbol-function 'yes-or-no-p) #'always-yes))
      (apply fun args))))

(defun mb-f-package-local-packages (&optional with-paths)
  "List of all packages under packages/.  Optionally WITH-PATHS."
  (let* ((directory (concat user-emacs-directory "packages/"))
         (filter   "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
    (if with-paths
        (directory-files directory t filter)
      (seq-map #'intern (directory-files directory nil filter)))))

(defun mb-f-package-local-not-installed-packages ()
  "List of all not installed packages under packages/."
  (seq-remove #'package-installed-p (mb-f-package-local-packages)))

(defun mb-f-package-remote-not-installed-packages ()
  "List of all remote not installed packages."
  (seq-remove #'package-installed-p (mb-f-package-remote-packages)))

(defun mb-f-package-install-local-package (package)
  "Install local PACKAGE from packages/."
  (let ((package-path (concat user-emacs-directory
                              "packages/"
                              (symbol-name package))))
    (package-install-file package-path)))

(defun mb-f-package-remote-packages ()
  "A list of all M/ELPA packages."
  (seq-difference package-selected-packages
                  (mb-f-package-local-packages)))

(defun mb-f-package-install-all-remote ()
  "Install all M/ELPA packages."
  (unless (seq-every-p #'package-installed-p
                       (mb-f-package-remote-packages))
    (message "Installing M/ELPA packages...")
    (let* ((pkg-symbols (mb-f-package-remote-not-installed-packages))
           (packages (seq-map #'symbol-name pkg-symbols)))
      (message (format "- %s" (string-join packages "\n- "))))
    (package-refresh-contents)
    (mb-f-no-confirm #'package-install-selected-packages)))

(defun mb-f-package-install-all-local ()
  "Install all local packages."
  (let ((not-installed (mb-f-package-local-not-installed-packages)))
    (when (> (length not-installed) 0)
      (message "Installing local packages: %S" not-installed)
      (mapc #'mb-f-package-install-local-package not-installed)
      (package-quickstart-refresh))))

(defun mb-f-package-install-all ()
  "Install all missing packages."
  (require 'package)
  (mb-f-package-install-all-remote)
  (mb-f-package-install-all-local))

(defun mb-f-make-cache-dirs ()
  "Create all cache directories needed by Emacs."
  (let ((cache-dir "~/.cache/emacs")
        (dirs (list "auto-save-list"
                    "autosave"
                    "backup"
                    "elpa"
                    "emojis"
                    "lock"
                    "transient"
                    "url")))
    (dolist (dir dirs) (make-directory (format "%s/%s" cache-dir dir) t))))

;; See: https://github.com/minad/tempel#defining-custom-elements
(defun mb-f-tempel-include (elt)
  "Include element for Tempel."
  (when (eq (car-safe elt) 'i)
    (if-let (template (alist-get (cadr elt) (tempel--templates)))
        (cons 'l template)
      (message "Template %s not found" (cadr elt))
      nil)))

(defun mb-f-tempel-insert-quoted (template)
  "Insert template quoted if not in string."
  (if (nth 3 (syntax-ppss))
      (tempel-insert template)
    (insert "\"")
    (save-excursion (insert "\""))
    (tempel-insert template)))

(defun mb-f-buf-base ()
  "Get buffer or buffer filename base."
  (file-name-base (or (buffer-file-name) (buffer-name))))

(defun mb-f-buf-name ()
  "Get buffer or buffer filename."
  (file-name-nondirectory (or (buffer-file-name) (buffer-name))))

(defun mb-f-kebab-to-human (str)
  "Convert STR from kebab-format to Human Readable Format."
  (mapconcat 'capitalize (split-string str "-") " "))

(defun mb-f-emacs-version ()
  "Get version number of Emacs."
  (nth 2 (split-string (version) " +")))

(defun mb-f-emacs-version-major ()
  "Get the major version number of Emacs."
  (nth 0 (split-string (mb-f-emacs-version) "\\.")))

(defun mb-f-select-buffer (buffer)
  "Select window of a buffer."
  (select-window (get-buffer-window buffer)))

(provide 'mb-f)
;;; mb-f.el ends here
