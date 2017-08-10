;;; mb-f.el --- Some functions and macros I use.    -*- lexical-binding: t; -*-

;; Copyright ⓒ 2013-2016 Mattias Bengtsson
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
;; Keywords         : extensions, tools
;; Package-Requires : ((emacs "25.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 25.x

;;; Commentary:

;;; Note:

;;; Code:

(require 'yasnippet)
(require 'fill-column-indicator)
(require 'popup)

(defun mb-f-shorten-minor-modes (modes)
  "Shorten the displayed name for MODES in the modeline."
  (dolist (mode-and-line modes)
    (let ((line (cdr mode-and-line))
          (mode (car mode-and-line)))
      (mb-f-shorten-minor-mode mode line))))

(defun mb-f-shorten-minor-mode (mode line)
  "Replace the displayed name for MODE by LINE."
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

(defun mb-f-lower-camel-case (s)
  "Camel case S."
  (mb-f-mapconcat-head 'downcase
                       'capitalize
                       (mb-f-split-name s)
                       ""))

(defun mb-f-camel-case (s)
  "Camel case S."
  (mapconcat #'capitalize (mb-f-split-name s) ""))

(defun mb-f-snake-case (s)
  "Snake case S."
  (mapconcat #'downcase (mb-f-split-name s) "_"))

(defun mb-f-dash-case (s)
  "Dash case S."
  (mapconcat #'downcase (mb-f-split-name s) "-"))

(defun mb-f-dash-case-p (s)
  "Return T if S is in dash-case."
  (let ((case-fold-search nil))
    (string-match-p "[a-z]+\\(?:-[a-z]+\\)+" s)))

(defun mb-f-camel-case-p (s)
  "Return T if S is in camel-case."
  (let ((case-fold-search nil))
    (string-match-p "^\\(?:[A-Z][a-z]+\\)+"  s)))

(defun mb-f-lower-camel-case-p (s)
  "Return T if S is in lower-camel-case."
  (let ((case-fold-search nil))
    (string-match-p "^[a-z]+\\(?:[A-Z][a-z]+\\)+"  s)))

(defun mb-f-snake-case-p (s)
  "Return T if S is in snake-case."
  (let ((case-fold-search nil))
    (string-match-p "^[a-z]+\\(?:_[a-z]+\\)+" s)))

(defun mb-f-toggle-programming-case (s) ;; UP
  "Toggle programming style casing of S."
  (cond ((mb-f-snake-case-p       s) (mb-f-dash-case        s))
        ((mb-f-dash-case-p        s) (mb-f-camel-case       s))
        ((mb-f-camel-case-p       s) (mb-f-lower-camel-case s))
        ((mb-f-lower-camel-case-p s) (mb-f-snake-case       s))))

(defun mb-f-toggle-programming-case-reverse (s)
  "Toggle programming style casing of S in reverse."
  (cond ((mb-f-dash-case-p        s) (mb-f-snake-case       s))
        ((mb-f-snake-case-p       s) (mb-f-lower-camel-case s))
        ((mb-f-lower-camel-case-p s) (mb-f-camel-case       s))
        ((mb-f-camel-case-p       s) (mb-f-dash-case        s))))

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

(defun mb-f-autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string)
                      (point-min)
                      (point-max)))

(defun mb-f-yas-choose-package-keyword ()
  "Choose a package keyword to expand."
  (yas-choose-value "abbrev"
                    "bib"
                    "c"
                    "calendar"
                    "comm"
                    "convenience"
                    "data"
                    "docs"
                    "emulations"
                    "extensions"
                    "faces"
                    "files"
                    "frames"
                    "games"
                    "hardware"
                    "help"
                    "hypermedia"
                    "i18n"
                    "internal"
                    "languages"
                    "lisp"
                    "local"
                    "maint"
                    "mail"
                    "matching"
                    "mouse"
                    "multimedia"
                    "news"
                    "outlines"
                    "processes"
                    "terminals"
                    "tex"
                    "tools"
                    "unix"
                    "vc"
                    "wp"))

(defun mb-f-yas-choose-license ()
  "Choose a license to expand."
  (yas-choose-value
   (directory-files (concat user-emacs-directory "licenses/")
                    nil
                    "^[A-Za-z0-9-+_][A-Za-z0-9-+_.]*$")))

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

(defvar ido-matches)
(defvar ido-max-prospects)
(defun mb-f-ido-visible-prospects ()
  "The number of visible prospects."
  ;; TODO: fix smex's `…' (The out-commented stuff is for that)
  (let* ((available-lines (1- (ffloor (* max-mini-window-height (frame-height)))))
         ;; (prospects-len (length ido-matches))
         ;; (dot-dot (< available-lines (max ido-max-prospects prospects-len)))
         )
    (1- available-lines)))

(defun mb-f-fci-turn-off (&rest _)
  "Turn off `fci-mode'."
  (when (boundp 'fci-mode)
    (turn-off-fci-mode)))

(defun mb-f-fci-turn-on (&rest _)
  "Turn on `fci-mode'."
  (when (boundp 'fci-mode)
    (turn-on-fci-mode)))

(defun mb-f-create-non-existent-directory ()
  "Offer to create parent directory for current buffer if it doesn't exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it? "
                                 parent-directory)))
      (make-directory parent-directory t))))

(defun mb-f-aim-new-block (mode control-stmts &optional char-tokens)
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

(defun mb-f-yas-popup (prompt choices &optional display-fn)
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
   :isearch t))

(defun mb-f-wrap-in-comment (string)
  "Wrap STRING inside comment."
  (format "%s%s%s" comment-start string comment-end))

(defvar control-mode)
(defun mb-f-control-mode-set-cursor ()
  "Update cursor based for `control-mode'."
  (if (display-graphic-p)
      (setq cursor-type (if control-mode
                            'box
                          `(bar . ,(/ (display-pixel-width) 640))))
    (send-string-to-terminal (if control-mode
                                 "\e[1 q"
                               "\e[5 q"))))
(defun mb-f-focus-buffer-dwim (buffer)
  "Switch to BUFFER in other window unless it's currently in view."
  (unless (string-equal buffer (buffer-name (current-buffer)))
    (switch-to-buffer-other-window buffer)))

(defun mb-f-advice-describe-func (describe-function)
  "Advice DESCRIBE-FUNCTION to switch to the *Help* buffer after popping it up."
  (advice-add describe-function
              :after (lambda (&rest _) (mb-f-focus-buffer-dwim "*Help*"))))

(defun mb-f-other-window (&rest _args)
  "Like `(other-window 1)' but skip all arguments."
  (other-window 1))

(defun mb-f-advice-other-window-after (func)
  "Advice FUNC to switch window after been run."
  (advice-add func :after #'mb-f-other-window))

(defun mb-f-set-terminal-cursors ()
  "Set up the terminal cursors."
  (send-string-to-terminal (concat "\033]12;" (face-background 'cursor) "\007"))
  (add-hook 'kill-emacs-hook
            (lambda ()
              (send-string-to-terminal "\033]12;white\007\e[1 q"))))

(defun mb-f-set-terminal-window-separator ()
  "Set a unicode terminal window separator character."
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (let ((display-table (or buffer-display-table
                                       standard-display-table)))
                (set-display-table-slot display-table 5 ?│)
                (set-window-display-table (selected-window)
                                          display-table)))))

(defvar mb-f-jhbuild-src-path   "~/Code/gnome/src")
(defvar mb-f-jhbuild-build-path "~/Code/gnome/build")
(defun mb-f-projectile-regen-rtags-jhbuild ()
  "Create `compile_commands.json' for this `JHBuild' module and feed it to rc.

Perform a `cdcc-gen' and get the compile_commands.json from the
src-dir if this is an autotools project.

Needs `autogenargs += ' CC=cdcc-gcc CXX=cdcc-g++'' in jhbuildrc.

Meson projects Just Works™ and CMake will work automatically as
well if you add this line:
    `cmakeargs = '-DCMAKE_EXPORT_COMPILE_COMMANDS=ON''
... to your ~/.config/jhbuildrc"
  (let* ((autotools (mb-f-projectile-autotools-p t))
         (module    (projectile-project-name))
         (src-dir   (projectile-project-root))
         (build-dir (format "%s/%s" mb-f-jhbuild-build-path module))
         (extra-cmd (if autotools
                        (concat " cdcc-gen " src-dir
                                " &&"
                                (format " mv %s/compile_commands.json %s"
                                        src-dir
                                        build-dir))
                      "true")))
    (compile (concat " jhbuild make"
                     " &&"
                     extra-cmd
                     " &&"
                     " rc -J " build-dir "/compile_commands.json" ))))

(defun mb-f-projectile-regen-rtags-cmake ()
  "Create a `compile_commands.json' file for current project and feed it to rc."
  (let* ((project-root     (projectile-project-root))
         (build-dir-prefix (concat "build-" (projectile-project-name)))
         (build-dir        (make-temp-file build-dir-prefix t "")))
    (compile (concat "mkdir -p " build-dir
                     " &&"
                     " pushd " build-dir
                     " &&"
                     " cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON " project-root
                     " &&"
                     " rc -J ./compile_commands.json"
                     " ; popd"))))

(defun mb-f-projectile-regen-rtags-meson ()
  "Create a `compile_commands.json' file for current project and feed it to rc."
  (let* ((build-dir-prefix (concat "build-" (projectile-project-name)))
         (build-dir        (make-temp-file build-dir-prefix t "")))
    (compile (concat " pushd " (projectile-project-root)
                     " &&"
                     " meson " build-dir
                     " && "
                     (format "rc -J %s/compile_commands.json" build-dir)
                     "; popd"))))

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
  (let* ((depth-flag (if depth (format "-maxdepth %d" depth) ""))
         (cmd (format "find %s %s -name '.git' -type d" dir depth-flag))
         (result (split-string (shell-command-to-string cmd))))
    (mapcar (lambda (s) (substring s 0 -4)) result)))

(defun mb-f-enclosing-paren ()
  "Return the opening paren type we're currently enclosed by or nil."
  (let ((ppss (syntax-ppss)))
    (when (nth 1 ppss)
      (char-after (nth 1 ppss)))))

(defun mb-f-elisp-get-namespace ()
  "Get the namespace of the current module."
  (if (and (derived-mode-p 'emacs-lisp-mode)
           (string= (file-name-extension (buffer-name)) "el"))
      (let ((base (file-name-base)))
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

(provide 'mb-f)
;;; mb-f.el ends here
