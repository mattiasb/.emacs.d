;;; mb-f.el --- Some functions and macros I use.    -*- lexical-binding: t; -*-

;; Copyright ⓒ 2013-2016 Mattias Bengtsson

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
;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Version          : 20170305
;; Keywords         : extensions, tools
;; Package-Requires : ((emacs "25.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 25.x

;;; Commentary:

;;; Note:

;;; Code:

(defun my/shorten-minor-modes (modes)
  "Shorten the displayed name for MODES in the modeline."
  (dolist (mode-and-line modes)
    (let ((line (cdr mode-and-line))
          (mode (car mode-and-line)))
      (my/shorten-minor-mode mode line))))

(defun my/shorten-minor-mode (mode line)
  "Replace the displayed name for MODE by LINE."
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook (lambda () (diminish mode line)))))

(defun my/shorten-major-modes (modes)
  "Shorten the displayed name for MODES in the mode line."
  (dolist (mode-and-line modes)
    (let ((line (cdr mode-and-line))
          (mode (car mode-and-line)))
      (my/shorten-major-mode mode line))))

(defun my/shorten-major-mode (mode line)
  "Replace the displayed name for MODE by LINE."
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook (lambda () (setq-local mode-name line)))))

(defun my/auto-modes (modes)
  "Add many MODES to `auto-mode-alist'."
  (setq auto-mode-alist (append modes auto-mode-alist)))

(defun my/global-define-keys (keybindings)
  "Set a bunch of global KEYBINDINGS at the same time."
  (my/define-keys (current-global-map)
                  keybindings))

(defun my/global-remap-keys (mappings)
  "Remap a bunch of global keybindings defined in MAPPINGS."
  (my/remap-keys (current-global-map) mappings))

(defun my/define-keys (mode-map keybindings)
  "Set a bunch of MODE-MAP specific KEYBINDINGS at the same time."
  (dolist (binding keybindings)
    (let* ((key   (kbd (car binding)))
           (def   (cdr binding))
           (def-v (when (boundp def) (symbol-value def))))
      (if (or (fboundp def)
              (not (keymapp def-v)))
          (define-key mode-map key def)
        (define-key mode-map key (quote def-v))))))

(defun my/remap-keys (mode-map mappings)
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

(defmacro my/define-keymap (keymap bindings)
  "Define a new KEYMAP and add a bunch of BINDINGS."
  `(progn (defvar ,keymap
            (let ((map (make-sparse-keymap)))
              (my/define-keys map ,bindings)
              map))
          (fset (quote ,keymap) ,keymap)))

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

(defun my/split-name (s)
  "Split S by name."
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun my/lower-camel-case (s)
  "Camel case S."
  (my/mapconcat-head 'downcase
                     'capitalize
                     (my/split-name s)
                     ""))

(defun my/camel-case (s)
  "Camel case S."
  (mapconcat #'capitalize (my/split-name s) ""))

(defun my/snake-case (s)
  "Snake case S."
  (mapconcat #'downcase (my/split-name s) "_"))

(defun my/dash-case (s)
  "Dash case S."
  (mapconcat #'downcase (my/split-name s) "-"))

(defun my/is-dash-case (s)
  "Return T if S is in dash-case."
  (let ((case-fold-search nil))
    (string-match-p "[a-z]+\\(?:-[a-z]+\\)+" s)))

(defun my/is-camel-case (s)
  "Return T if S is in camel-case."
  (let ((case-fold-search nil))
    (string-match-p "^\\(?:[A-Z][a-z]+\\)+"  s)))

(defun my/is-lower-camel-case (s)
  "Return T if S is in lower-camel-case."
  (let ((case-fold-search nil))
    (string-match-p "^[a-z]+\\(?:[A-Z][a-z]+\\)+"  s)))

(defun my/is-snake-case (s)
  "Return T if S is in snake-case."
  (let ((case-fold-search nil))
    (string-match-p "^[a-z]+\\(?:_[a-z]+\\)+" s)))

(defun my/toggle-programming-case (s) ;; UP
  "Toggle programming style casing of S."
  (cond ((my/is-snake-case       s) (my/dash-case        s))
        ((my/is-dash-case        s) (my/camel-case       s))
        ((my/is-camel-case       s) (my/lower-camel-case s))
        ((my/is-lower-camel-case s) (my/snake-case       s))))

(defun my/toggle-programming-case-reverse (s)
  "Toggle programming style casing of S in reverse."
  (cond ((my/is-dash-case        s) (my/snake-case       s))
        ((my/is-snake-case       s) (my/lower-camel-case s))
        ((my/is-lower-camel-case s) (my/camel-case       s))
        ((my/is-camel-case       s) (my/dash-case        s))))

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

(defun my/preceding-char-match-p (pattern)
  "Match preceding char with PATTERN."
  (let ((str (string (preceding-char))))
    (string-match-p pattern str)))

(defun my/following-char-match-p (pattern)
  "Match following char with PATTERN."
  (let ((str (string (following-char))))
    (string-match-p pattern str)))

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
  (yas-expand-snippet (buffer-string)
                      (point-min)
                      (point-max)))

(defun my/yas-choose-package-keyword ()
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

(defvar ido-matches)
(defvar ido-max-prospects)
(defun my/ido-visible-prospects ()
  "The number of visible prospects."
  ;; TODO: fix smex's `…' (The out-commented stuff is for that)
  (let* ((available-lines (1- (ffloor (* max-mini-window-height (frame-height)))))
         ;; (prospects-len (length ido-matches))
         ;; (dot-dot (< available-lines (max ido-max-prospects prospects-len)))
         )
    (1- available-lines)))

(defvar yas-fallback-behavior)
(defun my/yas-expand ()
  "Perform a `yas-expand' but return nil on failure."
  (when (yas-minor-mode)
    (let ((yas-fallback-behavior 'return-nil))
      (yas-expand))))

(defun my/fci-turn-off (&rest _)
  "Turn off `fci-mode'."
  (when (boundp 'fci-mode)
    (turn-off-fci-mode)))

(defun my/fci-turn-on (&rest _)
  "Turn on `fci-mode'."
  (when (boundp 'fci-mode)
    (turn-on-fci-mode)))

(defun my/create-non-existent-directory ()
  "Offer to create parent directory for current buffer if it doesn't exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it? "
                                 parent-directory)))
      (make-directory parent-directory t))))

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
   :isearch t))

(defun my/wrap-in-comment (string)
  "Wrap STRING inside comment."
  (format "%s%s%s" comment-start string comment-end))

(defvar control-mode)
(defun my/control-mode-set-cursor ()
  "Update cursor based for `control-mode'."
  (if (display-graphic-p)
      (setq cursor-type (if control-mode
                            'box
                          '(bar . 5)))
    (send-string-to-terminal (if control-mode
                                 "\e[1 q"
                               "\e[5 q"))))
(defun my/focus-buffer-dwim (buffer)
  "Switch to BUFFER in other window unless it's currently in view."
  (unless (string-equal buffer (buffer-name (current-buffer)))
    (switch-to-buffer-other-window buffer)))

(defun my/advice-describe-func (describe-function)
  "Advice DESCRIBE-FUNCTION to switch to the *Help* buffer after popping it up."
  (advice-add describe-function
              :after (lambda (&rest _) (my/focus-buffer-dwim "*Help*"))))

(defun my/other-window (&rest args)
  "Like `(other-window 1)' but skip ARGS."
  (other-window 1))

(defun my/advice-other-window-after (func)
  "Advice FUNC to switch window after been run."
  (advice-add func :after #'my/other-window))

(defun my/set-imenu-create-index-function (&optional function separator)
  "Set up a flat `imenu'.
Generate index with FUNCTION (default: `imenu-create-index-function').
Separate with SEPARATOR if set (default: '/')"
  (let ((func (or function imenu-create-index-function))
        (sepa (or separator "/")))
    (setq-local imenu-create-index-function
                (lambda ()
                  (my/flatten-imenu-index (funcall func) sepa)))))

(defun my/flatten-imenu-index (index separator)
  "Flatten `imenu' INDEX w/ SEPARATOR."
  (let ((cdr-is-index (listp (cdr index))))
    (cond ((not (stringp (car index)))
           (cl-mapcan (lambda (idx) (my/flatten-imenu-index idx separator))
                      index))
          (cdr-is-index (my/imenu-prefix-flattened index separator))
          (t (list index)))))

(defun my/imenu-prefix-flattened (index separator)
  "Flatten `imenu' INDEX w/ SEPARATOR."
  (let ((flattened (my/flatten-imenu-index (cdr index) separator)))
    (cl-loop for sub-item in flattened
             collect
             `(,(concat (car index)
                        separator
                        (car sub-item))
               .
               ,(cdr sub-item)))))

(defun my/package-init ()
  "Initialize the package system."
  (package-initialize)
  (unless (seq-every-p #'package-installed-p
                       package-selected-packages)
    (package-refresh-contents)
    (my/install-packages-in-dir "~/.emacs.d/packages/")
    (package-install-selected-packages)))

(defun my/install-packages-in-dir (directory)
  "Install all packages in DIRECTORY."
  (mapc #'package-install-file
        (directory-files directory t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))

(defun my/set-terminal-cursors ()
  "Set up the terminal cursors."
  (send-string-to-terminal (concat "\033]12;" (face-background 'cursor) "\007"))
  (add-hook 'kill-emacs-hook
            (lambda ()
              (send-string-to-terminal "\033]12;white\007\e[1 q"))))

(defun my/projectile-regen-rtags-jhbuild (module)
  "Create a `compile_commands.json' file for `JHBuild' MODULE and feed it to rc."
  (let* ((jhbuild-prefix (format "jhbuild run --in-builddir=%s -- " module))
         (compile-cmd (mapconcat
                       (lambda (s) (concat jhbuild-prefix s))
                       '("make clean" "bear make" "rc -J compile_commands.json")
                       " && ")))
    (compile compile-cmd)))

(defun my/find-git-projects (dir &optional depth)
  "Find all git projects under DIR.
Optionally only search as deep as DEPTH."
  (let* ((depth-flag (if depth (format "-maxdepth %d" depth) ""))
         (cmd (format "find %s %s -name '.git' -type d" dir depth-flag))
         (result (split-string (shell-command-to-string cmd))))
    (mapcar (lambda (s) (substring s 0 -4)) result)))

(provide 'mb-f)
;;; mb-f.el ends here
