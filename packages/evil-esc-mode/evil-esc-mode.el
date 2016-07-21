;;; evil-esc-mode.el --- Toggle interception of \\e (escape). -*- lexical-binding: t; -*-

;;; License:

;; Copyright ⓒ Vegard Øye
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
;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Version: 20160721
;; Keywords: none
;; Package-Requires: ()
;; URL: TBA
;; Doc URL: TBA
;; Compatibility: GNU Emacs: 24.x

;;; Commentary:

;; Toggle interception of \\e (escape).
;; Useful if you want to bind the ESC key in the terminal.
;;
;; This code is extracted from Evil (evil-core.el and evil-vars.el)

;;; Note:

;;; Code:

(defvar evil-esc-mode nil
  "Non-nil if `evil-esc-mode' is enabled.")

(defcustom evil-intercept-esc 'always
  "Whether evil should intercept the ESC key.
In terminal, a plain ESC key and a meta-key-sequence both
generate the same event.  In order to distinguish both evil
modifies `input-decode-map'.  This is necessary in terminal but
not in X mode.  However, the terminal ESC is equivalent to C-[, so
if you want to use C-[ instead of ESC in X, then Evil must
intercept the ESC event in X, too.  This variable determines when
Evil should intercept the event."
  :type '(radio (const :tag "Never" :value nil)
                (const :tag "In terminal only" :value t)
                (const :tag "Always" :value always))
  :group 'evil)

(defvar evil-inhibit-esc nil
  "If non-nil, the \\e event will never be translated to 'escape.")

(defcustom evil-esc-delay 0.01
  "Time in seconds to wait for another key after ESC."
  :type 'number
  :group 'evil)

;;;###autoload
(defun evil-esc-mode (&optional arg)
  "Toggle interception of \\e (escape).
Enable with positive ARG and disable with negative ARG.

When enabled, `evil-esc-mode' modifies the entry of \\e in
`input-decode-map'.  If such an event arrives, it is translated to
a plain 'escape event if no further event occurs within
`evil-esc-delay' seconds.  Otherwise no translation happens and
the ESC prefix map (i.e. the map originally bound to \\e in
`input-decode-map`) is returned."
  (cond
   ((or (null arg) (eq arg 0))
    (evil-esc-mode (if evil-esc-mode -1 +1)))
   ((> arg 0)
    (unless evil-esc-mode
      (setq evil-esc-mode t)
      (add-hook 'after-make-frame-functions #'evil-init-esc)
      (mapc #'evil-init-esc (frame-list))))
   ((< arg 0)
    (when evil-esc-mode
      (remove-hook 'after-make-frame-functions #'evil-init-esc)
      (mapc #'evil-deinit-esc (frame-list))
      (setq evil-esc-mode nil)))))

(defun evil-init-esc (frame)
  "Update `input-decode-map' in terminal with FRAME."
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (and
             (or (eq evil-intercept-esc 'always)
                 (and evil-intercept-esc
                      (eq (terminal-live-p term) t))) ; only patch tty
             (not (terminal-parameter term 'evil-esc-map)))
        (let ((evil-esc-map (lookup-key input-decode-map [?\e])))
          (set-terminal-parameter term 'evil-esc-map evil-esc-map)
          (define-key input-decode-map [?\e]
            `(menu-item "" ,evil-esc-map :filter ,#'evil-esc)))))))

(defun evil-deinit-esc (frame)
  "Restore `input-decode-map' in terminal with FRAME."
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (terminal-live-p term)
        (let ((evil-esc-map (terminal-parameter term 'evil-esc-map)))
          (when evil-esc-map
            (define-key input-decode-map [?\e] evil-esc-map)
            (set-terminal-parameter term 'evil-esc-map nil)))))))

(defun evil-esc (map)
  "Translate \\e to 'escape if no further event arrives in MAP.
This function is used to translate a \\e event either to 'escape
or to the standard ESC prefix translation map.  If \\e arrives,
this function waits for `evil-esc-delay' seconds for another
event.  If no other event arrives, the event is translated to
'escape, otherwise it is translated to the standard ESC prefix
map stored in `input-decode-map'.  If `evil-inhibit-esc' is
non-nil or if evil is in Emacs state, the event is always
translated to the ESC prefix.

The translation to 'escape happens only if the current command
has indeed been triggered by \\e.  In other words, this will only
happen when the keymap is accessed from `read-key-sequence'.  In
particular, if it is access from `define-key' the returned
mapping will always be the ESC prefix map."
  (if (and (not evil-inhibit-esc)
           ;; (or evil-local-mode (evil-ex-p))
           ;; (not (evil-emacs-state-p))
           (let ((keys (this-single-command-keys)))
             (and (> (length keys) 0)
                  (= (aref keys (1- (length keys))) ?\e)))
           (sit-for evil-esc-delay))
      (prog1 [escape]
        (when defining-kbd-macro
          (end-kbd-macro)
          (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
          (start-kbd-macro t t)))
    map))

(provide 'evil-esc-mode)
;;; evil-esc-mode.el ends here
