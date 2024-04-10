;;; mb-package.el --- My package install helpers -*- lexical-binding: t -*-

;; Copyright 2024, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20240410
;; Keywords         : local
;; Package-Requires : ((emacs "29.3"))
;; URL              : https://github.com/mattiasb/.emacs.d
;; Compatibility    : GNU Emacs: 29.x

;;; Commentary:

;;; Code:


(require 'package)

(defun mb-package-local-packages (&optional with-paths)
  "List of all packages under packages/.  Optionally WITH-PATHS."
  (let* ((directory (concat user-emacs-directory "packages/"))
         (filter   "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
    (if with-paths
        (directory-files directory t filter)
      (seq-map #'intern (directory-files directory nil filter)))))

(defun mb-package-local-not-installed-packages ()
  "List of all not installed packages under packages/."
  (seq-remove #'package-installed-p (mb-package-local-packages)))

(defun mb-package-remote-not-installed-packages ()
  "List of all remote not installed packages."
  (seq-remove #'package-installed-p (mb-package-remote-packages)))

(defun mb-package-install-local-package (package)
  "Install local PACKAGE from packages/."
  (let ((package-path (concat user-emacs-directory
                              "packages/"
                              (symbol-name package))))
    (package-install-file package-path)))

(defun mb-package-remote-packages ()
  "A list of all M/ELPA packages."
  (seq-difference package-selected-packages
                  (mb-package-local-packages)))

(defun mb-package-install-all-remote ()
  "Install all M/ELPA packages."
  (unless (seq-every-p #'package-installed-p
                       (mb-package-remote-packages))
    (message "Installing M/ELPA packages...")
    (let* ((pkg-symbols (mb-package-remote-not-installed-packages))
           (packages (seq-map #'symbol-name pkg-symbols)))
      (message (format "- %s" (string-join packages "\n- "))))
    (package-refresh-contents)
    (package-install-selected-packages t)))

(defun mb-package-install-all-local ()
  "Install all local packages."
  (let ((not-installed (mb-package-local-not-installed-packages)))
    (when (> (length not-installed) 0)
      (message "Installing local packages: %S" not-installed)
      (mapc #'mb-package-install-local-package not-installed)
      (package-quickstart-refresh))))

(defun mb-package-install-all ()
  "Install all missing packages."
  (require 'package)
  (mb-package-install-all-remote)
  (mb-package-install-all-local))

(provide 'mb-package)
;;; mb-package.el ends here
