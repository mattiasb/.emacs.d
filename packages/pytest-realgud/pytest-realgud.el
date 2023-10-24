;;; pytest-realgud.el --- Run pytests with realgud -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2023, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 0.1
;; Keywords         : languages
;; Package-Requires : ((emacs "28.3") (pytest "0.2.1") (realgud "1.5.1"))
;; URL              : https://github.com/mattiasb/.emacs.d
;; Compatibility    : GNU Emacs: 28.x

;;; Commentary:

;; Integrates realgud:pdb with pytest allowing you to run pytests within
;; realgud.

;;; Note:

;;; Code:

(require 'pytest)
(require 'realgud)

(defun pytest-realgud-run (&optional tests flags)
  "Run pytest in realgud:pdb.
Optional argument TESTS Tests to run.
Optional argument FLAGS pytest command line flags."
  (interactive "fTest directory or file: \nspytest flags: ")
  (let ((cmd (string-join (list (pytest-find-test-runner)
                                (if flags flags pytest-cmd-flags)
                                "--pdb"
                                "--trace"
                                tests)
                          " ")))
    (message (format "CMD: %s" cmd))
    (realgud:pdb cmd)))

;;;###autoload
(defun pytest-realgud-all (&optional flags)
  "Run pytest on all the files in the current buffer.
Optional argument FLAGS pytest command line flags."
  (interactive)
  (pytest-realgud-run nil flags))

;;;###autoload
(defun pytest-realgud-directory (&optional flags)
  "Run pytest on all the files in the current buffer.
Optional argument FLAGS pytest command line flags."
  (interactive)
  (pytest-realgud-run (file-name-directory buffer-file-name) flags))

;;;###autoload
(defun pytest-realgud-module (&optional flags)
  "Run pytest in realgud:pdb on current buffer.
Optional argument FLAGS pytest command line flags."
  (interactive)
  (pytest-realgud-run buffer-file-name flags))

;;;###autoload
(defun pytest-realgud-one (&optional flags)
  "Run pytest in realgud:pdb on testable thing at point in current buffer.
Optional argument FLAGS pytest command line flags."
  (interactive)
  (pytest-realgud-run (format "%s" (pytest-py-testable)) flags))

(provide 'pytest-realgud)
;;; pytest-realgud.el ends here
