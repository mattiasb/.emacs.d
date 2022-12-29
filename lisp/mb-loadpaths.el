;;; mb-loadpaths.el --- Helps me silent byte-compile warnings.    -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2023, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;;; Commentary:

;; My functions

;;; Note:

;;; Code:

(require 'mb-custom)

(eval-when-compile
  (let ((default-directory package-user-dir))
    (normal-top-level-add-subdirs-to-load-path)))

(provide 'mb-loadpaths)
;;; mb-loadpaths.el ends here
