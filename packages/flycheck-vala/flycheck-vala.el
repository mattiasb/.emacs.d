;;; flycheck-vala.el --- Hook up valac with FlyCheck -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Koz Ross

;; Author           : Koz Ross <>
;; Version          : 20150127
;; Keywords         : none
;; Package-Requires : ((emacs "24.1") (flycheck "0.20"))
;; URL              : http://www.retrofreedom.nz/blog/2014/11/17/using-valac-as-a-syntax-checker-for-vala-in-flycheck
;; Doc URL          : TBA
;; Compatibility    : GNU Emacs: 24.x

;;; Commentary:

;;; Note:

;;; Code:

(require 'flycheck)

(flycheck-def-option-var flycheck-vala-packages nil vala-valac
  "A list of additional packages for valac.
The value of this variable is a list of strings, where each string is what
would get passed to valac's --pkg option."
  :type '(repeat (string :tag "Package name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-define-checker vala-valac
  "A syntax checker for Vala using valac."
  :command ("valac"
            source
            "-o" null-device
            (option-list "--pkg" flycheck-vala-packages))
  :error-patterns ((error line-start
                          (file-name)
                          ":" line "." column "-" line "." column ": error" (message) line-end)
                   (warning line-start
                            (file-name)
                            ":" line "." column "-" line "." column ": warning: " (message) line-end))
  :modes vala-mode)

(provide 'flycheck-vala)
;;; flycheck-vala.el ends here
