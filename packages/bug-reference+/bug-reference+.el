;;; bug-reference+.el --- Some improvements to bug-reference.el -*- lexical-binding: t -*-


;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20231215
;; Keywords         : local
;; Package-Requires : ((emacs "29.1"))
;; URL              : https://github.com/mattiasb/.emacs.d
;; Compatibility    : GNU Emacs: 29.x

;;; Commentary:

;;; Note:

;;; Code:

(require 'map)
(require 'cl-extra)
(require 'bug-reference)

(defgroup bug-reference+ nil
  "Improvements to `bug-reference'."
  :prefix "bug-reference+-"
  :group 'comm
  :link '(url-link "https://github.com/mattiasb/.emacs.d"))

(defcustom bug-reference+-alist ()
  "An alist from pattern to URL-format."
  :type '(alist :key-type regexp
                :value-type (choice string function)))



(defun bug-reference+--bug-regexp ()
  "Construct a `bug-reference-bug-regexp' string from `bug-reference+-alist'."
  (let ((base-format "\\b\\(\\(%s\\)\\)\\b")
        (divider "\\)\\|\\(")
        (regexps (map-keys bug-reference+-alist)))
    (format base-format (string-join regexps divider))))

(defun bug-reference+--indexed-urls ()
  ""
  (let ((keys (mapcar #'car bug-reference+-alist)))
    (cl-mapcar 'cons (number-sequence 2 (+ 2 (length keys))) keys)))

(defun bug-reference+-format-function ()
  ""
  (cl-some (lambda (e)
             (let ((url-format (cdr e))
                   (match (match-string (car e))))
               (when match (format url-format match))))
           (bug-reference+--indexed-urls)))

;;;###autoload
(define-minor-mode bug-reference+-mode
  ""
  :init-value nil
  :lighter ""
  :group 'comm
  (if bug-reference+-mode
      (progn
        (bug-reference-mode 1)
        (setq-local bug-reference-bug-regexp (bug-reference+--bug-regexp))
        (setq-local bug-reference-url-format #'bug-reference+-format-function))
    (bug-reference-mode -1)
    (setq-local bug-reference-bug-regexp
                (eval (car (get 'bug-reference-bug-regexp 'standard-value))))
    (setq-local bug-reference-url-format
                (eval (car (get 'bug-reference-url-format 'standard-value)))))
  )

;;;###autoload
(define-minor-mode bug-reference+-prog-mode
  ""
  :init-value nil
  :lighter ""
  :group 'comm
  (if bug-reference+-prog-mode
      (progn
        (bug-reference-prog-mode 1)
        (setq-local bug-reference-bug-regexp (bug-reference+--bug-regexp))
        (setq-local bug-reference-url-format #'bug-reference+-format-function))
    (bug-reference-prog-mode -1)
    (setq-local bug-reference-bug-regexp
                (eval (car (get 'bug-reference-bug-regexp 'standard-value))))
    (setq-local bug-reference-url-format
                (eval (car (get 'bug-reference-url-format 'standard-value))))))

(provide 'bug-reference+)
;;; bug-reference+.el ends here
