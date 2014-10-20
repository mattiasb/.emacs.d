(defmacro bol-with-prefix (function)
  "Define a new function which calls FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument. The FUNCTION still receives the
prefix argument."
  (let ((name (intern (format "endless/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format 
           "Call `%s', but move to BOL when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

(global-set-key [remap kill-line] (bol-with-prefix kill-line))

(defun rtags-start-process ()
  "Start rdm if it isn't running."
  (interactive)
  (unless rtags-process (rtags-restart-process)))

(defun company-select-next-five ()
  "A bit more eager company-select"
  (interactive)
  (dotimes (number 5 nil) (company-select-next)))

(defun company-select-previous-five ()
  "A bit more eager company-select"
  (interactive)
  (dotimes (number 5 nil) (company-select-previous)))

(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (let ((old-indent (current-indentation)))
      (indent-for-tab-command)
      (if (= old-indent (current-indentation))
          (if (or (not yas-minor-mode)
                  (null (do-yas-expand)))
              (company-complete-common)
            ))
      )))

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  "Use popup.el for yasnippet."
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
   :isearch t
   ))

(defun wrap-in-comment (str)
  (format "%s%s%s" comment-start str comment-end))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments current region or line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun rename-current-buffer-and-file ()
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

(defun package-list-installed-packages ()
  "Like `package-list-packages', but shows only installed
optional packages."
  (interactive)
  (package-initialize)
  (package-show-package-list
   (cl-remove-if-not (lambda (x) (and (not (package-built-in-p x))
                                      (package-installed-p x)))
                     (mapcar 'car package-archive-contents))))

(defun rename-modeline (hook name)
  (add-hook 'hook (lambda () (setq-local mode-name name))))

;; Set proxy from environment
(defun set-proxy ()
  (interactive)
  (if (and (getenv "HTTP_PROXY") (getenv "HTTPS_PROXY"))
      (setq-default url-proxy-services '(("http"  . (getenv "HTTP_PROXY"))
                                         ("https" . (getenv "HTTPS_PROXY"))
                                         ))))
