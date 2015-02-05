;;; align-by-current-symbol.el --- Align lines to the symbol at point.

;; Copyright (c) 2010 Chris Done. All rights reserved.

;; Author:   Chris Done <chrisdone@gmail.com>
;; Created:  14-May-2010
;; Version:  0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "24.1"))
;; X-URL:    http://emacswiki.org/align-by-current-symbol.el

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
;; Chris Done BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;; OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Commentary:
;; Example usage:
;;
;; (load "align-by-current-symbol.el")
;; (global-set-key (kbd "C-c C-.") 'align-by-current-symbol)
;;
;; By default it requires spaces to be around the symbol.
;;
;; Use the following to turn this off:
;;
;; (global-set-key (kbd "C-c C-.")
;;    (lambda ()
;;       (interactive) (align-by-current-symbol t)))
;;
;; Example demonstration:
;;
;; mumumu = zotzot
;; chi = far
;; popo = k
;; zarlo => mu
;;
;; place point at `=' after `chi', hit C-c C-.:
;;
;; mumumu = zotzot
;; chi    = far
;; popo   = k
;; zarlo => mu

;;; Code:

;;;###autoload
(defun align-by-current-symbol (&optional add-space)
  "Indent all lines above and below by the current non-whitespace symbol.
Optionally ADD-SPACE around the symbol."
  (interactive "P")
  (let ((symbol (thing-at-point 'symbol)))
    (if symbol
        (let* ((symbol. (if add-space
                            symbol
                          (concat " " symbol " ")))
               (start (or (first/last-occurance symbol. 'search-backward 'previous-line 'line-beginning-position)
                          (line-beginning-position)))
               (end (or (first/last-occurance symbol. 'search-forward-regexp 'next-line 'line-end-position)
                        (line-end-position))))
          (align-string start end (regexp-opt (list symbol.)) (point-min))))))

(defun first/last-occurance (string search move-line line-position)
  "Search forward- or backward for the first or last line that contain STRING.
The three functions SEARCH, MOVE-LINE and LINE-POSITION is used to define in
what direction to search."
  (let ((pos   nil)
        (first nil)
        (try   t))
    (save-excursion
      (goto-char (funcall line-position))
      (while (or try (not (equal pos nil)))
        (setq try nil)
        (setq first pos)
        (setq pos (funcall search
                           string
                           (save-excursion (funcall move-line) (funcall line-position))
                           t)))
      (if first (line-beginning-position)))))

;;; align-by-current-symbol.el ends here
(provide 'align-by-current-symbol)
