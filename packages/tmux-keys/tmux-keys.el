;;; tmux-keys.el --- Make Emacs work in a tmux shell -*- lexical-binding: t; -*-

;; Copyright ⓒ 2016 Aleksandar Valchev
;; Copyright ⓒ 2016 Mattias Bengtsson
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with This program. If not, see <http://www.gnu.org/licenses/>.
;;
;; Author: Aleksandar Valchev <aleksandar.valchev@gmail.com>

;; Version: 20160427
;; Keywords: tmux
;; Package-Requires: ()
;; URL: https://github.com/mattiasb/.emacs.d
;; Doc URL: TBA
;; Compatibility: GNU Emacs: 24.x

;;; Commentary:

;; Copied from https://github.com/avalchev/emacs/blob/master/init.el#L215 and
;; slightly modified by Mattias Bengtsson

;;; Note:

;;; Code:

(defun tmux-keys--translate (from to)
  "Add a key translation FROM a key TO another inside `key-translation-map'."
  (define-key key-translation-map (kbd from) (kbd to)))

;;;###autoload
(defun tmux-keys ()
  "Fix a bunch of issues with keys and Emacs in a tmux shell."
  (let ((x 2) (tkey ""))
    (while (<= x 8)
      ;; shift
      (if (= x 2)
          (setq tkey "S-"))
      ;; alt
      (if (= x 3)
          (setq tkey "M-"))
      ;; alt + shift
      (if (= x 4)
          (setq tkey "M-S-"))
      ;; ctrl
      (if (= x 5)
          (setq tkey "C-"))
      ;; ctrl + shift
      (if (= x 6)
          (setq tkey "C-S-"))
      ;; ctrl + alt
      (if (= x 7)
          (setq tkey "C-M-"))
      ;; ctrl + alt + shift
      (if (= x 8)
          (setq tkey "C-M-S-"))

      ;; up
      (tmux-keys--translate (format "M-[ 1 ; %d A"  x)
                            (format "%s<up>"     tkey))
      ;; down
      (tmux-keys--translate (format "M-[ 1 ; %d B"  x)
                            (format "%s<down>"   tkey))
      ;; right
      (tmux-keys--translate (format "M-[ 1 ; %d C"  x)
                            (format "%s<right>"  tkey))
      ;; left
      (tmux-keys--translate (format "M-[ 1 ; %d D"  x)
                            (format "%s<left>"   tkey))
      ;; home
      (tmux-keys--translate (format "M-[ 1 ; %d H"  x)
                            (format "%s<home>"   tkey))
      ;; end
      (tmux-keys--translate (format "M-[ 1 ; %d F"  x)
                            (format "%s<end>"    tkey))
      ;; page up
      (tmux-keys--translate (format "M-[ 5 ; %d ~"  x)
                            (format "%s<prior>"  tkey))
      ;; page down
      (tmux-keys--translate (format "M-[ 6 ; %d ~"  x)
                            (format "%s<next>"   tkey))
      ;; insert
      (tmux-keys--translate (format "M-[ 2 ; %d ~"  x)
                            (format "%s<delete>" tkey))
      ;; delete
      (tmux-keys--translate (format "M-[ 3 ; %d ~"  x)
                            (format "%s<delete>" tkey))
      ;; f1
      (tmux-keys--translate (format "M-[ 1 ; %d P"  x)
                            (format "%s<f1>"     tkey))
      ;; f2
      (tmux-keys--translate (format "M-[ 1 ; %d Q"  x)
                            (format "%s<f2>"     tkey))
      ;; f3
      (tmux-keys--translate (format "M-[ 1 ; %d R"  x)
                            (format "%s<f3>"     tkey))
      ;; f4
      (tmux-keys--translate (format "M-[ 1 ; %d S"  x)
                            (format "%s<f4>"     tkey))
      ;; f5
      (tmux-keys--translate (format "M-[ 15 ; %d ~" x)
                            (format "%s<f5>"     tkey))
      ;; f6
      (tmux-keys--translate (format "M-[ 17 ; %d ~" x)
                            (format "%s<f6>"     tkey))
      ;; f7
      (tmux-keys--translate (format "M-[ 18 ; %d ~" x)
                            (format "%s<f7>"     tkey))
      ;; f8
      (tmux-keys--translate (format "M-[ 19 ; %d ~" x)
                            (format "%s<f8>"     tkey))
      ;; f9
      (tmux-keys--translate (format "M-[ 20 ; %d ~" x)
                            (format "%s<f9>"     tkey))
      ;; f10
      (tmux-keys--translate (format "M-[ 21 ; %d ~" x)
                            (format "%s<f10>"    tkey))
      ;; f11
      (tmux-keys--translate (format "M-[ 23 ; %d ~" x)
                            (format "%s<f11>"    tkey))
      ;; f12
      (tmux-keys--translate (format "M-[ 24 ; %d ~" x)
                            (format "%s<f12>"    tkey))
      ;; f13
      (tmux-keys--translate (format "M-[ 25 ; %d ~" x)
                            (format "%s<f13>"    tkey))
      ;; f14
      (tmux-keys--translate (format "M-[ 26 ; %d ~" x)
                            (format "%s<f14>"    tkey))
      ;; f15
      (tmux-keys--translate (format "M-[ 28 ; %d ~" x)
                            (format "%s<f15>"    tkey))
      ;; f16
      (tmux-keys--translate (format "M-[ 29 ; %d ~" x)
                            (format "%s<f16>"    tkey))
      ;; f17
      (tmux-keys--translate (format "M-[ 31 ; %d ~" x)
                            (format "%s<f17>"    tkey))
      ;; f18
      (tmux-keys--translate (format "M-[ 32 ; %d ~" x)
                            (format "%s<f18>"    tkey))
      ;; f19
      (tmux-keys--translate (format "M-[ 33 ; %d ~" x)
                            (format "%s<f19>"    tkey))
      ;; f20
      (tmux-keys--translate (format "M-[ 34 ; %d ~" x)
                            (format "%s<f20>"    tkey))

      (setq x (+ x 1)))))


(provide 'tmux-keys)
;;; tmux-keys.el ends here
