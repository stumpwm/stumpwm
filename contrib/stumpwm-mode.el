;;; stumpwm-mode.el --- special lisp mode for evaluating code into running stumpwm

;; Copyright (C) 2007  Shawn Betts

;; Maintainer: Shawn Betts
;; Keywords: comm, lisp, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; load this file, set stumpwm-shell-program to point to stumpish and
;; run M-x stumpwm-mode in your stumpwm lisp files. Now, you can
;; easily eval code into a running stumpwm using the regular bindings.

;;; Code:

(defvar stumpwm-shell-program "stumpish"
  "program name, including path if needed, for the stumpish program.")

(define-minor-mode stumpwm-mode
    "add some bindings to eval code into a running stumpwm using stumpish."
  :global nil
  :lighter " StumpWM"
  :keymap (let ((m (make-sparse-keymap)))
            (define-key m (kbd "C-M-x") 'stumpwm-eval-defun)
            (define-key m (kbd "C-x C-e") 'stumpwm-eval-last-sexp)
            m))

(defun stumpwm-eval-region (start end)
  (interactive "r")
  (let ((s (buffer-substring-no-properties start end)))
    (message "%s"
             (with-temp-buffer
               (call-process stumpwm-shell-program nil (current-buffer) nil
                             "eval"
                             s)
               (buffer-string)))))

(defun stumpwm-eval-defun ()
  (interactive)
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f")
    (let ((end (point)))
      (beginning-of-defun)
      (stumpwm-eval-region (point) end))))

(defun stumpwm-eval-last-sexp ()
  (interactive)
  (stumpwm-eval-region (save-excursion (backward-sexp) (point)) (point)))

(provide 'stumpwm-mode)
;;; stumpwm-mode.el ends here
