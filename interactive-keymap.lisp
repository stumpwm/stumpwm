;; Copyright (C) 2016, 2017 Caio Oliveira
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;;; Macro for defining interactive command. Just pushes and pops new keymaps.
;;
;; Code:

(in-package #:stumpwm)

(export '(define-interactive-keymap))

(defun enter-interactive-keymap (kmap name)
  "Enter interactive mode"
  (message "~S started" name)
  (push-top-map kmap))

(defun exit-interactive-keymap (name)
  "Exits interactive mode"
  (message "~S finished" name)
  (pop-top-map))

(defmacro define-interactive-keymap
    (name (&key on-enter on-exit abort-if (exit-on '((kbd "RET")
                                                     (kbd "ESC")
                                                     (kbd "C-g"))))
     &body key-bindings)
  "Declare an interactive keymap mode. This can be used for developing
interactive modes or command trees, such as @command{iresize}.

The NAME argument follows the same convention as in @command{defcommand}.

ON-ENTER and ON-EXIT are optional functions to run before and after the
interactive keymap mode, respectively. If ABORT-IF is defined, the interactive
keymap will only be activated if calling ABORT-IF returns true.

KEY-BINDINGS is a list of the following form: ((KEY COMMAND) (KEY COMMAND) ...)

Each element in KEY-BINDINGS declares a command inside the interactive keymap.
Be aware that these commands won't require a prefix to run."
  (let* ((command (if (listp name) (car name) name))
         (exit-command (format nil "EXIT-~A" command))
         (keymap (gensym "m")))
    (multiple-value-bind (key-bindings decls docstring)
        (parse-body key-bindings :documentation t)
      `(let ((,keymap (make-sparse-keymap)))
         ,@(loop for keyb in key-bindings
                 collect `(define-key ,keymap ,@keyb))
         ,@(loop for keyb in exit-on
                 collect `(define-key ,keymap ,keyb ,exit-command))

         (defcommand ,name () ()
           ,@decls
           ,(or docstring
                (format nil "Starts interactive command \"~A\"" command))
           ,@(when abort-if `((when (funcall ,abort-if)
                                (return-from ,command))))

           ,@(when on-enter `((funcall ,on-enter)))
           (enter-interactive-keymap ,keymap (quote ,command)))

         (defcommand ,(intern exit-command) () ()
           ,@(when on-exit `((funcall ,on-exit)))
           (exit-interactive-keymap (quote ,command)))))))
