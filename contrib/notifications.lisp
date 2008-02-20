;;; notifications.lisp -- Poor man's systray for StumpWM

;; Copyright 2008 Tassilo Horn <tassilo@member.fsf.org>

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:

;; This StumpWM module acts as notification monitor for external applications.
;; They can send messages via `stumpish' which will be displayed in the
;; mode-line.  (Thus `stumpish' has to be in your PATH.)
;;
;; To use it add this to your ~/.stumpwmrc.lisp:
;;
;;   (load "/path/to/stumpwm/contrib/notifications.lisp")
;;
;; Then add the formatter %n to your mode-line spec, i.e. like this:
;;
;;   (setf *screen-mode-line-format* "[%W] {%g} (%n)")
;;
;; You might want to bind *notifications-map* to a key:
;;
;;   (define-key *root-map* (kbd "N") '*notifications-map*)
;;
;; With this map you can add notifications with a, reset them with r, delete
;; the first/last with d/D or show them in a popup with s.
;;
;; External applications can add notification messages using stumpish:
;;
;;   $ stumpish notifications-add 'Foo Bar Baz'
;;
;; For example this is the elisp code that I use to let rcirc (an Emacs IRC
;; client) notify me when a message with my nickname or a IM message arrives:
;;
;; (defun th-rcirc-notification (process sender response target text)
;;   (let ((my-nick (rcirc-nick process)))
;;     (when (and (string= response "PRIVMSG")
;;                (not (string= sender my-nick))
;;                (or
;;                 ;; BitlBee IM messages
;;                 (string-match "localhost" (format "%s" process))
;;                 ;; Messages that mention my name
;;                 (string-match my-nick text)))
;;       (th-notifications-add (concat "rcirc: " target)))))
;;
;; (add-hook 'rcirc-print-hooks 'th-rcirc-notification)
;;
;; (defun th-notifications-add (str)
;;   (interactive "sNotification: ")
;;   (shell-command (concat "stumpish notifications-add '" str "'"))
;;   (th-notifications-show))

;;; Code:

(in-package :stumpwm)

(push '(#\n notifications-as-string) *screen-mode-line-formatters*)

(defvar notifications nil
  "A list of notification strings.")

(define-stumpwm-command "notifications-add" ((str :rest "Notification: "))
  "Add a notification string.
If a notification is already included, it will be moved to the front instead of
added anew."
  (when (not (string= (car notifications) str))
    (when (member str notifications :test #'string=)
      (setf notifications (delete str notifications :test #'string=)))
    (push str notifications)))

(define-stumpwm-command "notifications-reset" ()
  "Clear all notifications."
  (setf notifications nil))

(define-stumpwm-command "notifications-delete-first" ()
  "Delete the first notification."
  (setf notifications (cdr notifications)))

(define-stumpwm-command "notifications-delete-last" ()
  "Delete the first notification."
  (setf notifications (nreverse (cdr (nreverse notifications)))))

(defun notifications-as-string (&rest r)
  (declare (ignore r))
  (let ((str "+"))
    (loop for n in notifications
       do (setf str (concat str " " n " +")))
    str))

(define-stumpwm-command "notifications-show" ()
  "Messages all notifications."
  (message "Notifications: ~a" (notifications-as-string)))

(defvar *notifications-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "a")     "notifications-add")
    (define-key m (kbd "r")     "notifications-reset")
    (define-key m (kbd "d")     "notifications-delete-first")
    (define-key m (kbd "D")     "notifications-delete-last")
    (define-key m (kbd "s")     "notifications-show")
    m))

;; Local Variables:
;; mode: outline-minor
;; coding: utf-8
;; End:
