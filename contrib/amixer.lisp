;;; Amixer module for StumpWM.
;;;
;;; Copyright 2007 Amy Templeton, Jonathan Moore Liles.
;;;
;;; Maintainer: 
;;;
;;; This module is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This module is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA
;;;

;;; USAGE:
;;;
;;; Make sure you have your media keys (or whatever) mapped to the appropriate
;;; keysyms (using xmodmap), then put:
;;;
;;;     (load "/path/to/amixer.lisp")
;;;
;;; In your ~/.stumpwmrc

(defun volcontrol (channel amount)
  (let ((percent
         (parse-integer
          (run-shell-command
           (concat "amixer sset " channel " " (or amount "toggle")
                   "| sed -n 's/^.*\\[\\([[:digit:]]\\+\\)%\\].*$/\\1/p' | head -1")
           t))))
    (message
     (concat "Mixer: " channel " " (or amount "toggled") (format nil "~C^B~A%" #\Newline percent) "^b [^[^7*"
             (bar percent 50 #\# #\:) "^]]"))))

(defcommand amixer-PCM-1- () ()
  "Lowers PCM volume"
  (volcontrol "PCM" "1-"))

(defcommand amixer-PCM-1+ () ()
  "Raises PCM volume"
  (volcontrol "PCM" "1+"))

(defcommand amixer-PCM-toggle () ()
  "Un/mutes PCM volume"
  (volcontrol "PCM" nil))

(defcommand amixer-Master-1- () ()
  "Lowers Master volume"
  (volcontrol "Master" "1-"))

(defcommand amixer-Master-1+ () ()
  "Raises Master volume"
  (volcontrol "Master" "1+"))

(defcommand amixer-Master-toggle () ()
  "Un/mutes Master volume"
  (volcontrol "Master" nil))

(defcommand amixer-Headphone-1- () ()
  "Lowers Headphone volume"
  (volcontrol "Headphone" "1-"))

(defcommand amixer-Headphone-1+ () ()
  "Raises Headphone volume"
  (volcontrol "Headphone" "1+"))

(defcommand amixer-Headphone-toggle () ()
  "Un/mutes Headphone volume"
  (volcontrol "Headphone" nil))

(defcommand amixer-sense-toggle () ()
  (message
   (concat "Headphone Jack Sense toggled"
           (run-shell-command "amixer sset 'Headphone Jack Sense' toggle" t))))

(define-keysym #x1008ff11 "XF86AudioLowerVolume")
(define-keysym #x1008ff12 "XF86AudioMute")
(define-keysym #x1008ff13 "XF86AudioRaiseVolume")

;;; Some bindings

(define-key *top-map* (kbd "XF86AudioLowerVolume")   "amixer-PCM-1-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume")   "amixer-PCM-1+")
(define-key *top-map* (kbd "XF86AudioMute")          "amixer-PCM-toggle")
(define-key *top-map* (kbd "C-XF86AudioLowerVolume") "amixer-Master-1-")
(define-key *top-map* (kbd "C-XF86AudioRaiseVolume") "amixer-Master-1+")
(define-key *top-map* (kbd "C-XF86AudioMute")        "amixer-Master-toggle")
(define-key *top-map* (kbd "M-XF86AudioLowerVolume") "amixer-Headphone-1-")
(define-key *top-map* (kbd "M-XF86AudioRaiseVolume") "amixer-Headphone-1+")
(define-key *top-map* (kbd "M-XF86AudioMute")        "amixer-Headphone-toggle")
(define-key *top-map* (kbd "S-XF86AudioMute")        "amixer-sense-toggle")
