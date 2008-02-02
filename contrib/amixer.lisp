;;; Amixer module for StumpWM.
;;;
;;; Copyright 2007 Amy Templeton, Jonathan Moore Liles.
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

(defvar +amixer-channels+ '("PCM" "Master" "Headphone"))
(defvar +amixer-options+ '(nil "1+" "1-"))

;; A command to create volume-control commands
(defun def-volcontrol (channel amount)
  "Commands for controling the volume"
  (define-stumpwm-command
      (concat "amixer-" channel "-" (or amount "toggle")) ()
    (let ((percent
           (parse-integer
            (run-shell-command
             (concat "amixer sset " channel " " (or amount "toggle") "| sed -n 's/^.*\\[\\([[:digit:]]\\+\\)%\\].*$/\\1/p' | head -1") t))))
      (message
       (concat "Mixer: " channel " " (or amount "toggled") (format nil "~C^B~A%" #\Newline percent) "^b [^[^7*"
               (bar percent 50 #\# #\:) "^]]")))))

(let ((channels +amixer-channels+))
  (loop while channels do
       (let ((options +amixer-options+))
         (loop while options do
              (def-volcontrol (car channels) (car options))
              (setq options (cdr options))))
       (setq channels (cdr channels))))

(define-stumpwm-command "amixer-sense-toggle" ()
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

