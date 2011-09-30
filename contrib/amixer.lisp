;;; Amixer module for StumpWM.
;;;
;;; Copyright 2007 Amy Templeton, Jonathan Moore Liles, Ivy Foster.
;;;
;;; Maintainer: Ivy Foster
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
;;; along with this software; see the file COPYING.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;

;;; USAGE:
;;;
;;; Make sure you have your media keys (or whatever) mapped to the appropriate
;;; keysyms (using xmodmap), then put:
;;;
;;;     (load "/path/to/amixer.lisp")
;;;
;;; ...in your ~/.stumpwmrc, followed by some keybindings (according
;;; to your preference)

;;; TODO:
;;;
;;; Make the `defvolcontrol' macro create all the necessary commands at once.
;;;
;;;   - Should it just create, say, amixer-pcm, which would be passed an
;;;     argument? i.e., (define-key *this-map* (kbd "e") "amixer-pcm 1-")
;;;
;;;   - Else, figure out how to make the macro not error when converting a
;;;     string to a symbol for the name of the command

;;; Code:

(in-package :stumpwm)

(defun volcontrol (channel amount)
  (let ((percent (parse-integer
		  (run-shell-command
		   (concat "amixer sset " channel " " (or amount "toggle")
			   "| tail -1"
			   "| sed 's/^.*\\[\\([[:digit:]]\\+\\)%\\].*$/\\1/'")
		   t))))
    (message
     (concat "Mixer: " channel " " (or amount "toggled")
	     (format nil "~C^B~A%" #\Newline percent) "^b [^[^7*"
             (bar percent 50 #\# #\:) "^]]"))))

(defmacro defvolcontrol (name channel valence)
  `(defcommand ,name () ()
     (volcontrol ,channel ,valence)))

(defvolcontrol amixer-PCM-1- "PCM" "1-")
(defvolcontrol amixer-PCM-1+ "PCM" "1+")
(defvolcontrol amixer-PCM-toggle "PCM" "toggle")

(defvolcontrol amixer-Front-1- "Front" "1-")
(defvolcontrol amixer-Front-1+ "Front" "1+")
(defvolcontrol amixer-Front-toggle "Front" "toggle")

(defvolcontrol amixer-Master-1- "Master" "1-")
(defvolcontrol amixer-Master-1+ "Master" "1+")
(defvolcontrol amixer-Master-toggle "Master" "toggle")

(defvolcontrol amixer-Headphone-1- "Headphone" "1-")
(defvolcontrol amixer-Headphone-1+ "Headphone" "1+")
(defvolcontrol amixer-Headphone-toggle "Headphone" "toggle")

(defcommand amixer-sense-toggle () ()
  (message
   (concat "Headphone Jack Sense toggled"
           (run-shell-command "amixer sset 'Headphone Jack Sense' toggle" t))))

;;; End of file
