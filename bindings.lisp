;; Copyright (C) 2003-2008 Shawn Betts
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
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;; Commentary:
;;
;; define standard key bindings
;;
;; Code:

(in-package #:stumpwm)

(export '(*groups-map*
          *help-map*
          *root-map*
	  set-prefix-key))

(defvar *root-map* nil
  "This is the keymap by default bound to @kbd{C-t}. It is known as the @dfn{prefix map}.")

(defvar *groups-map* nil
  "The keymap that group related key bindings sit on. It is bound to @kbd{C-t g} by default.")

(defvar *help-map* nil
  "Help related bindings hang from this keymap")

;; Do it this way so its easier to wipe the map and get a clean one.
(when (null *top-map*)
  (setf *top-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "C-t") '*root-map*)
          m)))

(when (null *root-map*)
  (setf *root-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "c") "exec xterm")
          (define-key m (kbd "C-c") "exec xterm")
          (define-key m (kbd "e") "emacs")
          (define-key m (kbd "C-e") "emacs")
          (define-key m (kbd "n") "pull-hidden-next")
          (define-key m (kbd "C-n") "pull-hidden-next")
          (define-key m (kbd "M-n") "next")
          (define-key m (kbd "C-M-n") "next-in-frame")
          (define-key m (kbd "SPC") "pull-hidden-next")
          (define-key m (kbd "C-SPC") "pull-hidden-next")
          (define-key m (kbd "p") "pull-hidden-previous")
          (define-key m (kbd "C-p") "pull-hidden-previous")
          (define-key m (kbd "M-p") "prev")
          (define-key m (kbd "C-M-p") "prev-in-frame")
          (define-key m (kbd "w") "windows")
          (define-key m (kbd "C-w") "windows")
          (define-key m (kbd "W") "place-existing-windows")
          (define-key m (kbd "k") "delete")
          (define-key m (kbd "C-k") "delete")
          (define-key m (kbd "K") "kill")
          (define-key m (kbd "b") "banish")
          (define-key m (kbd "C-b") "banish")
          (define-key m (kbd "a") "time")
          (define-key m (kbd "C-a") "time")
          (define-key m (kbd "'") "select")
          (define-key m (kbd "\"") "windowlist")
          (define-key m (kbd "C-t") "pull-hidden-other")
          (define-key m (kbd "M-t") "other-in-frame")
          (define-key m (kbd "!") "exec")
          (define-key m (kbd "C-g") "abort")
          (define-key m (kbd "0") "pull 0")
          (define-key m (kbd "1") "pull 1")
          (define-key m (kbd "2") "pull 2")
          (define-key m (kbd "3") "pull 3")
          (define-key m (kbd "4") "pull 4")
          (define-key m (kbd "5") "pull 5")
          (define-key m (kbd "6") "pull 6")
          (define-key m (kbd "7") "pull 7")
          (define-key m (kbd "8") "pull 8")
          (define-key m (kbd "9") "pull 9")
          (define-key m (kbd "R") "remove")
          (define-key m (kbd "s") "vsplit")
          (define-key m (kbd "S") "hsplit")
          (define-key m (kbd "r") "iresize")
          (define-key m (kbd "o") "fnext")
          (define-key m (kbd "TAB") "fnext")
          (define-key m (kbd "f") "fselect")
          (define-key m (kbd "F") "curframe")
          (define-key m (kbd "t") "meta C-t")
          (define-key m (kbd "C-N") "number")
          (define-key m (kbd ";") "colon")
          (define-key m (kbd ":") "eval")
          (define-key m (kbd "C-h") "help")
          (define-key m (kbd "-") "fclear")
          (define-key m (kbd "Q") "only")
          (define-key m (kbd "Up") "move-focus up")
          (define-key m (kbd "Down") "move-focus down")
          (define-key m (kbd "Left") "move-focus left")
          (define-key m (kbd "Right") "move-focus right")
          (define-key m (kbd "M-Up") "move-window up")
          (define-key m (kbd "M-Down") "move-window down")
          (define-key m (kbd "M-Left") "move-window left")
          (define-key m (kbd "M-Right") "move-window right")
          (define-key m (kbd "v") "version")
          (define-key m (kbd "#") "mark")
          (define-key m (kbd "m") "lastmsg")
          (define-key m (kbd "C-m") "lastmsg")
          (define-key m (kbd "G") "vgroups")
          (define-key m (kbd "g") '*groups-map*)
          (define-key m (kbd "F1") "gselect 1")
          (define-key m (kbd "F2") "gselect 2")
          (define-key m (kbd "F3") "gselect 3")
          (define-key m (kbd "F4") "gselect 4")
          (define-key m (kbd "F5") "gselect 5")
          (define-key m (kbd "F6") "gselect 6")
          (define-key m (kbd "F7") "gselect 7")
          (define-key m (kbd "F8") "gselect 8")
          (define-key m (kbd "F9") "gselect 9")
          (define-key m (kbd "F10") "gselect 10")
          (define-key m (kbd "F11") "fullscreen")
          (define-key m (kbd "?") "help")
          (define-key m (kbd "+") "balance-frames")
          (define-key m (kbd "A") "title")
          (define-key m (kbd "h") '*help-map*)
          m)))

(when (null *groups-map*)
  (setf *groups-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "g") "groups")
          (define-key m (kbd "c") "gnew")
          (define-key m (kbd "n") "gnext")
          (define-key m (kbd "C-n") "gnext")
          (define-key m (kbd "SPC") "gnext")
          (define-key m (kbd "C-SPC") "gnext")
          (define-key m (kbd "p") "gprev")
          (define-key m (kbd "C-p") "gprev")
          (define-key m (kbd "o") "gother")
          (define-key m (kbd "'") "gselect")
          (define-key m (kbd "\"") "grouplist")
          (define-key m (kbd "m") "gmove")
          (define-key m (kbd "M") "gmove-marked")
          (define-key m (kbd "k") "gkill")
          (define-key m (kbd "A") "grename")
          (define-key m (kbd "r") "grename")
          (define-key m (kbd "1") "gselect 1")
          (define-key m (kbd "2") "gselect 2")
          (define-key m (kbd "3") "gselect 3")
          (define-key m (kbd "4") "gselect 4")
          (define-key m (kbd "5") "gselect 5")
          (define-key m (kbd "6") "gselect 6")
          (define-key m (kbd "7") "gselect 7")
          (define-key m (kbd "8") "gselect 8")
          (define-key m (kbd "9") "gselect 9")
          (define-key m (kbd "0") "gselect 10")
          m)))

(when (null *help-map*)
  (setf *help-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "v") "describe-variable")
          (define-key m (kbd "f") "describe-function")
          (define-key m (kbd "k") "describe-key")
          (define-key m (kbd "c") "describe-command")
          (define-key m (kbd "w") "where-is")
          m)))

(defcommand command-mode () ()
"Command mode allows you to type ratpoison commands without needing the
@key{C-t} prefix. Keys not bound in StumpWM will still get sent to the
current window. To exit command mode, type @key{C-g}."
  (message "Press C-g to exit command-mode.")
  (push-top-map *root-map*))

(defun set-prefix-key (key)
  "Change the stumpwm prefix key to KEY.
@example
\(stumpwm:set-prefix-key (stumpwm:kbd \"C-M-H-s-z\"))
@end example

This will change the prefix key to @key{Control} + @key{Meta} + @key{Hyper} + @key{Super} +
the @key{z} key. By most standards, a terrible prefix key but it makes a
great example."
  (check-type key key)
  (let (prefix)
    (dolist (i (lookup-command *top-map* '*root-map*))
      (setf prefix i)
      (undefine-key *top-map* i))
    (define-key *top-map* key '*root-map*)
    (let* ((meta (make-key :keysym (key-keysym key)))
           (old-cmd (concatenate 'string "meta " (print-key prefix)))
           (cmd (concatenate 'string "meta " (print-key key))))
      (dolist (i (lookup-command *root-map* old-cmd))
        (undefine-key *root-map* i))
      (define-key *root-map* meta cmd))
    (define-key *root-map* key "other")
    (sync-keys)))

(defcommand escape (key) ((:string "Key: "))
  "Set the prefix key. Here's how you would change the prefix key to @kbd{C-z}.

@example
escape C-z
@end example"
  (set-prefix-key (kbd key)))

(defcommand bind (key command) 
                 ((:text "Key Chord: ")
                  (:rest "Command: "))
  "Hang a key binding off the escape key."
  (define-key *root-map* (kbd key) command))
