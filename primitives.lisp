;; Copyright (C) 2003 Shawn Betts
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
;; This file contains primitive data structures and functions used
;; throughout stumpwm.
;;
;; Code:

(in-package :stumpwm)

(defun char->keysym (ch)
  "Convert a char to a keysym"
  (first (xlib:character->keysyms ch)))


;;; Message Timer

(defvar *timeout-wait* 5
  "The amount of time a timeout takes.")

;; Internal variable. When this variable is >0 then a timeout will
;; occur in that many seconds.
(defvar *timeout* 0)

(defun reset-timeout ()
  "Set the timer to timeout in *timeout-wait* seconds."
  (setf *timeout* *timeout-wait*))

;;; Hooks

(defvar *map-window-hook* '()
  "A hook called whenever a window is mapped.")

(defvar *unmap-window-hook* '()
  "A hook called whenever a window is withdrawn.")

(defvar *new-window-hook* '()
  "A hook called whenever a window is created.")

(defvar *destroy-window-hook* '()
  "A hook called whenever a window is destroyed.")

(defvar *focus-window-hook* '()
  "A hook called when a window is given focus.")

(defvar *unfocus-window-hook* '()
  "A hook called when a window loses focus.")

(defvar *start-hook* '()
  "A hook called when stumpwm starts.")

;; Data types and globals used by stumpwm

(defvar *display* nil
  "The display for the X server")

(defvar *font-name* "9x15bold"
  "The name of the font to use when stumpwm displays messages.")

(defvar *prefix-key* (char->keysym #\t)
  "The key to use as the prefix key")

(defvar *prefix-modifiers* '(:control)
  "The modifier list for the prefix key")

(defvar *shell-program* "/bin/sh"
  "The shell program used by SHELL-COMMAND.")

(defvar *window-format-fn* 'default-window-format
		   "The function called when printing a window list. It is passed the
screen and window. It should return a string.")

(defstruct key-binding
  "A structure to map from a keystroke to a command."
  (key nil :type xlib:keysym)
  (mods nil :type list)
  (fn nil :type function))

(defparameter *key-bindings* (make-hash-table)
  "An alist of keysym function pairs.")

;; FIXME: This variable is set only once but it needs to be set after
;; the display is opened. So should it have +'s around it even though
;; it's defined as a variable?
(defvar +wm-delete-window+ nil
  "The atom used to delete a window.")

;; Window states
(defconstant +withdrawn-state+ 0)
(defconstant +normal-state+ 1)
(defconstant +iconic-state+ 3)  

;; Message window constants
(defvar *message-window-padding* 5)

;; line editor
(defvar *editor-bindings* 
  "A list of key-bindings for line editing."
  nil)

(defstruct frame
  number
  x
  y
  width
  height
  window)

(defstruct modifiers
  (meta nil)
  (alt nil)
  (hyper nil)
  (super nil))
  

(defstruct screen
  number
  frame-tree
  ;; 
  modifiers
  font
  current-frame
  current-window
  ;; A list of all mapped windows. Used for navigating windows.
  mapped-windows
  ;; A hash table for stumpwm properties on any absorbed windows.
  window-table
  message-window
  input-window
  frame-window)

(defvar *screen-list* '()
  "List of screens")

;;; Hook functionality

(defun run-hook-with-args (hook &rest args)
  "Call each function in HOOK and pass args to it" 
  (dolist (fn hook)
    (apply fn args)))

(defun run-hook (hook)
  "Call each function in HOOK."
  (run-hook-with-args hook))

(defmacro add-hook (hook fn)
  "Add a function to a hook."
  `(setf ,hook (adjoin ,fn ,hook)))

;; Misc. utility functions

(defun conc1 (list arg)
  "Append arg to the end of list"
  (nconc list (list arg)))

(defun sort1 (list sort-fn)
  "Return a sorted copy of list."
  (let ((copy (copy-list list)))
    (sort copy sort-fn)))

(defun mapcar-hash (fn hash)
  "Just like maphash except it accumulates the result in a list and
calls fn on the value for the key hash-key, not the pair."
  (let ((accum nil))
    (labels ((mapfn (key val)
	       (declare (ignorable key))
	       (push (funcall fn val) accum)))
      (maphash #'mapfn hash))
    accum))

(defun is-modifier (keysym)
  "Return t if keycode is a modifier"
  (member keysym (list (char->keysym :character-set-switch)
		       (char->keysym :left-shift)
		       (char->keysym :right-shift)
		       (char->keysym :left-control)
		       (char->keysym :right-control)
		       (char->keysym :caps-lock)
		       (char->keysym :shift-lock)
		       (char->keysym :left-meta)
		       (char->keysym :right-meta)
		       (char->keysym :left-alt)
		       (char->keysym :right-alt)
		       (char->keysym :left-super)
		       (char->keysym :right-super)
		       (char->keysym :left-hyper)
		       (char->keysym :right-hyper))))
