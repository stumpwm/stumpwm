;; Copyright (C) 2006-2008 Matthew Kennedy
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
;; Alias Emacs names for X11 keysyms.
;;
;; Code:

(in-package #:stumpwm)

(defun define-keysym-name (stumpwm-name keysym-name &optional deprecated)
  "Define a key. Consider wrapping then using DEFALIASES instead."
  (declare (ignore deprecated));For now.
  (setf (gethash stumpwm-name *name-keysym-translations*) (get-keysym keysym-name)
        (gethash keysym-name *ugly-to-pretty*) stumpwm-name))

(defvar *ugly-to-pretty* (make-hash-table :test 'equal)
  "Mapping of Emacs to non-emacs names, for pretty printing of keys.")

(defmacro defaliases (&body aliases)
  "Define a mapping from a STUMPWM-NAME to KEYSYM-NAME.
This function is used to translate Emacs-like names to keysym
names."
  `(progn
     ,@(loop for a in aliases
             collect `(define-keysym-name ,(car a) ,(cadr a)))))

(defun keysym->stumwpm-name (keysym)
  "Show a pretty Emacs version of the KEYSYM if it has one.
If not, show the ugly one."
  (or (gethash (keysym-name keysym) *ugly-to-pretty*)
      (keysym-name keysym)))

(defaliases
  ("RET" "Return")
  ("ESC" "Escape")
  ("TAB" "Tab")
  ("DEL" "BackSpace")
  ("SPC" "space")
  ("!" "exclam")
  ("\"" "quotedbl")
  ("$" "dollar")
  ("%" "percent")
  ("&" "ampersand")
  ("'" "quoteright" t) ;deprecated
  ("'" "apostrophe")
  ("`" "quoteleft" t) ;deprecated
  ("`" "grave")
  ("&" "ampersand")
  ("(" "parenleft")
  (")" "parenright")
  ("*" "asterisk")
  ("+" "plus")
  ("," "comma")
  ("-" "minus")
  ("." "period")
  ("/" "slash")
  (":" "colon")
  (";" "semicolon")
  ("<" "less")
  ("=" "equal")
  (">" "greater")
  ("?" "question")
  ("@" "at")
  ("[" "bracketleft")
  ("\\" "backslash")
  ("]" "bracketright")
  ("^" "asciicircum")
  ("_" "underscore")
  ("#" "numbersign")
  ("{" "braceleft")
  ("|" "bar")
  ("}" "braceright")
  ("~" "asciitilde"))
