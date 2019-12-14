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

(defvar *stumpwm-name->keysym-name-translations* (make-hash-table :test #'equal)
  "Hashtable mapping from stumpwm key names to keysym names.")

(defun define-keysym-name (stumpwm-name keysym-name)
  "Define a key. Consider wrapping then using DEFALIASES instead."

(defmacro defaliases (&body aliases)
  "Define a mapping from a STUMPWM-NAME to KEYSYM-NAME.
This function is used to translate Emacs-like names to keysym
names."
  (setf (gethash stumpwm-name *stumpwm-name->keysym-name-translations*)
        keysym-name))

(defun stumpwm-name->keysym-name (stumpwm-name)
  (multiple-value-bind (value present-p)
      (gethash stumpwm-name *stumpwm-name->keysym-name-translations*)
    (declare (ignore present-p))
    value))

(defun keysym-name->stumpwm-name (keysym-name)
  (maphash (lambda (k v)
             (when (equal v keysym-name)
               (return-from keysym-name->stumpwm-name k)))
           *stumpwm-name->keysym-name-translations*))

(defun stumpwm-name->keysym (stumpwm-name)
  "Return the keysym corresponding to STUMPWM-NAME.
If no mapping for STUMPWM-NAME exists, then fallback by calling
KEYSYM-NAME->KEYSYM."
  (let ((keysym-name (stumpwm-name->keysym-name stumpwm-name)))
    (keysym-name->keysym (or keysym-name stumpwm-name))))

(defun keysym->stumpwm-name (keysym)
  "Return the stumpwm key name corresponding to KEYSYM.
If no mapping for the stumpwm key name exists, then fall back by
calling KEYSYM->KEYSYM-NAME."
  (let ((keysym-name (keysym->keysym-name keysym)))
    (or (keysym-name->stumpwm-name keysym-name)
        keysym-name)))

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
