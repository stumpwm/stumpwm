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
;; Translate between stumpwm key names and keysym names.
;;
;; Code:

(in-package #:stumpwm)

(defvar *stumpwm-name->keysym-name-translations* (make-hash-table :test #'equal)
  "Hashtable mapping from stumpwm key names to keysym names.")

(defun define-keysym-name (stumpwm-name keysym-name)
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

(define-keysym-name "RET" "Return")
(define-keysym-name "ESC" "Escape")
(define-keysym-name "TAB" "Tab")
(define-keysym-name "DEL" "BackSpace")
(define-keysym-name "SPC" "space")
(define-keysym-name "!" "exclam")
(define-keysym-name "\"" "quotedbl")
(define-keysym-name "$" "dollar")
(define-keysym-name "£" "sterling")
(define-keysym-name "%" "percent")
(define-keysym-name "&" "ampersand")
(define-keysym-name "'" "quoteright")   ;deprecated
(define-keysym-name "'" "apostrophe")
(define-keysym-name "`" "quoteleft")    ;deprecated
(define-keysym-name "`" "grave")
(define-keysym-name "&" "ampersand")
(define-keysym-name "(" "parenleft")
(define-keysym-name ")" "parenright")
(define-keysym-name "*" "asterisk")
(define-keysym-name "+" "plus")
(define-keysym-name "," "comma")
(define-keysym-name "-" "minus")
(define-keysym-name "." "period")
(define-keysym-name "/" "slash")
(define-keysym-name ":" "colon")
(define-keysym-name ";" "semicolon")
(define-keysym-name "<" "less")
(define-keysym-name "=" "equal")
(define-keysym-name ">" "greater")
(define-keysym-name "?" "question")
(define-keysym-name "@" "at")
(define-keysym-name "[" "bracketleft")
(define-keysym-name "\\" "backslash")
(define-keysym-name "]" "bracketright")
(define-keysym-name "^" "asciicircum")
(define-keysym-name "_" "underscore")
(define-keysym-name "#" "numbersign")
(define-keysym-name "{" "braceleft")
(define-keysym-name "|" "bar")
(define-keysym-name "}" "braceright")
(define-keysym-name "~" "asciitilde")
(define-keysym-name "«" "guillemotleft")
(define-keysym-name "»" "guillemotright")
(define-keysym-name "À" "Agrave")
(define-keysym-name "à" "agrave")
(define-keysym-name "Ç" "Ccedilla")
(define-keysym-name "ç" "ccedilla")
(define-keysym-name "É" "Eacute")
(define-keysym-name "é" "eacute")
(define-keysym-name "È" "Egrave")
(define-keysym-name "è" "egrave")
(define-keysym-name "Ê" "Ecircumflex")
(define-keysym-name "ê" "ecircumflex")
