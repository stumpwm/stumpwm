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
;; This file handles keymaps
;;
;; Code:

(in-package stumpwm)

(defstruct key
  char shift control meta alt hyper super)

(defun make-sparse-keymap ()
  (make-hash-table :test 'equalp))

(defun lookup-command (keymap command)
  "Return a list of keys that are bound to command"
  (let (acc)
    (maphash (lambda (k v)
	       (when (equal command v)
		 (push k acc)))
	     keymap)
    acc))

(defun lookup-key (keymap key &optional accept-default)
  (or (gethash key keymap)
      (and accept-default
	   (gethash t keymap))))

(defun key-mods-p (key)
  (or (key-shift key)
      (key-control key)
      (key-meta key)
      (key-alt key)
      (key-hyper key)
      (key-super key)))

(defun x11-mods (key)
  ;; FIXME: we need full mod support.
  (apply 'xlib:make-state-mask
	 (append
	  (when (key-shift key) '(:shift))
	  (when (key-control key) '(:control)))))

(define-condition kbd-parse ()
  () (:documentation "Raised when a kbd string failed to parse."))

(defun parse-mods (mods end)
  "MODS is a sequence of <MOD CHAR> #\- pairs. Return a list suitable
for passing as the last argument to (apply #'make-key ...)"
  (unless (evenp end)
    (signal 'kbd-parse))
  (apply #'nconc (loop for i from 0 below end by 2
		       if (char/= (char mods (1+ i)) #\-)
		       do (signal 'kbd-parse)
		       collect (case (char mods i)
				 (#\M (list :meta t))
				 (#\A (list :alt t))
				 (#\C (list :control t))
				 (#\H (list :hyper t))
				 (#\s (list :super t))
				 (#\S (list :shift t))
				 (t (signal 'kbd-parse))))))

(defun parse-char-name (string)
  "Return the char-code of the char whose name is STRING."
  (or (name-char string)
      (and (= (length string) 1)
	   (char-code (char string 0)))))

(defun parse-key (string)
  "Parse STRING and return a key structure."
  ;; FIXME: we want to return NIL when we get a kbd-parse error
  ;;(ignore-errors
    (let* ((p (when (> (length string) 2)
		(position #\- string :from-end t :end (- (length string) 1))))
	   (mods (parse-mods string (if p (1+ p) 0)))
	   (ch (parse-char-name (subseq string (if p (1+ p) 0)))))
      (and ch
	   (apply 'make-key :char ch mods))))
  
(defun parse-key-seq (keys)
  "KEYS is a key sequence. Parse it and return the list of keys."
  (mapcar 'parse-key (split-string keys)))

(defun kbd (keys)
  "Convert KEYS to the internal Emacs key representation.
KEYS should be a string constant in the format used for
saving keyboard macros ***(see `insert-kbd-macro')."
  ;; XXX: define-key needs to be fixed to handle a list of keys
  (first (parse-key-seq keys)))

(defun print-mods (key)
  (concatenate 'string
	       (when (key-control key) "C-")
	       (when (key-meta key) "M-")
	       (when (key-alt key) "A-")
	       (when (key-shift key) "S-")
	       (when (key-super key) "s-")
	       (when (key-hyper key) "H-")))

(defun print-key (key)
  (let ((ch (code-char (key-char key))))
    (format nil "~a~a" (print-mods key) (or (char-name ch) ch))))

(defun define-key (map key command)
  (setf (gethash key map) command))

(defun undefine-key (map key)
  (remhash key map))
