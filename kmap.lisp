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
;; This file handles keymaps
;;
;; Code:

(in-package stumpwm)

(export '(*top-map*
          define-key
	  kbd
	  lookup-command
	  lookup-key
	  make-sparse-keymap
	  undefine-key))

(defvar *top-map* nil
  "The top level key map. This is where you'll find the binding for the
@dfn{prefix map}.")

(defstruct key
  keysym shift control meta alt hyper super)

(defun make-sparse-keymap ()
  "Create an empty keymap. If you want to create a new list of bindings
in the key binding tree, this is where you start. To hang frame
related bindings off @kbd{C-t C-f} one might use the following code:

@example
\(defvar *my-frame-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd \"f\") \"curframe\")
    (stumpwm:define-key m (stumpwm:kbd \"M-b\") \"move-focus left\")
    m ; NOTE: this is important
  ))

\(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd \"C-f\") '*my-frame-bindings*)
@end example"
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

(defun x11-mods (key &optional with-numlock)
  "Return the modifiers for key in a format that clx
understands. if WITH-NUMLOCK is non-nil then include the numlock
modifier. Most of the time numlock just gets in the way."
  (let (mods)
    (when (key-shift key) (push :shift mods))
    (when (key-control key) (push :control mods))
    (when (key-meta key) (setf mods (append (modifiers-meta *modifiers*) mods)))
    (when (key-alt key) (setf mods (append (modifiers-alt *modifiers*) mods)))
    (when (key-hyper key) (setf mods (append (modifiers-hyper *modifiers*) mods)))
    (when (key-super key) (setf mods (append (modifiers-super *modifiers*) mods)))
    (when with-numlock (setf mods (append (modifiers-numlock *modifiers*) mods)))
    (apply 'xlib:make-state-mask mods)))

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

(defun parse-key (string)
  "Parse STRING and return a key structure."
  ;; FIXME: we want to return NIL when we get a kbd-parse error
  ;;(ignore-errors
  (let* ((p (when (> (length string) 2)
              (position #\- string :from-end t :end (- (length string) 1))))
         (mods (parse-mods string (if p (1+ p) 0)))
         (keysym (stumpwm-name->keysym (subseq string (if p (1+ p) 0)))))
    (and keysym
         (apply 'make-key :keysym keysym mods))))

(defun parse-key-seq (keys)
  "KEYS is a key sequence. Parse it and return the list of keys."
  (mapcar 'parse-key (split-string keys)))

(defun kbd (keys)
  "This compiles a key string into a key structure used by
`define-key', `undefine-key', `set-prefix-key' and
others."
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
  (format nil "~a~a"
          (print-mods key)
          (keysym->stumpwm-name (key-keysym key))))

(defun print-key-seq (seq)
  (format nil "^5*~{~a~^ ~}^n" (mapcar 'print-key seq)))

(defun define-key (map key command)
  "Add a keybinding mapping the key, @var{key}, to the command,
@var{command}, in the specified keymap. For example,

@example
\(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd \"C-z\") \"echo Zzzzz...\")
@end example

Now when you type C-t C-z, you'll see the text ``Zzzzz...'' pop up."
  (setf (gethash key map) command)
  ;; We need to tell the X server when changing the top-map bindings.
  (when (eq map *top-map*)
    (sync-keys)))

(defun undefine-key (map key)
  "Clear the key binding in the specified keybinding."
  (remhash key map)
  ;; We need to tell the X server when changing the top-map bindings.
  (when (eq map *top-map*)
    (sync-keys)))

(defun lookup-key-sequence (kmap key-seq)
  "Return the command bound to the key sequenc, KEY-SEQ, in keymap KMAP."
  (when (and (symbolp kmap)
             (boundp kmap)
             (hash-table-p (symbol-value kmap)))
    (setf kmap (symbol-value kmap)))
  (check-type kmap hash-table)
  (let* ((key (car key-seq))
         (cmd (lookup-key kmap key)))
    (cond ((null (cdr key-seq))
           cmd)
          (cmd
           (if (or (hash-table-p cmd)
                   (and (symbolp cmd)
                        (boundp cmd)
                        (hash-table-p (symbol-value cmd))))
               (lookup-key-sequence cmd (cdr key-seq))
               cmd))
          (t nil))))

(defun kmap-symbol-p (x)
  (and (symbolp x)
       (boundp x)
       (hash-table-p (symbol-value x))))

(defun kmap-p (x)
  (or (hash-table-p x)
      (kmap-symbol-p x)))

(defun dereference-kmaps (kmaps)
  (mapcar (lambda (m)
            (if (kmap-symbol-p m)
                (symbol-value m)
                m))
          kmaps))

(defun search-kmap (command keymap &key (test 'equal))
  "Search the keymap for the specified binding. Return the key
sequences that run binding."
  (labels ((search-it (cmd kmap key-seq)
             (when (and (symbolp kmap)
                        (boundp kmap)
                        (hash-table-p (symbol-value kmap)))
               (setf kmap (symbol-value kmap)))
             (check-type kmap hash-table)
             (let (cmds)
               (maphash (lambda (k v)
                          (cond ((funcall test v cmd)
                                 (push (cons k key-seq) cmds))
                                ((kmap-p v)
                                 (setf cmds (append cmds (search-it cmd v (cons k key-seq)))))))
                        kmap)
               cmds)))
    (mapcar 'reverse (search-it command keymap nil))))


;;; The Top Map

(defvar *top-map-list* nil)

(defun push-top-map (new-top)
  (push *top-map* *top-map-list*)
  (setf *top-map* new-top)
  (sync-keys))

(defun pop-top-map ()
  (when *top-map-list*
    (setf *top-map* (pop *top-map-list*))
    (sync-keys)
    t))
