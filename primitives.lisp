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

(defvar *timeout-frame-indicator-wait* 1
  "The amount of time a frame indicator timeout takes.")

(defvar *timeout* 0
  "Internal variable. When this variable is >0 then a timeout will
occur in that many seconds.")

(defun reset-timeout ()
  "Set the timer to timeout in *timeout-wait* seconds."
  (setf *timeout* *timeout-wait*))

(defun reset-timeout-for-frame-indicator ()
  "Set the timer to timeout in *timeout-frame-indicator-wait* seconds."
  (setf *timeout* *timeout-frame-indicator-wait*))

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

(defvar *internal-loop-hook* '()
  "A hook called inside stumpwm's inner loop.")

;; Data types and globals used by stumpwm

(defvar *display* nil
  "The display for the X server")

(defvar *shell-program* "/bin/sh"
  "The shell program used by SHELL-COMMAND.")

(defvar *maxsize-border-width* 1
  "The default border width for maxsize windows.")

(defvar *transient-border-width* 1
  "The default border width for transient windows.")

(defvar *normal-border-width* 0
  "The default border width for normal windows.")
    
;; FIXME: This variable is set only once but it needs to be set after
;; the display is opened. So should it have +'s around it even though
;; it's defined as a variable?
(defvar +wm-delete-window+ nil
  "The atom used to delete a window.")

(defvar +wm-take-focus+ nil
  "The WM_TAKE_FOCUS atom")

(defvar +wm-state+ nil
  "the WM_STATE atom")

(defvar +wm-protocols+ nil
  "the WM_PROTOCOLS atom")

;; Window states
(defconstant +withdrawn-state+ 0)
(defconstant +normal-state+ 1)
(defconstant +iconic-state+ 3)  

(defvar *window-events* '(:structure-notify
			  :property-change
			  :colormap-change
			  :focus-change)
  "The events to listen for on managed windows.")

;; Message window variables
(defvar *message-window-padding* 5)

;; line editor
(defvar *editor-bindings* nil
  "A list of key-bindings for line editing.")

;; default values. use the set-* functions to these attributes
(defparameter +default-foreground-color+ "White")
(defparameter +default-background-color+ "Black")
(defparameter +default-border-color+ "White")
(defparameter +default-font-name+ "9x15bold")

(defvar *top-level-error-action* :message
  "If an error is encountered at the top level, in
STUMPWM-INTERNAL-LOOP, then this variable decides what action
shall be taken. By default it will print a message to the screen
and to *standard-output*. 

Valid values are :message, :break. :break will break to the
debugger. This can be problematic because if the user hit's a
mapped key the ENTIRE keyboard will be frozen and you will have
to login remotely to regain control.")

(defstruct frame
  (number nil :type integer)
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
  ;; From this frame tree a list of frames can be gathered
  frame-tree
  modifiers
  border-color
  fg-color
  bg-color
  font
  current-frame
  ;; A list of all mapped windows. Used for navigating windows.
  mapped-windows
  ;; A hash table for stumpwm properties on any absorbed windows.
  window-hash
  message-window
  input-window
  frame-window
  ;; The window that gets focus when no window has focus
  focus-window)

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
  "Just like maphash except it accumulates the result in a list."
  (let ((accum nil))
    (labels ((mapfn (key val)
               (push (funcall fn key val) accum)))
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

(defun find-free-number (l)
  "Return a number that is not in the list l."
  (let* ((nums (sort l #'<))
	 (new-num (loop for n from 0 to (or (car (last nums)) 0)
			for i in nums
			when (/= n i)
			do (return n))))
    (dformat "Free number: ~S~%" nums)
    (if new-num
	new-num
      ;; there was no space between the numbers, so use the last + 1
      (if (car (last nums))
	  (1+ (car (last nums)))
	0))))


(defun remove-plist (plist &rest keys)
  "Remove the keys from the plist.
Useful for re-using the &REST arg after removing some options."
  (do (copy rest)
      ((null (setq rest (nth-value 2 (get-properties plist keys))))
       (nreconc copy plist))
    (do () ((eq plist rest))
      (push (pop plist) copy)
      (push (pop plist) copy))
    (setq plist (cddr plist))))

(defun run-prog (prog &rest opts &key args (wait t) &allow-other-keys)
  "Common interface to shell. Does not return anything useful."
  #+gcl (declare (ignore wait))
  (setq opts (remove-plist opts :args :wait))
  #+allegro (apply #'excl:run-shell-command (apply #'vector prog prog args)
                   :wait wait opts)
  #+(and clisp      lisp=cl)
  (apply #'ext:run-program prog :arguments args :wait wait opts)
  #+(and clisp (not lisp=cl))
  (if wait
      (apply #'lisp:run-program prog :arguments args opts)
      (lisp:shell (format nil "~a~{ '~a'~} &" prog args)))
  #+cmu (apply #'ext:run-program prog args :wait wait opts)
  #+gcl (apply #'si:run-process prog args)
  #+liquid (apply #'lcl:run-program prog args)
  #+lispworks (apply #'sys::call-system
                     (format nil "~a~{ '~a'~}~@[ &~]" prog args (not wait))
                     opts)
  #+lucid (apply #'lcl:run-program prog :wait wait :arguments args opts)
  #+sbcl (apply #'sb-ext:run-program prog args :wait wait opts)
  #-(or allegro clisp cmu gcl liquid lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'run-prog prog opts)))

(defun getenv (var)
  "Return the value of the environment variable."
  #+allegro (sys::getenv (string var))
  #+clisp (ext:getenv (string var))
  #+(or cmu scl)
  (cdr (assoc (string var) ext:*environment-list* :test #'equalp
              :key #'string))
  #+gcl (si:getenv (string var))
  #+lispworks (lw:environment-variable (string var))
  #+lucid (lcl:environment-variable (string var))
  #+mcl (ccl::getenv var)
  #+sbcl (sb-posix:getenv (string var))
  #-(or allegro clisp cmu gcl lispworks lucid mcl sbcl scl)
  (error 'not-implemented :proc (list 'getenv var)))

(defun (setf getenv) (val var)
  "Set an environment variable."
  #+allegro (setf (sys::getenv (string var)) (string val))
  #+clisp (setf (ext:getenv (string var)) (string val))
  #+(or cmu scl)
  (let ((cell (assoc (string var) ext:*environment-list* :test #'equalp
                     :key #'string)))
    (if cell
        (setf (cdr cell) (string val))
        (push (cons (intern (string var) "KEYWORD") (string val))
              ext:*environment-list*)))
  #+gcl (si:setenv (string var) (string val))
  #+lispworks (setf (lw:environment-variable (string var)) (string val))
  #+lucid (setf (lcl:environment-variable (string var)) (string val))
  #+sbcl (sb-posix:putenv (format nil "~A=~A" (string var) (string val)))
  #-(or allegro clisp cmu gcl lispworks lucid sbcl scl)
  (error 'not-implemented :proc (list '(setf getenv) var)))


(defun split-string (string &optional (separators " 
"))
  "Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
***If SEPARATORS is absent, it defaults to \"[ \f\t\n\r\v]+\".

If there is match for SEPARATORS at the beginning of STRING, we do not
include a null substring for that.  Likewise, if there is a match
at the end of STRING, we don't include a null substring for that.

Modifies the match data; use `save-match-data' if necessary."
  ;; FIXME: This let is here because movitz doesn't 'lend optional'
  (let ((seps separators))
    (labels ((sep (c)
		  (find c seps :test #'char=)))
      (loop for i = (position-if (complement #'sep) string) 
	    then (position-if (complement #'sep) string :start j)
	    while i
	    as j = (position-if #'sep string :start i)
	    collect (subseq string i j)
	    while j))))

(defun dformat (fmt &rest args)
#+ignore
  (with-open-file (s #p"/tmp/stumplog" :direction :output :if-exists :append :if-does-not-exist :create)
    (apply 'format s fmt args)))

;;; 
;;; formatting routines

(defun format-expand (fmt-alist fmt &rest args)
  (let* ((chars (coerce fmt 'list))
	 (output "")
	 (cur chars))
    ;; FIXME: this is horribly inneficient
    (loop
     (cond ((null cur)
	    (return-from format-expand output))
	   ((char= (car cur) #\%)
	    (setf cur (cdr cur))
	    (let ((fmt (cadr (assoc (car cur) fmt-alist :test 'char=))))
	      (setf output (concatenate 'string output
					(cond (fmt
					       ;; it can return any type, not jut as string.
					       (format nil "~a" (apply fmt args)))
					      ((char= (car cur) #\%)
					       (string #\%))
					      (t
					       (string #\%) (string (car cur)))))))
	    (setf cur (cdr cur)))
	   (t
	    (setf output (concatenate 'string output (string (car cur)))
		  cur (cdr cur)))))))

(defvar *window-formatters* '((#\n window-number)
			      (#\s fmt-window-status)
			      (#\t window-name)
			      (#\c window-class)
			      (#\i window-res-name))
  "an alist containing format character format function pairs for formatting window lists.")

(defvar *window-format* "%n%s%t"
  "The format string for echoing the window list.")

(defun font-height (font)
  (+ (xlib:font-descent font)
     (xlib:font-ascent font)))
