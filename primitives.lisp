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


;;; Message Timer
(defvar *suppress-abort-messages* nil
  "Suppress abort message when non-nil.")

(defvar *timeout-wait* 5
  "The amount of time a timeout takes.")

(defvar *timeout-frame-indicator-wait* 1
  "The amount of time a frame indicator timeout takes.")

(defvar *frame-indicator-timer* nil
  "Keep track of the timer that hides the frame indicator.")

(defvar *message-window-timer* nil
  "Keep track of the timer that hides the message window.")

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
  "A hook called when a window is given focus. It is called with
  2 arguments: the current window and the last window (could be
  nil).")

(defvar *start-hook* '()
  "A hook called when stumpwm starts.")

(defvar *internal-loop-hook* '()
  "A hook called inside stumpwm's inner loop.")

(defvar *focus-frame-hook* '()
  "A hook called when a frame is given focus. The hook functions
  are called with 2 arguments: the current frame and the last
  frame.")

(defvar *new-frame-hook* '()
  "A hook called when a new frame is created. the hook is
  called with the frame as an argument.")

(defvar *message-hook* '()
  "A hook called whenever stumpwm displays a message. The hook
function is passed any number of arguments. Each argument is a
line of text.")

(defvar *top-level-error-hook* '()
  "Called when a top level error occurs. Note that this hook is
run before the error is dealt with according to
*top-level-error-action*.")

(defvar *focus-group-hook* '()
  "A hook called whenever stumpwm switches groups. It is called with 2 arguments: the current group and the last group.")

;; Data types and globals used by stumpwm

(defvar *display* nil
  "The display for the X server")

(defvar *shell-program* "/bin/sh"
  "The shell program used by SHELL-COMMAND.")

(defvar *maxsize-border-width* 1
  "The default border width for maxsize windows.")

(defvar *transient-border-width* 1
  "The default border width for transient windows.")

(defvar *normal-border-width* 1
  "The default border width for normal windows.")

(defvar *focus-color* "gray"
  "The color a window's border becomes when it is focused")

(defvar *unfocus-color* "Black"
  "The border color of an unfocused window")

(defparameter +netwm-supported+
  '(:_NET_SUPPORTING_WM_CHECK
    :_NET_NUMBER_OF_DESKTOPS
    :_NET_DESKTOP_GEOMETRY
    :_NET_DESKTOP_VIEWPORT
    :_NET_CURRENT_DESKTOP
    :_NET_WM_WINDOW_TYPE)
  "Supported NETWM properties.
Window types are in +WINDOW-TYPES+.")

(defparameter +netwm-window-types+
  '(
    ;; (:_NET_WM_WINDOW_TYPE_DESKTOP . :desktop)
    ;; (:_NET_WM_WINDOW_TYPE_DOCK . :dock)
    ;; (:_NET_WM_WINDOW_TYPE_TOOLBAR . :toolbar)
    ;; (:_NET_WM_WINDOW_TYPE_MENU . :menu)
    ;; (:_NET_WM_WINDOW_TYPE_UTILITY . :utility)
    ;; (:_NET_WM_WINDOW_TYPE_SPLASH . :splash)
    (:_NET_WM_WINDOW_TYPE_DIALOG . :dialog)
    (:_NET_WM_WINDOW_TYPE_NORMAL . :normal))
  "Alist mapping NETWM window types to keywords.
Include only those we are ready to support.")

;; Window states
(defconstant +withdrawn-state+ 0)
(defconstant +normal-state+ 1)
(defconstant +iconic-state+ 3)  

(defvar *window-events* '(:structure-notify
			  :property-change
			  :colormap-change
			  :focus-change)
  "The events to listen for on managed windows.")

(defvar *window-parent-events* '(:substructure-notify
				 :substructure-redirect)
  "The events to listen for on managed windows' parents.")

;; Message window variables
(defvar *message-window-padding* 5)

(defvar *message-window-gravity* :top-right
  "Message window gravity. One of :top-left, :top-right, :bottom-left,
:bottom-right or :center.")

;; line editor
(defvar *editor-bindings* nil
  "A list of key-bindings for line editing.")

(defvar *input-window-gravity* :top-right
  "input window gravity. see *message-window-gravity*.")

;; default values. use the set-* functions to these attributes
(defparameter +default-foreground-color+ "White")
(defparameter +default-background-color+ "Black")
(defparameter +default-window-background-color+ "Black")
(defparameter +default-border-color+ "White")
(defparameter +default-font-name+ "9x15bold")

;; Don't set these variables directly, use set-<var name> instead
(defvar *normal-gravity* :center)
(defvar *maxsize-gravity* :center)
(defvar *transient-gravity* :center)

(defvar *top-level-error-action* :abort
  "If an error is encountered at the top level, in
STUMPWM-INTERNAL-LOOP, then this variable decides what action
shall be taken. By default it will print a message to the screen
and to *standard-output*. 

Valid values are :message, :break, :abort. :break will break to the
debugger. This can be problematic because if the user hit's a
mapped key the ENTIRE keyboard will be frozen and you will have
to login remotely to regain control. :abort quits stumpmwm.")

(defvar *window-name-source* :title
  "This variable controls what is used for the window's
name. :title, :resource-name, :class are valid values.")

(defstruct window
  xwin
  width height
  gravity
  group
  frame
  number
  parent
  title
  user-title
  class
  type
  res
  unmap-ignores
  state
  normal-hints
  marked
  plist)

(defstruct frame
  (number nil :type integer)
  x 
  y
  width
  height
  window)

(defstruct group
  ;; A list of all windows in this group. They are of the window
  ;; struct variety.
  screen
  windows
  number
  name)

(defstruct (tile-group (:include group))
  ;; From this frame tree a list of frames can be gathered
  frame-tree
  last-frame
  current-frame)

(defstruct screen
  id
  host
  number
  ;; the list of groups available on this screen
  groups
  current-group
  border-color
  fg-color
  bg-color
  win-bg-color
  msg-border-width
  font
  ;; A list of all mapped windows. These are the raw
  ;; xlib:window's. window structures are stored in groups.
  mapped-windows
  ;; A list of withdrawn windows. These are of type stumpwm::window
  ;; and when they're mapped again they'll be put back in the group
  ;; they were in when they were unmapped unless that group doesn't
  ;; exist, in which case they go into the current group.
  withdrawn-windows
  message-window
  input-window
  frame-window
  ;; The window that gets focus when no window has focus
  focus-window
  ;; a bar along the top or bottom that displays anything you want.
  mode-line
  ;; graphic contexts
  message-gc
  marked-gc
  ;; the window that has focus
  focus
  last-msg
  last-msg-highlights)

(defmethod print-object ((object frame) stream)
  (format stream "#S(frame ~d ~a ~d ~d ~d ~d)" 
	  (frame-number object) (frame-window object) (frame-x object) (frame-y object) (frame-width object) (frame-height object)))

(defmethod print-object ((object window) stream)
  (format stream "#S(window ~s)" (window-name object)))

(defvar *frame-number-map* nil
  "Set this to a string to remap the regular frame numbers to more convenient keys.
For instance,

\"hutenosa\"

would map frame 0 to 7 to be selectable by hitting the
appropriate homerow key on a dvorak keyboard. Currently only
single char keys are supported.")

(defun get-frame-number-translation (frame)
  "Given a frame return its number translation using *frame-number-map* as a char."
  (let ((num (frame-number frame)))
    (or (and (< num (length *frame-number-map*))
	     (char *frame-number-map* num))
	;; translate the frame number to a char. FIXME: it loops after 9
	(char (prin1-to-string num) 0))))

(defstruct modifiers
  (meta nil)
  (alt nil)
  (hyper nil)
  (super nil)
  (numlock nil))
  
(defvar *all-modifiers* nil
  "A list of all keycodes that are considered modifiers")

(defvar *modifiers* nil
  "A mapping from modifier type to x11 modifier.")

(defmethod print-object ((object screen) stream)
  (format stream "#S<screen ~s>" (screen-number object)))

(defvar *screen-list* '()
  "List of screens")

;;; Hook functionality

(defun run-hook-with-args (hook &rest args)
  "Call each function in HOOK and pass args to it" 
  ;; FIXME: silently failing is bad
  (ignore-errors
    (dolist (fn hook)
      (apply fn args))))

(defun run-hook (hook)
  "Call each function in HOOK."
  ;; FIXME: silently failing is bad
  (ignore-errors
    (run-hook-with-args hook)))

(defmacro add-hook (hook fn)
  "Add a function to a hook."
  `(setf ,hook (adjoin ,fn ,hook)))

(defmacro remove-hook (hook fn)
  "Remove a function from a hook."
  `(setf ,hook (remove ,fn ,hook)))

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

(defun find-free-number (l &optional (min 0))
  "Return a number that is not in the list l."
  (let* ((nums (sort l #'<))
	 (new-num (loop for n from min to (or (car (last nums)) 0)
			for i in nums
			when (/= n i)
			do (return n))))
    (dformat 3 "Free number: ~S~%" nums)
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

(defun screen-display-string (screen)
  (format nil "DISPLAY=~a:~d.~d"
	  (screen-host screen)
	  (xlib:display-display *display*)
	  (screen-id screen)))

;;; XXX: DISPLAY env var isn't set for cmucl
(defun run-prog (prog &rest opts &key args (wait t) &allow-other-keys)
  "Common interface to shell. Does not return anything useful."
  #+gcl (declare (ignore wait))
  (setq opts (remove-plist opts :args :wait))
  #+allegro (apply #'excl:run-shell-command (apply #'vector prog prog args)
                   :wait wait opts)
  #+(and clisp      lisp=cl)
  (progn
    ;; Arg. We can't pass in an environment so just set the DISPLAY
    ;; variable so it's inherited by the child process.
    (setf (getenv "DISPLAY") (format nil "~a:~d.~d"
				     (screen-host (current-screen))
				     (xlib:display-display *display*)
				     (screen-id (current-screen))))
    (apply #'ext:run-program prog :arguments args :wait wait opts))
  #+(and clisp (not lisp=cl))
  (if wait
      (apply #'lisp:run-program prog :arguments args opts)
      (lisp:shell (format nil "~a~{ '~a'~} &" prog args)))
  #+cmu (apply #'ext:run-program prog args :output t :error t :wait wait opts)
  #+gcl (apply #'si:run-process prog args)
  #+liquid (apply #'lcl:run-program prog args)
  #+lispworks (apply #'sys::call-system
                     (format nil "~a~{ '~a'~}~@[ &~]" prog args (not wait))
                     opts)
  #+lucid (apply #'lcl:run-program prog :wait wait :arguments args opts)
  #+sbcl (apply #'sb-ext:run-program prog args :output t :error t :wait wait
		;; inject the DISPLAY variable in so programs show up
		;; on the right screen.
		:environment (cons (screen-display-string (current-screen))
				   (remove-if (lambda (str)
						(string= "DISPLAY=" str :end2 (min 8 (length str))))
					      (sb-ext:posix-environ)))
		opts)
  #-(or allegro clisp cmu gcl liquid lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'run-prog prog opts)))

;;; XXX: DISPLAY isn't set for cmucl 
(defun run-prog-collect-output (prog &rest args)
  "run a command and read its output."
  #+allegro (with-output-to-string (s) 
              (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                      :output s :wait t))
  ;; FIXME: this is a dumb hack but I don't care right now.
  #+clisp (with-output-to-string (s)
	    ;; Arg. We can't pass in an environment so just set the DISPLAY
	    ;; variable so it's inherited by the child process.
	    (setf (getenv "DISPLAY") (format nil "~a:~d.~d"
					     (screen-host (current-screen))
					     (xlib:display-display *display*)
					     (screen-id (current-screen))))
	    (let ((out (ext:run-program prog :arguments args :wait t :output :stream)))
	      (loop for i = (read-char out nil out)
		 until (eq i out)
		 do (write-char i s))))
  #+cmu (with-output-to-string (s) (ext:run-program prog args :output s :error s :wait t))
  #+sbcl (with-output-to-string (s)
	   (sb-ext:run-program prog args :output s :error s :wait t
			       ;; inject the DISPLAY variable in so programs show up
			       ;; on the right screen.
			       :environment (cons (screen-display-string (current-screen))
						  (remove-if (lambda (str)
							       (string= "DISPLAY=" str :end2 (min 8 (length str))))
							     (sb-ext:posix-environ)))))
  #-(or allegro clisp cmu sbcl)
  (error 'not-implemented :proc (list 'pipe-input prog args)))

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

(defun pathname-is-executable-p (pathname)
  #+sbcl
  (let ((filename (coerce (sb-int:unix-namestring pathname) 'base-string)))
    (and (eq (sb-unix:unix-file-kind filename) :file)
	 (sb-unix:unix-access filename sb-unix:x_ok)))
  ;; FIXME: add the code for clisp
  #-sbcl t)

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
      (or (loop for i = (position-if (complement #'sep) string) 
	     then (position-if (complement #'sep) string :start j)
	     as j = (position-if #'sep string :start (or i 0))
	     while i
	     collect (subseq string i j)
	     while j)
	  ;; the empty string causes the above to return NIL, so help
	  ;; it out a little.
	  '("")))))

(defvar *debug-level* 0
  "Set this to a number > 0 and debugging output will be
  produced. The higher the number the more output.")

(defvar *debug-stream* *error-output*
  "Where to send debugging output.")

(defun dformat (level fmt &rest args)
  (when (>= *debug-level* level)
    (multiple-value-bind (sec m h) (decode-universal-time (get-universal-time))
      (format *debug-stream* "~d:~d:~d " h m sec))
    ;; strip out non base-char chars quick-n-dirty like
    (write-string (map 'string (lambda (ch)
                                 (if (typep ch 'base-char)
                                     ch #\?))
                       (apply 'format nil fmt args))
                  *debug-stream*)))

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
             ;; if % is the last char in the string then it's a literal.
             ((and (char= (car cur) #\%)
                   (cdr cur))
              (setf cur (cdr cur))
              (let* ((tmp (loop while (and cur (char<= #\0 (car cur) #\9))
                             collect (pop cur)))
                     (len (and tmp (parse-integer (coerce tmp 'string)))))
                (if (null cur)
                    (format t "%~a" len)
                    (let* ((fmt (cadr (assoc (car cur) fmt-alist :test 'char=)))
                           (str (cond (fmt
                                       ;; it can return any type, not jut as string.
                                       (format nil "~a" (apply fmt args)))
                                      ((char= (car cur) #\%)
                                       (string #\%))
                                      (t
                                       (concatenate 'string (string #\%) (string (car cur)))))))
                      ;; crop string if needed
                      (setf output (concatenate 'string output (if len 
                                                                   (subseq str 0 (min len (length str)))
                                                                   str)))
                      (setf cur (cdr cur))))))
             (t
              (setf output (concatenate 'string output (string (car cur)))
                    cur (cdr cur)))))))

(defvar *window-formatters* '((#\n window-number)
			      (#\s fmt-window-status)
			      (#\t window-name)
			      (#\c window-class)
			      (#\i window-res)
			      (#\m fmt-window-marked))
  "an alist containing format character format function pairs for formatting window lists.")

(defvar *window-format* "%m%n%s%50t"
  "The format string for echoing the window list. Note the title
is cropped to 50 characters.")

(defvar *group-formatters* '((#\n group-number)
			      (#\s fmt-group-status)
			      (#\t group-name))
  "an alist containing format character format function pairs for formatting window lists.")

(defvar *group-format* "%n%s%t"
  "The format string for echoing the window list.")

(defun font-height (font)
  (+ (xlib:font-descent font)
     (xlib:font-ascent font)))

(defvar *x-selection* nil
  "This holds stumpwm's current selection. It is generally set
when killing text in the input bar.")

;; This is here to avoid warnings
(defvar *top-map* nil
  "Top level bindings.")

(defvar *last-command* nil
  "Set to the last interactive command run.")

(defvar *max-last-message-size* 20
  "how many previous messages to keep.")

(defvar *record-last-msg-override* nil
  "assign this to T and messages won't be recorded. It is
recommended this is assigned using LET.")

(defvar *suppress-echo-timeout* nil
  "Asign this T and messages will not time out. It is recommended this is assigned using LET.")

(defvar *run-or-raise-all-groups* t
  "When this is T the run-or-raise function searches all groups
  for a running instance. Set it to NIL to search only the
  current group.")

(defvar *deny-map-request* nil
  "A list of window names, window types, and class-id resource-id cons
pairs that stumpwm should deny matching windows' requests to become
mapped for the first time.

If set to T, then deny all map requests.

If a window type is added to the list stumpwm denies map
requests for the window type. Valid window types are :normal,
:transient, :maxsize.

If a the resource-id is nil in a class-id, resource-id cons pair
then only the class ID is matched.")

(defvar *deny-raise-request* nil
  "A list of window names, window types, and class-id resource-id
cons pairs that stumpwm should deny matching windows' requests to
be raised and focused.

If set to T, then deny all raise requests.

If a window type is added to the list stumpwm denies
map requests for the window type. Valid window types are :normal,
:transient, :maxsize.

If a the resource-id is nil in a class-id, resource-id cons pair
then only the class ID is matched.")

(defvar *suppress-deny-messages* nil
  "Set this to T so stumpwm doesn't notify you of denied raise/map requests.")

;; FIXME: maybe this should be a method to allow user customizable matching?
(defun match-res-or-type (window res)
  (or (and (stringp res)
           (string-equal (window-name window) res))
      (and (symbolp res)
           (eq (window-type window) res))
      ;; if its a cons expect a class . res pair
      (and (consp res)
           (stringp (car res))
           (string-equal (window-class window) (car res))
           ;; the res can be nil in order to match only the class
           (or (null (cdr res))
               (and (stringp (cdr res))
                    (string-equal (window-res window) (cdr res)))))))

(defun deny-request-p (window deny-list)
  (or (eq deny-list t)
      (and
       (listp deny-list)
       (find-if (lambda (res)
                  (match-res-or-type window res))
                deny-list)
       t)))

(defun list-splice-replace (item list &rest replacements)
  "splice REPLACEMENTS into LIST where ITEM is, removing
ITEM. Return the new list."
  (let ((p (position item list)))
    (if p
        (nconc (subseq list 0 p) replacements (subseq list (1+ p)))
        list)))

(defvar *min-frame-width* 50
  "A frame will not shrink below this width. Splitting will not
affect frames if the new frame widths are less than this value.")

(defvar *min-frame-height* 50
  "A frame will not shrink below this height. Splitting will not
affect frames if the new frame heights are less than this
value.")

(defvar *new-frame-action* :last-window
  "Controls what to do with new frame. Valid values are :last-window, :empty.")

(defvar *new-window-prefered-frame* '(:focused)
  "Controls where new windows appear. It's a list where the more
  prefered possibilities are closer to the head. Valid list
  elements are :focused, :last, :empty, :unfocused.")

(defun print-backtrace (&optional (frames 100))
  "print a backtrace of FRAMES number of frames to standard-output"
  #+sbcl (sb-debug:backtrace frames *standard-output*)
  #+clisp (ext:show-stack 1 frames (sys::the-frame))

  #-(or sbcl clisp) (write-line "Sorry, no backtrace for you."))

(defun utf8-to-string (octets)
  "Convert the list of octets to a string."
  #+sbcl (sb-ext:octets-to-string
          (coerce octets '(vector (unsigned-byte 8)))
          :external-format :utf-8)
  ;; TODO: handle UTF-8 for other lisps
  #-sbcl
  (map 'string #'code-char octets))

(defvar *startup-message* "Welcome to The Stump Window Manager!"
  "StumpWM's startup message. Set to NIL to suppress.")

(defvar *default-package* (find-package "CL-USER")
  "What package does stumpwm startup in? This affects the package
symbols are read into in the eval command and the package the rc
file is loaded in. Of course it can explicitely set in the rc
with IN-PACKAGE.")

(defun concat (&rest strings)
  (apply 'concatenate 'string strings))
