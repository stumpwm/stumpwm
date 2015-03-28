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
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; This file contains primitive data structures and functions used
;; throughout stumpwm.
;;
;; Code:

(in-package :stumpwm)

#+ecl (require "clx")

(export '(*suppress-abort-messages*
          *suppress-frame-indicator*
          *suppress-window-placement-indicator*
          *timeout-wait*
          *timeout-frame-indicator-wait*
          *frame-indicator-text*
          *frame-indicator-timer*
          *message-window-timer*
          *command-mode-start-hook*
          *command-mode-end-hook*
          *urgent-window-hook*
          *new-window-hook*
          *destroy-window-hook*
          *focus-window-hook*
          *place-window-hook*
          *start-hook*
          *quit-hook*
          *internal-loop-hook*
          *event-processing-hook*
          *focus-frame-hook*
          *new-frame-hook*
          *split-frame-hook*
          *message-hook*
          *top-level-error-hook*
          *focus-group-hook*
          *key-press-hook*
          *root-click-hook*
          *new-mode-line-hook*
          *destroy-mode-line-hook*
          *mode-line-click-hook*
          *display*
          *shell-program*
          *maxsize-border-width*
          *transient-border-width*
          *normal-border-width*
          *text-color*
          *window-events*
          *window-parent-events*
          *message-window-padding*
          *message-window-gravity*
          *editor-bindings*
          *input-window-gravity*
          *normal-gravity*
          *maxsize-gravity*
          *transient-gravity*
          *top-level-error-action*
          *window-name-source*
          *frame-number-map*
          *all-modifiers*
          *modifiers*
          *screen-list*
          *initializing*
          *processing-existing-windows*
          *executing-stumpwm-command*
          *debug-level*
          *debug-expose-events*
          *debug-stream*
          *window-formatters*
          *window-format*
          *group-formatters*
          *group-format*
          *list-hidden-groups*
          *x-selection*
          *last-command*
          *max-last-message-size*
          *record-last-msg-override*
          *suppress-echo-timeout*
          *run-or-raise-all-groups*
          *run-or-raise-all-screens*
          *deny-map-request*
          *deny-raise-request*
          *suppress-deny-messages*
          *honor-window-moves*
          *resize-hides-windows*
          *min-frame-width*
          *min-frame-height*
          *new-frame-action*
          *new-window-preferred-frame*
          *startup-message*
          *default-package*
          *window-placement-rules*
          *mouse-focus-policy*
          *root-click-focuses-frame*
          *banish-pointer-to*
          *xwin-to-window*
          *resize-map*
          *default-group-name*
          *window-border-style*
          *data-dir*
          add-hook
          clear-window-placement-rules
          concat
          data-dir-file
          dformat
          flatten
          define-frame-preference
          redirect-all-output
          remove-hook
          run-hook
          run-hook-with-args
          command-mode-start-message
          command-mode-end-message
          split-string
          with-restarts-menu
          with-data-file
          move-to-head
          format-expand

          ;; Frame accessors
          frame-x
          frame-y
          frame-width
          frame-height

          ;; Screen accessors
          screen-heads
          screen-root
          screen-focus
          screen-float-focus-color
          screen-float-unfocus-color

          ;; Window states
          +withdrawn-state+
          +normal-state+
          +iconic-state+

          ;; Modifiers
          modifiers
          modifiers-p
          modifiers-alt
          modifiers-altgr
          modifiers-super
          modifiers-meta
          modifiers-hyper
          modifiers-numlock))


;;; Message Timer
(defvar *suppress-abort-messages* nil
  "Suppress abort message when non-nil.")

(defvar *timeout-wait* 5
  "Specifies, in seconds, how long a message will appear for. This must
be an integer.")

(defvar *timeout-frame-indicator-wait* 1
  "The amount of time a frame indicator timeout takes.")

(defvar *frame-indicator-timer* nil
  "Keep track of the timer that hides the frame indicator.")

(defvar *frame-indicator-text* " Current Frame "
  "What appears in the frame indicator window?")

(defvar *suppress-frame-indicator* nil
  "Set this to T if you never want to see the frame indicator.")

(defvar *suppress-window-placement-indicator* nil
  "Set to T if you never want to see messages that windows were placed
  according to rules.")

(defvar *message-window-timer* nil
  "Keep track of the timer that hides the message window.")

;;; Grabbed pointer

(defvar *grab-pointer-count* 0
  "The number of times the pointer has been grabbed.")

(defvar *grab-pointer-font* "cursor"
  "The font used for the grabbed pointer.")

(defvar *grab-pointer-character* 64
  "ID of a character used for the grabbed pointer.")

(defvar *grab-pointer-character-mask* 65
  "ID of a character mask used for the grabbed pointer.")

(defvar *grab-pointer-foreground*
  (xlib:make-color :red 0.0 :green 0.0 :blue 0.0)
  "The foreground color of the grabbed pointer.")

(defvar *grab-pointer-background*
  (xlib:make-color :red 1.0 :green 1.0 :blue 1.0)
  "The background color of the grabbed pointer.")

;;; Hooks

(defvar *command-mode-start-hook* '(command-mode-start-message)
  "A hook called whenever command mode is started")

(defvar *command-mode-end-hook* '(command-mode-end-message)
  "A hook called whenever command mode is ended")

(defvar *urgent-window-hook* '()
  "A hook called whenever a window sets the property indicating that
  it demands the user's attention")

(defvar *map-window-hook* '()
  "A hook called whenever a window is mapped.")

(defvar *unmap-window-hook* '()
  "A hook called whenever a window is withdrawn.")

(defvar *new-window-hook* '()
  "A hook called whenever a window is added to the window list. This
includes a genuinely new window as well as bringing a withdrawn window
back into the window list.")

(defvar *destroy-window-hook* '()
  "A hook called whenever a window is destroyed or withdrawn.")

(defvar *focus-window-hook* '()
  "A hook called when a window is given focus. It is called with 2
arguments: the current window and the last window (could be nil).")

(defvar *place-window-hook* '()
  "A hook called whenever a window is placed by rule. Arguments are
window group and frame")

(defvar *start-hook* '()
  "A hook called when stumpwm starts.")

(defvar *quit-hook* '()
  "A hook called when stumpwm quits.")

(defvar *internal-loop-hook* '()
  "A hook called inside stumpwm's inner loop.")

(defvar *event-processing-hook* '()
  "A hook called inside stumpwm's inner loop, before the default event
  processing takes place. This hook is run inside (with-event-queue ...).")

(defvar *focus-frame-hook* '()
  "A hook called when a frame is given focus. The hook functions are
called with 2 arguments: the current frame and the last frame.")

(defvar *new-frame-hook* '()
  "A hook called when a new frame is created. the hook is called with
the frame as an argument.")

(defvar *split-frame-hook* '()
  "A hook called when a frame is split. the hook is called with
the old frame (window is removed), and two new frames as arguments.")

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

(defvar *key-press-hook* '()
  "A hook called whenever a key under *top-map* is pressed.
It is called with 3 argument: the key, the (possibly incomplete) key
sequence it is a part of, and command value bound to the key.")

(defvar *root-click-hook* '()
  "A hook called whenever there is a mouse click on the root
window. Called with 4 arguments, the screen containing the root
window, the button clicked, and the x and y of the pointer.")

(defvar *new-mode-line-hook* '()
  "Called whenever the mode-line is created. It is called with argument,
the mode-line")

(defvar *destroy-mode-line-hook* '()
  "Called whenever the mode-line is destroyed. It is called with argument,
the mode-line")

(defvar *mode-line-click-hook* '()
  "Called whenever the mode-line is clicked. It is called with 4 arguments,
the mode-line, the button clicked, and the x and y of the pointer.")

;; Data types and globals used by stumpwm

(defvar *display* nil
  "The display for the X server")

(defvar *shell-program* "/bin/sh"
  "The shell program used by @code{run-shell-command}.")

(defvar *maxsize-border-width* 1
  "The width in pixels given to the borders of windows with maxsize or ratio hints.")

(defvar *transient-border-width* 1
  "The width in pixels given to the borders of transient or pop-up windows.")

(defvar *normal-border-width* 1
  "The width in pixels given to the borders of regular windows.")

(defvar *text-color* "white"
  "The color of message text.")

(defvar *menu-maximum-height* nil
  "Defines the maxium number of lines to display in the menu before enabling
   scrolling. If NIL scrolling is disabled.")

(defvar *menu-scrolling-step* 1
  "Number of lines to scroll when hitting the menu list limit.")

(defparameter +netwm-supported+
  '(:_NET_SUPPORTING_WM_CHECK
    :_NET_NUMBER_OF_DESKTOPS
    :_NET_DESKTOP_GEOMETRY
    :_NET_DESKTOP_VIEWPORT
    :_NET_CURRENT_DESKTOP
    :_NET_WM_WINDOW_TYPE
    :_NET_WM_STATE
    :_NET_WM_STATE_MODAL
    :_NET_WM_ALLOWED_ACTIONS
    :_NET_WM_STATE_FULLSCREEN
    :_NET_WM_STATE_HIDDEN
    :_NET_WM_STATE_DEMANDS_ATTENTION
    :_NET_WM_FULL_WINDOW_PLACEMENT
    :_NET_CLOSE_WINDOW
    :_NET_CLIENT_LIST
    :_NET_CLIENT_LIST_STACKING
    :_NET_ACTIVE_WINDOW
    :_NET_WM_DESKTOP
    :_KDE_NET_SYSTEM_TRAY_WINDOW_FOR)
  "Supported NETWM properties.
Window types are in +WINDOW-TYPES+.")

(defparameter +netwm-allowed-actions+
  '(:_NET_WM_ACTION_CHANGE_DESKTOP
    :_NET_WM_ACTION_FULLSCREEN
    :_NET_WM_ACTION_CLOSE)
  "Allowed NETWM actions for managed windows")

(defparameter +netwm-window-types+
  '(
    ;; (:_NET_WM_WINDOW_TYPE_DESKTOP . :desktop)
    (:_NET_WM_WINDOW_TYPE_DOCK . :dock)
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
                          :focus-change
                          :enter-window)
  "The events to listen for on managed windows.")

(defvar *window-parent-events* '(:substructure-notify
                                 :substructure-redirect)

  "The events to listen for on managed windows' parents.")

;; Message window variables
(defvar *message-window-padding* 5
  "The number of pixels that pad the text in the message window.")

(defvar *message-window-gravity* :top-right
  "This variable controls where the message window appears. The follow
are valid values.
@table @asis
@item :top-left
@item :top-right
@item :bottom-left
@item :bottom-right
@item :center
@item :top
@item :left
@item :right
@item :bottom
@end table")

;; line editor
(defvar *editor-bindings* nil
  "A list of key-bindings for line editing.")

(defvar *input-window-gravity* :top-right
  "This variable controls where the input window appears. The follow
are valid values.
@table @asis
@item :top-left
@item :top-right
@item :bottom-left
@item :bottom-right
@item :center
@item :top
@item :left
@item :right
@item :bottom
@end table")

;; default values. use the set-* functions to these attributes
(defparameter +default-foreground-color+ "White")
(defparameter +default-background-color+ "Black")
(defparameter +default-window-background-color+ "Black")
(defparameter +default-border-color+ "White")
(defparameter +default-font-name+ "9x15")
(defparameter +default-focus-color+ "White")
(defparameter +default-unfocus-color+ "Black")
(defparameter +default-float-focus-color+ "Orange")
(defparameter +default-float-unfocus-color+ "SteelBlue4")
(defparameter +default-frame-outline-width+ 2)

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
  "This variable controls what is used for the window's name. The default is @code{:title}.

@table @code
@item :title
Use the window's title given to it by its owner.

@item :class
Use the window's resource class.

@item :resource-name
Use the window's resource name.
@end table")

(defstruct frame
  (number nil :type integer)
  x
  y
  width
  height
  window)

(defstruct (head (:include frame))
  ;; point back to the screen this head belongs to
  screen
  ;; a bar along the top or bottom that displays anything you want.
  mode-line)

(defclass screen ()
  ((id :initform nil :accessor screen-id)
   (host :initform nil :accessor screen-host)
   (number :initform nil :accessor screen-number)
   (heads :initform nil :accessor screen-heads :documentation
    "heads of screen")
   (groups :initform nil :accessor screen-groups :documentation
    "the list of groups available on this screen")
   (current-group :initform nil :accessor screen-current-group)
   ;; various colors (as returned by alloc-color)
   (border-color :initform nil :accessor screen-border-color)
   (fg-color :initform nil :accessor screen-fg-color)
   (bg-color :initform nil :accessor screen-bg-color)
   (win-bg-color :initform nil :accessor screen-win-bg-color)
   (focus-color :initform nil :accessor screen-focus-color)
   (unfocus-color :initform nil :accessor screen-unfocus-color)
   (float-focus-color :initform nil :accessor screen-float-focus-color)
   (float-unfocus-color :initform nil :accessor screen-float-unfocus-color)
   (msg-border-width :initform nil :accessor screen-msg-border-width)
   (frame-outline-width :initform nil :accessor screen-frame-outline-width)
   (fonts :initform '(nil) :accessor screen-fonts)
   (mapped-windows :initform nil :accessor screen-mapped-windows :documentation
    "A list of all mapped windows. These are the raw xlib:window's. window structures are stored in groups.")
   (withdrawn-windows :initform nil :accessor screen-withdrawn-windows :documentation
    "A list of withdrawn windows. These are of type stumpwm::window
and when they're mapped again they'll be put back in the group
they were in when they were unmapped unless that group doesn't
exist, in which case they go into the current group.")
   (urgent-windows :initform nil :accessor screen-urgent-windows :documentation
    "a list of windows for which (window-urgent-p) currently true.")
   (input-window :initform nil :accessor screen-input-window)

   (key-window :initform nil :accessor screen-key-window :documentation
    "the window that accepts further keypresses after a toplevel key has been pressed.")
   (focus-window :initform nil :accessor screen-focus-window :documentation
    "The window that gets focus when no window has focus")
   ;;
   (frame-window :initform nil :accessor screen-frame-window)
   (frame-outline-gc :initform nil :accessor screen-frame-outline-gc)
   ;; color contexts
   (message-cc :initform nil :accessor screen-message-cc)
   (mode-line-cc :initform nil :accessor screen-mode-line-cc)
   ;; color maps
   (color-map-normal :initform nil :accessor screen-color-map-normal)
   (color-map-bright :initform nil :accessor screen-color-map-bright)
   (ignore-msg-expose :initform nil :accessor screen-ignore-msg-expose :documentation
    "used to ignore the first expose even when mapping the message window.")
   ;; the window that has focus
   (focus :initform nil :accessor screen-focus)
   (current-msg :initform nil :accessor screen-current-msg)
   (current-msg-highlights :initform nil :accessor screen-current-msg-highlights)
   (last-msg :initform nil :accessor screen-last-msg)
   (last-msg-highlights :initform nil :accessor screen-last-msg-highlights)))

(defstruct ccontext
  screen
  win
  px
  gc
  default-fg
  default-bright
  default-bg
  fg
  bg
  brightp
  reversep
  color-stack
  font)

(defun screen-message-window (screen)
  (ccontext-win (screen-message-cc screen)))

(defun screen-message-pixmap (screen)
  (ccontext-px (screen-message-cc screen)))

(defun screen-message-gc (screen)
  (ccontext-gc (screen-message-cc screen)))

(defun screen-font (screen)
  (first (screen-fonts screen)))

(defmethod print-object ((object frame) stream)
  (format stream "#S(frame ~d ~a ~d ~d ~d ~d)"
          (frame-number object) (frame-window object) (frame-x object) (frame-y object) (frame-width object) (frame-height object)))

(defvar *window-number-map* "0123456789"
  "Set this to a string to remap the window numbers to something more convenient.")

(defvar *group-number-map* "1234567890"
  "Set this to a string to remap the group numbers to something more convenient.")

(defvar *frame-number-map* "0123456789abcdefghijklmnopqrstuvxwyz"
  "Set this to a string to remap the frame numbers to more convenient keys.
For instance,

\"hutenosa\"

would map frame 0 to 7 to be selectable by hitting the appropriate
homerow key on a dvorak keyboard. Currently, only single char keys are
supported. By default, the frame labels are the 36 (lower-case)
alphanumeric characters, starting with numbers 0-9.")

(defun get-frame-number-translation (frame)
  "Given a frame return its number translation using *frame-number-map* as a
char."
  (let ((num (frame-number frame)))
    (if (< num (length *frame-number-map*))
        (char *frame-number-map* num)
        ;; translate the frame number to a char. FIXME: it loops after 9
        (char (prin1-to-string num) 0))))

(defstruct modifiers
  (meta nil)
  (alt nil)
  (hyper nil)
  (super nil)
  (altgr nil)
  (numlock nil))

(defvar *all-modifiers* nil
  "A list of all keycodes that are considered modifiers")

(defvar *modifiers* nil
  "A mapping from modifier type to x11 modifier.")

(defmethod print-object ((object screen) stream)
  (format stream "#S<screen ~s>" (screen-number object)))

(defvar *screen-list* '()
  "The list of screens managed by stumpwm.")

(defvar *initializing* nil
  "True when starting stumpwm. Use this variable in your rc file to
run code that should only be executed once, when stumpwm starts up and
loads the rc file.")

(defvar *processing-existing-windows* nil
  "True when processing pre-existing windows at startup.")

(defvar *executing-stumpwm-command* nil
  "True when executing external commands.")

(defvar *interactivep* nil
  "True when a defcommand is executed from colon or a keybinding")

;;; The restarts menu macro

(defmacro with-restarts-menu (&body body)
  "Execute BODY. If an error occurs allow the user to pick a
restart from a menu of possible restarts. If a restart is not
chosen, resignal the error."
  (let ((c (gensym)))
    `(handler-bind
         ((warning #'muffle-warning)
          ((or serious-condition error)
           (lambda (,c)
             (restarts-menu ,c)
             (signal ,c))))
       ,@body)))

;;; Hook functionality

(defun run-hook-with-args (hook &rest args)
  "Call each function in HOOK and pass args to it."
  (handler-case
      (with-simple-restart (abort-hooks "Abort running the remaining hooks.")
        (with-restarts-menu
            (dolist (fn hook)
              (with-simple-restart (continue-hooks "Continue running the remaining hooks.")
                (apply fn args)))))
    (t (c) (message "^B^1*Error on hook ^b~S^B!~% ^n~A" hook c) (values nil c))))

(defun run-hook (hook)
  "Call each function in HOOK."
  (run-hook-with-args hook))

(defmacro add-hook (hook fn)
  "Add @var{function} to the hook @var{hook-variable}. For example, to
display a message whenever you switch frames:

@example
\(defun my-rad-fn (to-frame from-frame)
  (stumpwm:message \"Mustard!\"))

\(stumpmwm:add-hook stumpwm:*focus-frame-hook* 'my-rad-fn)
@end example"
  `(setf ,hook (adjoin ,fn ,hook)))

(defmacro remove-hook (hook fn)
"Remove the specified function from the hook."
  `(setf ,hook (remove ,fn ,hook)))

;; Misc. utility functions

(defun conc1 (list arg)
  "Append arg to the end of list"
  (nconc list (list arg)))

(defun sort1 (list sort-fn &rest keys &key &allow-other-keys)
  "Return a sorted copy of list."
  (let ((copy (copy-list list)))
    (apply 'sort copy sort-fn keys)))

(defun mapcar-hash (fn hash)
  "Just like maphash except it accumulates the result in a list."
  (let ((accum nil))
    (labels ((mapfn (key val)
               (push (funcall fn key val) accum)))
      (maphash #'mapfn hash))
    accum))

(defun find-free-number (l &optional (min 0) dir)
  "Return a number that is not in the list l. If dir is :negative then
look for a free number in the negative direction. anything else means
positive direction."
  (let* ((dirfn (if (eq dir :negative) '> '<))
         ;; sort it and crop numbers below/above min depending on dir
         (nums (sort (remove-if (lambda (n)
                                  (funcall dirfn n min))
                                l) dirfn))
         (max (car (last nums)))
         (inc (if (eq dir :negative) -1 1))
         (new-num (loop for n = min then (+ n inc)
                        for i in nums
                        when (/= n i)
                        do (return n))))
    (dformat 3 "Free number: ~S~%" nums)
    (if new-num
        new-num
        ;; there was no space between the numbers, so use the max+inc
        (if max
            (+ inc max)
            min))))

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

(defun screen-display-string (screen &optional (assign t))
  (format nil
          (if assign "DISPLAY=~a:~d.~d" "~a:~d.~d")
          (screen-host screen)
          (xlib:display-display *display*)
          (screen-id screen)))

(defun split-seq (seq separators &key test default-value)
  "split a sequence into sub sequences given the list of seperators."
  (let ((seps separators))
    (labels ((sep (c)
               (find c seps :test test)))
      (or (loop for i = (position-if (complement #'sep) seq)
                then (position-if (complement #'sep) seq :start j)
                as j = (position-if #'sep seq :start (or i 0))
                while i
                collect (subseq seq i j)
                while j)
          ;; the empty seq causes the above to return NIL, so help
          ;; it out a little.
          default-value))))

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
  (split-seq string separators :test #'char= :default-value '("")))


(defun insert-before (list item nth)
  "Insert ITEM before the NTH element of LIST."
  (declare (type (integer 0 *) nth))
  (let* ((nth (min nth (length list)))
         (pre (subseq list 0 nth))
         (post (subseq list nth)))
    (nconc pre (list item) post)))

(defvar *debug-level* 0
  "Set this variable to a number > 0 to turn on debugging. The greater the number the more debugging output.")

(defvar *debug-expose-events* nil
  "Set this variable for a visual indication of expose events on internal StumpWM windows.")

(defvar *debug-stream* *error-output*
  "This is the stream debugging output is sent to. It defaults to
*error-output*. It may be more convenient for you to pipe debugging
output directly to a file.")

(defun dformat (level fmt &rest args)
  (when (>= *debug-level* level)
    (multiple-value-bind (sec m h) (decode-universal-time (get-universal-time))
      (format *debug-stream* "~2,'0d:~2,'0d:~2,'0d " h m sec))
    ;; strip out non base-char chars quick-n-dirty like
    (write-string (map 'string (lambda (ch)
                                 (if (typep ch 'standard-char)
                                     ch #\?))
                       (apply 'format nil fmt args))
                  *debug-stream*)
    (force-output *debug-stream*)))

(defvar *redirect-stream* nil
  "This variable Keeps track of the stream all output is sent to when
`redirect-all-output' is called so if it changes we can close it
before reopening.")

(defun redirect-all-output (file)
  "Elect to redirect all output to the specified file. For instance,
if you want everything to go to ~/.stumpwm.d/debug-output.txt you would
do:

@example
(redirect-all-output (data-dir-file \"debug-output\" \"txt\"))
@end example
"
  (when (typep *redirect-stream* 'file-stream)
    (close *redirect-stream*))
  (setf *redirect-stream* (open file :direction :output :if-exists :append :if-does-not-exist :create)
        *error-output*    *redirect-stream*
        *standard-output* *redirect-stream*
        *trace-output*    *redirect-stream*
        *debug-stream*    *redirect-stream*))

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
                   (len (and tmp (parse-integer (coerce tmp 'string))))
                   ;; So that eg "%25^t" will trim from the left
                   (from-left-p (when (char= #\^ (car cur)) (pop cur))))
              (if (null cur)
                  (format t "%~a~@[~a~]" len from-left-p)
                  (let* ((fmt (cadr (assoc (car cur) fmt-alist :test 'char=)))
                         (str (cond (fmt
                                     ;; it can return any type, not jut as string.
                                     (format nil "~a" (apply fmt args)))
                                    ((char= (car cur) #\%)
                                     (string #\%))
                                    (t
                                     (concatenate 'string (string #\%) (string (car cur)))))))
                    ;; crop string if needed
                    (setf output (concatenate 'string output
                                              (cond ((null len) str)
                                                    ((not from-left-p) ; Default behavior
                                                     (subseq str 0 (min len (length str))))
                                                    ;; New behavior -- trim from the left
                                                    (t (subseq str (max 0 (- (length str) len)))))))
                    (setf cur (cdr cur))))))
           (t
            (setf output (concatenate 'string output (string (car cur)))
                  cur (cdr cur)))))))

(defvar *window-formatters* '((#\n window-map-number)
                              (#\s fmt-window-status)
                              (#\t window-name)
                              (#\c window-class)
                              (#\i window-res)
                              (#\r window-role)
                              (#\m fmt-window-marked)
                              (#\h window-height)
                              (#\w window-width)
                              (#\g gravity-for-window))
  "an alist containing format character format function pairs for formatting window lists.")

(defvar *window-format* "%m%n%s%50t"
  "This variable decides how the window list is formatted. It is a string
with the following formatting options:

@table @asis
@item %n
Substitutes the windows number translated via *window-number-map*, if there
are more windows than *window-number-map* then will use the window-number.
@item %s
Substitute the window's status. * means current window, + means last
window, and - means any other window.
@item %t
Substitute the window's name.
@item %c
Substitute the window's class.
@item %i
Substitute the window's resource ID.
@item %m
Draw a # if the window is marked.
@end table

Note, a prefix number can be used to crop the argument to a specified
size. For instance, @samp{%20t} crops the window's title to 20
characters.")

(defvar *window-info-format* "%wx%h %n (%t)"
  "The format used in the info command.
  @var{*window-format*} for formatting details.")

(defparameter *window-format-by-class* "%m%n %c %s%50t"
  "The format used in the info winlist-by-class command.
 @var{*window-format*} for formatting details.")

(defvar *group-formatters* '((#\n group-map-number)
                             (#\s fmt-group-status)
                             (#\t group-name))
  "An alist of characters and formatter functions. The character can be
used as a format character in @var{*group-format*}. When the character
is encountered in the string, the corresponding function is called
with a group as an argument. The functions return value is inserted
into the string. If the return value isn't a string it is converted to
one using @code{prin1-to-string}.")

(defvar *group-format* "%n%s%t"
  "The format string that decides what information will show up in the
group listing. The following format options are available:

@table @asis
@item %n
Substitutes the group number translated via *group-number-map*, if there
are more windows than *group-number-map* then will use the group-number.

@item %s
The group's status. Similar to a window's status.

@item %t
The group's name.
@end table")

(defvar *list-hidden-groups* nil
  "Controls whether hidden groups are displayed by 'groups' and 'vgroups' commands")

;; (defun font-height (font)
;;   (+ (font-descent font)
;;      (font-ascent font)))

(defvar *x-selection* nil
  "This is a plist of stumpwm's current selections. The different properties are
generally set when killing text in the input bar.")

(defvar *last-command* nil
  "Set to the last interactive command run.")

(defvar *max-last-message-size* 20
  "how many previous messages to keep.")

(defvar *record-last-msg-override* nil
  "assign this to T and messages won't be recorded. It is
recommended this is assigned using LET.")

(defvar *suppress-echo-timeout* nil
  "Assign this T and messages will not time out. It is recommended this is assigned using LET.")

(defvar *ignore-echo-timeout* nil
  "Assign this T and the message time out won't be touched. It is recommended this is assigned using LET.")

(defvar *run-or-raise-all-groups* t
  "When this is @code{T} the @code{run-or-raise} function searches all groups for a
running instance. Set it to NIL to search only the current group.")

(defvar *run-or-raise-all-screens* nil
  "When this is @code{T} the @code{run-or-raise} function searches all screens for a
running instance. Set it to @code{NIL} to search only the current screen. If
@var{*run-or-raise-all-groups*} is @code{NIL} this variable has no effect.")

(defvar *deny-map-request* nil
  "A list of window properties that stumpwm should deny matching windows'
requests to become mapped for the first time.")

(defvar *deny-raise-request* nil
  "Exactly the same as @var{*deny-map-request*} but for raise requests.

Note that no denial message is displayed if the window is already visible.")

(defvar *suppress-deny-messages* nil
  "For complete focus on the task at hand, set this to @code{T} and no
raise/map denial messages will be seen.")

(defvar *honor-window-moves* t
  "Allow windows to move between frames.")

(defvar *resize-hides-windows* nil
  "Set to T to hide windows during interactive resize")

(defun deny-request-p (window deny-list)
  (or (eq deny-list t)
      (and
       (listp deny-list)
       (find-if (lambda (props)
                  (apply 'window-matches-properties-p window props))
                deny-list)
       t)))

(defun flatten (list)
  "Flatten LIST"
  (labels ( (mklist (x) (if (listp x) x (list x))) )
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) list)))

(defun list-splice-replace (item list &rest replacements)
  "splice REPLACEMENTS into LIST where ITEM is, removing
ITEM. Return the new list."
  (let ((p (position item list)))
    (if p
        (nconc (subseq list 0 p) replacements (subseq list (1+ p)))
        list)))

(defvar *min-frame-width* 50
  "The minimum width a frame can be. A frame will not shrink below this
width. Splitting will not affect frames if the new frame widths are
less than this value.")

(defvar *min-frame-height* 50
  "The minimum height a frame can be. A frame will not shrink below this
height. Splitting will not affect frames if the new frame heights are
less than this value.")

(defvar *new-frame-action* :last-window
  "When a new frame is created, this variable controls what is put in the
new frame. Valid values are

@table @code
@item :empty
The frame is left empty

@item :last-window
The last focused window that is not currently visible is placed in the
frame. This is the default.
@end table")

(defvar *new-window-preferred-frame* '(:focused)
  "This variable controls what frame a new window appears in. It is a
list of preferences. The first preference that is satisfied is
used. Valid list elements are as follows:

@table @code
@item :focused
Choose the focused frame.

@item :last
Choose the last focused frame.

@item :empty
Choose any empty frame.

@item :unfocused
Choose any unfocused frame.
@end table

Alternatively, it can be set to a function that takes one argument, the new
window, and returns the preferred frame or a list of the above preferences.")

(defun backtrace-string ()
  "Similar to print-backtrace, but return the backtrace as a string."
  (with-output-to-string (*standard-output*)
    (print-backtrace)))

(defvar *startup-message* "^2*Welcome to The ^BStump^b ^BW^bindow ^BM^banager!
Press ^5*~a ?^2* for help."
  "This is the message StumpWM displays when it starts. Set it to NIL to
suppress.")

(defvar *default-package* (find-package '#:stumpwm-user)
  "This is the package eval reads and executes in. You might want to set
this to @code{:stumpwm} if you find yourself using a lot of internal
stumpwm symbols. Setting this variable anywhere but in your rc file
will have no effect.")

(defun concat (&rest strings)
  (apply 'concatenate 'string strings))

(defvar *window-placement-rules* '()
  "List of rules governing window placement. Use define-frame-preference to
add rules")

(defmacro define-frame-preference (target-group &rest frame-rules)
  "Create a rule that matches windows and automatically places them in
a specified group and frame. Each frame rule is a lambda list:
@example
\(frame-number raise lock &key create restore dump-name class instance type role title)
@end example

@table @var
@item frame-number
The frame number to send matching windows to

@item raise
When non-nil, raise and focus the window in its frame

@item lock
When this is nil, this rule will only match when the current group
matches @var{target-group}. When non-nil, this rule matches regardless
of the group and the window is sent to @var{target-group}. If
@var{lock} and @var{raise} are both non-nil, then stumpwm will jump to
the specified group and focus the matched window.

@item create
When non-NIL the group is created and eventually restored when the value of
create is a group dump filename in *DATA-DIR*. Defaults to NIL.

@item restore
When non-NIL the group is restored even if it already exists. This arg should
be set to the dump filename to use for forced restore. Defaults to NIL

@item class
The window's class must match @var{class}.

@item instance
The window's instance/resource name must match @var{instance}.

@item type
The window's type must match @var{type}.

@item role
The window's role must match @var{role}.

@item title
The window's title must match @var{title}.
@end table"
  (let ((x (gensym "X")))
    `(dolist (,x ',frame-rules)
       ;; verify the correct structure
       (destructuring-bind (frame-number raise lock
                                         &rest keys
                                         &key create restore class instance type role title) ,x
         (declare (ignore create restore class instance type role title))
         (push (list* ,target-group frame-number raise lock keys)
               *window-placement-rules*)))))

(defun clear-window-placement-rules ()
  "Clear all window placement rules."
  (setf *window-placement-rules* nil))

(defvar *mouse-focus-policy* :ignore
  "The mouse focus policy decides how the mouse affects input
focus. Possible values are :ignore, :sloppy, and :click. :ignore means
stumpwm ignores the mouse. :sloppy means input focus follows the
mouse; the window that the mouse is in gets the focus. :click means
input focus is transfered to the window you click on.")

(defvar *root-click-focuses-frame* t
  "Set to NIL if you don't want clicking the root window to focus the frame
  containing the pointer.")

(defvar *banish-pointer-to* :head
  "Where to put the pointer when no argument is given to (banish-pointer) or the banish
  command. May be one of :screen :head :frame or :window")

(defvar *xwin-to-window* (make-hash-table)
  "Hash table for looking up windows quickly.")

(defvar *resize-map* nil
  "The keymap used for resizing a window")

(defvar *default-group-name* "Default"
  "The name of the default group.")

(defmacro with-focus (xwin &body body)
  "Set the focus to xwin, do body, then restore focus"
  `(progn
     (grab-keyboard ,xwin)
     (unwind-protect
          (progn ,@body)
       (ungrab-keyboard))))

(defvar *last-unhandled-error* nil
  "If an unrecoverable error occurs, this variable will contain the
  condition and the backtrace.")

(defvar *show-command-backtrace* nil
  "When this is T a backtrace is displayed with errors that occurred
within an interactive call to a command.")

(defvar *window-border-style* :thick
  "This controls the appearance of the border around windows. valid
values are:
@table @var
@item :thick
All space within the frame not used by the window is dedicated to the
border.

@item :thin
Only the border width as controlled by *maxsize-border-width*
*normal-border-width* and *transient-border-width* is used as the
border. The rest is filled with the unfocus color.

@item :tight
The same as :thin but the border surrounds the window and the wasted
space within the frame is not obscured, revealing the background.

@item :none
Like :tight but no border is ever visible.
@end table

After changing this variable you may need to call
sync-all-frame-windows to see the change.")

(defvar *data-dir* nil
  "The directory used by stumpwm to store data between sessions.")

(defun data-dir-file (name &optional type)
  "Return a pathname inside stumpwm's data dir with the specified name and type"
  (ensure-directories-exist *data-dir*)
  (make-pathname :name name :type type :defaults *data-dir*))

(defmacro with-data-file ((s file &rest keys &key (if-exists :supersede) &allow-other-keys) &body body)
  "Open a file in StumpWM's data directory. keyword arguments are sent
directly to OPEN. Note that IF-EXISTS defaults to :supersede, instead
of :error."
  (declare (ignorable if-exists))
  `(progn
     (ensure-directories-exist *data-dir*)
     (with-open-file (,s ,(merge-pathnames file *data-dir*)
                         ,@keys)
       ,@body)))

(defmacro move-to-head (list elt)
   "Move the specified element in in LIST to the head of the list."
 `(progn
    (setf ,list (remove ,elt ,list))
    (push ,elt ,list)))

(define-condition stumpwm-error (error)
  () (:documentation "Any stumpwm specific error should inherit this."))

(defun intern1 (thing &optional (package *package*) (rt *readtable*))
  "A DWIM intern."
  (intern
   (ecase (readtable-case rt)
     (:upcase (string-upcase thing))
     (:downcase (string-downcase thing))
     ;; Prooobably this is what they want? It could make sense to
     ;; upcase them as well.
     (:preserve thing)
     (:invert (string-downcase thing)))
   package))

(defun command-mode-start-message ()
  (message "Press C-g to exit command-mode."))

(defun command-mode-end-message ()
  (message "Exited command-mode."))
