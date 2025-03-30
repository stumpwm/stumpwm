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

(export '(*suppress-abort-messages*
          *suppress-frame-indicator*
          *suppress-window-placement-indicator*
          *timeout-wait*
          *timeout-wait-multiline*
          *timeout-frame-indicator-wait*
          *frame-indicator-text*
          *frame-indicator-timer*
          *message-window-timer*
          *hooks-enabled-p*
          *command-mode-start-hook*
          *command-mode-end-hook*
          *urgent-window-hook*
          *new-window-hook*
          *new-head-hook*
          *destroy-window-hook*
          *focus-window-hook*
          *place-window-hook*
          *pre-thread-hook*
          *start-hook*
          *restart-hook*
          *quit-hook*
          *internal-loop-hook*
          *event-processing-hook*
          *focus-frame-hook*
          *new-frame-hook*
          *split-frame-hook*
          *remove-split-hook*
          *message-hook*
          *top-level-error-hook*
          *focus-group-hook*
          *key-press-hook*
          *root-click-hook*
          *new-mode-line-hook*
          *destroy-mode-line-hook*
          *mode-line-click-hook*
          *pre-command-hook*
          *post-command-hook*
          *selection-notify-hook*
          *menu-selection-hook*
          *display*
          *shell-program*
          *maxsize-border-width*
          *transient-border-width*
          *normal-border-width*
          *text-color*
          *window-events*
          *window-parent-events*
          *message-window-padding*
          *message-window-y-padding*
          *message-window-margin*
          *message-window-y-margin*
          *message-window-gravity*
          *message-window-real-gravity*
          *message-window-input-gravity*
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
          define-frame-preference
          redirect-all-output
          remove-hook
          remove-all-hooks
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
          modifiers-numlock
          ;; Conditions
          stumpwm-condition
          stumpwm-error
          stumpwm-warning

          ;; Completion Options
          *maximum-completions*

          ;; Minor mode keymaps
          *minor-mode-maps*))


;;; Completions
(defvar *maximum-completions* 100
  "Maximum number of completions to show in interactive prompts. Setting
  this too high can crash the completion process due to drawing too far
  off screen.")

;;; Message Timer
(defvar *suppress-abort-messages* nil
  "Suppress abort message when non-nil.")

(defvar *timeout-wait* 5
  "Specifies, in seconds, how long a message will appear for. This must
be an integer.")

(defvar *timeout-wait-multiline* nil
  "Specifies, in seconds, how long a message will more than one line will
appear for. This must be an integer. If falsy, default to *timeout-wait*.")

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
  "A hook called whenever command mode is started.")

(defvar *command-mode-end-hook* '(command-mode-end-message)
  "A hook called whenever command mode is ended.")

(defvar *urgent-window-hook* '()
  "A hook called whenever a window sets the property indicating that
  it demands the user's attention. Called with the window as an argument.")

(defvar *map-window-hook* '()
  "A hook called whenever a window is mapped.")

(defvar *unmap-window-hook* '()
  "A hook called whenever a window is withdrawn.")

(defvar *new-window-hook* '()
  "A hook called whenever a window is added to the window list. This
includes a genuinely new window as well as bringing a withdrawn window
back into the window list. Called with the window as an argument.")

(defvar *destroy-window-hook* '()
  "A hook called whenever a window is destroyed or withdrawn.
Called with the window as an argument.")

(defvar *focus-window-hook* '()
  "A hook called when a window is given focus. It is called with 2
arguments: the current window and the last window (could be nil).")

(defvar *place-window-hook* '()
  "A hook called whenever a window is placed by rule. Arguments are
window, group and frame.")

(defvar *pre-thread-hook* '()
  "A hook called before any threads are started. Useful if you need to fork.")

(defvar *start-hook* '()
  "A hook called when stumpwm starts.")

(defvar *quit-hook* '()
  "A hook called when stumpwm quits.")

(defvar *restart-hook* '()
  "A hook called when stumpwm restarts.")

(defvar *internal-loop-hook* '()
  "A hook called inside stumpwm's inner loop.")

(defvar *event-processing-hook* '()
  "A hook called inside stumpwm's inner loop, before the default event
  processing takes place. This hook is run inside (with-event-queue ...).")

(defvar *focus-frame-hook* '()
  "A hook called when a frame is given focus. The hook functions are
called with 2 arguments: the current frame and the last frame.")

(defvar *new-frame-hook* '()
  "A hook called when a new frame is created. The hook is called with
the frame as an argument.")

(defvar *split-frame-hook* '()
  "A hook called when a frame is split. the hook is called with
the old frame (window is removed), and two new frames as arguments.")

(defvar *remove-split-hook* '()
  "A hook called when a split is removed. the hook is called with
the current frame and removed frame as arguments.")

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

(defvar *click-hook* '()
  "A hook called whenever there is a mouse click.
Called with 4 arguments, the screen containing the
window (or nil if there isn't one), the button clicked,
and the x and y of the pointer.")

(defvar *new-mode-line-hook* '()
  "Called whenever the mode-line is created. It is called with argument,
the mode-line")

(defvar *destroy-mode-line-hook* '()
  "Called whenever the mode-line is destroyed. It is called with argument,
the mode-line")

(defvar *mode-line-click-hook* '()
  "Called whenever the mode-line is clicked. It is called with 4 arguments,
the mode-line, the button clicked, and the x and y of the pointer.")

(defvar *pre-command-hook* '()
  "Called before a command is called. It is called with 1 argument:
the command as a symbol.")

(defvar *post-command-hook* '()
  "Called after a command is called. It is called with 1 argument:
the command as a symbol.")

(defvar *selection-notify-hook* '()
  "Called after a :selection-notify event is processed. It is called
with 1 argument: the selection as a string.")

(defvar *menu-selection-hook* '()
  "Called after an item is selected in the windows menu. It is called
with 1 argument: the menu.")

(defvar *new-head-hook* '()
  "A hook called whenever a head is added. It is called with 2 arguments: the
 new head and the current screen.")
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

(defvar *draw-in-color* t
  "When NIL color formatters are ignored.")

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

(defvar *message-window-y-padding* 0
  "The number of pixels that pad the text in the message window vertically.")

(defvar *message-window-margin* 0
  "The number of pixels (i.e. the gap) between the message window and the
   horizontal edges of the head. The margin is disregarded if it takes more
   space than is available.")

(defvar *message-window-y-margin* 0
  "The number of pixels (i.e. the gap) between the message window and the
   vertical edges of the head. The margin is disregarded if it takes more
   space than is available.")

(defvar *message-window-gravity* :top-right
  "This variable controls where the message window appears. The following
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

(defvar *message-window-input-gravity* :top-left
  "This variable controls where the message window appears
when the input window is being displayed. The following are valid values.
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
  "This variable controls where the input window appears. The following
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

(declaim (type (member :message :break :abort) *top-level-error-action*))
(defvar *top-level-error-action* :abort
  "If an error is encountered at the top level, in
STUMPWM-INTERNAL-LOOP, then this variable decides what action
shall be taken. By default it will print a message to the screen
and to *standard-output*.

Valid values are :message, :break, :abort. :break will break to the
debugger. This can be problematic because if the user hit's a
mapped key the ENTIRE keyboard will be frozen and you will have
to login remotely to regain control. :abort quits stumpwm.")

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

(defclass swm-class ()
  ((new-objects
    :initform nil
    :accessor swm-class-new-objects
    :allocation :class
    :documentation
"Track all newly created objects in order to mix in the appropriate minor modes
when they are touched")))

(defmethod initialize-instance :after ((obj swm-class) &key &allow-other-keys)
  ;; Register all newly created objects so that they can have the relevant minor
  ;; modes autoenabled.
  (pushnew obj (swm-class-new-objects obj) :test #'eq))

(defgeneric print-swm-object (object stream)
  (:method (object stream)
    (format stream "~A" (type-of object))))

(defmethod print-object ((object swm-class) stream)
  (print-unreadable-object (object stream)
    (print-swm-object object stream)
    (when-let ((minor-modes (list-minor-modes object)))
      (format stream " :MINOR-MODES ~A" minor-modes))))

(defun make-swm-class-instance (class &rest initargs)
  "Make an instance of a StumpWM class and autoenable any relevant minor
modes. CLASS must be a symbol denoting a class which descends, directly or
indirectly, from swm-class. INITARGS must be all initargs one would pass to
make-instance."
  ;; This is implemented as a function instead of as an after method for
  ;; initialize-instance because autoenabling a minor mode involves changing the
  ;; class of the object, which is implied to be undefined behavior if called
  ;; within a method which accesses the objects slots.
  (declare (special *active-global-minor-modes*))
  (let ((object (apply #'make-instance class initargs)))
    (prog1 object
      (loop for class in *active-global-minor-modes*
            when (typep object (scope-type (minor-mode-scope class)))
              do (autoenable-minor-mode class object))
      (setf (swm-class-new-objects object)
            (remove object (swm-class-new-objects object) :test #'eq)))))

(defmacro define-swm-class (class-name superclasses slots &rest options)
  "Define a class and a method for DYNAMIC-MIXINS:REPLACE-CLASS which specializes
upon the class and replaces it. If SUPERCLASSES is NIL then (SWM-CLASS) is used."
  (unless superclasses (setq superclasses '(swm-class)))
  `(progn
     (defclass ,class-name ,superclasses ,slots ,@options)
     (defmethod dynamic-mixins-swm:replace-class ((object ,class-name) new &rest r)
       (apply #'dynamic-mixins-swm:replace-class-in-mixin
              object new ',class-name r))))

(define-swm-class frame ()
  ((number
    :initform nil
    :initarg :number
    :accessor frame-number)
   (x
    :initform nil
    :accessor frame-x
    :initarg :x)
   (y
    :initform nil
    :accessor frame-y
    :initarg :y)
   (width
    :initform nil
    :accessor frame-width
    :initarg :width)
   (height
    :initform nil
    :accessor frame-height
    :initarg :height)
   (window
    :initform nil
    :accessor frame-window
    :initarg :window)))

(defmethod print-swm-object ((object frame) stream)
  (format stream "FRAME ~d ~a ~d ~d ~d ~d"
          (frame-number object) (frame-window object) (frame-x object) (frame-y object) (frame-width object) (frame-height object)))

(defun frame-p (object)
  (typep object 'frame))

(defun make-frame (&rest rest &key number x y width height window)
  (declare (ignore number x y width height window))
  (apply 'make-swm-class-instance 'frame rest))

(defun copy-frame (instance)
  (make-swm-class-instance 'frame :number (frame-number instance)
                                  :x (frame-x instance)
                                  :y (frame-y instance)
                                  :width (frame-width instance)
                                  :height (frame-height instance)
                                  :window (frame-window instance)))

(define-swm-class head (frame)
  ((name
    :initform ""
    :accessor head-name
    :initarg :name)))

(defmethod print-swm-object ((object head) stream)
  (write-string "HEAD-" stream)
  (call-next-method))

;; duplicate frame accessors for heads.
(macrolet ((define-head-accessor (name)
             (let ((pkg (find-package :stumpwm)))
               `(progn
                  (defgeneric ,(intern (format nil "HEAD-~A" (symbol-name name)) pkg) (head)
                    (:method ((head head))
                      (,(intern (format nil "FRAME-~A" (symbol-name name)) pkg) head)))
                  
                  (defmethod (setf ,(intern (format nil "HEAD-~A" (symbol-name name)) pkg))
                      (new (head head))
                    (setf (,(intern (format nil "FRAME-~A" (symbol-name name)) pkg) head)
                          new))))))
  (define-head-accessor number)
  (define-head-accessor x)
  (define-head-accessor y)
  (define-head-accessor width)
  (define-head-accessor height)
  (define-head-accessor window))

(defun head-p (object)
  (typep object 'head))

(defun make-head (&rest rest &key number x y width height window name)
  (declare (ignore number x y width height window name))
  (apply 'make-swm-class-instance 'head rest))

(defun copy-head (instance)
  (make-swm-class-instance 'head :number (frame-number instance)
                                 :x (frame-x instance)
                                 :y (frame-y instance)
                                 :width (frame-width instance)
                                 :height (frame-height instance)
                                 :window (frame-window instance)
                                 :name (head-name instance)))

(define-swm-class screen ()
  ((id :initarg :id :reader screen-id)
   (host :initarg :host :reader screen-host)
   (number :initarg :number :reader screen-number)
   (heads :initform () :accessor screen-heads)
   (groups :initform () :accessor screen-groups)
   (current-group :accessor screen-current-group)
   ;; various colors (as returned by alloc-color)
   (border-color :initarg :border-color :accessor screen-border-color)
   (fg-color :initarg :fg-color :accessor screen-fg-color)
   (bg-color :initarg :bg-color :accessor screen-bg-color)
   (win-bg-color :initarg :win-bg-color :accessor screen-win-bg-color)
   (focus-color :initarg :focus-color :accessor screen-focus-color)
   (unfocus-color :initarg :unfocus-color :accessor screen-unfocus-color)
   (float-focus-color :initarg :float-focus-color :accessor screen-float-focus-color)
   (float-unfocus-color :initarg :float-unfocus-color :accessor screen-float-unfocus-color)
   (msg-border-width :initarg :msg-border-width :accessor screen-msg-border-width)
   (frame-outline-width :initarg :frame-outline-width :accessor screen-frame-outline-width)
   (fonts :initarg :fonts :accessor screen-fonts)
   (mapped-windows :initform () :accessor screen-mapped-windows :documentation
    "A list of all mapped windows. These are the raw xlib:window's. window structures are stored in groups.")
   (withdrawn-windows :initform () :accessor screen-withdrawn-windows :documentation
    "A list of withdrawn windows. These are of type stumpwm::window
and when they're mapped again they'll be put back in the group
they were in when they were unmapped unless that group doesn't
exist, in which case they go into the current group.")
   (urgent-windows :initform () :accessor screen-urgent-windows :documentation
    "a list of windows for which (window-urgent-p) currently true.")
   (input-window :initarg :input-window :reader screen-input-window)
   (key-window :initarg :key-window :reader screen-key-window :documentation
    "the window that accepts further keypresses after a toplevel key has been pressed.")
   (focus-window :initarg :focus-window :reader screen-focus-window :documentation
    "The window that gets focus when no window has focus")
   (frame-window :initarg :frame-window :reader screen-frame-window)
   (frame-outline-gc :initarg :frame-outline-gc :reader screen-frame-outline-gc)
   ;; color contexts
   (message-cc :initarg :message-cc :reader screen-message-cc)
   ;; color maps
   (color-map-normal :initform nil :accessor screen-color-map-normal)
   (color-map-bright :initform nil :accessor screen-color-map-bright)
   (ignore-msg-expose :initform 0 :accessor screen-ignore-msg-expose :documentation
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

(defvar *window-number-map* "0123456789"
  "Set this to a string to remap the window numbers to something more convenient.")

(defvar *group-number-map* "123456789"
  "Set this to a string to remap the group numbers to something more convenient.")

(defvar *frame-number-map* "0123456789abcdefghijklmnopqrstuvwxyz"
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

(defmethod print-swm-object ((object screen) stream)
  (format stream "SCREEN ~s" (screen-number object)))

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
(defvar *hooks-enabled-p* t
  "Controls whether hooks will actually run or not")

(defun run-hook-with-args (hook &rest args)
  "Call each function in HOOK and pass args to it."
  (when *hooks-enabled-p*
    (handler-case
        (with-simple-restart (abort-hooks "Abort running the remaining hooks.")
          (with-restarts-menu
            (dolist (fn hook)
              (with-simple-restart (continue-hooks "Continue running the remaining hooks.")
                (apply fn args)))))
      (t (c) (message "^B^1*Error on hook ^b~S^B!~% ^n~A" hook c) (values nil c)))))

(defun run-hook (hook)
  "Call each function in HOOK."
  (run-hook-with-args hook))

(defmacro add-hook (hook fn)
  "Add @var{function} to the @var{hook-variable}. For example, to
display a message whenever you switch frames:

@example
\(defun my-rad-fn (to-frame from-frame)
  (stumpwm:message \"Mustard!\"))

\(stumpwm:add-hook stumpwm:*focus-frame-hook* 'my-rad-fn)
@end example"
  `(setf ,hook (adjoin ,fn ,hook)))

(defmacro remove-hook (hook fn)
"Remove the specified function from the hook."
  `(setf ,hook (remove ,fn ,hook)))

(defmacro remove-all-hooks (hook)
"Remove all functions from a hook"
  `(setf ,hook NIL))

;; Misc. utility functions

(defun sort1 (list sort-fn &rest keys &key &allow-other-keys)
  "Return a sorted copy of list."
  (let ((copy (copy-list list)))
    (apply 'sort copy sort-fn keys)))

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

(defun split-seq (seq separators &key test default-value)
  "Split a sequence into subsequences given the list of seperators."
  (let ((seps separators))
    (labels ((sep (c)
               (position c seps :test test)))
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

(defun match-all-regexps (regexps target-string &key (case-insensitive t))
  "Return T if TARGET-STRING matches all regexps in REGEXPS.
REGEXPS can be a list of strings (one regexp per element) or a single
string which is split to obtain the individual regexps. "
  (let* ((regexps (if (listp regexps)
                      regexps
                      (split-string regexps " "))))
    (loop for pattern in regexps
       always (let ((scanner (ppcre:create-scanner pattern
                                                   :case-insensitive-mode case-insensitive)))
                (ppcre:scan scanner target-string)))))

(defun insert-before (list item nth)
  "Insert ITEM before the NTH element of LIST."
  (declare (type (integer 0 *) nth))
  (let* ((nth (min nth (length list)))
         (pre (subseq list 0 nth))
         (post (subseq list nth)))
    (nconc pre (list item) post)))

;;; 
;;; formatting routines

(declaim (ftype (function (vector list list &key (:element-type (or cons symbol))) vector) replace-ranges))
(defun replace-ranges (vec ranges replacements &key (element-type (array-element-type vec)))
  "Return a new vector with all (`START' `END') pairs in @var{`RANGES'} replaced with the corresponding vector in
the list @var{`REPLACEMENTS'}.

If the keyword argument `ELEMENT-TYPE' is provided, the resulting vector is defined to have elements of that type.
Ensure all replacement vectors are of compatible type or it will error, as it trusts this blindly.
Otherwise, it uses the element type of `VEC' - to use replacements with arbitrary element types,
set `ELEMENT-TYPE' to T.

The lengths of the replacements do not matter, and only a single non-resizeable vector will
be created for the result.

Example using strings:
@samp{(replace-ranges \"This is a test string with replacements.\"
'((0 0) (10 14) (27 40))
'(\"(Hi) \" \"simple\" \"three replaced sections.\"))} =>
\"(Hi!) This is a simple string with three replaced sections.\"

@samp{(replace-ranges \"A vector of characters, also known as a string.\"
'((12 22) (40 47))
'(#(\"not\" \"just\" \"one element type\")
\"simple-vector.\") :element-type T)} =>
#(#\A #\  #\v #\e #\c #\t #\o #\r #\  #\o #\f #\  \"not\" \"just\" \"one element type\" #\, #\
#\a #\l #\s #\o #\  #\k #\n #\o #\w #\n #\  #\a #\s #\  #\a #\
#\s #\i #\m #\p #\l #\e #\- #\v #\e #\c #\t #\o #\r #\.)"
  (let* ((base-len (length vec))
         (length (+ base-len
                    (loop for prev-end = 0 then end
                          for (start end) (integer integer) in ranges
                          for replacement vector in replacements
                          do (assert (>= end start prev-end) (start end)
                                     "Ranges must be in numerical order and not overlap.")
                          sum (- (length replacement) (- end start))))))
    (declare (type integer length))
    (loop
      ;; needs previous start as well
      with composed vector = (make-array length :element-type element-type)

      ;; Offset is to keep indexes synchronized between COMPOSED and STR
      for offset integer = 0 then (+ offset (- (length replacement) (- end start)))
      for prev-end integer = 0 then end
      for (start end) (integer integer) in ranges
      for replacement vector in replacements
      ;; Insert text between last replacement up until current, unless there is none
      unless (zerop (- start prev-end)) do
        (replace composed vec
                 :start1 (+ prev-end offset)
                 :start2 prev-end :end2 start)

      do (replace composed replacement
                  :start1 (+ start offset))
         ;; Add end of STR if necessary
      finally (unless (= prev-end base-len)
                (replace composed vec
                         :start1 (+ end offset)
                         :start2 end))
              (return composed))))

(defun string-shorten (str &optional trim-count trim-end-p)
  "Given a vector `STR', returns the string trimmed to length `TRIM-COUNT'. If `TRIM-END-P', trims from the end instead.

Does not trim if `TRIM-COUNT' is nil, and returns the empty string if it is zero."
  (if trim-count
      (let ((length (length str)))
        (cond ((> trim-count length) str)
              ((zerop trim-count) (copy-seq ""))
              (trim-end-p (subseq str (- length trim-count)))
              (t (subseq str 0 trim-count))))
      str))

(defun format-expand (fmt-alist str &rest args)
  (let ((length (1- (length str))) (start 0) (end 0)
        expander-char trim-count trim-end-p)
    (labels ((read-next-expander ()
               (when (> length end)
                 (if-let ((pos (position #\% str :start end :end length :test #'char=)))
                   (let ((percents (if-let ((end-percents (position #\% str :start (1+ pos) :end (1+ length) :test #'char/=)))
                                     (- end-percents pos)
                                     ;; nil means it /didn't/ find a char that wasn't a percent, so it must be percents allll
                                     ;; the way till the end.
                                     (1+ (- length pos)))))
                     ;; If there's more than one expander, just handle escapes and then have the next loop
                     ;; handle the actual expander itself, if there is one.
                     (if (= 1 percents)
                         (let* ((offset-pos (1+ pos))
                                (next-char (char str offset-pos)))
                           (let* ((trim-count (when (digit-char-p next-char)
                                                  ;; Read till length of digits and parse it
                                                  (let ((end-digits (position-if (complement #'digit-char-p) str :start offset-pos)))
                                                    (prog1 (parse-integer str :start offset-pos :end end-digits)
                                                      (incf offset-pos (- end-digits offset-pos))))))
                                  (trim-end-p (when (char= (char str offset-pos) #\^)
                                                (incf offset-pos) t)))
                             ;; length pos is offset (after percents, past padding specifier and ^) + 1 (past expander-char)
                             (values (char str offset-pos) pos (+ offset-pos 1) trim-count trim-end-p)))
                         ;; Getting rid of odd part means that, if there's an unescaped percent, it's kept for next iter
                         (let* ((escapes (floor percents 2))
                                (end-escapes (+ pos (* escapes 2))))
                           (values nil pos end-escapes escapes nil)))))))

             (handle-expander ()
               (if-let ((expander (second (assoc expander-char fmt-alist :test #'char=))))
                 (string-shorten
                  (let ((result (apply expander args)))
                    ;; Original would already produce an error since #'string would fail to convert to string,
                    ;; so this just adds the ability to handle a list of things uiop:strcat supports: chars, strings, and nil.
                    ;; If that fails, /then/ you have an error.
                    (etypecase result
                      (string result)
                      (atom (write-to-string result :escape nil))
                      (list (apply #'uiop:strcat result))))
                  trim-count trim-end-p)))
             ;; Separated so it can be run in the initially clause.
             (update-loop ()
               (setf (values expander-char start end trim-count trim-end-p)
                     (read-next-expander))))
      (loop
        ;; This can halve runtime is these cases
        initially
           (update-loop)
           (cond ((not (and start end))
                  (return str)) ; no expanders or escapes found
                 ((and (zerop start) (= (1- end) length)) ; The string is /only/ an expander
                  (return (if expander-char
                              ;; Insert result or leave expander str
                              ;; "%z" => "%z" when #\z has nothing assigned
                              (or (handle-expander) str)
                              ;; It's only percents, return trimmed (escaped)
                              ;; "%%%%" => "%%"
                              (string-shorten str trim-count trim-end-p)))))
           ;; loop start, check before update is to handle the values from initially form
        if expander-char
          collect (list start end) into ranges
        ;; Same as in cond, insert or leave in
          and collect (or (handle-expander) (subseq str start end)) into replacements
        else
          ;; string-replace-ranges will effectively erase unescaped percents, by not bothering adding them.
          collect (list (+ start trim-count) end) into ranges
          and collect "" into replacements
        end
        do (update-loop)
        while start ; While there are expanders/escapes, will always be a start pos.
        finally (return (replace-ranges str ranges replacements :element-type 'character))))))

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
Substitutes the window's number translated via *window-number-map*, if there
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
  "The format used in the info command. See
  @var{*window-format*} for formatting details.")

(defparameter *window-format-by-class* "%m%n %c %s%50t"
  "The format used in the info winlist-by-class command. See
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
  "Assign this T and messages will not time out. It is recommended to assign this using LET.")

(defvar *ignore-echo-timeout* nil
  "Assign this T and the message time out won't be touched. It is recommended to assign this using LET.")

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

(defmacro define-frame-preference (target-group &body frame-rules)
  "Create a rule that matches windows and automatically places them in
a specified group and frame or converts them to floating windows. Each
frame rule is a lambda list:
@example
\(frame-number raise lock &key from-group create restore dump-name class class-not
instance instance-not type type-not role role-not title title-not
match-properties-and-function match-properties-or-function)
@end example

@table @var
@item target-group
When nil, rule applies in the current group. When non nil, @var{lock} determines
applicability of rule

@item frame-number
The frame number to send matching windows to. If set to :float instead of a
frame number, the window will be converted to a floating window. This is
convenient for applications that should be launched as pop-ups.

@item raise
When non-nil, raise and focus the window in its frame

@item lock
When this is nil, this rule will only match when @var{target-group}
matches the group designated by @var{from-group}.
When non-nil, this rule matches regardless
of the group and the window is sent to @var{target-group}. If
@var{lock} and @var{raise} are both non-nil, then stumpwm will jump to
the specified group and focus the matched window.

@item from-group
When @var{lock} is NIL, and this is non-NIL, this rule will only match
when @var{target-group} matches @var{from-group}. This should be set
to either a group name(a string), or an expression that returns a group(e.g (current-group)).
When this is NIL, the rule matches if @var{target-group} matches
the group the window is in, or the current group if the window has no group.
@item create
When non-NIL the group is created and eventually restored when the value of
create is a group dump filename in *DATA-DIR*. Defaults to NIL.

@item restore
When non-NIL the group is restored even if it already exists. This arg should
be set to the dump filename to use for forced restore. Defaults to NIL

@item class
The windows class must match @var{class}.

@item class-not
The windows class must not match @var{class-not}

@item instance
The windows instance/resource name must match @var{instance}.

@item instance-not
The windows instance/resource name must not match @var{instance-not}.

@item type
The windows type must match @var{type}.

@item type-not
The windows type must not match @var{type-not}.

@item role
The windows role must match @var{role}.

@item role-not
The windows role must not match @var{role-not}.

@item title
The windows title must match @var{title}.

@item title-not
The windows title must not match @var{title-not}.

@item match-properties-and-function
A function that, if provided, must return true alongside the provided properties
in order for the rule to match. This function takes one argument, the window. 
Must be an unquoted symbol to be looked up at runtime. 

@item match-properties-or-function
A function that, if provided and returning true, will cause the rule to match
regardless of whether the window properties match. Takes one argument, the window.
Must be an unquoted symbol to be looked up at runtime. 
@end table"
  (let ((x (gensym "X")))
    `(dolist (,x ',frame-rules)
       ;; verify the correct structure
       (destructuring-bind (frame-number raise lock &rest keys) ,x
         (push (list* ,target-group frame-number raise lock keys)
               *window-placement-rules*)))))

(defun clear-window-placement-rules ()
  "Clear all window placement rules."
  (setf *window-placement-rules* nil))

(defvar *fullscreen-in-frame-p-window-functions* nil
  "A alist of predicate functions for determining if a window should be
fullscreen in frame.")

(defun fullscreen-in-frame-p (win)
  (some (lambda (r)
          (let ((res (funcall (cdr r) win)))
            (when res
              (dformat 3 "Fullscreen in frame selector ~A matches window ~A"
                       (car r) win))
            res))
        *fullscreen-in-frame-p-window-functions*))

(defun add-fullscreen-in-frame-rule (name function &key shadow)
  "Add a function to the fullscreen-in-frame window rules alist.  If @var{NAME}
already exists as a key in the alist and @var{SHADOW} is nil, then
@var{FUNCTION} replaces the existing value.  Otherwise @var{NAME} and
@var{FUNCTION} are pushed onto the alist."
  (let ((present (assoc name *fullscreen-in-frame-p-window-functions*)))
    (if (and present (not shadow))
        (setf (cdr present) function)
        (push (cons name function) *fullscreen-in-frame-p-window-functions*))))

(defun remove-fullscreen-in-frame-rule (name &key count)
  "Remove rules named @var{NAME} from the fullscreen-in-frame window rules alist.
If @var{COUNT} is NIL then all matching rules are removed, otherwise only the
first @var{COUNT} rules are removed."
  (setf *fullscreen-in-frame-p-window-functions*
        (remove name *fullscreen-in-frame-p-window-functions*
                :key #'car :count count)))

(defmacro define-fullscreen-in-frame-rule (name (window-argument) &body body)
  "Define a rule for a window to be fullscreened within the frame.  Each rule is a
function which will be called when a window is made fullscreen.  If the rule
returns NIL then the fullscreen window takes up the entire head, otherwise it
takes up only its frame. Within the body of the rule @var{WINDOW-ARGUMENT} is
bound to the window being processed."
  `(flet ((,name (,window-argument) ,@body))
     (add-fullscreen-in-frame-rule ',name #',name)))

(defvar *mouse-focus-policy* :ignore
  "The mouse focus policy decides how the mouse affects input
focus. Possible values are :ignore, :sloppy, and :click. :ignore means
stumpwm ignores the mouse. :sloppy means input focus follows the
mouse; the window that the mouse is in gets the focus. :click means
input focus is transfered to the window you click on.

If *MOUSE-FOCUS-POLICY* holds any value other than those listed above,
mouse focus will behave as though it contains :IGNORE")

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

(defun ensure-data-dir ()
  (ensure-directories-exist *data-dir* :mode #o700))

(defun default-data-dir ()
  "Return the default data dir pathname based on the loaded StumpWM configuration file."
  (let ((rc-file (or
                  (let ((pathspec (merge-pathnames #p".stumpwmrc" (user-homedir-pathname))))
                    (and (probe-file pathspec) pathspec))
                  (let ((pathspec (merge-pathnames #p".stumpwm.d/init.lisp" (user-homedir-pathname))))
                    (and (probe-file pathspec) pathspec))
                  (let ((pathspec (uiop:xdg-config-home #p"stumpwm/config")))
                    (and (probe-file pathspec) pathspec)))))
    (if rc-file
        (make-pathname :name nil :type nil :defaults rc-file)
        (merge-pathnames ".stumpwm.d/" (user-homedir-pathname)))))

(defun data-dir-file (name &optional type)
  "Return a pathname inside stumpwm's data dir with the specified name and type"
  (ensure-data-dir)
  (make-pathname :name name :type type :defaults *data-dir*))

(defmacro with-data-file ((s file &rest keys &key (if-exists :supersede) &allow-other-keys) &body body)
  "Open a file in StumpWM's data directory. keyword arguments are sent
directly to OPEN. Note that IF-EXISTS defaults to :supersede, instead
of :error."
  (declare (ignorable if-exists))
  `(progn
     (ensure-data-dir)
     (with-open-file (,s ,(merge-pathnames file *data-dir*)
                         ,@keys)
                     ,@body)))

(defun rotate-log ()
  (let ((log-filename (merge-pathnames "stumpwm.log" *data-dir*))
        (bkp-log-filename (merge-pathnames "stumpwm.log.1" *data-dir*)))
    (when (probe-file log-filename)
      (rename-file log-filename bkp-log-filename))))

(defun open-log ()
  (rotate-log)
  (let ((log-filename (merge-pathnames "stumpwm.log" *data-dir*)))
    (setf *debug-stream* (open log-filename :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create))))
(defun close-log ()
  (when (boundp '*debug-stream*)
    (close *debug-stream*)
    (makunbound '*debug-stream*)))

(defmacro move-to-head (list elt)
   "Move the specified element in in LIST to the head of the list."
 `(progn
    (setf ,list (remove ,elt ,list))
    (push ,elt ,list)))

(define-condition stumpwm-condition (condition)
  ((message :initarg :message :reader warning-message))
  (:documentation "Any stumpmwm specific condition should inherit from this.")
  (:report (lambda (condition stream)
            (format stream "~A~%" (warning-message condition)))))

(define-condition stumpwm-error (stumpwm-condition error)
  ()
  (:documentation "Any stumpwm specific error should inherit this."))

(define-condition stumpwm-warning (warning stumpwm-condition)
  ()
  (:documentation "Adds a message slot to warning. Any stumpwm specific warning
  should inherit from this."))

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

(defstruct (mode-line (:constructor %make-mode-line))
  screen
  head
  window
  format
  position
  contents
  cc
  height
  factor
  (mode :stump)
  on-click-bounds
  new-bounds)

(defstruct timer
  time repeat function args)

(defvar *minor-mode-maps* ()
  "A list of minor mode keymaps. An element of the list may be a single keymap or
a function. If an element is a function it must take a group instance and return
a list of keymaps.")

(defvar *custom-command-filters* ()
  "A list of functions which take a group instance and a command structure, and
return true when the command should be active.")
