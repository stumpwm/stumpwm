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
;; Screen functionality
;;
;; Code:

(in-package #:stumpwm)

(export '(*default-bg-color*
          current-screen
          current-window
          screen-current-window
          screen-number
          screen-groups
          screen-windows
          screen-height
          screen-width
          set-fg-color
          set-bg-color
          set-border-color
          set-win-bg-color
          set-focus-color
          set-unfocus-color
          set-float-focus-color
          set-float-unfocus-color
          set-msg-border-width
          set-frame-outline-width
          set-font))

(defvar *default-bg-color* #x333333
  "Default color for the desktop background.")

;; Screen helper functions

(defun translate-id (src src-start src-end font dst dst-start)
  "A simple replacement for xlib:translate-default.  just the
identity with a range check."
  (let ((min (xlib:font-min-char font))
        (max (xlib:font-max-char font)))
    (decf src-end)
    (loop for i from src-start to src-end
          for j from dst-start
          as e = (elt src i)
          as c = (if (characterp e) (char-code e) e)
          if (and (integerp c) (<= min c max))
            do (setf (aref dst j) c)
          else
            ;; replace unknown characters with question marks
            do (setf (aref dst j) (char-code #\?))
          finally (return i))))

(defun screen-x (screen)
  (declare (ignore screen))
  0)

(defun screen-y (screen)
  (declare (ignore screen))
  0)

(defun screen-height (screen)
  (let ((root (screen-root screen)))
    (xlib:drawable-height root)))

(defun screen-width (screen)
  (let ((root (screen-root screen)))
    (xlib:drawable-width root)))

(defun find-screen (root)
  "Return the screen containing the root window."
  (find-if (lambda (s)
             (xlib:window-equal (screen-root s) root))
           *screen-list*))

(defun screen-windows (screen)
  (mapcan (lambda (g) (copy-list (group-windows g))) (screen-groups screen)))

(defun screen-message-window (screen)
  (ccontext-win (screen-message-cc screen)))

(defun screen-message-pixmap (screen)
  (ccontext-px (screen-message-cc screen)))

(defun screen-message-gc (screen)
  (ccontext-gc (screen-message-cc screen)))

(defun screen-font (screen)
  (first (screen-fonts screen)))



(defun netwm-update-client-list-stacking (screen)
  (unless *initializing*
    (xlib:change-property (screen-root screen)
                          :_NET_CLIENT_LIST_STACKING
                          ;; Order is bottom to top.
                          (reverse (mapcar 'window-xwin (all-windows)))
                          :window 32
                          :transform #'xlib:drawable-id
                          :mode :replace)))

(defun netwm-update-client-list (screen)
  (xlib:change-property (screen-root screen)
                        :_NET_CLIENT_LIST
                        (screen-mapped-windows screen)
                        :window 32
                        :transform #'xlib:drawable-id
                        :mode :replace)
  (netwm-update-client-list-stacking screen))

(defun screen-add-mapped-window (screen xwin)
  (push xwin (screen-mapped-windows screen))
  (netwm-update-client-list screen))

(defun screen-remove-mapped-window (screen xwin)
  (unregister-window xwin)
  (setf (screen-mapped-windows screen)
        (remove xwin (screen-mapped-windows screen)))
  (netwm-update-client-list screen))

(defun sort-screens ()
  "Return the list of screen sorted by ID."
  (sort1 *screen-list* '< :key 'screen-id))

(defun next-screen (&optional (list (sort-screens)))
  (let ((matches (member (current-screen) list)))
    (if (null (cdr matches))
        ;; If the last one in the list is current, then
        ;; use the first one.
        (car list)
        ;; Otherwise, use the next one in the list.
        (cadr matches))))

(defun move-screen-to-head (screen)
  (move-to-head *screen-list* screen))

(defun switch-to-screen (screen)
  (when (and screen
             (not (eq screen (current-screen))))
    (if (screen-focus screen)
        (xlib:set-input-focus *display* (window-xwin (screen-focus screen)) :POINTER-ROOT)
        (xlib:set-input-focus *display* (screen-focus-window screen) :POINTER-ROOT))
    (move-screen-to-head screen)))

(defun screen-set-focus (screen window)
  (when (eq (window-group window)
            (screen-current-group screen))
    (xlib:set-input-focus *display* (window-xwin window) :POINTER-ROOT)
    (xlib:change-property (screen-root screen) :_NET_ACTIVE_WINDOW
                          (list (window-xwin window))
                          :window 32
                          :transform #'xlib:drawable-id
                          :mode :replace)
    (setf (screen-focus screen) window)
    (move-screen-to-head screen)))

(defun screen-current-window (screen)
  "Return the current window on the specified screen"
  (group-current-window (screen-current-group screen)))

(defun current-window ()
  "Return the current window on the current screen"
  (screen-current-window (current-screen)))

(defun register-window (window)
  (setf (gethash (xlib:window-id (window-xwin window)) *xwin-to-window*) window))

(defun unregister-window (xwin)
  (remhash (xlib:window-id xwin) *xwin-to-window*))

(defun window-by-id (id)
  (gethash id *xwin-to-window*))

(defun find-window (xwin)
  (window-by-id (xlib:window-id xwin)))

(defun find-window-by-parent (xwin &optional (windows (all-windows)))
  (dformat 3 "find-window-by-parent!~%")
  (find xwin windows :key 'window-parent :test 'xlib:window-equal))

(defun screen-root (screen)
  (xlib:screen-root (screen-number screen)))

(defun update-colors-for-screen (screen)
  (let ((fg (screen-fg-color screen))
        (bg (screen-bg-color screen)))
    (setf (xlib:gcontext-foreground (screen-message-gc screen)) fg
          (xlib:gcontext-background (screen-message-gc screen)) bg
          (xlib:gcontext-foreground (screen-frame-outline-gc screen)) fg
          (xlib:gcontext-background (screen-frame-outline-gc screen)) bg
          (ccontext-default-fg (screen-message-cc screen)) fg
          (ccontext-default-bg (screen-message-cc screen)) bg))
  (dolist (i (list (screen-message-window screen)
                   (screen-input-window screen)
                   (screen-frame-window screen)))
    (setf (xlib:window-border i) (screen-border-color screen)
          (xlib:window-background i) (screen-bg-color screen)))
  ;; update the backgrounds of all the managed windows
  (dolist (g (screen-groups screen))
    (dolist (w (group-windows g))
      (unless (eq w (group-current-window g))
        (setf (xlib:window-background (window-parent w)) (screen-win-bg-color screen))
        (xlib:clear-area (window-parent w)))))
  (dolist (i (screen-withdrawn-windows screen))
    (setf (xlib:window-background (window-parent i)) (screen-win-bg-color screen))
    (xlib:clear-area (window-parent i)))
  (update-screen-color-context screen))

(defun update-colors-all-screens ()
  "After setting the fg, bg, or border colors, call this to sync any existing windows."
  (mapc 'update-colors-for-screen *screen-list*))

(defun update-border-for-screen (screen)
  (setf (xlib:drawable-border-width (screen-input-window screen)) (screen-msg-border-width screen)
        (xlib:drawable-border-width (screen-message-window screen)) (screen-msg-border-width screen)
        (xlib:drawable-border-width (screen-frame-window screen)) (screen-msg-border-width screen)))

(defun update-border-all-screens ()
  "After setting the border width call this to sync any existing windows."
  (mapc 'update-border-for-screen *screen-list*))

(defun internal-window-p (screen win)
  "Return t if win is a window used by stumpwm"
  (or (xlib:window-equal (screen-message-window screen) win)
      (xlib:window-equal (screen-input-window screen) win)
      (xlib:window-equal (screen-focus-window screen) win)
      (xlib:window-equal (screen-key-window screen) win)))

(defmacro set-any-color (val color)
  `(progn (dolist (s *screen-list*)
            (setf (,val s) (alloc-color s ,color)))
    (update-colors-all-screens)))

;; FIXME: I don't like any of this.  Isn't there a way to define
;; a setf method to call (update-colors-all-screens) when the user
;; does eg. (setf *foreground-color* "green") instead of having
;; these redundant set-foo functions?
(defun set-fg-color (color)
  "Set the foreground color for the message bar and input
bar. @var{color} can be any color recognized by X."
  (setf *text-color* color)
  (set-any-color screen-fg-color color))

(defun set-bg-color (color)
  "Set the background color for the message bar and input
bar. @var{color} can be any color recognized by X."
  (set-any-color screen-bg-color color))

(defun set-border-color (color)
  "Set the border color for the message bar and input
bar. @var{color} can be any color recognized by X."
  (set-any-color screen-border-color color))

(defun set-win-bg-color (color)
  "Set the background color of the window. The background color will only
be visible for windows with size increment hints such as @samp{emacs}
and @samp{xterm}."
  (set-any-color screen-win-bg-color color))

(defun set-focus-color (color)
  "Set the border color for focused windows. This is only used when
there is more than one frame."
  (set-any-color screen-focus-color color))

(defun set-unfocus-color (color)
  "Set the border color for windows without focus. This is only used when
there is more than one frame."
  (set-any-color screen-unfocus-color color))

(defun set-float-focus-color (color)
  "Set the border color for focused windows in a float group."
  (set-any-color screen-float-focus-color color))

(defun set-float-unfocus-color (color)
  "Set the border color for windows without focus in a float group."
  (set-any-color screen-float-unfocus-color color))

(defun set-msg-border-width (width)
  "Set the border width for the message bar, input bar and frame indicator."
  (check-type width (integer 0))
  (dolist (i *screen-list*)
    (setf (screen-msg-border-width i) width))
  (update-border-all-screens)
  t)

(defun set-frame-outline-width (width)
  (check-type width (integer 0))
  (dolist (i *screen-list*)
    (setf (screen-frame-outline-width i) (if (oddp width) (1+ width) width)
          (xlib:gcontext-line-width (screen-frame-outline-gc i)) (screen-frame-outline-width i)))
  (update-border-all-screens)
  t)

(defun set-font (font)
  "Set the font(s) for the message bar and input bar."
  (when (if (listp font)
            (every #'identity (mapcar #'font-exists-p font))
            (font-exists-p font))
    (dolist (screen *screen-list*)
      (let ((fonts (if (listp font)
                       (mapcar (lambda (font) (open-font *display* font))
                               font)
                       (list (open-font *display* font)))))
        (mapc #'close-font (screen-fonts screen))
        (setf (screen-fonts screen) fonts)))
    t))

(defmacro with-current-screen (screen &body body)
  "A macro to help us out with early set up."
  `(let ((*screen-list* (list ,screen)))
    ,@body))

(defun current-screen ()
  "Return the current screen."
  (car *screen-list*))

(defun netwm-set-properties (screen)
  "Set NETWM properties on the root window of the specified screen.
FOCUS-WINDOW is an extra window used for _NET_SUPPORTING_WM_CHECK."
  (let* ((screen-number (screen-number screen))
         (focus-window (screen-focus-window screen))
         (root (screen-root screen)))
    ;; _NET_SUPPORTED
    (xlib:change-property root :_NET_SUPPORTED
                          (mapcar (lambda (a)
                                    (xlib:intern-atom *display* a))
                                  (append +netwm-supported+
                                          (mapcar #'car +netwm-window-types+)))
                          :atom 32)

    ;; _NET_SUPPORTING_WM_CHECK
    (xlib:change-property root :_NET_SUPPORTING_WM_CHECK
                          (list focus-window) :window 32
                          :transform #'xlib:drawable-id)
    (xlib:change-property focus-window :_NET_SUPPORTING_WM_CHECK
                          (list focus-window) :window 32
                          :transform #'xlib:drawable-id)
    (xlib:change-property focus-window :_NET_WM_NAME
                          "stumpwm"
                          :string 8 :transform #'xlib:char->card8)

    ;; _NET_CLIENT_LIST
    (xlib:change-property root :_NET_CLIENT_LIST
                          () :window 32
                          :transform #'xlib:drawable-id)

    ;; _NET_DESKTOP_GEOMETRY
    (xlib:change-property root :_NET_DESKTOP_GEOMETRY
                          (list (xlib:screen-width screen-number)
                                (xlib:screen-height screen-number))
                          :cardinal 32)

    ;; _NET_DESKTOP_VIEWPORT
    (xlib:change-property root :_NET_DESKTOP_VIEWPORT
                          (list 0 0) :cardinal 32)

    (netwm-set-group-properties screen)))

(defun init-screen (screen-number id host)
  "Given a screen number, returns a screen structure with initialized members"
  ;; Listen for the window manager events on the root window
  (dformat 1 "Initializing screen: ~a ~a~%" host id)
  (setf (xlib:window-event-mask (xlib:screen-root screen-number))
        '(:substructure-redirect
          :substructure-notify
          :property-change
          :structure-notify
          :button-press
          :exposure))
  (xlib:display-finish-output *display*)
  ;; Initialize the screen structure
  (labels ((ac (color)
             (xlib:alloc-color (xlib:screen-default-colormap screen-number) color)))
    (let* ((default-colormap (xlib:screen-default-colormap screen-number))
           (screen-root (xlib:screen-root screen-number))
           (fg-color (ac +default-foreground-color+))
           (bg-color (ac +default-background-color+))
           (win-bg-color (ac +default-window-background-color+))
           (border-color (ac +default-border-color+))
           (focus-color (ac +default-focus-color+))
           (unfocus-color (ac +default-unfocus-color+))
           (float-focus-color (ac +default-float-focus-color+))
           (float-unfocus-color (ac +default-float-unfocus-color+))
           (font (open-font *display*
                            (cond ((font-exists-p +default-font-name+)
                                   +default-font-name+)
                                  ((font-exists-p "fixed")
                                   "fixed")
                                  (t
                                   "*"))))
           (message-window (xlib:create-window :parent screen-root
                                               :x 0 :y 0 :width 1 :height 1
                                               :colormap default-colormap
                                               :background bg-color
                                               :border border-color
                                               :border-width 1
                                               :bit-gravity :north-east
                                               :event-mask '(:exposure)))
           (screen (make-instance 'screen
                                  :id id
                                  :host host
                                  :number screen-number
                                  :border-color border-color
                                  :fg-color fg-color
                                  :bg-color bg-color
                                  :win-bg-color win-bg-color
                                  :focus-color focus-color
                                  :unfocus-color unfocus-color
                                  :float-focus-color float-focus-color
                                  :float-unfocus-color float-unfocus-color
                                  :msg-border-width 1
                                  :frame-outline-width +default-frame-outline-width+
                                  :fonts (list font)
                                  :input-window (xlib:create-window
                                                 :parent screen-root
                                                 :x 0 :y 0 :width 20 :height 20
                                                 :colormap default-colormap
                                                 :background bg-color
                                                 :border border-color
                                                 :border-width 1
                                                 :event-mask '(:key-press :key-release))
                                  :focus-window (xlib:create-window
                                                 :parent screen-root
                                                 :x 0 :y 0 :width 1 :height 1)
                                  :key-window (xlib:create-window
                                               :parent screen-root
                                               :x 0 :y 0 :width 1 :height 1
                                               :event-mask '(:key-press :key-release))
                                  :frame-window (xlib:create-window
                                                 :parent screen-root
                                                 :x 0 :y 0 :width 20 :height 20
                                                 :colormap default-colormap
                                                 :background bg-color
                                                 :border border-color
                                                 :border-width 1
                                                 :event-mask '(:exposure))
                                  :frame-outline-gc (xlib:create-gcontext
                                                     :drawable screen-root
                                                     :font (when (typep font 'xlib:font) font)
                                                     :foreground fg-color
                                                     :background fg-color
                                                     :line-style :double-dash
                                                     :line-width +default-frame-outline-width+)
                                  :message-cc (make-ccontext
                                               :win message-window
                                               :font font
                                               :gc (xlib:create-gcontext
                                                    :drawable message-window
                                                    :font (when (typep font 'xlib:font) font)
                                                    :foreground fg-color
                                                    :background bg-color))))
           (group (make-instance 'tile-group
                                 :screen screen
                                 :number 1
                                 :name *default-group-name*)))
      (setf (screen-groups screen) (list group)
            (screen-current-group screen) group
            (ccontext-screen (screen-message-cc screen)) screen
            (screen-heads screen) (make-screen-heads screen screen-root)
            (tile-group-frame-tree group) (copy-heads screen)
            (tile-group-current-frame group) (first (tile-group-frame-tree group))
            (xlib:window-background screen-root) *default-bg-color*)
      ;; The focus window is mapped at all times
      (xlib:map-window (screen-focus-window screen))
      (xlib:map-window (screen-key-window screen))
      (netwm-set-properties screen)
      (update-colors-for-screen screen)
      (update-color-map screen)
      (xwin-grab-keys (screen-focus-window screen) group)
      screen)))

;;; Screen commands

(defcommand snext () ()
"Go to the next screen."
  (switch-to-screen (next-screen))
  (group-wake-up (current-group)))

(defcommand sprev () ()
"Go to the previous screen."
  (switch-to-screen (next-screen (reverse (sort-screens))))
  (group-wake-up (current-group)))

(defcommand sother () ()
"Go to the last screen."
  (switch-to-screen (cadr *screen-list*))
  (group-wake-up (current-group)))
