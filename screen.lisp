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

(export '(current-screen
          current-window
          screen-current-window
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

;; Screen helper functions

(defun translate-id (src src-start src-end font dst dst-start)
  "A simple replacement for xlib:translate-default.  just the
identity with a range check."
  (let ((min (xlib:font-min-char font))
        (max (xlib:font-max-char font)))
    (decf src-end)
    (if (stringp src)      ; clx does this test so i guess it's needed
        (loop for i from src-start to src-end
              for j from dst-start
              as c = (char-code (char src i))
              if (<= min c max) do (setf (aref dst j) c)
              ;; replace unknown characters with question marks
              else do (setf (aref dst j) (char-code #\?))
              finally (return i))
        (loop for i from src-start to src-end
              for j from dst-start
              as c = (elt src i)
              as n = (if (characterp c) (char-code c) c)
              if (and (integerp n) (<= min n max)) do (setf (aref dst j) n)
              ;; ditto
              else do (setf (aref dst j) (char-code #\?))
              finally (return i)))))

(defun screen-x (screen)
  (declare (ignore screen))
  0)

(defun screen-y (screen)
  (declare (ignore screen))
  0)

(defun screen-height (screen)
  (let ((root (screen-root screen)))
    (xlib:drawable-height root)))

(defun screen-true-height (screen)
  "Return the height of the screen regardless of the modeline"
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
    ;;(format t "FOCUS TO: ~a ~a~%" window (window-xwin window))
    ;;(format t "FOCUS BEFORE: ~a~%" (multiple-value-list (xlib:input-focus *display*)))
    ;;(format t "FOCUS RET: ~a~%" (xlib:set-input-focus *display* (window-xwin window) :POINTER-ROOT))
    (xlib:set-input-focus *display* (window-xwin window) :POINTER-ROOT)
    ;;(xlib:display-finish-output *display*)
    ;;(format t "FOCUS IS: ~a~%" (multiple-value-list (xlib:input-focus *display*)))
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
  "After setting the fg, bg, or border colors. call this to sync any existing windows."
  (mapc 'update-colors-for-screen *screen-list*))

(defun update-border-for-screen (screen)
  (setf (xlib:drawable-border-width (screen-input-window screen)) (screen-msg-border-width screen)
        (xlib:drawable-border-width (screen-message-window screen)) (screen-msg-border-width screen)))

(defun update-border-all-screens ()
  "After setting the border width call this to sync any existing windows."
  (mapc 'update-border-for-screen *screen-list*))

(defun internal-window-p (screen win)
  "Return t if win is a window used by stumpwm"
  (or (xlib:window-equal (screen-message-window screen) win)
      (xlib:window-equal (screen-input-window screen) win)
      (xlib:window-equal (screen-focus-window screen) win)
      (xlib:window-equal (screen-key-window screen) win)))

(defun color-exists-p (color)
  (handler-case
      (loop for i in *screen-list*
            always (xlib:lookup-color (xlib:screen-default-colormap (screen-number i)) color))
    (xlib:name-error () nil)))

;; (defun font-exists-p (font-name)
;;   ;; if we can list the font then it exists
;;   (plusp (length (xlib:list-font-names *display* font-name :max-fonts 1))))

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
  "Set the border width for the message bar and input
bar."
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
  "Set the font for the message bar and input bar."
  (when (font-exists-p font)
    (dolist (i *screen-list*)
      (let ((fobj (open-font *display* font)))
        (close-font (screen-font i))
        (setf (screen-font i) fobj)
        (when (typep fobj 'xlib:font)
          (setf
           (xlib:gcontext-font (screen-message-gc i)) fobj))
        ;; update the modelines too
        (dolist (h (screen-heads i))
          (when (and (head-mode-line h)
                     (eq (mode-line-mode (head-mode-line h)) :stump))
            (when (typep fobj 'xlib:font)
              (setf (xlib:gcontext-font (mode-line-gc (head-mode-line h))) fobj))
            (resize-mode-line (head-mode-line h))
            (sync-mode-line (head-mode-line h))))))
    t))

(defmacro with-current-screen (screen &body body)
  "A macro to help us out with early set up."
  `(let ((*screen-list* (list ,screen)))
    ,@body))

(defun current-screen ()
  "Return the current screen."
  (car *screen-list*))

(defun netwm-set-properties (screen focus-window)
  "Set NETWM properties on the root window of the specified screen.
FOCUS-WINDOW is an extra window used for _NET_SUPPORTING_WM_CHECK."
  (let* ((screen-number (screen-number screen))
         (root (xlib:screen-root screen-number)))
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
    (let* ((screen (make-instance 'screen))
           (fg (ac +default-foreground-color+))
           (bg (ac +default-background-color+))
           (border (ac +default-border-color+))
           (focus (ac +default-focus-color+))
           (unfocus (ac +default-unfocus-color+))
           (float-focus (ac +default-float-focus-color+))
           (float-unfocus (ac +default-float-unfocus-color+))
           (win-bg (ac +default-window-background-color+))
           (input-window (xlib:create-window :parent (xlib:screen-root screen-number)
                                             :x 0 :y 0 :width 20 :height 20
                                             :background bg
                                             :border border
                                             :border-width 1
                                             :colormap (xlib:screen-default-colormap
                                                        screen-number)
                                             :event-mask '(:key-press :key-release)))
           (focus-window (xlib:create-window :parent (xlib:screen-root screen-number)
                                             :x 0 :y 0 :width 1 :height 1))
           (key-window (xlib:create-window :parent (xlib:screen-root screen-number)
                                           :x 0 :y 0 :width 1 :height 1
                                           :event-mask '(:key-press :key-release)))
           (message-window (xlib:create-window :parent (xlib:screen-root screen-number)
                                               :x 0 :y 0 :width 1 :height 1
                                               :background bg
                                               :bit-gravity :north-east
                                               :border border
                                               :border-width 1
                                               :colormap (xlib:screen-default-colormap
                                                          screen-number)
                                               :event-mask '(:exposure)))
           (frame-window (xlib:create-window :parent (xlib:screen-root screen-number)
                                             :x 0 :y 0 :width 20 :height 20
                                             :background bg
                                             :border border
                                             :border-width 1
                                             :colormap (xlib:screen-default-colormap
                                                        screen-number)
                                             :event-mask '(:exposure)))
           (font (open-font *display*
                            (if (font-exists-p +default-font-name+)
                                +default-font-name+
                                "*")))
           (group (make-instance 'tile-group
                   :screen screen
                   :number 1
                   :name *default-group-name*)))
      ;; Create our screen structure
      ;; The focus window is mapped at all times
      (xlib:map-window focus-window)
      (xlib:map-window key-window)
      (setf (screen-number screen) screen-number
            (screen-id screen) id
            (screen-host screen) host
            (screen-groups screen) (list group)
            (screen-current-group screen) group
            (screen-font screen) font
            (screen-fg-color screen) fg
            (screen-bg-color screen) bg
            (screen-win-bg-color screen) win-bg
            (screen-border-color screen) border
            (screen-focus-color screen) focus
            (screen-unfocus-color screen) unfocus
            (screen-float-focus-color screen) float-focus
            (screen-float-unfocus-color screen) float-unfocus
            (screen-msg-border-width screen) 1
            (screen-frame-outline-width screen) +default-frame-outline-width+
            (screen-input-window screen) input-window
            (screen-focus-window screen) focus-window
            (screen-key-window screen) key-window
            (screen-frame-window screen) frame-window
            (screen-ignore-msg-expose screen) 0
            (screen-message-cc screen) (make-ccontext :win message-window
                                                      :gc (xlib:create-gcontext
                                                           :drawable message-window
                                                           :font font
                                                           :foreground fg
                                                           :background bg))
            (screen-frame-outline-gc screen) (xlib:create-gcontext :drawable (screen-root screen)
                                                                   :font font
                                                                   :foreground fg
                                                                   :background fg
                                                                   :line-style :double-dash
                                                                   :line-width +default-frame-outline-width+))
      (setf (screen-heads screen) (make-screen-heads screen (xlib:screen-root screen-number))
            (tile-group-frame-tree group) (copy-heads screen)
            (tile-group-current-frame group) (first (tile-group-frame-tree group)))
      (netwm-set-properties screen focus-window)
      (update-colors-for-screen screen)
      (update-color-map screen)
      (xwin-grab-keys focus-window screen)
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
