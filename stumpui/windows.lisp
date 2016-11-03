;;;; Copyright (C) 2016 Joram Schrijver
;;;;
;;;;  This file is part of stumpwm.
;;;;
;;;; stumpwm is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; stumpwm is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, see
;;;; <http://www.gnu.org/licenses/>.

(in-package :stumpui)

;;; Window --------------------------------------------------------------------
;;; The WINDOW class can be used as a basis that already implements part of the
;;; WINDOW protocol. It leaves the following functions of the window protocol
;;; unimplemented:
;;;
;;; - WINDOW-REQUIRED-SIZE
;;; - WINDOW-REDRAW

(defclass window ()
  ((screen :initarg :screen
           :reader window-screen)
   (event-mask :initarg :event-mask
               :initform ()
               :reader window-direct-event-mask)
   (xwin :reader window-xwin
         :writer (setf %window-xwin))
   (gravity :initarg :gravity
            :initform :center
            :accessor window-gravity)))

(defmethod window-p ((window window)) t)

(defmethod window-event-mask append ((window window))
  (window-direct-event-mask window))

(defmethod window-show ((window window) frame &key &allow-other-keys)
  (let ((xwin (window-xwin window)))
    ;; TODO: Should placement be moved into a separate protocol function?
    (multiple-value-bind (width height)
        (window-required-size window)
      (setf (xlib:drawable-height xwin) height
            (xlib:drawable-width xwin) width
            (xlib:window-priority xwin) :above)
      (stumpwm::gravitate-xwin (window-screen window)
                               frame
                               xwin
                               (window-gravity window)))
    (unless (eq (xlib:window-map-state xwin) :mapped)
      (xlib:map-window xwin))
    (window-redraw window)))

(defmethod window-hide ((window window))
  (let ((xwin (window-xwin window)))
    (unless (eq (xlib:window-map-state xwin) :unmapped)
      (xlib:unmap-window xwin))))

(defmethod initialize-instance :after ((window window)
                                       &key &allow-other-keys)
  (let* ((screen (window-screen window))
         (screen-number (stumpwm:screen-number screen)))
    (setf (%window-xwin window)
          (xlib:create-window
           :parent (stumpwm:screen-root screen)
           :x 0 :y 0 :width 1 :height 1
           :colormap (xlib:screen-default-colormap screen-number)
           :border-width 1
           :event-mask (window-event-mask window)))))

;;; Text window ---------------------------------------------------------------
;;; The TEXT-WINDOW class implements the Text window protocol.

(defclass text-window (window)
  ((ccontext :initarg :ccontext
             :reader text-window-ccontext
             :writer (setf %text-window-ccontext))
   (text :initarg :text
         :initform ()
         :accessor text-window-text)
   (highlights :initarg :highlights
               :initform ()
               :accessor text-window-highlights)
   (padding :initarg :padding
            :accessor text-window-padding)))

(defmethod text-window-p ((window text-window)) t)

(defmethod window-show :before ((window text-window) frame
                                &key
                                  text
                                  highlights
                                  &allow-other-keys)
  (when text
    (setf (text-window-text window) text))
  (when highlights
    (setf (text-window-highlights window) highlights)))

(defmethod window-required-size ((window text-window))
  (multiple-value-bind (width height)
      (stumpwm::rendered-size (text-window-text window)
                              (text-window-ccontext window))
    (values (+ width (* (text-window-padding window) 2))
            height)))

(defmethod window-redraw ((window text-window))
  (stumpwm::render-strings (text-window-ccontext window)
                           (text-window-padding window)
                           0
                           (text-window-text window)
                           (text-window-highlights window)))

(defmethod (setf text-window-text) :around (value (window text-window))
  (if (stringp value)
      (call-next-method (stumpwm:split-string value (string #\Newline))
                        window)
      (call-next-method)))

(defmethod initialize-instance :after ((window text-window)
                                       &key &allow-other-keys)
  (let* ((screen (window-screen window))
         (xwin (window-xwin window))
         (ccontext (stumpwm::make-ccontext
                    :screen screen
                    :win xwin
                    :gc (xlib:create-gcontext :drawable xwin))))
    (setf (%text-window-ccontext window)
          ccontext
          (text-window-foreground-color window)
          (stumpwm::screen-fg-color screen)
          (text-window-background-color window)
          (stumpwm::screen-bg-color screen)
          (text-window-border-color window)
          (stumpwm::screen-border-color screen))
    (stumpwm::reset-color-context ccontext)))

;;; Style

(defmethod (setf text-window-foreground-color) (color (window text-window))
  (let* ((screen (window-screen window))
         (ccontext (text-window-ccontext window))
         (gcontext (stumpwm::ccontext-gc ccontext)))
    (setf (xlib:gcontext-foreground gcontext) color
          (stumpwm::ccontext-default-fg ccontext) color
          ;; Default the bright foreground color to the normal foreground in
          ;; case we fail to get the proper bright one.
          (stumpwm::ccontext-default-bright ccontext) color)
    ;; KLUDGE: We can calculate the default bright color from the foreground
    ;; color, but that's a PIXEL, not a COLOR. To get the COLOR, we need to
    ;; look it up.
    (let* ((allocated-colors (xlib:query-colors
                              (xlib:screen-default-colormap
                               (stumpwm:screen-number screen))
                              (list color)))
           (bright (and (plusp (length allocated-colors))
                        (elt allocated-colors 0))))
      (when bright
        (stumpwm::adjust-color bright 0.25)
        (setf (stumpwm::ccontext-default-bright ccontext)
              (stumpwm::alloc-color screen bright))))))

(defmethod (setf text-window-background-color) (color (window text-window))
  (let* ((ccontext (text-window-ccontext window))
         (gcontext (stumpwm::ccontext-gc ccontext))
         (xwin (window-xwin window)))
    (setf (xlib:gcontext-background gcontext) color
          (xlib:window-background xwin) color
          (stumpwm::ccontext-default-bg ccontext) color)))

(defmethod (setf text-window-border-color) (color (window text-window))
  (setf (xlib:window-border (window-xwin window))
        color))

(defmethod (setf text-window-border-width) (width (window text-window))
  (setf (xlib:drawable-border-width (window-xwin window))
        width))

;;; Timed window --------------------------------------------------------------
;;; The TIMED-WINDOW class implements the Timed window protocol.

(defclass timed-window (window)
  ((timer :initform nil
          :accessor %timed-window-timer)))

(defmethod timed-window-p ((window timed-window)) t)

(defmethod timed-window-schedule-timeout ((window timed-window) seconds)
  (timed-window-cancel-timeout window)
  (setf (%timed-window-timer window)
        (stumpwm:run-with-timer seconds
                                nil
                                #'window-hide
                                window)))

(defmethod timed-window-cancel-timeout ((window timed-window))
  (let ((timer (%timed-window-timer window)))
    (when (stumpwm:timer-p timer)
      (stumpwm:cancel-timer timer))))

(defmethod window-show :after ((window timed-window) frame
                               &key
                                 timeout)
  (when timeout
    (timed-window-schedule-timeout window timeout)))

(defmethod window-hide :after ((window timed-window))
  (timed-window-cancel-timeout window))
