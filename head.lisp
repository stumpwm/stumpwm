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
;; Head functionality
;;
;; Code:

(in-package #:stumpwm)

(export '(current-head))

(defun head-by-number (screen n)
  (find n (screen-heads screen) :key 'head-number))

(defun screen-info-head (screen-info)
  "Transform SCREEN-INFO structure from CLX to a HEAD structure from StumpWM."
  (make-head :number (xinerama:screen-info-number screen-info)
             :x (xinerama:screen-info-x screen-info)
             :y (xinerama:screen-info-y screen-info)
             :width (xinerama:screen-info-width screen-info)
             :height (xinerama:screen-info-height screen-info)
             :window nil))

(defun output->head (output count)
  (multiple-value-bind
        (request-status _0 crtc _1 _2 status _3 _4 _5 _6 _7 name)
      (xlib:rr-get-output-info *display* output (get-universal-time))
    (declare (ignore _0 _1 _2 _3 _4 _5 _6 _7))
    (when (and (eq request-status :success)
               (eq status :connected)
               (plusp crtc))
      (multiple-value-bind
            (request-status config-timestamp x y width height)
          (xlib:rr-get-crtc-info *display* crtc (get-universal-time))
        (declare (ignore config-timestamp))
        (when (eq request-status :success)
          (make-head :number count
                     :x x
                     :y y
                     :width width
                     :height height
                     :window nil
                     :name name))))))

(defun make-screen-randr-heads (root)
  (loop :with outputs := (nth-value 3 (xlib:rr-get-screen-resources root))
        :for count :from 0
        :for output :in outputs
        :for head := (output->head output count)
        :when head
          :collect head))


(defun make-screen-heads (screen root)
  (declare (ignore screen))
  ;; Query for whether the server supports RANDR, if so, call the
  ;; randr version of make-screen-heads.
  (or
   (and (xlib:query-extension *display* "RANDR") (make-screen-randr-heads root))
   (and (xlib:query-extension *display* "XINERAMA")
        (xinerama:xinerama-is-active *display*)
        (mapcar 'screen-info-head
                (xinerama:xinerama-query-screens *display*)))
   (list (make-head :number 0 :x 0 :y 0
                    :width (xlib:drawable-width root)
                    :height (xlib:drawable-height root)
                    :window nil))))

(defun copy-heads (screen)
  "Return a copy of screen's heads."
  (mapcar 'copy-frame (screen-heads screen)))

(defun find-head-by-position (screen x y)
  (dolist (head (screen-heads screen))
    (when (and (>= x (head-x head))
               (>= y (head-y head))
               (<= x (+ (head-x head) (head-width head)))
               (<= y (+ (head-y head) (head-height head))))
      (return head))))

(defgeneric frame-head (group frame)
  (:documentation "Return the head frame is on")
  (:method (group frame)
    "As a fallback, use the frame's position on the screen to return a head
 in the same position. This can be out of sync with stump's state if was
 moved by something else, such as X11 during an external monitor change"
    (let ((center-x (+ (frame-x frame) (ash (frame-width frame) -1)))
          (center-y (+ (frame-y frame) (ash (frame-height frame) -1))))
      (find-head-by-position (group-screen group) center-x center-y))))

(defun group-heads (group)
  (screen-heads (group-screen group)))

(defun resize-head (number x y width height)
  "Resize head number `number' to given dimension."
  (let* ((screen (current-screen))
         (oh (find number (screen-heads screen) :key 'head-number))
         (nh (make-head :number number
                        :x x :y y
                        :width width
                        :height height
                        :window nil)))
    (scale-head screen oh nh)
    (dolist (group (screen-groups screen)) (group-sync-head group oh))
    (update-mode-lines screen)))

(defun current-head (&optional (group (current-group)))
  (group-current-head group))

(defun head-windows (group head)
  "Returns a list of windows on HEAD of GROUP"
  (remove-if-not
   (lambda (w)
     (handler-case (eq head (window-head w))
       (unbound-slot () nil)))
   (group-windows group)))

(defun frame-is-head (group frame)
  (< (frame-number frame) (length (group-heads group))))

(defun add-head (screen head)
  (dformat 1 "Adding head #~D~%" (head-number head))
  (setf (screen-heads screen) (sort (push head (screen-heads screen)) #'<
                                    :key 'head-number))
  (dolist (group (screen-groups screen))
    (group-add-head group head)))

(defun remove-head (screen head)
  (dformat 1 "Removing head #~D~%" (head-number head))
  (let ((mode-line (head-mode-line head)))
    (when mode-line
      (destroy-mode-line mode-line)))
  (dolist (group (screen-groups screen))
    (group-remove-head group head))
  ;; Remove it from SCREEN's head list.
  (setf (screen-heads screen) (delete head (screen-heads screen))))

(defun replace-head (screen old-head new-head)
  "Replaces one head with another, while preserving its frame-tree"
  (dformat 1 "Replacing head ~A with head ~A" old-head new-head)
  (when-let (mode-line (head-mode-line old-head))
    (move-mode-line-to-head mode-line new-head))
  (dolist (group (screen-groups screen))
    (group-replace-head screen group old-head new-head))
  (setf (screen-heads screen)
        (sort (append (list new-head)
                      (remove old-head (screen-heads screen)))
              #'<
              :key 'head-number))
  (scale-head screen new-head old-head)) ; opposite of its calling convention

(defun scale-head (screen oh nh)
  "Scales head OH to match the dimensions of NH."
  (dolist (group (screen-groups screen))
    (group-resize-head group oh nh))
  (setf (head-x oh) (head-x nh)
        (head-y oh) (head-y nh)
        (head-width oh) (head-width nh)
        (head-height oh) (head-height nh)))

(defun scale-screen (screen heads)
  "Scale all frames of all groups of SCREEN to match the dimensions of HEADS."
  (let ((old-heads (screen-heads screen)))
    (let* ((added-heads (set-difference heads old-heads :test '= :key 'head-number))
           (removed-heads (set-difference old-heads heads :test '= :key 'head-number))
           (max-change (max (length added-heads) (length removed-heads))))
      (loop repeat max-change ; This is, unfortunately, the loop syntax for stopping at the max of two lists
            for added-head-list = added-heads then (cdr added-head-list)
            for added-head = (car added-head-list)
            for removed-head-list = removed-heads then (cdr removed-head-list)
            for removed-head = (car removed-head-list)
            do (if added-head
                   (if removed-head
                       (replace-head screen removed-head added-head)
                       (add-head screen added-head))
                   (remove-head screen removed-head)))
      ;; This rescales altered, but existing screens eg a screen resolution change
      (dolist (head (intersection heads old-heads :test '= :key 'head-number))
              (let ((new-head (find (head-number head) heads  :test '= :key 'head-number))
                    (old-head (find (head-number head) old-heads :test '= :key 'head-number)))
                (scale-head screen old-head new-head))))
    (when-let ((orphaned-frames (orphaned-frames screen)))
      (let ((group (current-group)))
        (dformat 1 "Orphaned frames ~A found on screen ~A! Adopting into group ~A"
                 orphaned-frames screen group)
        (group-adopt-orphaned-windows group screen)))))

(defun head-force-refresh (screen new-heads)
  (scale-screen screen new-heads)
  (mapc 'group-sync-all-heads (screen-groups screen))
  (loop for new-head in new-heads
     do (run-hook-with-args *new-head-hook* new-head screen)))

(defcommand refresh-heads (&optional (screen (current-screen))) ()
  "Refresh screens in case a monitor was connected, but a
  ConfigureNotify event was snarfed by another program."
  (head-force-refresh screen (make-screen-heads screen (screen-root screen))))

(defun orphaned-frames (screen)
  "Returns a list of frames on a screen not associated with any group.
  These shouldn't exist."
  (let ((adopted-frames (loop for group in (screen-groups screen)
                              append (group-frames group))))
    (set-difference (screen-frames screen) adopted-frames)))
