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
  (cond ((xlib:query-extension *display* "RANDR")
         (make-screen-randr-heads root))
        ((and (xlib:query-extension *display* "XINERAMA")
              (xinerama:xinerama-is-active *display*))
         (mapcar 'screen-info-head
                 (xinerama:xinerama-query-screens *display*)))
        (t (list (make-head :number 0 :x 0 :y 0
                            :width (xlib:drawable-width root)
                            :height (xlib:drawable-height root)
                            :window nil)))))

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

;; Determining a frame's head based on position probably won't
;; work with overlapping heads. Would it be better to walk
;; up the frame tree?
(defun frame-head (group frame)
  (let ((center-x (+ (frame-x frame) (ash (frame-width frame) -1)))
        (center-y (+ (frame-y frame) (ash (frame-height frame) -1))))
    (find-head-by-position (group-screen group) center-x center-y)))

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
     (eq head (window-head w)))
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
  (let ((oheads (screen-heads screen)))
    (when (< (length heads) (length oheads))
      ;; Some heads were removed (or cloned), try to guess which.
      (dolist (oh oheads)
        (dolist (nh heads)
          (when (= (head-number oh) (head-number nh))
            ;; Same frame number, probably the same head
            (setf (head-number nh) (head-number oh))))))
    (dolist (h (set-difference oheads heads :test '= :key 'head-number))
      (remove-head screen h))
    (dolist (h (set-difference heads oheads :test '= :key 'head-number))
      (add-head screen h))
    (dolist (h (intersection heads oheads :test '= :key 'head-number))
      (let ((nh (find (head-number h) heads  :test '= :key 'head-number))
            (oh (find (head-number h) oheads :test '= :key 'head-number)))
        (scale-head screen oh nh)))))

(defun head-force-refresh (screen new-heads)
  (scale-screen screen new-heads)
  (mapc 'group-sync-all-heads (screen-groups screen))
  (loop for new-head in new-heads
     do (run-hook-with-args *new-head-hook* new-head screen)))

(defcommand refresh-heads (&optional (screen (current-screen))) ()
  "Refresh screens in case a monitor was connected, but a
  ConfigureNotify event was snarfed by another program."
  (head-force-refresh screen (make-screen-heads screen (screen-root screen))))
