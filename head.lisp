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
;; Head functionality
;;
;; Code:

(in-package #:stumpwm)

(export '(current-head))

(defun head-by-number (screen n)
  (find n (screen-heads screen) :key 'head-number))

(defun parse-xinerama-head (line)
  (ppcre:register-groups-bind (('parse-integer number width height x y))
                              ("^ +head #([0-9]+): ([0-9]+)x([0-9]+) @ ([0-9]+),([0-9]+)" line :sharedp t)
                              (handler-case
                                  (make-head :number number
                                             :x x :y y
                                             :width width
                                             :height height)
                                (parse-error ()
                                  nil))))

(defun make-screen-heads (screen root)
  "or use xdpyinfo to query the xinerama extension, if it's enabled."
  (or (and (xlib:query-extension *display* "XINERAMA")
           (with-current-screen screen
             ;; Ignore 'clone' heads.
             (loop
                for i = 0 then (1+ i)
                for h in
                (delete-duplicates
                 (loop for i in (split-string (run-shell-command "xdpyinfo -ext XINERAMA" t))
                    for head = (parse-xinerama-head i)
                    when head
                    collect head)
                 :test #'frames-overlap-p)
                do (setf (head-number h) i)
                collect h)))
      (list (make-head :number 0
                       :x 0 :y 0
                       :width (xlib:drawable-width root)
                       :height (xlib:drawable-height root)
                       :window nil))))

(defun copy-heads (screen)
  "Return a copy of screen's heads."
  (mapcar 'copy-frame (screen-heads screen)))


;; Determining a frame's head based on position probably won't
;; work with overlapping heads. Would it be better to walk
;; up the frame tree?
(defun frame-head (group frame)
  (let ((center-x (+ (frame-x frame) (ash (frame-width frame) -1)))
       (center-y (+ (frame-y frame) (ash (frame-height frame) -1))))
    (dolist (head (screen-heads (group-screen group)))
      (when (and
            (>= center-x (frame-x head))
            (>= center-y (frame-y head))
            (<= center-x
                (+ (frame-x head) (frame-width head)))
            (<= center-y
                (+ (frame-y head) (frame-height head))))
       (return head)))))

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
    (mapc 'group-add-head (screen-groups screen))
    (update-mode-lines screen)))

(defun tile-group-frame-head (group head)
  (elt (tile-group-frame-tree group) (position head (group-heads group))))

(defun (setf tile-group-frame-head) (frame group head)
  (setf (elt (tile-group-frame-tree group) (position head (group-heads group))) frame))

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
  (setf (screen-heads screen) (sort (push head (screen-heads screen)) #'< :key 'head-number))
  (dolist (group (screen-groups screen))
    (let ((new-frame-num (find-free-frame-number group)))
      (setf (tile-group-frame-tree group)
            (insert-before (tile-group-frame-tree group)
                           (copy-frame head)
                           (head-number head)))
      ;; Try to put something in the new frame and give it an unused number
      (let ((frame (tile-group-frame-head group head)))
        (setf (frame-number frame) new-frame-num)
        (choose-new-frame-window frame group)
        (when (frame-window frame)
          (unhide-window (frame-window frame)))))))

(defun remove-head (screen head)
  (dformat 1 "Removing head #~D~%" (head-number head))
  (when (head-mode-line head)
    (toggle-mode-line screen head))
  (dolist (group (screen-groups screen))
    ;; Hide its windows.
    (let ((windows (head-windows group head)))
      ;; Remove it from the frame tree.
      (setf (tile-group-frame-tree group) (delete (tile-group-frame-head group head) (tile-group-frame-tree group)))
      ;; Just set current frame to whatever.
      (let ((frame (first (group-frames group))))
        (setf (tile-group-current-frame group) frame
              (tile-group-last-frame group) nil)
        (dolist (window windows)
          (hide-window window)
          (setf (window-frame window) frame))))
    ;; Try to do something with the orphaned windows
    (populate-frames group))
  ;; Remove it from SCREEN's head list.
  (setf (screen-heads screen) (delete head (screen-heads screen))))

(defun scale-head (screen oh nh)
  "Scales head OH to match the dimensions of NH."
  (dolist (group (screen-groups screen))
    (resize-tree (tile-group-frame-head group oh) (head-width nh) (head-height nh) (head-x nh) (head-y nh)))
  (setf (head-x oh) (head-x nh)
        (head-y oh) (head-y nh)
        (head-width oh) (head-width nh)
        (head-height oh) (head-height nh)))

(defun scale-screen (screen heads)
  "Scale all frames of all groups of SCREEN to match the dimensions
  of HEADS."
  (when (< (length heads) (length (screen-heads screen)))
    ;; Some heads were removed (or cloned), try to guess which.
    (dolist (oh (screen-heads screen))
      (dolist (nh heads)
        (when (and (= (head-x nh) (head-x oh))
                   (= (head-y nh) (head-y oh)))
          ;; Same screen position; probably the same head.
          (setf (head-number nh) (head-number oh)))))
    ;; Actually remove the missing heads.
    (dolist (head (set-difference (screen-heads screen) heads :key 'head-number))
      (remove-head screen head)))
  (loop
   for nh in heads
   as oh = (find (head-number nh) (screen-heads screen) :key 'head-number)
   do (if oh
          (scale-head screen oh nh)
          (add-head screen nh))))

