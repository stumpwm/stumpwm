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
;; All group related code resides here
;;
;; Code:

(in-package #:stumpwm)

(export '(current-group))

(defun current-group (&optional (screen (current-screen)))
  "Return the current group for the current screen, unless
otherwise specified."
  (screen-current-group screen))

(defun move-group-to-head (screen group)
  "Move window to the head of the group's window list."
                                        ;(assert (member window (screen-mapped-windows screen)))
  (setf (screen-groups screen) (delete group (screen-groups screen)))
  (push group (screen-groups screen)))

(defun sort-groups (screen)
  "Return a copy of the screen's group list sorted by number."
  (sort1 (screen-groups screen) '< :key 'group-number))

(defun fmt-group-status (group)
  (let ((screen (group-screen group)))
    (cond ((eq group (screen-current-group screen))
           #\*)
          ((and (typep (second (screen-groups screen)) 'group)
                (eq group (second (screen-groups screen))))
           #\+)
          (t #\-))))

(defun find-free-group-number (screen)
  "Return a free group number in SCREEN."
  (find-free-number (mapcar 'group-number (screen-groups screen)) 1))

(defun find-free-hidden-group-number (screen)
  "Return a free hidden group number for SCREEN. Hidden group numbers
start at -1 and go down."
  (find-free-number (mapcar 'group-number (screen-groups screen)) -1 :negative))

(defun group-current-window (group)
  (frame-window (tile-group-current-frame group)))

(defun non-hidden-groups (groups)
  "Return only those groups that are not hidden."
  (remove-if (lambda (g)
               (< (group-number g) 1))
             groups))

(defun netwm-group-id (group)
  "netwm specifies that desktop/group numbers are contiguous and start
at 0. Return a netwm compliant group id."
  (let ((screen (group-screen group)))
    (position group (sort-groups screen))))

(defun switch-to-group (new-group)
  (let* ((screen (group-screen new-group))
         (old-group (screen-current-group screen)))
    (unless (eq new-group old-group)
      ;; restore the visible windows
      (dolist (w (group-windows new-group))
        (when (eq (window-state w) +normal-state+)
          (xwin-unhide (window-xwin w) (window-parent w))))
      (dolist (w (reverse (group-windows old-group)))
        (when (eq (window-state w) +normal-state+)
          (xwin-hide w)))
      (setf (screen-current-group screen) new-group)
      (move-group-to-head screen new-group)
      ;; restore the focus
      (setf (screen-focus screen) nil)
      (focus-frame new-group (tile-group-current-frame new-group))
      (xlib:change-property (screen-root screen) :_NET_CURRENT_DESKTOP
                            (list (netwm-group-id new-group))
                            :cardinal 32)
      (run-hook-with-args *focus-group-hook* new-group old-group)))
  (show-frame-indicator new-group))

(defun move-window-to-group (window to-group)
  (labels ((really-move-window (window to-group)
             (unless (eq (window-group window) to-group)
               (let ((old-group (window-group window))
                     (old-frame (window-frame window)))
                 (hide-window window)
                 ;; house keeping
                 (setf (group-windows (window-group window))
                       (remove window (group-windows (window-group window))))
                 (setf (window-frame window) (tile-group-current-frame to-group)
                       (window-group window) to-group
                       (window-number window) (find-free-window-number to-group))
                 ;; try to put the window in the appropriate frame for the group
                 (multiple-value-bind (placed-group frame raise) (get-window-placement (window-screen window) window)
                   (declare (ignore placed-group))
                   (when frame
                     (setf (window-frame window) frame)
                     (when raise
                       (setf (tile-group-current-frame to-group) frame
                             (frame-window frame) nil))))
                 (push window (group-windows to-group))
                 (sync-frame-windows to-group (tile-group-current-frame to-group))
                 ;; maybe pick a new window for the old frame
                 (when (eq (frame-window old-frame) window)
                   (setf (frame-window old-frame) (first (frame-windows old-group old-frame)))
                   (focus-frame old-group old-frame))
                 ;; maybe show the window in its new frame
                 (when (null (frame-window (window-frame window)))
                   (frame-raise-window (window-group window) (window-frame window) window))
                 (xlib:change-property (window-xwin window) :_NET_WM_DESKTOP
                                       (list (netwm-group-id to-group))
                                       :cardinal 32)))))
    ;; When a modal window is moved, all the windows it shadows must be moved
    ;; as well. When a shadowed window is moved, the modal shadowing it must
    ;; be moved.
    (cond
      ((window-modal-p window)
       (mapc (lambda (w)
               (really-move-window w to-group))
             (append (list window) (shadows-of window))))
      ((modals-of window)
       (mapc (lambda (w)
               (move-window-to-group w to-group))
             (modals-of window)))
      (t
       (really-move-window window to-group)))))

(defun next-group (current &optional (list (screen-groups (group-screen current))))
  ;; ditch the negative groups
  (setf list (non-hidden-groups list))
  (let* ((matches (member current list)))
    (if (null (cdr matches))
        ;; If the last one in the list is current, then
        ;; use the first one.
        (car list)
        ;; Otherwise, use the next one in the list.
        (cadr matches))))

(defun merge-groups (from-group to-group)
  "Merge all windows in FROM-GROUP into TO-GROUP."
  (dolist (window (group-windows from-group))
    (move-window-to-group window to-group)))

(defun netwm-update-groups (screen)
  "update all windows to reflect a change in the group list."
  ;; FIXME: This could be optimized only to update windows when there
  ;; is a need.
  (loop for i from 0
        for group in (sort-groups screen)
        do (dolist (w (group-windows group))
             (xlib:change-property (window-xwin w) :_NET_WM_DESKTOP
                                   (list i)
                                   :cardinal 32))))

(defun kill-group (group to-group)
  (when (> (length (screen-groups (group-screen group))) 1)
    (let ((screen (group-screen group)))
      (merge-groups group to-group)
      (setf (screen-groups screen) (remove group (screen-groups screen)))
      (netwm-update-groups screen))))

(defun netwm-set-group-properties (screen)
  "Set NETWM properties regarding groups of SCREEN.
Groups are known as \"virtual desktops\" in the NETWM standard."
  (let ((root (screen-root screen)))
    ;; _NET_NUMBER_OF_DESKTOPS
    (xlib:change-property root :_NET_NUMBER_OF_DESKTOPS
                          (list (length (screen-groups screen)))
                          :cardinal 32)
    (unless *initializing*
      ;; _NET_CURRENT_DESKTOP
      (xlib:change-property root :_NET_CURRENT_DESKTOP
                            (list (netwm-group-id (screen-current-group screen)))
                            :cardinal 32))
    ;; _NET_DESKTOP_NAMES
    (xlib:change-property root :_NET_DESKTOP_NAMES
                          (let ((names (mapcan
                                        (lambda (group)
                                          (list (string-to-utf8 (group-name group))
                                                '(0)))
                                        (sort-groups screen))))
                            (apply #'concatenate 'list names))
                          :UTF8_STRING 8)))

(defun add-group (screen name)
  "Create a new group in SCREEN with the supplied name. group names
    starting with a . are considered hidden groups. Hidden groups are
    skipped by gprev and gnext and do not show up in the group
    listings (unless *list-hidden-groups* is T). They also use negative
    numbers."
  (check-type screen screen)
  (check-type name string)
  (unless (or (string= name "")
              (string= name "."))
    (or (find-group screen name)
        (let* ((heads (copy-heads screen))
               (ng (make-tile-group
                    :frame-tree heads
                    :current-frame (first heads)
                    :screen screen
                    :number (if (char= (char name 0) #\.)
                                (find-free-hidden-group-number screen)
                                (find-free-group-number screen))
                    :name name)))
          (setf (screen-groups screen) (append (screen-groups screen) (list ng)))
          (netwm-set-group-properties screen)
          (netwm-update-groups screen)
          ng))))

(defun find-group (screen name)
  "Return the group with the name, NAME. Or NIL if none exists."
  (find name (screen-groups screen) :key 'group-name :test 'string=))
