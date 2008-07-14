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
      (show-frame-indicator new-group) ; doesn't get called by focus-frame
      (xlib:change-property (screen-root screen) :_NET_CURRENT_DESKTOP
                            (list (netwm-group-id new-group))
                            :cardinal 32)
      (update-all-mode-lines)
      (run-hook-with-args *focus-group-hook* new-group old-group))))

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
  (unless (eq group to-group)
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

;;; Group commands

;; FIXME: groups are to screens exactly as windows are to
;; groups. There is a lot of duplicate code that could be globbed
;; together.

(defun group-forward (current list)
  (let ((ng (next-group current list)))
    (when ng
      (switch-to-group ng))))

(defcommand gnew (name) ((:string "Group Name: "))
"Create a new group with the specified name. The new group becomes the
current group. If @var{name} begins with a dot (``.'') the group new
group will be created in the hidden state. Hidden groups have group
numbers less than one and are invisible to from gprev, gnext, and, optionally,
groups and vgroups commands."
  (let ((group (add-group (current-screen) name)))
    (if group
        (switch-to-group group)
        (message "^B^3*Groups must have a name!"))))

(defcommand gnewbg (name) ((:string "Group Name: "))
"Create a new group but do not switch to it."
  (unless (find-group (current-screen) name)
    (add-group (current-screen) name)))

(defcommand gnext () ()
"Cycle to the next group in the group list."
  (group-forward (current-group)
                 (sort-groups (current-screen))))

(defcommand gprev () ()
"Cycle to the previous group in the group list."
  (group-forward (current-group)
                 (reverse (sort-groups (current-screen)))))

(defcommand gother () ()
  "Go back to the last group."
  (let ((groups (screen-groups (current-screen))))
    (when (> (length groups) 1)
      (switch-to-group (second groups)))))

(defcommand grename (name) ((:string "New name for group: "))
  "Rename the current group."
  (let ((group (current-group)))
    (cond ((find-group (current-screen) name)
           (message "^1*^BError: Name already exists"))
          ((or (zerop (length name))
               (string= name "."))
           (message "^1*^BError: empty name"))
          (t
           (cond ((and (char= (char name 0) #\.) ;change to hidden group
                       (not (char= (char (group-name group) 0) #\.)))
                  (setf (group-number group) (find-free-hidden-group-number (current-screen))))
                 ((and (not (char= (char name 0) #\.)) ;change from hidden group
                       (char= (char (group-name group) 0) #\.))
                  (setf (group-number group) (find-free-group-number (current-screen)))))
           (setf (group-name group) name)))))

(defun echo-groups (screen fmt &optional verbose (wfmt *window-format*))
  "Print a list of the windows to the screen."
  (let* ((groups (sort-groups screen))
         (names (mapcan (lambda (g)
                          (list*
                           (format-expand *group-formatters* fmt g)
                           (when verbose
                             (mapcar (lambda (w)
                                       (format-expand *window-formatters*
                                                      (concatenate 'string "  " wfmt)
                                                      w))
                                     (sort-windows g)))))
                        (if *list-hidden-groups* groups (non-hidden-groups groups)))))
    (echo-string-list screen names)))

(defcommand groups (&optional (fmt *group-format*)) (:rest)
"Display the list of groups with their number and
name. @var{*group-format*} controls the formatting. The optional
argument @var{fmt} can be used to override the default group
formatting."
  (echo-groups (current-screen) fmt))

(defcommand vgroups (&optional gfmt wfmt) (:string :rest)
"Like @command{groups} but also display the windows in each group. The
optional arguments @var{gfmt} and @var{wfmt} can be used to override
the default group formatting and window formatting, respectively."
  (echo-groups (current-screen)
               (or gfmt *group-format*)
               t (or wfmt *window-format*)))

(defcommand gselect (to-group) ((:group "Select Group: "))
"Select the first group that starts with
@var{substring}. @var{substring} can also be a number, in which case
@command{gselect} selects the group with that number."
  (when to-group
    (switch-to-group to-group)))

(defcommand grouplist (&optional (fmt *group-format*)) (:rest)
  "Allow the user to select a group from a list, like windowlist but
  for groups"
  (let ((group (second (select-from-menu
		(current-screen)
		(mapcar (lambda (g)
			  (list (format-expand *group-formatters* fmt g) g))
			(screen-groups (current-screen)))))))
    (when group
      (switch-to-group group))))

(defcommand gmove (to-group) ((:group "To Group: "))
"Move the current window to the specified group."
  (when (and to-group
             (current-window))
    (move-window-to-group (current-window) to-group)))

(defcommand gmove-marked (to-group) ((:group "To Group: "))
  (when to-group
    (let ((group (current-group)))
      (dolist (i (marked-windows group))
        (setf (window-marked i) nil)
        (move-window-to-group i to-group)))))

(defcommand gkill () ()
"Kill the current group. All windows in the current group are migrated
to the next group."
  (let ((dead-group (current-group))
	(to-group (next-group (current-group))))
    (if (eq dead-group to-group)
    (message "There's only one visible group")
    (if (or (not %interactivep%)
	    (not (group-windows dead-group))
	    (yes-or-no-p
	     (format nil "You are about to kill non-empty group \"^B^3*~a^n\"
The windows will be moved to group \"^B^2*~a^n\"
^B^6*Confirm ?^n" (group-name dead-group) (group-name to-group))))
	(progn
	  (switch-to-group to-group)
	  (kill-group dead-group to-group)
	  (message "Deleted"))
	(message "Canceled")))))

(defcommand gmerge (from) ((:group "From Group: "))
"Merge @var{from} into the current group. @var{from} is not deleted."
  (if (eq from (current-group))
      (message "^B^3*Cannot merge group with itself!")
      (merge-groups from (current-group))))
