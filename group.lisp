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
;; All group related code resides here
;;
;; Code:

(in-package #:stumpwm)

(export '(current-group group-windows move-window-to-group add-group
          ;; Group accessors
          group group-screen group-windows group-number group-name
          ;; Group API
          group-startup group-add-window group-delete-window group-wake-up
          group-suspend group-current-window group-current-head
          group-resize-request group-move-request group-raise-request
          group-lost-focus group-indicate-focus group-focus-window
          group-button-press group-root-exposure group-add-head
          group-remove-head group-resize-head group-sync-all-heads
          group-sync-head))

(defvar *default-group-type* 'tile-group
  "The type of group that should be created by default.")

(defvar *always-show-windows* ()
  "The list of windows shown in all groups")

;;; The group API
(defgeneric group-startup (group)
  (:documentation "Called on all groups while stumpwm is starting up."))
(defgeneric group-add-window (group window &key &allow-other-keys)
  (:documentation "Called when a window is added to the group. All
house keeping is already taken care of. Only the group's specific
window managing housekeeping need be done.  This function accepts keys
to inform the group on how to place the window."))
(defgeneric group-delete-window (group window)
  (:documentation "Called when a window is removed from thegroup. All
house keeping is already taken care of. Only the group's specific
window managing housekeeping need be done."))
(defgeneric group-wake-up (group)
  (:documentation "When the group becomes the current group, this
function is called. This call is expected to set the focus."))
(defgeneric group-suspend (group)
  (:documentation "When the group is no longer the current group, this
function is called."))
(defgeneric group-current-window (group)
  (:documentation "The group is asked to return its focused window."))
(defgeneric group-raised-window (group)
  (:documentation "The group is asked to return its topmost window."))
(defgeneric group-current-head (group)
  (:documentation "The group is asked to return its current head."))
(defgeneric group-resize-request (group window width height)
  (:documentation "The window requested a width and/or height change."))
(defgeneric group-move-request (group window x y relative-to)
  (:documentation "The window requested a position change."))
(defgeneric group-raise-request (group window type)
  (:documentation "A request has been made to raise the window. TYPE
is the type of raise request being made. :MAP means the window has
made requested to be mapped. :above means the window has requested to
to be placed above its siblings."))
(defgeneric group-lost-focus (group)
  (:documentation "The current window was hidden or destroyed or
something happened to it. So the group is asked to do something smart
about it."))
(defgeneric group-indicate-focus (group)
  (:documentation "The group is asked to in some way show the user where the keyboard focus is."))
(defgeneric group-focus-window (group win)
  (:documentation "The group is asked to focus the specified window wherever it is."))
(defgeneric group-button-press (group button x y child)
  (:documentation "The user clicked somewhere in the group."))
(defgeneric group-root-exposure (group)
  (:documentation "The root window got an exposure event. If the group
needs to redraw anything on it, this is where it should do it."))
(defgeneric group-add-head (group head)
  (:documentation "A head is being added to this group's screen."))
(defgeneric group-remove-head (group head)
  (:documentation "A head is being removed from this group's screen."))
(defgeneric group-resize-head (group oh nh)
  (:documentation "A head is being resized on this group's screen."))
(defgeneric group-sync-all-heads (group)
  (:documentation "Called when the head configuration for the group changes."))
(defgeneric group-sync-head (group head)
  (:documentation "When a head or its usable area is resized, this is
called. When the modeline size changes, this is called."))

(defclass group ()
  ((screen :initarg :screen :accessor group-screen)
   (windows :initform nil :accessor group-windows)
   (current-window :initform nil :accessor group-current-window)
   (raised-window :initform nil :accessor group-raised-window)
   (number :initarg :number :accessor group-number)
   (name :initarg :name :accessor group-name)
   (on-top-windows :initform nil :accessor group-on-top-windows)))

(defmethod group-delete-window (group window)
  (when (find window *always-show-windows*)
    (disable-always-show-window window (current-screen)))
  (call-next-method))

(defun current-group (&optional (screen (current-screen)))
  "Return the current group for the current screen, unless
otherwise specified."
  (screen-current-group screen))

(defun move-group-to-head (screen group)
  "Move window to the head of the group's window list."
                                        ;(assert (member window (screen-mapped-windows screen)))
  (move-to-head (screen-groups screen) group))

(defun sort-groups (screen)
  "Return a copy of the screen's group list sorted by number."
  (sort1 (screen-groups screen) '< :key 'group-number))

(defun group-map-number (group)
  (let* ((num (group-number group))
         (index (1- (abs num))))
    (if (and (>= index 0)
             (< index (length *group-number-map*)))
        (format nil "~:[~;-~]~a" (minusp num) (elt *group-number-map* index))
        (princ-to-string num))))

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
      (group-wake-up new-group)
      (xlib:change-property (screen-root screen) :_NET_CURRENT_DESKTOP
                            (list (netwm-group-id new-group))
                            :cardinal 32)
      (mapc (lambda (w)
              (xwin-unhide (window-xwin w) (window-parent w)))
            *always-show-windows*)
      (update-all-mode-lines)
      (run-hook-with-args *focus-group-hook* new-group old-group))))

(defun copy-window-to-group (group window)
  (setf (window-number window) (find-free-window-number group))
  (push window (group-windows group))
  (group-add-window group window))

(defun always-show-window (window screen)
  (let ((groups-to-add-w-to (remove (current-group) (screen-groups screen))))
    (mapc (lambda (group) (copy-window-to-group group window))
          groups-to-add-w-to))
  (xlib:change-property (window-xwin window) :_NET_WM_DESKTOP
                        (list #xFFFFFFFF)
                        :cardinal 32)
  (push window *always-show-windows*))

(defun disable-always-show-window (window screen)
  (let* ((g (current-group))
         (groups-to-remove-w-from (remove g (screen-groups screen))))
    (mapc (lambda (group)
            (setf (group-windows group)
                  (remove window (group-windows group))))
          groups-to-remove-w-from)
    (setf (window-group window) g
          (window-number window) (find-free-window-number g)
          *always-show-windows* (remove window *always-show-windows*))
    (xlib:change-property (window-xwin window) :_NET_WM_DESKTOP
                          (list (netwm-group-id g))
                          :cardinal 32)))

(defcommand toggle-always-show () ()
  "Toggle whether the current window is shown in all groups."
  (let ((w (current-window))
        (screen (current-screen)))
    (when w
      (if (find w *always-show-windows*)
          (disable-always-show-window w screen)
          (always-show-window w screen)))))

(defun move-window-to-group (window to-group)
  (labels ((really-move-window (window to-group)
             (unless (eq (window-group window) to-group)
               (hide-window window)
               ;; house keeping
               (setf (group-windows (window-group window))
                     (remove window (group-windows (window-group window))))
               (group-delete-window (window-group window) window)
               (setf (window-group window) to-group
                     (window-number window) (find-free-window-number to-group))
               (push window (group-windows to-group))
               (xlib:change-property (window-xwin window) :_NET_WM_DESKTOP
                                     (list (netwm-group-id to-group))
                                     :cardinal 32)
               (group-add-window to-group window))))
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

(defun next-group (current &optional
                   (groups (non-hidden-groups (screen-groups
                                               (group-screen current)))))
  "Return the group following @var{current} in @var{groups}. If none
are found return @code{NIL}."
  (let* ((matches (member current groups))
         (next-group (if (null (cdr matches))
                         ;; If the last one in the list is current, then
                         ;; use the first one.
                         (car groups)
                         ;; Otherwise, use the next one in the list.
                         (cadr matches))))
    (unless (eq next-group current)
      next-group)))

(defun merge-groups (from-group to-group)
  "Merge all windows in FROM-GROUP into TO-GROUP."
  (dolist (window (group-windows from-group))
    (move-window-to-group window to-group)))

(defun netwm-group (window &optional (screen (window-screen window)))
  "Get the window's desktop property and return a matching group, if
there exists one."
  (let ((id (first (xlib:get-property (window-xwin window) :_NET_WM_DESKTOP))))
    (when (and id (< id (length (screen-groups screen))))
      (elt (sort-groups screen) id))))

(defun netwm-set-group (window)
  "Set the desktop property for the given window."
  (xlib:change-property (window-xwin window) :_NET_WM_DESKTOP
                        (list (netwm-group-id (window-group window)))
                        :cardinal 32))

(defun netwm-set-allowed-actions (window)
  (xlib:change-property (window-xwin window) :_NET_WM_ALLOWED_ACTIONS
                        (mapcar (lambda (a)
                                  (xlib:intern-atom *display* a))
                                +netwm-allowed-actions+)
                        :atom 32))

(defun netwm-update-group (group index)
  (dolist (w (group-windows group))
    (xlib:change-property (window-xwin w) :_NET_WM_DESKTOP
                          (list index)
                          :cardinal 32)))

(defun netwm-update-groups (screen)
  "Update all windows to reflect a change in the group list."
  ;; FIXME: This could be optimized only to update windows when there
  ;; is a need.
  (loop for i from 0
        for group in (sort-groups screen)
        do (netwm-update-group group i)))

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
                                          (list (sb-ext:string-to-octets
                                                 (group-name group)
                                                 :external-format :utf-8)
                                                '(0)))
                                        (sort-groups screen))))
                            (apply #'concatenate 'list names))
                          :UTF8_STRING 8)))

(defun kill-group (group to-group)
  (unless (eq group to-group)
    (let ((screen (group-screen group)))
      (merge-groups group to-group)
      (setf (screen-groups screen) (remove group (screen-groups screen)))
      (netwm-update-groups screen)
      (netwm-set-group-properties screen))))

(defun %ensure-group (group-name group-type screen)
  "If there is a group named with GROUP-NAME in SCREEN return it, otherwise create it."
  (or (find-group screen group-name)
      (let ((group (make-instance group-type
                                  :screen screen
                                  :number (if (char= (char group-name 0) #\.)
                                                     (find-free-hidden-group-number screen)
                                                     (find-free-group-number screen))
                                  :name group-name)))
        (setf (screen-groups screen) (append (screen-groups screen) (list group)))
        (netwm-set-group-properties screen)
        (netwm-update-groups screen)
        group)))

(defun add-group (screen name &key background (type *default-group-type*))
  "Create a new group in SCREEN with the supplied name. group names
    starting with a . are considered hidden groups. Hidden groups are
    skipped by gprev and gnext and do not show up in the group
    listings (unless *list-hidden-groups* is T). They also use negative
    numbers."
  (check-type screen screen)
  (check-type name string)
  (assert (not (member name '("" ".") :test #'string=)) (name) "Groups must have a name.")
  (let ((group (%ensure-group name type screen)))
    (unless background
      (switch-to-group group))
    (mapc (lambda (window) (copy-window-to-group group window))
          *always-show-windows*)
    group))

(defun find-group (screen name)
  "Return the group with the name, NAME. Or NIL if none exists."
  (find name (screen-groups screen) :key 'group-name :test 'string=))

;;; Group commands

;; FIXME: groups are to screens exactly as windows are to
;; groups. There is a lot of duplicate code that could be globbed
;; together.

(defun group-forward (current list)
  "Switch to the next non-hidden-group in the list, if one
exists. Returns the new group."
  (when-let ((next (next-group current (non-hidden-groups list))))
    (switch-to-group next)
    next))

(defun group-forward-with-window (current list)
  "Switch to the next group in the list, if one exists, and moves the
current window of the current group to the new one."
  (when-let ((next (group-forward current list))
             (win (group-current-window current)))
    (move-window-to-group win next)
    (really-raise-window win)))

(defcommand gnew (name) ((:string "Group Name: "))
  "Create a new group with the specified name. The new group becomes the
current group. If @var{name} begins with a dot (``.'') the group new
group will be created in the hidden state. Hidden groups have group
numbers less than one and are invisible to from gprev, gnext, and, optionally,
groups and vgroups commands."
  (unless name 
    (throw 'error :abort))
  (add-group (current-screen) name))

(defcommand gnewbg (name) ((:string "Group Name: "))
  "Create a new group but do not switch to it."
  (unless name
    (throw 'error :abort))
  (add-group (current-screen) name :background t))

(defcommand gnext () ()
"Cycle to the next group in the group list."
  (group-forward (current-group)
                 (sort-groups (current-screen))))

(defcommand gprev () ()
"Cycle to the previous group in the group list."
  (group-forward (current-group)
                 (reverse (sort-groups (current-screen)))))

(defcommand gnext-with-window () ()
  "Cycle to the next group in the group list, taking the current
window along."
  (group-forward-with-window (current-group)
                             (sort-groups (current-screen))))

(defcommand gprev-with-window () ()
  "Cycle to the previous group in the group list, taking the current
window along."
  (group-forward-with-window (current-group)
                             (reverse (sort-groups (current-screen)))))

(defcommand gother () ()
  "Go back to the last group."
  (let ((groups (screen-groups (current-screen))))
    (when (> (length groups) 1)
      (switch-to-group (second groups)))))

(defun %grename (name group)
  (let ((group-name (group-name group)))
    (cond ((and (starts-with #\. name) ; change to hidden group
                (not (starts-with #\. group-name)))
           (setf (group-number group)
                 (find-free-hidden-group-number (current-screen))))
          ((and (not (starts-with #\. name)) ; change from hidden group
                (starts-with #\. group-name))
           (setf (group-number group)
                 (find-free-group-number (current-screen))))))
       (setf (group-name group) name))

(defcommand grename (name) ((:string "New name for group: "))
  "Rename the current group."
  (cond ((find-group (current-screen) name)
         (message "^1*^BError: Name already exists"))
        ((or (zerop (length name))
             (string= name "."))
         (message "^1*^BError: Name cannot be empty name"))
        (t (%grename name (current-group)))))

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
  (when-let ((group (second (select-from-menu
                             (current-screen)
                             (mapcar (lambda (g)
                                       (list (format-expand *group-formatters* fmt g) g))
                                     (screen-groups (current-screen)))))))
    (switch-to-group group)))

(defcommand gmove (to-group) ((:group "To Group: "))
"Move the current window to the specified group."
  (when (and to-group
             (current-window))
    (move-window-to-group (current-window) to-group)))

(defcommand gmove-and-follow (to-group) ((:group "To Group: "))
  "Move the current window to the specified group, and switch to it."
  (let ((window (current-window)))
    (gmove to-group)
    (gselect to-group)
    (when window (really-raise-window window))))

(defcommand gmove-marked (to-group) ((:group "To Group: "))
  "move the marked windows to the specified group."
  (when to-group
    (let ((group (current-group)))
      (dolist (i (marked-windows group))
        (setf (window-marked i) nil)
        (move-window-to-group i to-group)))))

(defcommand gkill () ()
"Kill the current group. All windows in the current group are migrated
to the next group."
  (let* ((dead-group (current-group))
         (groups (screen-groups (current-screen)))
         ;; If no "visible" group is found, try with all groups
         (to-group (or (next-group dead-group (non-hidden-groups groups))
                       (next-group dead-group groups))))
    (if to-group
        (if (or (not %interactivep%)
            (not (group-windows dead-group))
            (y-or-n-p
             (format nil "You are about to kill non-empty group \"^B^3*~a^n\"
The windows will be moved to group \"^B^2*~a^n\"
^B^6*Confirm?^n " (group-name dead-group) (group-name to-group))))
            (progn
              (switch-to-group to-group)
              (kill-group dead-group to-group)
              (message "Deleted"))
            (message "Canceled"))
        (message "There's only one group left"))))

(defcommand gkill-other () ()
"Kill other groups. All windows in other groups are migrated
to the current group."
  (let* ((current-group (current-group))
         (groups (remove current-group
                         (screen-groups (current-screen)))))
    (dolist (dead-group groups)
      (kill-group dead-group current-group))))

(defcommand gmerge (from) ((:group "From Group: "))
"Merge @var{from} into the current group. @var{from} is not deleted."
  (if (eq from (current-group))
      (message "^B^3*Cannot merge group with itself!")
      (merge-groups from (current-group))))
