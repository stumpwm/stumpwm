;; Copyright (C) 2003 Shawn Betts
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
;; This file contains core functionality including functions on
;; windows, screens, and events.
;;
;; Code:

(in-package :stumpwm)

(export '(*top-map*
          current-group
          current-screen
          current-srceen
          current-window
          def-window-attr
          echo-string
          err
          get-x-selection
          message
          save-frame-excursion
          screen-current-window
          set-normal-gravity
          set-maxsize-gravity
          set-transient-gravity
          set-window-geometry
          set-fg-color
          set-bg-color
          set-border-color
          set-win-bg-color
          set-focus-color
          set-unfocus-color
          set-msg-border-width
          set-frame-outline-width
          set-font
          set-x-selection
          window-send-string))

;; Do it this way so its easier to wipe the map and get a clean one.
(when (null *top-map*)
  (setf *top-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "C-t") '*root-map*)
          m)))

;; Wow, is there an easier way to do this?
(defmacro def-thing-attr-macro (thing hash-slot)
  (let ((attr (gensym "ATTR"))
        (obj (gensym "METAOBJ"))
        (val (gensym "METAVAL")))
    `(defmacro ,(intern (format nil "DEF-~a-ATTR" thing)) (,attr)
      "Create a new attribute and corresponding get/set functions."
      (let ((,obj (gensym "OBJ"))
            (,val (gensym "VAL")))
        `(progn
          (defun ,(intern (format nil ,(format nil "~a-~~a" thing) ,attr)) (,,obj)
            (gethash ,,attr (,(quote ,hash-slot) ,,obj)))
          (defun (setf ,(intern (format nil ,(format nil "~a-~~a" thing) ,attr))) (,,val ,,obj)
            (setf (gethash ,,attr (,(quote ,hash-slot) ,,obj))) ,,val))))))

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


;;; Group function

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


;;; Window functions


;; Since StumpWM already uses the term 'group' to refer to Virtual Desktops,
;; we'll call the grouped windows of an application a 'gang'

;; maybe follow transient_for to find leader.
(defun window-leader (window)
  (when window
    (or (first (window-property window :WM_CLIENT_LEADER))
        (let ((id (window-transient-for window)))
          (when id
            (window-leader (window-by-id id)))))))

;; A modal dialog can either shadow a single window, or all windows
;; in its gang, depending on the value of WM_TRANSIENT_FOR

;; If a window is shadowed by a modal dialog, so are any other
;; transients belonging to that window.

(defun window-transient-for (window)
  (first (window-property window :WM_TRANSIENT_FOR)))

(defun window-modal-p (window)
  (find-wm-state (window-xwin window) :_NET_WM_STATE_MODAL))

(defun window-transient-p (window)
  (find (window-type window) '(:transient :dialog)))

;; FIXME: use WM_HINTS.group_leader
(defun window-gang (window)
  "Return a list of other windows in WINDOW's gang."
  (let ((leader (window-leader window))
        (screen (window-screen window)))
    (when leader
      (loop for w in (screen-windows screen)
            as l = (window-leader w)
            if (and (not (eq w window)) l (= leader l))
            collect w))))

(defun only-modals (windows)
  "Out of WINDOWS, return a list of those which are modal."
  (remove-if-not 'window-modal-p (copy-list windows)))

(defun x-of (window filter)
  (let* ((root (screen-root (window-screen window)))
         (root-id (xlib:drawable-id root))
         (win-id (xlib:window-id (window-xwin window))))
    (loop for w in (funcall filter (window-gang window))
          as tr = (window-transient-for w)
          when (or (not tr)             ; modal for group
                   (eq tr root-id)      ; ditto
                   (eq tr win-id))      ; modal for win
          collect w)))


;; The modals of a transient are the modals of the window
;; the transient belongs to.
(defun modals-of (window)
  "Given WINDOW return the modal dialogs which are shadowing it, if any."
  (loop for m in (only-modals (window-gang window))
        when (find window (shadows-of m))
        collect m))

(defun transients-of (window)
  "Return the transient dialogs belonging to WINDOW"
  (x-of window 'only-transients))

(defun shadows-of (window)
  "Given modal window WINDOW return the list of windows in its shadow."
  (let* ((root (screen-root (window-screen window)))
         (root-id (xlib:drawable-id root))
         (tr (window-transient-for window)))
    (cond
      ((or (not tr)
           (eq tr root-id))
       (window-gang window))
      (t
       (let ((w (window-by-id tr)))
         (append (list w) (transients-of w)))))))

(defun only-transients (windows)
  "Out of WINDOWS, return a list of those which are transient."
  (remove-if-not 'window-transient-p (copy-list windows)))

(defun really-raise-window (window)
  (frame-raise-window (window-group window) (window-frame window) window))

(defun raise-modals-of (window)
  (mapc 'really-raise-window (modals-of window)))

(defun raise-modals-of-gang (window)
  (mapc 'really-raise-window (only-modals (window-gang window))))

(defun raise-transients-of-gang (window)
  (mapc 'really-raise-window (only-transients (window-gang window))))

(defun all-windows ()
  (mapcan (lambda (s) (copy-list (screen-windows s))) *screen-list*))

(defun visible-windows ()
  "Return a list of visible windows (on all screens)"
  (loop for s in *screen-list*
        nconc (delete-if 'window-hidden-p (copy-list (group-windows (screen-current-group s))))))

(defun top-windows ()
  "Return a list of windows on top (on all screen)"
  (loop for s in *screen-list*
        nconc (mapcar 'frame-window (group-frames (screen-current-group s)))))

(defun window-name (window)
  (or (window-user-title window)
      (case *window-name-source*
        (:resource-name (window-res window))
        (:class (window-class window))
        (t (window-title window)))))

(defun window-id (window)
  (xlib:window-id (window-xwin window)))

(defun window-in-current-group-p (window)
  (eq (window-group window)
      (screen-current-group (window-screen window))))

(defun window-screen (window)
  (group-screen (window-group window)))

(defun update-window-border (window)
  ;; give it a colored border but only if there are more than 1 frames.
  (let* ((group (window-group window))
         (screen (group-screen group)))
    (let ((c (if (and (> (length (group-frames group)) 1)
                      (eq (group-current-window group) window))
                 (screen-focus-color screen)
                 (screen-unfocus-color screen))))
      (setf (xlib:window-border (window-parent window)) c
            ;; windows that dont fill the entire screen have a transparent background.
            (xlib:window-background (window-parent window))
            (if (eq (window-type window) :normal)
                c :none))
      ;; get the background updated
      (xlib:clear-area (window-parent window)))))

(defun send-client-message (window type &rest data)
  "Send a client message to a client's window."
  (xlib:send-event (window-xwin window)
                   :client-message nil
                   :window (window-xwin window)
                   :type type
                   :format 32
                   :data data))

(defun fmt-window-status (window)
  (let ((group (window-group window)))
    (cond ((eq window (group-current-window group))
           #\*)
          ((and (typep (second (group-windows group)) 'window)
                (eq window (second (group-windows group))))
           #\+)
          (t #\-))))

(defun fmt-window-marked (window)
  (if (window-marked window)
      #\#
      #\Space))

;; (defun update-window-mark (window)
;;   "Called when we need to draw or clear the mark."
;;   ;; FIXME: This doesn't work at all. I'd like to have little squares
;;   ;; that look like clamps on the corners of the window, likes its
;;   ;; sorta grabbed. But i dunno how to properly draw them.
;;   (let* ((screen (window-screen window)))
;;     (if (window-marked window)
;;      (xlib:draw-rectangle (window-parent window) (screen-marked-gc (window-screen window))
;;                           0 0 300 200 t)
;;      (xlib:clear-area (window-parent window)))))

(defun xwin-net-wm-name (win)
  "Return the netwm wm name"
  (let ((name (xlib:get-property win :_NET_WM_NAME)))
    (when name
      (utf8-to-string name))))

(defun xwin-name (win)
  (or
   (xwin-net-wm-name win)
   (xlib:wm-name win)))

;; FIXME: should we raise the winodw or its parent?
(defun raise-window (win)
  "Map the window if needed and bring it to the top of the stack. Does not affect focus."
  (when (window-hidden-p win)
    (unhide-window win)
    (update-configuration win))
  (when (window-in-current-group-p win)
    (setf (xlib:window-priority (window-parent win)) :top-if)))

;; some handy wrappers

(defun true-height (win)
  (xlib:with-state (win)
    (+ (xlib:drawable-height win) (* (xlib:drawable-border-width win) 2))))

(defun true-width (win)
  (xlib:with-state (win)
    (+ (xlib:drawable-width win) (* (xlib:drawable-border-width win) 2))))

(defun xwin-border-width (win)
  (xlib:drawable-border-width win))

(defun (setf xwin-border-width) (width win)
  (setf (xlib:drawable-border-width win) width))

(defun default-border-width-for-type (type)
  (ecase type
    (:dock 0)
    (:normal *normal-border-width*)
    (:maxsize *maxsize-border-width*)
    ((:transient :dialog) *transient-border-width*)))

(defun xwin-class (win)
  (multiple-value-bind (res class) (xlib:get-wm-class win)
    (declare (ignore res))
    class))

(defun xwin-res-name (win)
  (multiple-value-bind (res class) (xlib:get-wm-class win)
    (declare (ignore class))
    res))

(defun xwin-role (win)
  "Return WM_WINDOW_ROLE"
  (let ((name (xlib:get-property win :WM_WINDOW_ROLE)))
    (dformat 10 "role: ~a~%" name)
    (if name
        (utf8-to-string name)
        "")))

(defmacro def-window-attr (attr)
  "Create a new window attribute and corresponding get/set functions."
  (let ((win (gensym))
        (val (gensym)))
    `(progn
      (defun ,(intern (format nil "WINDOW-~a" attr)) (,win)
        (gethash ,attr (window-plist ,win)))
      (defun (setf ,(intern (format nil "WINDOW-~a" attr))) (,val ,win)
        (setf (gethash ,attr (window-plist ,win))) ,val))))

(defun sort-windows (group)
  "Return a copy of the screen's window list sorted by number."
  (sort1 (group-windows group) '< :key 'window-number))

(defun marked-windows (group)
  "Return the marked windows in the specified group."
  (loop for i in (sort-windows group)
        when (window-marked i)
        collect i))

(defun clear-window-marks (group &optional (windows (group-windows group)))
  (dolist (w windows)
    (setf (window-marked w) nil)))

(defun (setf xwin-state) (state xwin)
  "Set the state (iconic, normal, withdrawn) of a window."
  (xlib:change-property xwin
                        :WM_STATE
                        (list state)
                        :WM_STATE
                        32))

(defun xwin-state (xwin)
  "Get the state (iconic, normal, withdraw of a window."
  (first (xlib:get-property xwin :WM_STATE)))

(defun window-hidden-p (window)
  (eql (window-state window) +iconic-state+))

(defun add-wm-state (xwin state)
  (xlib:change-property xwin :_NET_WM_STATE
                        (list (xlib:find-atom *display* state))
                        :atom 32
                        :mode :append))

(defun remove-wm-state (xwin state)
  (xlib:change-property xwin :_NET_WM_STATE
                        (delete (xlib:find-atom *display* state) (xlib:get-property xwin :_NET_WM_STATE))
                        :atom 32))

(defun window-property (window prop)
  (xlib:get-property (window-xwin window) prop))

(defun find-wm-state (xwin state)
  (find (xlib:find-atom *display* state) (xlib:get-property xwin :_NET_WM_STATE) :test #'=))

(defun xwin-unhide (xwin parent)
  (xlib:map-subwindows parent)
  (xlib:map-window parent)
  (setf (xwin-state xwin) +normal-state+))

(defun unhide-window (window)
  (when (window-in-current-group-p window)
    (xwin-unhide (window-xwin window) (window-parent window)))
  (setf (window-state window) +normal-state+)
  ;; Mark window as unhiden
  (remove-wm-state (window-xwin window) :_NET_WM_STATE_HIDDEN))

;; Despite the naming convention, this function takes a window struct,
;; not an xlib:window.
(defun xwin-hide (window)
  (declare (type window window))
  (unless (eq (xlib:window-map-state (window-xwin window)) :unmapped)
    (setf (xwin-state (window-xwin window)) +iconic-state+)
    (incf (window-unmap-ignores window))
    (xlib:unmap-window (window-parent window))
    (xlib:unmap-subwindows (window-parent window))))

(defun hide-window (window)
  (dformat 2 "hide window: ~s~%" window)
  (unless (eql (window-state window) +iconic-state+)
    (setf (window-state window) +iconic-state+)
    ;; Mark window as hidden
    (add-wm-state (window-xwin window) :_NET_WM_STATE_HIDDEN)
    (when (window-in-current-group-p window)
      (xwin-hide window)
      (when (eq window (current-window))
        ;; If this window had the focus, try to avoid losing it.
        (let ((group (window-group window))
              (frame (window-frame window)))
          (setf (frame-window frame)
                (first (remove-if 'window-hidden-p (frame-windows group frame))))
          (focus-frame group (tile-group-current-frame group)))))))

(defun xwin-type (win)
  "Return one of :desktop, :dock, :toolbar, :utility, :splash,
:dialog, :transient, :maxsize and :normal.  Right now
only :dialog, :normal, :maxsize and :transient are
actually returned; see +NETWM-WINDOW-TYPES+."
  (or (and (let ((hints (xlib:wm-normal-hints win)))
             (and hints (or (xlib:wm-size-hints-max-width hints)
                            (xlib:wm-size-hints-max-height hints)
                            (xlib:wm-size-hints-min-aspect hints)
                            (xlib:wm-size-hints-max-aspect hints))))
           :maxsize)
      (let ((net-wm-window-type (xlib:get-property win :_NET_WM_WINDOW_TYPE)))
        (when net-wm-window-type
          (dolist (type-atom net-wm-window-type)
            (when (assoc (xlib:atom-name *display* type-atom) +netwm-window-types+)
              (return (cdr (assoc (xlib:atom-name *display* type-atom) +netwm-window-types+)))))))
      (and (xlib:get-property win :WM_TRANSIENT_FOR)
           :transient)
      :normal))

(defun xwin-strut (screen win)
  "Return the area that the window wants to reserve along the edges of the screen.
Values are left, right, top, bottom, left_start_y, left_end_y,
right_start_y, right_end_y, top_start_x, top_end_x, bottom_start_x
and bottom_end_x."
  (let ((net-wm-strut-partial (xlib:get-property win :_NET_WM_STRUT_PARTIAL)))
    (if (= (length net-wm-strut-partial) 12)
        (apply 'values net-wm-strut-partial)
        (let ((net-wm-strut (xlib:get-property win :_NET_WM_STRUT)))
          (if (= (length net-wm-strut) 4)
              (apply 'values (concatenate 'list net-wm-strut
                                          (list 0 (screen-height screen)
                                                0 (screen-height screen)
                                                0 (screen-width screen)
                                                0 (screen-width screen))))
              (values 0 0 0 0 0 0 0 0 0 0 0 0))))))

;; Stolen from Eclipse
(defun xwin-send-configuration-notify (xwin x y w h bw)
  "Send a synthetic configure notify event to the given window (ICCCM 4.1.5)"
  (xlib:send-event xwin :configure-notify nil
                   :event-window xwin
                   :window xwin
                   :x x :y y
                   :width w
                   :height h
                   :border-width bw
                   :propagate-p nil))

(defun update-window-gravity ()
  (dolist (s *screen-list*)
    (dolist (g (screen-groups s))
      (mapc 'maximize-window (group-windows g)))))

(defun set-normal-gravity (gravity)
  "Set the default gravity for normal windows. Possible values are
@code{:center} @code{:top} @code{:left} @code{:right} @code{:bottom}
@code{:top-left} @code{:top-right} @code{:bottom-left} and
@code{:bottom-right}."
  (setf *normal-gravity* gravity)
  (update-window-gravity))

(defun set-maxsize-gravity (gravity)
  "Set the default gravity for maxsize windows."
  (setf *maxsize-gravity* gravity)
  (update-window-gravity))

(defun set-transient-gravity (gravity)
  "Set the default gravity for transient/pop-up windows."
  (setf *transient-gravity* gravity)
  (update-window-gravity))

(defun gravity-for-window (win)
  (or (window-gravity win)
      (ecase (window-type win)
        (:dock *normal-gravity*)
        (:normal *normal-gravity*)
        (:maxsize *maxsize-gravity*)
        ((:transient :dialog) *transient-gravity*))))

(defun geometry-hints (win)
  "Return hints for max width and height and increment hints. These
hints have been modified to always be defined and never be greater
than the root window's width and height."
  (let* ((f (window-frame win))
         (x (frame-x f))
         (y (frame-display-y (window-group win) f))
         (border (xlib:drawable-border-width (window-parent win)))
         (fwidth (- (frame-width f) (* 2 border)))
         (fheight (- (frame-display-height (window-group win) f)
                     (* 2 border)))
         (width fwidth)
         (height fheight)
         (hints (window-normal-hints win))
         (hints-min-width (and hints (xlib:wm-size-hints-min-width hints)))
         (hints-min-height (and hints (xlib:wm-size-hints-min-height hints)))
         (hints-max-width (and hints (xlib:wm-size-hints-max-width hints)))
         (hints-max-height (and hints (xlib:wm-size-hints-max-height hints)))
         (hints-width (and hints (xlib:wm-size-hints-base-width hints)))
         (hints-height (and hints (xlib:wm-size-hints-base-height hints)))
         (hints-inc-x (and hints (xlib:wm-size-hints-width-inc hints)))
         (hints-inc-y (and hints (xlib:wm-size-hints-height-inc hints)))
         (hints-min-aspect (and hints (xlib:wm-size-hints-min-aspect hints)))
         (hints-max-aspect (and hints (xlib:wm-size-hints-max-aspect hints)))
         center)
    ;;    (dformat 4 "hints: ~s~%" hints)
    ;; determine what the width and height should be
    (cond
      ;; handle specially fullscreen windows.
      ((window-fullscreen win)
       (let ((head (frame-head (window-group win) f)))
         (setf x (frame-x head)
               y (frame-y head)
               width (frame-width head)
               height (frame-height head)
               (xlib:window-priority (window-parent win)) :above
               (xlib:drawable-border-width (window-parent win)) 0))
       (return-from geometry-hints (values x y 0 0 width height t)))
      ;; Adjust the defaults if the window is a transient_for window.
      ((find (window-type win) '(:transient :dialog))
       (setf center t
             width (min (max (or hints-width 0)
                             (or hints-min-width 0)
                             (window-width win))
                        width)
             height (min (max (or hints-height 0)
                              (or hints-min-height 0)
                              (window-height win))
                         height)))
      ;; aspect hints are handled similar to max size hints
      ((and hints-min-aspect hints-max-aspect)
       (let ((ratio (/ width height)))
         (cond ((< ratio hints-min-aspect)
                (setf height (ceiling width hints-min-aspect)))
               ((> ratio hints-max-aspect)
                (setf width  (ceiling (* height hints-max-aspect)))))
         (setf center t)))
      ;; Update our defaults if the window has the maxsize hints
      ((or hints-max-width hints-max-height)
       (when (and hints-max-width
                  (< hints-max-width width))
         (setf width hints-max-width))
       (when (and hints-max-height
                  (< hints-max-height height))
         (setf height hints-max-height))
       (setf center t))
      (t
       ;; if they have inc hints then start with the size and adjust
       ;; based on those increments until the window fits in the frame
       (when hints-inc-x
         (let ((w (or hints-width (window-width win))))
           (setf width (+ w (* hints-inc-x
                               (+ (floor (- fwidth w) hints-inc-x)))))))
       (when hints-inc-y
         (let ((h (or hints-height (window-height win))))
           (setf height (+ h (* hints-inc-y
                                (+ (floor (- fheight h -1) hints-inc-y)))))))))
    ;; adjust for gravity
    (multiple-value-bind (wx wy) (get-gravity-coords (gravity-for-window win)
                                                     width height
                                                     0 0
                                                     fwidth fheight)
      (when center
        (setf x (+ wx (frame-x f))
              y (+ wy (frame-display-y (window-group win) f))
              wx 0 wy 0))
      ;; Now return our findings
      (values x y wx wy width height center))))

(defun set-window-geometry (win &key x y width height border-width)
  (macrolet ((update (xfn wfn v)
               `(when ,v ;; (/= (,wfn win) ,v))
                 (setf (,xfn (window-xwin win)) ,v)
                 ,(when wfn `(setf (,wfn win) ,v)))))
    (xlib:with-state ((window-xwin win))
      (update xlib:drawable-x nil x)
      (update xlib:drawable-y nil y)
      (update xlib:drawable-width window-width width)
      (update xlib:drawable-height window-height height)
      (update xlib:drawable-border-width nil border-width)
      )))

(defun maximize-window (win)
  "Maximize the window."
  (multiple-value-bind (x y wx wy width height stick)
      (geometry-hints win)
    (dformat 4 "maximize window ~a x: ~d y: ~d width: ~d height: ~d stick: ~s~%" win x y width height stick)
    ;; Move the parent window
    (xlib:with-state ((window-parent win))
      (setf (xlib:drawable-x (window-parent win)) x
            (xlib:drawable-y (window-parent win)) y))
    ;; This is the only place a window's geometry should change
    (set-window-geometry win :x wx :y wy :width width :height height :border-width 0)
    ;; the parent window should stick to the size of the window
    ;; unless it isn't being maximized to fill the frame.
    (xlib:with-state ((window-parent win))
      (if stick
          (setf (xlib:drawable-width (window-parent win)) (window-width win)
                (xlib:drawable-height (window-parent win)) (window-height win))
          (let ((frame (window-frame win)))
            (setf (xlib:drawable-width (window-parent win)) (- (frame-width frame)
                                                               (* 2 (xlib:drawable-border-width (window-parent win))))
                  (xlib:drawable-height (window-parent win)) (- (frame-display-height (window-group win) frame)
                                                                (* 2 (xlib:drawable-border-width (window-parent win))))))))))

(defun find-free-window-number (group)
  "Return a free window number for GROUP."
  (find-free-number (mapcar 'window-number (group-windows group))))

(defun reparent-window (window)
  ;; apparently we need to grab the server so the client doesn't get
  ;; the mapnotify event before the reparent event. that's what fvwm
  ;; says.
  (xlib:with-server-grabbed (*display*)
    (let* ((screen (window-screen window))
           (master-window (xlib:create-window
                           :parent (screen-root screen)
                           :x (xlib:drawable-x (window-xwin window)) :y (xlib:drawable-y (window-xwin window))
                           :width (window-width window)
                           :height (window-height window)
                           ;; normal windows get a black background
                           :background (if (eq (window-type window) :normal)
                                           (screen-bg-color screen)
                                           :none)
                           :border (screen-unfocus-color screen)
                           :border-width (default-border-width-for-type (window-type window))
                           :event-mask *window-parent-events*)))
      (unless (eq (xlib:window-map-state (window-xwin window)) :unmapped)
        (incf (window-unmap-ignores window)))
      (xlib:reparent-window (window-xwin window) master-window 0 0)
      (xwin-grab-buttons master-window)
      ;;     ;; we need to update these values since they get set to 0,0 on reparent
      ;;     (setf (window-x window) 0
      ;;          (window-y window) 0)
      (xlib:add-to-save-set (window-xwin window))
      (setf (window-parent window) master-window))))

(defun process-existing-windows (screen)
  "Windows present when stumpwm starts up must be absorbed by stumpwm."
  (let ((children (xlib:query-tree (screen-root screen)))
        (*processing-existing-windows* t)
        (stacking (xlib:get-property (screen-root screen) :_NET_CLIENT_LIST_STACKING :type :window)))
    (when stacking
      (dformat 3 "Using window stacking: ~{~X ~}~%" stacking)
      ;; sort by _NET_CLIENT_LIST_STACKING
      (setf children (stable-sort children #'< :key
                                  (lambda (xwin)
                                    (or (position (xlib:drawable-id xwin) stacking :test #'=) 0)))))
    (dolist (win children)
      (let ((map-state (xlib:window-map-state win))
            (wm-state (xwin-state win)))
        ;; Don't process override-redirect windows.
        (unless (or (eq (xlib:window-override-redirect win) :on)
                    (internal-window-p screen win))
          (if (eq (xwin-type win) :dock)
              (progn
                (dformat 1 "Window ~S is dock-type. Placing in mode-line.~%" win)
                (place-mode-line-window screen win))
              (if (or (eql map-state :viewable)
                      (eql wm-state +iconic-state+))
                  (progn
                    (dformat 1 "Processing ~S ~S~%" (xwin-name win) win)
                    (process-mapped-window screen win))))))))
  (dolist (w (screen-windows screen))
    (setf (window-state w) +normal-state+)
    (xwin-hide w)))

(defun xwin-grab-keys (win)
  (labels ((grabit (w key)
             (let ((code (xlib:keysym->keycodes *display* (key-keysym key))))
               ;; some keysyms aren't mapped to keycodes so just ignore them.
               (when code
                 (xlib:grab-key w code
                                :modifiers (x11-mods key) :owner-p t
                                :sync-pointer-p nil :sync-keyboard-p nil)
                 ;; Ignore numlock by also grabbing the keycombo with
                 ;; numlock on.
                 (when (modifiers-numlock *modifiers*)
                   (xlib:grab-key w code
                                  :modifiers (x11-mods key t) :owner-p t
                                  :sync-pointer-p nil :sync-keyboard-p nil))))))
    (maphash (lambda (k v)
               (declare (ignore v))
               (grabit win k))
             *top-map*)))

(defun grab-keys-on-window (win)
  (xwin-grab-keys (window-xwin win)))

(defun xwin-ungrab-keys (win)
  (xlib:ungrab-key win :any :modifiers :any))

(defun ungrab-keys-on-window (win)
  (xwin-ungrab-keys (window-xwin win)))

(defun xwin-grab-buttons (win)
  ;; FIXME: Why doesn't grabbing button :any work? We have to
  ;; grab them one by one instead.
  (xwin-ungrab-buttons win)
  (loop for i from 1 to 7
        do (xlib:grab-button win i '(:button-press)
                             :modifiers :any
                             :owner-p nil
                             :sync-pointer-p t
                             :sync-keyboard-p nil)))


(defun xwin-ungrab-buttons (win)
  (xlib:ungrab-button win :any :modifiers :any))

(defun sync-keys ()
  "Any time *top-map* is modified this must be called."
  (loop for i in *screen-list*
        do (xwin-ungrab-keys (screen-focus-window i))
        do (loop for j in (screen-mapped-windows i)
                 do (xwin-ungrab-keys j))
        do (xlib:display-finish-output *display*)
        do (loop for j in (screen-mapped-windows i)
                 do (xwin-grab-keys j))
        do (xwin-grab-keys (screen-focus-window i)))
  (xlib:display-finish-output *display*))


;;; Window placement routines

(defun xwin-to-window (xwin)
  "Build a window for XWIN"
  (make-window
   :xwin xwin
   :width (xlib:drawable-width xwin) :height (xlib:drawable-height xwin)
   :x (xlib:drawable-x xwin) :y (xlib:drawable-y xwin)
   :title (xwin-name xwin)
   :class (xwin-class xwin)
   :res (xwin-res-name xwin)
   :role (xwin-role xwin)
   :type (xwin-type xwin)
   :normal-hints (xlib:wm-normal-hints xwin)
   :state +iconic-state+
   :plist (make-hash-table)
   :unmap-ignores 0))

(defun string-match (string pat)
  (let ((l (length pat)))
    (when (> l 0)
      (if (and (> l 3) (equal (subseq pat 0 3) "..."))
          (search (subseq pat 3 l) string)
          (equal string pat)))))

(defun window-matches-properties-p (window &key class instance type role title)
  "Returns T if window matches all the given properties"
  (and
   (if class (equal (window-class window) class) t)
   (if instance (equal (window-res window) instance) t)
   (if type (equal (window-type window) type) t)
   (if role (string-match (window-role window) role) t)
   (if title (string-match (window-title window) title) t) t))

(defun window-matches-rule-p (w rule)
  "Returns T if window matches rule"
  (destructuring-bind (group-name frame raise lock &rest props) rule
    (declare (ignore frame raise))
    (if (or lock
            (equal group-name (group-name (or (window-group w) (current-group)))))
        (apply 'window-matches-properties-p w props))))

;; TODO: add rules allowing matched windows to create their own groups/frames

(defun rule-matching-window (window)
  (dolist (rule *window-placement-rules*)
    (when (window-matches-rule-p window rule) (return rule))))

(defun get-window-placement (screen window)
  "Returns the ideal group and frame that WINDOW should belong to and whether
  the window should be raised."
  (let ((match (rule-matching-window window)))
    (if match
        (destructuring-bind (group-name frame raise lock &rest props) match
          (declare (ignore lock props))
          (let ((group (find-group screen group-name)))
            (if group
                (values group (frame-by-number group frame) raise)
                (progn
                  (message "^B^1*Error placing window, group \"^b~a^B\" does not exist." group-name)
                  (values)))))
        (values))))

(defun sync-window-placement ()
  "Re-arrange existing windows according to placement rules"
  (dolist (screen *screen-list*)
    (dolist (window (screen-windows screen))
      (multiple-value-bind (to-group frame raise) (get-window-placement screen window)
        (declare (ignore raise))
        (when to-group
          (unless (eq (window-group window) to-group)
            (move-window-to-group window to-group))
          (unless (eq (window-frame window) frame)
            (pull-window window frame)))))))

(defun assign-window (window group frame &optional (where :tail))
  (setf (window-group window) group
        (window-number window) (find-free-window-number group)
        (window-frame window) (or frame (pick-prefered-frame window)))
  (if (eq where :head)
      (push window (group-windows group))
      (setf (group-windows group) (append (group-windows group) (list window)))))

(defun place-existing-window (screen xwin)
  "Called for windows existing at startup."
  (let* ((window (xwin-to-window xwin))
         (netwm-id (first (xlib:get-property xwin :_NET_WM_DESKTOP)))
         (group (if (and netwm-id (< netwm-id (length (screen-groups screen))))
                    (elt (sort-groups screen) netwm-id)
                    (screen-current-group screen))))
    (dformat 3 "Assigning pre-existing window ~S to group ~S~%" (window-name window) (group-name group))
    (assign-window window group (find-frame group (xlib:drawable-x xwin) (xlib:drawable-y xwin)) :head)
    (setf (frame-window (window-frame window)) window)
    window))

(defun place-window (screen xwin)
  "Pick a group and frame for XWIN."
  (let* ((window (xwin-to-window xwin))
         (group (screen-current-group screen))
         (frame nil)
         (raise nil))
    (multiple-value-bind (to-group to-frame to-raise) (get-window-placement screen window)
      (setf group (or to-group group)
            frame to-frame
            raise to-raise))
    (assign-window window group frame)
    (setf (xwin-state xwin) +iconic-state+)
    (xlib:change-property xwin :_NET_WM_DESKTOP
                          (list (netwm-group-id group))
                          :cardinal 32)
    (when frame
      (unless (eq (current-group) group)
        (if raise
            (switch-to-group group)
            (message "Placing window ~a in frame ~d of group ~a"
                     (window-name window) (frame-number frame) (group-name group))))
      (when raise
        (switch-to-screen (group-screen group))
        (focus-frame group frame))
      (run-hook-with-args *place-window-hook* window group frame))
    window))

(defun pick-prefered-frame (window)
  (let* ((group (window-group window))
         (frames (group-frames group))
         (default (tile-group-current-frame group)))
    (or
     (if (or (functionp *new-window-prefered-frame*)
             (and (symbolp *new-window-prefered-frame*)
                  (fboundp *new-window-prefered-frame*)))
         (handler-case
             (funcall *new-window-prefered-frame* window)
           (error (c)
             (message "^1*^BError while calling ^b^3**new-window-prefered-frame*^1*^B: ^n~a" c)
             nil))
         (loop for i in *new-window-prefered-frame*
               thereis (case i
                         (:last
                          ;; last-frame can be stale
                          (and (> (length frames) 1)
                               (tile-group-last-frame group)))
                         (:unfocused
                          (find-if (lambda (f)
                                     (not (eq f (tile-group-current-frame group))))
                                   frames))
                         (:empty
                          (find-if (lambda (f)
                                     (null (frame-window f)))
                                   frames))
                         (:choice
                          ;; Transient windows sometimes specify a location
                          ;; relative to the TRANSIENT_FOR window. Just ignore
                          ;; these hints.
                          (unless (find (window-type window) '(:transient :dialog))
                            (let ((hints (window-normal-hints window)))
                              (when (and hints (xlib:wm-size-hints-user-specified-position-p hints))
                                (find-frame group (window-x window) (window-y window))))))
                         (t             ; :focused
                          (tile-group-current-frame group)))))
     default)))

(defun add-window (screen xwin)
  (screen-add-mapped-window screen xwin)
  (register-window (if *processing-existing-windows*
                       (place-existing-window screen xwin)
                       (place-window screen xwin))))

(defun netwm-remove-window (window)
  (xlib:delete-property (window-xwin window) :_NET_WM_DESKTOP))

(defun process-mapped-window (screen xwin)
  "Add the window to the screen's mapped window list and process it as
needed."
  (let ((window (add-window screen xwin)))
    (setf (xlib:window-event-mask (window-xwin window)) *window-events*)
    ;; windows always have border width 0. Their parents provide the
    ;; border.
    (set-window-geometry window :border-width 0)
    (reparent-window window)
    (maximize-window window)
    (grab-keys-on-window window)
    ;; quite often the modeline displays the window list, so update it
    (update-all-mode-lines)
    ;; Set allowed actions
    (xlib:change-property xwin :_NET_WM_ALLOWED_ACTIONS
                          (mapcar (lambda (a)
                                    (xlib:intern-atom *display* a))
                                  +netwm-allowed-actions+)
                          :atom 32)
    ;; Run the new window hook on it.
    (run-hook-with-args *new-window-hook* window)
    window))

(defun find-withdrawn-window (xwin)
  "Return the window and screen for a withdrawn window."
  (declare (type xlib:window xwin))
  (dolist (i *screen-list*)
    (let ((w (find xwin (screen-withdrawn-windows i) :key 'window-xwin :test 'xlib:window-equal)))
      (when w
        (return-from find-withdrawn-window (values w i))))))

(defun restore-window (window)
  "Restore a withdrawn window"
  (declare (type window window))
  ;; put it in a valid group
  (let ((screen (window-screen window)))
    ;; Use window plaecment rules
    (multiple-value-bind (group frame raise) (get-window-placement screen window)
      (declare (ignore raise))
      (unless (find (window-group window)
                    (screen-groups screen))
        (setf (window-group window) (or group (screen-current-group screen))))
      ;; FIXME: somehow it feels like this could be merged with group-add-window
      (setf (window-title window) (xwin-name (window-xwin window))
            (window-class window) (xwin-class (window-xwin window))
            (window-res window) (xwin-res-name (window-xwin window))
            (window-role window) (xwin-role (window-xwin window))
            (window-type window) (xwin-type (window-xwin window))
            (window-normal-hints window) (xlib:wm-normal-hints (window-xwin window))
            (window-number window) (find-free-window-number (window-group window))
            (window-state window) +iconic-state+
            (xwin-state (window-xwin window)) +iconic-state+
            (screen-withdrawn-windows screen) (delete window (screen-withdrawn-windows screen))
            ;; put the window at the end of the list
            (group-windows (window-group window)) (append (group-windows (window-group window)) (list window))
            (window-frame window) (or frame (pick-prefered-frame window))))
    (screen-add-mapped-window screen (window-xwin window))
    (register-window window)
    (xlib:change-property (window-xwin window) :_NET_WM_DESKTOP
                          (list (netwm-group-id (window-group window)))
                          :cardinal 32)
    (maximize-window window)
    ;; It is effectively a new window in terms of the window list.
    (run-hook-with-args *new-window-hook* window)
    ;; give it focus
    (if (deny-request-p window *deny-map-request*)
        (unless *suppress-deny-messages*
          (if (eq (window-group window) (current-group))
              (echo-string (window-screen window) (format nil "'~a' denied map request" (window-name window)))
              (echo-string (window-screen window) (format nil "'~a' denied map request in group ~a" (window-name window) (group-name (window-group window))))))
        (frame-raise-window (window-group window) (window-frame window) window
                            (if (eq (window-frame window)
                                    (tile-group-current-frame (window-group window)))
                                t nil)))))

(defun withdraw-window (window)
  "Withdrawing a window means just putting it in a list til we get a destroy event."
  (declare (type window window))
  ;; This function cannot request info about WINDOW from the xserver as it may not exist anymore.
  (let ((f (window-frame window))
        (group (window-group window))
        (screen (window-screen window)))
    (dformat 1 "withdraw window ~a~%" screen)
    ;; Save it for later since it is only withdrawn, not destroyed.
    (push window (screen-withdrawn-windows screen))
    (setf (window-state window) +withdrawn-state+
          (xwin-state (window-xwin window)) +withdrawn-state+)
    (xlib:unmap-window (window-parent window))
    ;; Clean up the window's entry in the screen and group
    (screen-remove-mapped-window screen (window-xwin window))
    (setf (group-windows group)
          (delete window (group-windows group)))
    ;; remove it from it's frame structures
    (when (eq (frame-window f) window)
      (frame-raise-window group f (first (frame-windows group f)) nil))
    (when (window-in-current-group-p window)
      ;; since the window doesn't exist, it doesn't have focus.
      (setf (screen-focus screen) nil))
    (netwm-remove-window window)
    ;; If the current window was removed, then refocus the frame it
    ;; was in, since it has a new current window
    (when (eq (tile-group-current-frame group) f)
      (focus-frame (window-group window) f))
    ;; quite often the modeline displays the window list, so update it
    (update-all-mode-lines)
    ;; Run the destroy hook on the window
    (run-hook-with-args *destroy-window-hook* window)))

(defun destroy-window (window)
  (declare (type window window))
  "The window has been destroyed. clean up our data structures."
  ;; This function cannot request info about WINDOW from the xserver
  (let ((screen (window-screen window)))
    (unless (eql (window-state window) +withdrawn-state+)
      (withdraw-window window))
    ;; now that the window is withdrawn, clean up the data structures
    (setf (screen-withdrawn-windows screen)
          (delete window (screen-withdrawn-windows screen)))
    (dformat 1 "destroy window ~a~%" screen)
    (dformat 3 "destroying parent window~%")
    (xlib:destroy-window (window-parent window))))

(defun move-window-to-head (group window)
  "Move window to the head of the group's window list."
  (declare (type group group))
  (declare (type window window))
                                        ;(assert (member window (screen-mapped-windows screen)))
  (setf (group-windows group) (delete window (group-windows group)))
  (push window (group-windows group))
  (netwm-update-client-list-stacking (group-screen group)))

(defun no-focus (group last-win)
  "don't focus any window but still read keyboard events."
  (dformat 3 "no-focus~%")
  (let* ((screen (group-screen group)))
    (when (eq group (screen-current-group screen))
      (xlib:set-input-focus *display* (screen-focus-window screen) :POINTER-ROOT)
      (setf (screen-focus screen) nil)
      (move-screen-to-head screen))
    (when last-win
      (update-window-border last-win))))

(defun focus-window (window)
  "Give the window focus. This means the window will be visible,
maximized, and given focus."
  (dformat 3 "focus-window: ~s~%" window)
  (let* ((group (window-group window))
         (screen (group-screen group))
         (cw (screen-focus screen)))
    ;; If window to focus is already focused then our work is done.
    (unless (eq window cw)
      (update-all-mode-lines)
      (raise-window window)
      (screen-set-focus screen window)
      ;;(send-client-message window :WM_PROTOCOLS +wm-take-focus+)
      (update-window-border window)
      (when cw
        (update-window-border cw))
      ;; Move the window to the head of the mapped-windows list
      (move-window-to-head group window)
      (run-hook-with-args *focus-window-hook* window cw))))

(defun delete-window (window)
  "Send a delete event to the window."
  (dformat 3 "Delete window~%")
  (send-client-message window :WM_PROTOCOLS (xlib:intern-atom *display* :WM_DELETE_WINDOW)))

(defun xwin-kill (window)
  "Kill the client associated with window."
  (dformat 3 "Kill client~%")
  (xlib:kill-client *display* (xlib:window-id window)))


;;; Message printing functions

(defun color-exists-p (color)
  (handler-case
      (loop for i in *screen-list*
            always (xlib:lookup-color (xlib:screen-default-colormap (screen-number i)) color))
    (xlib:name-error () nil)))

(defun font-exists-p (font-name)
  ;; if we can list the font then it exists
  (plusp (length (xlib:list-font-names *display* font-name :max-fonts 1))))

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
  (set-any-color screen-focus-color color))

(defun set-unfocus-color (color)
  (set-any-color screen-unfocus-color color))

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
      (let ((fobj (xlib:open-font *display* (first (xlib:list-font-names *display* font :max-fonts 1)))))
        (xlib:close-font (screen-font i))
        (setf (screen-font i) fobj
              (xlib:gcontext-font (screen-message-gc i)) fobj)
        ;; update the modelines too
        (dolist (h (screen-heads i))
          (when (and (head-mode-line h)
                     (eq (mode-line-mode (head-mode-line h)) :stump))
            (setf (xlib:gcontext-font (mode-line-gc (head-mode-line h))) fobj)
            (resize-mode-line (head-mode-line h))
            (sync-mode-line (head-mode-line h))))))
    t))

(defun max-width (font l)
  "Return the width of the longest string in L using FONT."
  (loop for i in l
        maximize (xlib:text-width font i :translate #'translate-id)))

(defun get-gravity-coords (gravity width height minx miny maxx maxy)
  "Return the x y coords for a window on with gravity etc"
  (values (case gravity
            ((:top-right :bottom-right :right) (- maxx width))
            ((:top :bottom :center) (truncate (- maxx minx width) 2))
            (t minx))
          (case gravity
            ((:bottom-left :bottom-right :bottom) (- maxy height))
            ((:left :right :center) (truncate (- maxy miny height) 2))
            (t miny))))

(defun setup-win-gravity (screen win gravity)
  "Position the x, y of the window according to its gravity. This
function expects to be wrapped in a with-state for win."
  (xlib:with-state ((screen-root screen))
    (let ((w (xlib:drawable-width win))
          (h (xlib:drawable-height win))
          (screen-width (head-width (current-head)))
          (screen-height (head-height (current-head))))
      (let ((x (case gravity
                 ((:top-left :bottom-left) 0)
                 (:center (truncate (- screen-width w (* (xlib:drawable-border-width win) 2)) 2))
                 (t (- screen-width w (* (xlib:drawable-border-width win) 2)))))
            (y (case gravity
                 ((:bottom-right :bottom-left) (- screen-height h (* (xlib:drawable-border-width win) 2)))
                 (:center (truncate (- screen-height h (* (xlib:drawable-border-width win) 2)) 2))
                 (t 0))))
        (setf (xlib:drawable-y win) (max (head-y (current-head)) (+ (head-y (current-head)) y))
              (xlib:drawable-x win) (max (head-x (current-head)) (+ (head-x (current-head)) x)))))))

(defun setup-message-window (screen lines width)
  (let ((height (* lines
                   (+ (xlib:font-ascent (screen-font screen))
                      (xlib:font-descent (screen-font screen)))))
        (win (screen-message-window screen)))
    ;; Now that we know the dimensions, raise and resize it.
    (xlib:with-state (win)
      (setf (xlib:drawable-height win) height
            (xlib:drawable-width win) (+ width (* *message-window-padding* 2))
            (xlib:window-priority win) :above)
      (setup-win-gravity screen win *message-window-gravity*))
    (xlib:map-window win)
    ;; Clear the window
    (xlib:clear-area win)
    (incf (screen-ignore-msg-expose screen))
    ;; Have to flush this or the window might get cleared
    ;; after we've already started drawing it.
    (xlib:display-finish-output *display*)))

(defun invert-rect (screen win x y width height)
  "invert the color in the rectangular area. Used for highlighting text."
  (let ((gcontext (xlib:create-gcontext :drawable win
                                        :foreground (screen-fg-color screen)
                                        :function boole-xor)))
    (xlib:draw-rectangle win gcontext x y width height t)
    (setf (xlib:gcontext-foreground gcontext) (screen-bg-color screen))
    (xlib:draw-rectangle win gcontext x y width height t)))


;;; Frame functions

(defun populate-frames (group)
  "Try to fill empty frames in GROUP with hidden windows"
  (dolist (f (group-frames group))
    (unless (frame-window f)
      (choose-new-frame-window f group)
      (when (frame-window f)
        (maximize-window (frame-window f))
        (unhide-window (frame-window f))))))

(defun frame-by-number (group n)
  (unless (eq n nil)
    (find n (group-frames group)
          :key 'frame-number
          :test '=)))

(defun find-frame (group x y)
  "Return the frame of GROUP containing the pixel at X Y"
  (dolist (f (group-frames group))
    (let* ((fy (frame-y f))
           (fx (frame-x f))
           (fwx (+ fx (frame-width f)))
           (fhy (+ fy (frame-height f))))
      (when (and
             (>= y fy) (<= y fhy)
             (>= x fx) (<= x fwx)
             (return f))))))


(defun frame-set-x (frame v)
  (decf (frame-width frame)
        (- v (frame-x frame)))
  (setf (frame-x frame) v))

(defun frame-set-y (frame v)
  (decf (frame-height frame)
        (- v (frame-y frame)))
  (setf (frame-y frame) v))

(defun frame-set-r (frame v)
  (setf (frame-width frame)
        (- v (frame-x frame))))

(defun frame-set-b (frame v)
  (setf (frame-height frame)
        (- v (frame-y frame))))

(defun frame-r (frame)
  (+ (frame-x frame) (frame-width frame)))

(defun frame-b (frame)
  (+ (frame-y frame) (frame-height frame)))

(defun frame-intersect (f1 f2)
  "Return a new frame representing (only) the intersection of F1 and F2. WIDTH and HEIGHT will be <= 0 if there is no overlap"
  (let ((r (copy-frame f1)))
    (when (> (frame-x f2) (frame-x f1))
      (frame-set-x r (frame-x f2)))
    (when (< (+ (frame-x f2) (frame-width f2))
             (+ (frame-x f1) (frame-width f1)))
      (frame-set-r r (frame-r f2)))
    (when (> (frame-y f2) (frame-y f1))
      (frame-set-y r (frame-y f2)))
    (when (< (+ (frame-y f2) (frame-height f2))
             (+ (frame-y f1) (frame-height f1)))
      (frame-set-b r (frame-b f2)))
  (values r)))

(defun frames-overlap-p (f1 f2)
  "Returns T if frames F1 and F2 overlap at all"
  (and (and (frame-p f1) (frame-p f2))
       (let ((frame (frame-intersect f1 f2)))
         (values (not (and (plusp (frame-width frame))
                           (plusp (frame-height frame))))))))

(defun frame-raise-window (g f w &optional (focus t))
  "Raise the window w in frame f in group g. if FOCUS is
T (default) then also focus the frame."
  (let ((oldw (frame-window f)))
    ;; nothing to do when W is nil
    (setf (frame-window f) w)
    (unless (and w (eq oldw w))
      (if w
          (raise-window w)
          (mapc 'hide-window (frame-windows g f))))
    (when focus
      (focus-frame g f))
    (when (and w (not (window-modal-p w)))
      (raise-modals-of w))))

(defun focus-frame (group f)
  (let ((w (frame-window f))
        (last (tile-group-current-frame group))
        (show-indicator nil))
    (setf (tile-group-current-frame group) f)
    ;; record the last frame to be used in the fother command.
    (unless (eq f last)
      (setf (tile-group-last-frame group) last)
      (run-hook-with-args *focus-frame-hook* f last)
      (setf show-indicator t))
    (if w
        (focus-window w)
        (no-focus group (frame-window last)))
    (when show-indicator
      (show-frame-indicator group))))

(defun frame-windows (group f)
  (remove-if-not (lambda (w) (eq (window-frame w) f))
                 (group-windows group)))

(defun frame-sort-windows (group f)
  (remove-if-not (lambda (w) (eq (window-frame w) f))
                 (sort-windows group)))

(defun copy-frame-tree (tree)
  "Return a copy of the frame tree."
  (cond ((null tree) tree)
        ((typep tree 'frame)
         (copy-structure tree))
        (t
         (mapcar #'copy-frame-tree tree))))

(defun group-frames (group)
  (tree-accum-fn (tile-group-frame-tree group) 'nconc 'list))

(defun head-frames (group head)
  (tree-accum-fn (tile-group-frame-head group head) 'nconc 'list))

(defun find-free-frame-number (group)
  (find-free-number (mapcar (lambda (f) (frame-number f))
                            (group-frames group))))

(defun choose-new-frame-window (frame group)
  "Find out what window should go in a newly created frame."
  (let ((win (case *new-frame-action*
               (:last-window (other-hidden-window group))
               (t nil))))
    (setf (frame-window frame) win)
    (when win
      (setf (window-frame win) frame))))

(defun split-frame-h (group p)
  "Return 2 new frames. The first one stealing P's number and window"
  (let* ((w (truncate (/ (frame-width p) 2)))
         (h (frame-height p))
         (f1 (make-frame :number (frame-number p)
                         :x (frame-x p)
                         :y (frame-y p)
                         :width w
                         :height h
                         :window (frame-window p)))
         (f2 (make-frame :number (find-free-frame-number group)
                         :x (+ (frame-x p) w)
                         :y (frame-y p)
                         ;; gobble up the modulo
                         :width (- (frame-width p) w)
                         :height h
                         :window nil)))
    (run-hook-with-args *new-frame-hook* f2)
    (values f1 f2)))

(defun split-frame-v (group p)
  "Return 2 new frames. The first one stealing P's number and window"
  (let* ((w (frame-width p))
         (h (truncate (/ (frame-height p) 2)))
         (f1 (make-frame :number (frame-number p)
                         :x (frame-x p)
                         :y (frame-y p)
                         :width w
                         :height h
                         :window (frame-window p)))
         (f2 (make-frame :number (find-free-frame-number group)
                         :x (frame-x p)
                         :y (+ (frame-y p) h)
                         :width w
                         ;; gobble up the modulo
                         :height (- (frame-height p) h)
                         :window nil)))
    (run-hook-with-args *new-frame-hook* f2)
    (values f1 f2)))

(defun funcall-on-leaf (tree leaf fn)
  "Return a new tree with LEAF replaced with the result of calling FN on LEAF."
  (cond ((atom tree)
         (if (eq leaf tree)
             (funcall fn leaf)
             tree))
        (t (mapcar (lambda (sib)
                     (funcall-on-leaf sib leaf fn))
                   tree))))

(defun funcall-on-node (tree fn match)
  "Call fn on the node where match returns t."
  (if (funcall match tree)
      (funcall fn tree)
      (cond ((atom tree) tree)
            (t (mapcar (lambda (sib)
                         (funcall-on-node sib fn match))
                       tree)))))

(defun replace-frame-in-tree (tree f &rest frames)
  (funcall-on-leaf tree f (lambda (f)
                            (declare (ignore f))
                            frames)))

(defun sibling-internal (tree leaf fn)
  "helper for next-sibling and prev-sibling."
  (cond ((atom tree) nil)
        ((find leaf tree)
         (let* ((rest (cdr (member leaf (funcall fn tree))))
                (pick (car (if (null rest) (funcall fn tree) rest))))
           (unless (eq pick leaf)
             pick)))
        (t (find-if (lambda (x)
                      (sibling-internal x leaf fn))
                    tree))))

(defun next-sibling (tree leaf)
  "Return the sibling of LEAF in TREE."
  (sibling-internal tree leaf 'identity))

(defun prev-sibling (tree leaf)
  (sibling-internal tree leaf 'reverse))

(defun closest-sibling (tree leaf)
  "Return the sibling to the right/below of leaf or left/above if
leaf is the most right/below of its siblings."
  (let* ((parent (tree-parent tree leaf))
         (lastp (= (position leaf parent) (1- (length parent)))))
    (if lastp
        (prev-sibling parent leaf)
        (next-sibling parent leaf))))

(defun migrate-frame-windows (group src dest)
  "Migrate all windows in SRC frame to DEST frame."
  (mapc (lambda (w)
          (when (eq (window-frame w) src)
            (setf (window-frame w) dest)))
        (group-windows group)))

(defun tree-accum-fn (tree acc fn)
  "Run an accumulator function on fn applied to each leaf"
  (cond ((null tree) nil)
        ((atom tree)
         (funcall fn tree))
        (t (apply acc (mapcar (lambda (x) (tree-accum-fn x acc fn)) tree)))))

(defun tree-iterate (tree fn)
  "Call FN on every leaf in TREE"
  (cond ((null tree) nil)
        ((atom tree)
         (funcall fn tree))
        (t (mapc (lambda (x) (tree-iterate x fn)) tree))))

(defun tree-x (tree)
  (tree-accum-fn tree 'min 'frame-x))

(defun tree-y (tree)
  (tree-accum-fn tree 'min 'frame-y))

(defun tree-width (tree)
  (cond ((atom tree) (frame-width tree))
        ((tree-row-split tree)
         ;; in row splits, all children have the same width, so use the
         ;; first one.
         (tree-width (first tree)))
        (t
         ;; for column splits we add the width of each child
         (reduce '+ tree :key 'tree-width))))

(defun tree-height (tree)
  (cond ((atom tree) (frame-height tree))
        ((tree-column-split tree)
         ;; in row splits, all children have the same width, so use the
         ;; first one.
         (tree-height (first tree)))
        (t
         ;; for column splits we add the width of each child
         (reduce '+ tree :key 'tree-height))))

(defun tree-parent (top node)
  "Return the list in TOP that contains NODE."
  (cond ((atom top) nil)
        ((find node top) top)
        (t (loop for i in top
                 thereis (tree-parent i node)))))

(defun tree-leaf (top)
  "Return a leaf of the tree. Use this when you need a leaf but
you don't care which one."
  (tree-accum-fn top
                 (lambda (&rest siblings)
                   (car siblings))
                 #'identity))

(defun tree-row-split (tree)
  "Return t if the children of tree are stacked vertically"
  (loop for i in (cdr tree)
        with head = (car tree)
        always (= (tree-x head) (tree-x i))))

(defun tree-column-split (tree)
  "Return t if the children of tree are side-by-side"
  (loop for i in (cdr tree)
        with head = (car tree)
        always (= (tree-y head) (tree-y i))))

(defun tree-split-type (tree)
  "return :row or :column"
  (cond ((tree-column-split tree) :column)
        ((tree-row-split tree) :row)
        (t (error "tree-split-type unknown"))))

(defun offset-tree (tree x y)
  "move the screen's frames around."
  (tree-iterate tree (lambda (frame)
                       (incf (frame-x frame) x)
                       (incf (frame-y frame) y))))

(defun offset-tree-dir (tree amount dir)
  (ecase dir
    (:left   (offset-tree tree (- amount) 0))
    (:right  (offset-tree tree amount 0))
    (:top    (offset-tree tree 0 (- amount)))
    (:bottom (offset-tree tree 0 amount))))

(defun expand-tree (tree amount dir)
  "expand the frames in tree by AMOUNT in DIR direction. DIR can be :top :bottom :left :right"
  (labels ((expand-frame (f amount dir)
             (ecase dir
               (:left   (decf (frame-x f) amount)
                        (incf (frame-width f) amount))
               (:right  (incf (frame-width f) amount))
               (:top    (decf (frame-y f) amount)
                        (incf (frame-height f) amount))
               (:bottom (incf (frame-height f) amount)))))
    (cond ((null tree) nil)
          ((atom tree)
           (expand-frame tree amount dir))
          ((or (and (find dir '(:left :right))
                    (tree-row-split tree))
               (and (find dir '(:top :bottom))
                    (tree-column-split tree)))
           (dolist (i tree)
             (expand-tree i amount dir)))
          (t
           (let* ((children (if (find dir '(:left :top))
                              (reverse tree)
                              tree))
                  (sz-fn (if (find dir '(:left :right))
                           'tree-width
                           'tree-height))
                  (total (funcall sz-fn tree))
                  (amt-list (loop for i in children
                                  for old-sz = (funcall sz-fn i)
                                  collect (truncate (/ (* amount old-sz) total))))
                  (remainder (- amount (apply '+ amt-list)))
                  (ofs 0))
             ;; spread the remainder out as evenly as possible
             (assert (< remainder (length amt-list)))
             (loop for i upfrom 0
                   while (> remainder 0)
                   do
                   (incf (nth i amt-list))
                   (decf remainder))
             ;; resize proportionally
             (loop for i in children
                   for amt in amt-list
                   do
                   (expand-tree i amt dir)
                   (offset-tree-dir i ofs dir)
                   (incf ofs amt)))))))

(defun join-subtrees (tree leaf)
  "expand the children of tree to occupy the space of
LEAF. Return tree with leaf removed."
  (let* ((others (remove leaf tree))
         (newtree (if (= (length others) 1)
                      (car others)
                      others))
         (split-type (tree-split-type tree))
         (dir (if (eq split-type :column) :right :bottom))
         (ofsdir (if (eq split-type :column) :left :top))
         (amt (if (eq split-type :column)
                  (tree-width leaf)
                  (tree-height leaf)))
         (after (cdr (member leaf tree))))
    ;; align all children after the leaf with the edge of the
    ;; frame before leaf.
    (offset-tree-dir after amt ofsdir)
    (expand-tree newtree amt dir)
    newtree))

(defun resize-tree (tree w h &optional x y)
  "Scale TREE to width W and height H, ignoring aspect. If X and Y are
  provided, reposition the TREE as well."
  (let* ((tw (tree-width tree))
         (th (tree-height tree))
         (wf (/ 1 (/ tw w)))
         (hf (/ 1 (/ th h)))
         (xo (if x (- x (tree-x tree)) 0))
         (yo (if y (- y (tree-y tree)) 0)))
    (tree-iterate tree (lambda (f)
                         (setf (frame-height f) (round (* (frame-height f) hf))
                               (frame-y f) (round (* (frame-y f) hf))
                               (frame-width f) (round (* (frame-width f) wf))
                               (frame-x f) (round (* (frame-x f) wf)))
                         (incf (frame-y f) yo)
                         (incf (frame-x f) xo)))
    (dformat 4 "resize-tree ~Dx~D -> ~Dx~D~%" tw th (tree-width tree) (tree-height tree))))

(defun remove-frame (tree leaf)
  "Return a new tree with LEAF and it's sibling merged into
one."
  (cond ((atom tree) tree)
        ((find leaf tree)
         (join-subtrees tree leaf))
        (t (mapcar (lambda (sib)
                     (remove-frame sib leaf))
                   tree))))

(defun sync-frame-windows (group frame)
  "synchronize windows attached to FRAME."
  (mapc (lambda (w)
          (when (eq (window-frame w) frame)
            (dformat 3 "maximizing ~S~%" w)
            (maximize-window w)))
        (group-windows group)))

(defun sync-all-frame-windows (group)
  "synchronize all frames in GROUP."
  (let ((tree (tile-group-frame-tree group)))
    (tree-iterate tree
                  (lambda (f)
                    (sync-frame-windows group f)))))

(defun sync-head-frame-windows (group head)
  "synchronize all frames in GROUP and HEAD."
  (dolist (f (head-frames group head))
    (sync-frame-windows group f)))

(defun offset-frames (group x y)
  "move the screen's frames around."
  (let ((tree (tile-group-frame-tree group)))
    (tree-iterate tree (lambda (frame)
                         (incf (frame-x frame) x)
                         (incf (frame-y frame) y)))))

(defun resize-frame (group frame amount dim)
  "Resize FRAME by AMOUNT in DIM dimension, DIM can be
either :width or :height"
  (check-type group group)
  (check-type frame frame)
  (check-type amount integer)
  ;; (check-type dim (member :width :height))
  (labels ((max-amount (parent node min dim-fn)
             (dformat 10 "max ~@{~a~^ ~}~%" parent node min dim-fn)
             (if parent
                 (- (funcall dim-fn parent)
                    (funcall dim-fn node)
                    (* min (1- (length parent))))
                 ;; no parent means the frame can't get any bigger.
                 0)))
    (let* ((tree (tile-group-frame-tree group))
           (parent (tree-parent tree frame))
           (gparent (tree-parent tree parent))
           (split-type (tree-split-type parent)))
      (dformat 10 "~s ~s parent: ~s ~s width: ~s h: ~s~%" dim amount split-type parent (tree-width parent) (tree-height parent))
      ;; normalize amount
      (let* ((max (ecase dim
                    (:width
                     (if (>= (frame-width frame) (frame-width (frame-head group frame)))
                         0
                         (if (eq split-type :column)
                             (max-amount parent frame *min-frame-width* 'tree-width)
                             (max-amount gparent parent *min-frame-width* 'tree-width))))
                    (:height
                     (if (>= (frame-height frame) (frame-height (frame-head group frame)))
                         0
                         (if (eq split-type :row)
                             (max-amount parent frame *min-frame-height* 'tree-height)
                             (max-amount gparent parent *min-frame-height* 'tree-height))))))
             (min (ecase dim
                    ;; Frames taking up the entire HEAD in one
                    ;; dimension can't be resized in that dimension.
                    (:width
                     (if (and (eq split-type :row)
                              (or (null gparent)
                                  (>= (frame-width frame) (frame-width (frame-head group frame)))))
                         0
                         (- *min-frame-width* (frame-width frame))))
                    (:height
                     (if (and (eq split-type :column)
                              (or (null gparent)
                                  (>= (frame-height frame) (frame-height (frame-head group frame)))))
                         0
                         (- *min-frame-height* (frame-height frame)))))))
        (setf amount (max (min amount max) min))
        (dformat 10 "bounds ~d ~d ~d~%" amount max min))
      ;; if FRAME is taking up the whole DIM or if AMOUNT = 0, do nothing
      (unless (zerop amount)
        (let* ((resize-parent (or (and (eq split-type :column)
                                       (eq dim :height))
                                  (and (eq split-type :row)
                                       (eq dim :width))))
               (to-resize (if resize-parent parent frame))
               (to-resize-parent (if resize-parent gparent parent))
               (lastp (= (position to-resize to-resize-parent) (1- (length to-resize-parent))))
               (to-shrink (if lastp
                              (prev-sibling to-resize-parent to-resize)
                              (next-sibling to-resize-parent to-resize))))
          (expand-tree to-resize amount (ecase dim
                                          (:width (if lastp :left :right))
                                          (:height (if lastp :top :bottom))))
          (expand-tree to-shrink (- amount) (ecase dim
                                              (:width (if lastp :right :left))
                                              (:height (if lastp :bottom :top))))
          (unless (and *resize-hides-windows* (eq *top-map* *resize-map*))
            (tree-iterate to-resize
                          (lambda (leaf)
                            (sync-frame-windows group leaf)))
            (tree-iterate to-shrink
                          (lambda (leaf)
                            (sync-frame-windows group leaf)))))))))

(defun balance-frames-internal (group tree)
  "Resize all the children of tree to be of equal width or height
depending on the tree's split direction."
  (let* ((split-type (tree-split-type tree))
         (fn (if (eq split-type :column)
                 'tree-width
                 'tree-height))
         (side (if (eq split-type :column)
                   :right
                   :bottom))
         (total (funcall fn tree))
         size rem)
    (multiple-value-setq (size rem) (truncate total (length tree)))
    (loop
     for i in tree
     for j = rem then (1- j)
     for totalofs = 0 then (+ totalofs ofs)
     for ofs = (+ (- size (funcall fn i)) (if (plusp j) 1 0))
     do
     (expand-tree i ofs side)
     (offset-tree-dir i totalofs side)
     (tree-iterate i (lambda (leaf)
                       (sync-frame-windows group leaf))))))

(defun split-frame (group how)
  "split the current frame into 2 frames. return T if it succeeded. NIL otherwise."
  (check-type how (member :row :column))
  (let* ((frame (tile-group-current-frame group))
         (head (frame-head group frame)))
    ;; don't create frames smaller than the minimum size
    (when (or (and (eq how :row)
                   (>= (frame-height frame) (* *min-frame-height* 2)))
              (and (eq how :column)
                   (>= (frame-width frame) (* *min-frame-width* 2))))
      (multiple-value-bind (f1 f2) (funcall (if (eq how :column)
                                                'split-frame-h
                                                'split-frame-v)
                                            group frame)
        (setf (tile-group-frame-head group head)
              (if (atom (tile-group-frame-head group head))
                  (list f1 f2)
                  (funcall-on-node (tile-group-frame-head group head)
                                   (lambda (tree)
                                     (if (eq (tree-split-type tree) how)
                                         (list-splice-replace frame tree f1 f2)
                                         (substitute (list f1 f2) frame tree)))
                                   (lambda (tree)
                                     (unless (atom tree)
                                       (find frame tree))))))
        (migrate-frame-windows group frame f1)
        (choose-new-frame-window f2 group)
        (if (eq (tile-group-current-frame group)
                frame)
            (setf (tile-group-current-frame group) f1))
        (setf (tile-group-last-frame group) f2)
        (sync-frame-windows group f1)
        (sync-frame-windows group f2)
        ;; we also need to show the new window in the other frame
        (when (frame-window f2)
          (unhide-window (frame-window f2)))
        t))))

(defun draw-frame-outline (group f tl br)
  "Draw an outline around FRAME."
  (let* ((screen (group-screen group))
         (win (if (frame-window f) (window-xwin (frame-window f)) (screen-root screen)))
         (width (screen-frame-outline-width screen))
         (gc (screen-frame-outline-gc screen))
         (halfwidth (/ width 2)))
    (let ((x (frame-x f))
          (y (frame-display-y group f))
          (w (frame-width f))
          (h (frame-display-height group f)))
      (when tl
        (xlib:draw-line win gc
                        x (+ halfwidth y) w 0 t)
        (xlib:draw-line win gc
                        (+ halfwidth x) y 0 h t))
      (when br
        (xlib:draw-line win gc
                        (+ x (- w halfwidth)) y 0 h t)
        (xlib:draw-line win gc
                        x (+ y (- h halfwidth)) w 0 t)))))

(defun draw-frame-outlines (group &optional head)
  "Draw an outline around all frames in GROUP."
  (clear-frame-outlines group)
  (dolist (h (if head (list head) (group-heads group)))
    (draw-frame-outline group h nil t)
    (tree-iterate (tile-group-frame-head group h) (lambda (f)
                                                    (draw-frame-outline group f t nil)))))

(defun clear-frame-outlines (group)
  "Clear the outlines drawn with DRAW-FRAME-OUTLINES."
  (xlib:clear-area (screen-root (group-screen group))))

(defun draw-frame-numbers (group)
  "Draw the number of each frame in its corner. Return the list of
windows used to draw the numbers in. The caller must destroy them."
  (let ((screen (group-screen group)))
    (mapcar (lambda (f)
              (let ((w (xlib:create-window
                        :parent (screen-root screen)
                        :x (frame-x f) :y (frame-display-y group f) :width 1 :height 1
                        :background (screen-fg-color screen)
                        :border (screen-border-color screen)
                        :border-width 1
                        :event-mask '())))
                (xlib:map-window w)
                (setf (xlib:window-priority w) :above)
                (echo-in-window w (screen-font screen)
                                (screen-fg-color screen)
                                (screen-bg-color screen)
                                (string (get-frame-number-translation f)))
                (xlib:display-finish-output *display*)
                (dformat 3 "mapped ~S~%" (frame-number f))
                w))
            (group-frames group))))


;;; Screen functions

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
  (setf *screen-list* (remove screen *screen-list*))
  (push screen *screen-list*))

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
                   (screen-input-window screen)))
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

(defun unmap-message-window (screen)
  "Unmap the screen's message window, if it is mapped."
  (unless (eq (xlib:window-map-state (screen-message-window screen)) :unmapped)
    (xlib:unmap-window (screen-message-window screen))))

(defun unmap-all-message-windows ()
  (mapc #'unmap-message-window *screen-list*)
  (when (timer-p *message-window-timer*)
    (cancel-timer *message-window-timer*)
    (setf *message-window-timer* nil)))

(defun unmap-frame-indicator-window (screen)
  "Unmap the screen's message window, if it is mapped."
;;  (unless (eq (xlib:window-map-state (screen-frame-window screen)) :unmapped)
    (xlib:unmap-window (screen-frame-window screen)))

(defun unmap-all-frame-indicator-windows ()
  (mapc #'unmap-frame-indicator-window *screen-list*)
  (when (timer-p *frame-indicator-timer*)
    (cancel-timer *frame-indicator-timer*)
    (setf *frame-indicator-timer* nil)))

(defun reset-message-window-timer ()
  "Set the message window timer to timeout in *timeout-wait* seconds."
  (unless *ignore-echo-timeout*
    (when (timer-p *message-window-timer*)
      (cancel-timer *message-window-timer*))
    (setf *message-window-timer* (run-with-timer *timeout-wait* nil
                                                 'unmap-all-message-windows))))

(defun reset-frame-indicator-timer ()
  "Set the message window timer to timeout in *timeout-wait* seconds."
  (when (timer-p *frame-indicator-timer*)
    (cancel-timer *frame-indicator-timer*))
  (setf *frame-indicator-timer* (run-with-timer *timeout-frame-indicator-wait* nil
                                                'unmap-all-frame-indicator-windows)))

(defun show-frame-outline (group &optional (clear t))
  ;; Don't draw if this isn't a current group!
  (when (find group (mapcar 'screen-current-group *screen-list*))
    (dformat 5 "show-frame-outline!~%")
    ;; *resize-hides-windows* uses the frame outlines for display,
    ;; so try not to interfere.
    (unless (eq *top-map* *resize-map*)
      (when clear
        (clear-frame-outlines group))
      (let ((frame (tile-group-current-frame group)))
        (unless (and (= 1 (length (tile-group-frame-tree group)))
                     (atom (first (tile-group-frame-tree group))))
          ;; draw the outline
          (unless (frame-window frame)
            (draw-frame-outline group frame t t)))))))

(defun show-frame-indicator (group &optional force)
  (show-frame-outline group)
  ;; FIXME: Arg, these tests are already done in show-frame-outline
  (when (find group (mapcar 'screen-current-group *screen-list*))
    (when (or force
              (and (or (> 1 (length (tile-group-frame-tree group)))
                       (not (atom (first (tile-group-frame-tree group)))))
                   (not *suppress-frame-indicator*)))
      (let ((frame (tile-group-current-frame group))
            (w (screen-frame-window (current-screen)))
            (string *frame-indicator-text*)
            (font (screen-font (current-screen))))
        (xlib:with-state (w)
          (setf (xlib:drawable-x w) (+ (frame-x frame)
                                       (truncate (- (frame-width frame) (xlib:text-width font string)) 2))
                (xlib:drawable-y w) (+ (frame-y frame)
                                       (truncate (- (frame-height frame) (font-height font)) 2))
                (xlib:window-priority w) :above))
        (xlib:map-window w)
        (echo-in-window w font (screen-fg-color (current-screen)) (screen-bg-color (current-screen)) string)
        (reset-frame-indicator-timer)))))

(defun echo-in-window (win font fg bg string)
  (let* ((height (font-height font))
         (gcontext (xlib:create-gcontext :drawable win
                                         :font font
                                         :foreground fg
                                         :background bg))
         (width (xlib:text-width font string)))
    (xlib:with-state (win)
      (setf (xlib:drawable-height win) height
            (xlib:drawable-width win) width))
    (xlib:clear-area win)
    (xlib:display-finish-output *display*)
    (xlib:draw-image-glyphs win gcontext 0 (xlib:font-ascent font) string)))

(defun push-last-message (screen strings highlights)
  ;; only push unique messages
  (unless *record-last-msg-override*
    (push strings (screen-last-msg screen))
    (push highlights (screen-last-msg-highlights screen))
    ;; crop for size
    (when (>= (length (screen-last-msg screen)) *max-last-message-size*)
      (setf (screen-last-msg screen) (butlast (screen-last-msg screen)))
      (setf (screen-last-msg-highlights screen) (butlast (screen-last-msg-highlights screen))))))

(defun redraw-current-message (screen)
  (let ((*record-last-msg-override* t)
        (*ignore-echo-timeout* t))
    (dformat 5 "Redrawing message window!~%")
    (apply 'echo-string-list screen (screen-current-msg screen) (screen-current-msg-highlights screen))))

(defun echo-nth-last-message (screen n)
  (let ((*record-last-msg-override* t))
    (apply 'echo-string-list screen (nth n (screen-last-msg screen)) (nth n (screen-last-msg-highlights screen)))))

(defun echo-string-list (screen strings &rest highlights)
  "Draw each string in l in the screen's message window. HIGHLIGHT is
  the nth entry to highlight."
  (unless *executing-stumpwm-command*
    (let ((width (render-strings screen (screen-message-cc screen) *message-window-padding* 0 strings '() nil)))
      (setup-message-window screen (length strings) width)
      (render-strings screen (screen-message-cc screen) *message-window-padding* 0 strings highlights))
    (setf (screen-current-msg screen)
          strings
          (screen-current-msg-highlights screen)
          highlights))
  (push-last-message screen strings highlights)
  (xlib:display-finish-output *display*)
  ;; Set a timer to hide the message after a number of seconds
  (if *suppress-echo-timeout*
      ;; any left over timers need to be canceled.
      (when (timer-p *message-window-timer*)
        (cancel-timer *message-window-timer*)
        (setf *message-window-timer* nil))
      (reset-message-window-timer))
  (apply 'run-hook-with-args *message-hook* strings))

(defun echo-string (screen msg)
  "Display @var{string} in the message bar on @var{screen}. You almost always want to use @command{message}."
  (echo-string-list screen (split-string msg (string #\Newline))))

(defun message (fmt &rest args)
  "run FMT and ARGS through `format' and echo the result to the current screen."
  (echo-string (current-screen) (apply 'format nil fmt args)))


(defun err (fmt &rest args)
  "run FMT and ARGS through format and echo the result to the
current screen along with a backtrace. For careful study, the
message does not time out."
  (let ((*suppress-echo-timeout* t))
    (echo-string (current-screen)
                 (concat (apply 'format nil fmt args)
                         (backtrace-string)))))

(defun message-no-timeout (fmt &rest args)
  "Like message, but the window doesn't disappear after a few seconds."
  (let ((*suppress-echo-timeout* t))
    (apply 'message fmt args)))

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
    (let* ((screen (make-screen))
           (fg (ac +default-foreground-color+))
           (bg (ac +default-background-color+))
           (border (ac +default-border-color+))
           (focus (ac +default-focus-color+))
           (unfocus (ac +default-unfocus-color+))
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
           (font (xlib:open-font *display* +default-font-name+))
           (group (make-tile-group
                   :screen screen
                   :number 1
                   :name "Default")))
      ;; Create our screen structure
      ;; The focus window is mapped at all times
      (xlib:map-window focus-window)
      (xlib:map-window key-window)
      (xwin-grab-keys focus-window)
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
      screen)))


;;; Head functions

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
  (dolist (head (screen-heads (group-screen group)))
    (when (and
           (>= (frame-x frame) (frame-x head))
           (>= (frame-y frame) (frame-y head))
           (<= (+ (frame-x frame) (frame-width frame))
               (+ (frame-x head) (frame-width head)))
           (<= (+ (frame-y frame) (frame-height frame))
               (+ (frame-y head) (frame-height head))))
      (return head))))

(defun group-heads (group)
  (screen-heads (group-screen group)))

(defun tile-group-frame-head (group head)
  (elt (tile-group-frame-tree group) (position head (group-heads group))))

(defun (setf tile-group-frame-head) (frame group head)
  (setf (elt (tile-group-frame-tree group) (position head (group-heads group))) frame))

(defun current-head (&optional (group (current-group)))
  (frame-head group (tile-group-current-frame group)))

(defun head-windows (group head)
  "Returns a list of windows on HEAD of GROUP"
  (remove-if-not
   (lambda (w)
     (eq head (frame-head group (window-frame w))))
   (group-windows group)))

(defun frame-is-head (group frame)
  (< (frame-number frame) (length (group-heads group))))

(defun add-head (screen head)
  (dformat 1 "Adding head #~D~%" (head-number head))
  (setf (screen-heads screen) (sort (push head (screen-heads screen)) #'< :key 'head-number))
  (dolist (group (screen-groups screen))
    (setf (tile-group-frame-tree group)
          (sort (push (copy-frame head) (tile-group-frame-tree group))
                #'< :key (lambda (tile)
                           (if (atom tile)
                               (frame-number tile)
                               (frame-number (car tile))))))
    ;; Try to put something in the new frame
    (let ((frame (tile-group-frame-head group head)))
      (choose-new-frame-window frame group)
      (when (frame-window frame)
        (unhide-window (frame-window frame))))))

(defun remove-head (screen head)
  (dformat 1 "Removing head #~D~%" (head-number head))
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
  (when (head-mode-line head)
    (toggle-mode-line screen head))
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


;;; keyboard helper functions

(defun key-to-keycode+state (key)
  (let ((code (xlib:keysym->keycodes *display* (key-keysym key))))
    (cond ((eq (xlib:keycode->keysym *display* code 0) (key-keysym key))
           (values code (x11-mods key)))
          ((eq (xlib:keycode->keysym *display* code 1) (key-keysym key))
           (values code (apply 'xlib:make-state-mask
                               (cons :shift (xlib:make-state-keys (x11-mods key))))))
          (t
           ;; just warn them and go ahead as scheduled
           (warn "Don't know how to encode ~s" key)
           (values code (x11-mods key))))))

(defun send-fake-key (win key)
  "Send a fake key event to win. ch is the character and mods is a
list of modifier symbols."
  (multiple-value-bind (code state) (key-to-keycode+state key)
    (xlib:send-event (window-xwin win) :key-press (xlib:make-event-mask :key-press)
                     :display *display*
                     :root (screen-root (window-screen win))
                     ;; Apparently we need these in here, though they
                     ;; make no sense for a key event.
                     :x 0 :y 0 :root-x 0 :root-y 0
                     :window (window-xwin win) :event-window (window-xwin win)
                     :code code
                     :state state)))

(defun send-fake-click (win button)
  "Send a fake key event to win. ch is the character and mods is a
list of modifier symbols."
  ;; I don't know why this doesn't work. Sadly CLX doesn't have the
  ;; XTest extension like xlib does. With it this would be 2 lines.
  (multiple-value-bind (x y) (xlib:query-pointer (window-xwin win))
    (multiple-value-bind (rx ry) (xlib:query-pointer (screen-root (window-screen win)))
      (xlib:send-event (window-xwin win) :button-press (xlib:make-event-mask :button-press)
                       :display *display*
                       :root (screen-root (window-screen win))
                       :window (window-xwin win) :event-window (window-xwin win)
                       :code button
                       :state 0
                       :x x :y y :root-x rx :root-y ry
                       :same-screen-p t)
      (xlib:send-event (window-xwin win) :button-release (xlib:make-event-mask :button-release)
                       :display *display*
                       :root (screen-root (window-screen win))
                       :window (window-xwin win) :event-window (window-xwin win)
                       :code button
                       :state #x100
                       :x x :y y :root-x rx :root-y ry
                       :same-screen-p t))))


;;; Pointer helper functions

(defun grab-pointer (screen)
  "Grab the pointer and set the pointer shape."
  (let* ((white (xlib:make-color :red 1.0 :green 1.0 :blue 1.0))
         (black (xlib:make-color :red 0.0 :green 0.0 :blue 0.0))
         (cursor-font (xlib:open-font *display* "cursor"))
         (cursor (xlib:create-glyph-cursor :source-font cursor-font
                                           :source-char 64
                                           :mask-font cursor-font
                                           :mask-char 65
                                           :foreground black
                                           :background white)))
    (xlib:grab-pointer (screen-root screen) nil :owner-p nil
                       :cursor cursor)))

(defun ungrab-pointer ()
  "Remove the grab on the cursor and restore the cursor shape."
  (xlib:ungrab-pointer *display*)
  (xlib:display-finish-output *display*))

(defun grab-keyboard (screen)
  (let ((ret (xlib:grab-keyboard (screen-root screen) :owner-p nil
                                 :sync-keyboard-p nil :sync-pointer-p nil)))
    (dformat 5 "vvv Grab keyboard: ~s~%" ret)
    ret))

(defun ungrab-keyboard ()
  (let ((ret (xlib:ungrab-keyboard *display*)))
    (dformat 5 "^^^ Ungrab keyboard: ~s~%" ret)
    ret))

(defun warp-pointer (screen x y)
  "Move the pointer to the specified location."
  (let ((root (screen-root screen)))
    (xlib:warp-pointer root x y)))

(defun warp-pointer-relative (dx dy)
  "Move the pointer by DX and DY relative to the current location."
  (xlib:warp-pointer-relative *display* dx dy))


;; Event handler functions

(defparameter *event-fn-table* (make-hash-table)
  "A hash of event types to functions")

(defmacro define-stump-event-handler (event keys &body body)
  (let ((fn-name (gensym))
        (event-slots (gensym)))
    `(labels ((,fn-name (&rest ,event-slots &key ,@keys &allow-other-keys)
               (declare (ignore ,event-slots))
               ,@body))
      (setf (gethash ,event *event-fn-table*) #',fn-name))))

                                        ;(define-stump-event-handler :map-notify (event-window window override-redirect-p)
                                        ;  )

(defun handle-mode-line-window (xwin x y width height)
  (declare (ignore width))
  (let ((ml (find-mode-line-window xwin)))
    (when ml
      (setf (xlib:drawable-height xwin) height)
      (update-mode-line-position ml x y)
      (resize-mode-line ml)
      (sync-mode-line ml))))

(defun handle-unmanaged-window (xwin x y width height border-width value-mask)
  "Call this function for windows that stumpwm isn't
  managing. Basically just give the window what it wants."
  (labels ((has-x (mask) (= 1 (logand mask 1)))
           (has-y (mask) (= 2 (logand mask 2)))
           (has-w (mask) (= 4 (logand mask 4)))
           (has-h (mask) (= 8 (logand mask 8)))
           (has-bw (mask) (= 16 (logand mask 16)))
           ;; (has-stackmode (mask) (= 64 (logand mask 64)))
           )
    (xlib:with-state (xwin)
      (when (has-x value-mask)
        (setf (xlib:drawable-x xwin) x))
      (when (has-y value-mask)
        (setf (xlib:drawable-y xwin) y))
      (when (has-h value-mask)
        (setf (xlib:drawable-height xwin) height))
      (when (has-w value-mask)
        (setf (xlib:drawable-width xwin) width))
      (when (has-bw value-mask)
        (setf (xlib:drawable-border-width xwin) border-width)))))

(defun update-configuration (win)
  ;; Send a synthetic configure-notify event so that the window
  ;; knows where it is onscreen.
  (xwin-send-configuration-notify (window-xwin win)
                                  (xlib:drawable-x (window-parent win))
                                  (xlib:drawable-y (window-parent win))
                                  (window-width win) (window-height win) 0))

(defun handle-managed-window (window width height stack-mode value-mask)
  "This is a managed window so deal with it appropriately."
  ;; Grant the stack-mode change (if it's mapped)
  (set-window-geometry window :width width :height height)
  (maximize-window window)
  (when (and (window-in-current-group-p window)
             ;; stack-mode change?
             (= 64 (logand value-mask 64)))
    (case stack-mode
      (:above
       (maybe-raise-window window))))
  (update-configuration window))

(defun handle-window-move (win x y relative-to &optional (value-mask -1))
  (when *honor-window-moves*
    (dformat 3 "Window requested new position ~D,~D relative to ~S~%" x y relative-to)
    (labels ((has-x (mask) (= 1 (logand mask 1)))
             (has-y (mask) (= 2 (logand mask 2))))
      (when (or (eq relative-to :root) (has-x value-mask) (has-y value-mask))
        (let* ((group (window-group win))
               (pos  (if (eq relative-to :parent)
                         (list
                          (+ (xlib:drawable-x (window-parent win)) x)
                          (+ (xlib:drawable-y (window-parent win)) y))
                         (list x y)))
               (frame (apply #'find-frame group pos)))
          (when frame
            (pull-window win frame)))))))

(define-stump-event-handler :configure-request (stack-mode #|parent|# window #|above-sibling|# x y width height border-width value-mask)
  ;; Grant the configure request but then maximize the window after the granting.
  (dformat 3 "CONFIGURE REQUEST ~@{~S ~}~%" stack-mode window x y width height border-width value-mask)
  (let ((win (find-window window)))
    (cond
      (win
       (handle-window-move win x y :parent value-mask)
       (handle-managed-window win width height stack-mode value-mask))
      ((handle-mode-line-window window x y width height))
      (t (handle-unmanaged-window window x y width height border-width value-mask)))))

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

(define-stump-event-handler :configure-notify (stack-mode #|parent|# window #|above-sibling|# x y width height border-width value-mask)
  (dformat 4 "CONFIGURE NOTIFY ~@{~S ~}~%" stack-mode window x y width height border-width value-mask)
  (let ((screen (find-screen window)))
    (when screen
      (let ((old-heads (copy-list (screen-heads screen))))
        (setf (screen-heads screen) nil)
        (let ((new-heads (make-screen-heads screen (screen-root screen))))
          (setf (screen-heads screen) old-heads)
          (cond
            ((equalp old-heads new-heads)
             (dformat 3 "Bogus configure-notify on root window of ~S~%" screen) t)
            (t
             (dformat 1 "Updating Xinerama configuration for ~S.~%" screen)
             (if new-heads
                 (progn
                   (scale-screen screen new-heads)
                   (mapc 'sync-all-frame-windows (screen-groups screen))
                   (update-mode-lines screen))
                 (dformat 1 "Invalid configuration! ~S~%" new-heads)))))))))

(define-stump-event-handler :map-request (parent send-event-p window)
  (unless send-event-p
    ;; This assumes parent is a root window and it should be.
    (dformat 3 "map request: ~a ~a ~a~%" window parent (find-window window))
    (let ((screen (find-screen parent))
          (win (find-window window))
          (wwin (find-withdrawn-window window)))
      ;; only absorb it if it's not already managed (it could be iconic)
      (cond
        (win (dformat 1 "map request for mapped window ~a~%" win))
        ((eq (xwin-type window) :dock)
         (when wwin
           (setf screen (window-screen wwin)))
         (dformat 1 "window is dock-type. attempting to place in mode-line.")
         (place-mode-line-window screen window)
         ;; Some panels are broken and only set the dock type after they map and withdraw.
         (when wwin
           (setf (screen-withdrawn-windows screen) (delete wwin (screen-withdrawn-windows screen))))
         t)
        (wwin (restore-window wwin))
        ((xlib:get-property window :_KDE_NET_WM_SYSTEM_TRAY_WINDOW_FOR)
         ;; Do nothing if this is a systray window (the system tray
         ;; will handle it, if there is one, and, if there isn't the
         ;; user doesn't want this popping up as a managed window
         ;; anyway.
         t)
        (t
         (let ((window (process-mapped-window screen window)))
           ;; Give it focus
           (if (deny-request-p window *deny-map-request*)
               (unless *suppress-deny-messages*
                 (if (eq (window-group window) (current-group))
                     (echo-string (window-screen window) (format nil "'~a' denied map request" (window-name window)))
                     (echo-string (window-screen window) (format nil "'~a' denied map request in group ~a" (window-name window) (group-name (window-group window))))))
               (frame-raise-window (window-group window) (window-frame window) window
                                   (if (eq (window-frame window)
                                           (tile-group-current-frame (window-group window)))
                                       t nil)))))))))

(define-stump-event-handler :unmap-notify (send-event-p event-window window #|configure-p|#)
  ;; There are two kinds of unmap notify events: the straight up
  ;; ones where event-window and window are the same, and
  ;; substructure unmap events when the event-window is the parent
  ;; of window.
  (dformat 2 "UNMAP: ~s ~s ~a~%" send-event-p (not (xlib:window-equal event-window window)) (find-window window))
  (unless (and (not send-event-p)
               (not (xlib:window-equal event-window window)))
    (let ((window (find-window window)))
      ;; if we can't find the window then there's nothing we need to
      ;; do.
      (when window
        (if (plusp (window-unmap-ignores window))
            (progn
              (dformat 3 "decrement ignores! ~d~%" (window-unmap-ignores window))
              (decf (window-unmap-ignores window)))
            (withdraw-window window))))))

;;(define-stump-event-handler :create-notify (#|window parent x y width height border-width|# override-redirect-p))
;; (unless (or override-redirect-p
;;          (internal-window-p (window-screen window) window))
;;    (process-new-window (window-screen window) window))
;;    (run-hook-with-args *new-window-hook* window)))

(define-stump-event-handler :destroy-notify (send-event-p event-window window)
  (unless (or send-event-p
              (xlib:window-equal event-window window))
    ;; Ignore structure destroy notifies and only
    ;; use substructure destroy notifiers. This way
    ;; event-window is the window's parent.
    (let ((win (or (find-window window)
                   (find-withdrawn-window window))))
      (if win
          (destroy-window win)
          (progn
            (let ((ml (find-mode-line-window window)))
              (when ml (destroy-mode-line-window ml))))))))

(defun read-from-keymap (kmap &optional update-fn)
  "Read a sequence of keys from the user, guided by the keymap,
KMAP and return the binding or nil if the user hit an unbound sequence.

The Caller is responsible for setting up the input focus."
  (let* ((code-state (read-key-no-modifiers))
         (code (car code-state))
         (state (cdr code-state)))
    (handle-keymap kmap code state nil nil update-fn)))

(defun handle-keymap (kmap code state key-seq grab update-fn)
  "Find the command mapped to the (code state) and return it."
  ;; a symbol is assumed to have a hashtable as a value.
  (dformat 1 "Awaiting key ~a~%" kmap)
  (let ((keymap '()))
    (when (and (symbolp kmap)
               (boundp kmap)
               (hash-table-p (symbol-value kmap)))
      (setf
       keymap kmap
       kmap (symbol-value kmap)))
    (check-type kmap hash-table)
    (let* ((key (code-state->key code state))
           (cmd (lookup-key kmap key))
           (key-seq (cons key key-seq)))
      (dformat 1 "key-press: ~S ~S ~S~%" key state cmd)
      (run-hook-with-args *key-press-hook* key key-seq cmd)
      (when update-fn
        (funcall update-fn key-seq))
      (if cmd
          (cond
            ((or (hash-table-p cmd)
                 (and (symbolp cmd)
                      (boundp cmd)
                      (hash-table-p (symbol-value cmd))))
             (when grab
               (grab-pointer (current-screen)))
             (let* ((code-state (read-key-no-modifiers))
                    (code (car code-state))
                    (state (cdr code-state)))
               (unwind-protect
                    (handle-keymap cmd code state key-seq nil update-fn)
                 (when grab (ungrab-pointer)))))
            (t (values cmd key-seq)))
          (if (find key (list (kbd "?")
                              (kbd "C-h"))
                    :test 'equalp)
              (progn (display-keybinding keymap) (values t key-seq))
              (values nil key-seq))))))

(define-stump-event-handler :key-press (code state #|window|#)
  (labels ((get-cmd (code state)
             (with-focus (screen-key-window (current-screen))
               (handle-keymap *top-map* code state nil t nil))))
    (unwind-protect
         ;; modifiers can sneak in with a race condition. so avoid that.
         (unless (is-modifier code)
           (multiple-value-bind (cmd key-seq) (get-cmd code state)
             (cond
               ((eq cmd t))
               (cmd
                (unmap-message-window (current-screen))
                (interactive-command cmd) t)
               (t (message "~{~a ~}not bound." (mapcar 'print-key (nreverse key-seq))))))))))

(defun bytes-to-window (bytes)
  "A sick hack to assemble 4 bytes into a 32 bit number. This is
because ratpoison sends the rp_command_request window in 8 byte
chunks."
  (+ (first bytes)
     (ash (second bytes) 8)
     (ash (third bytes) 16)
     (ash (fourth bytes) 24)))

(defun handle-rp-commands (root)
  "Handle a ratpoison style command request."
  (labels ((one-cmd ()
             (multiple-value-bind (win type format bytes-after) (xlib:get-property root :rp_command_request :end 4 :delete-p t)
               (declare (ignore type format))
               (setf win (xlib::lookup-window *display* (bytes-to-window win)))
               (when (xlib:window-p win)
                 (let* ((data (xlib:get-property win :rp_command))
                        (interactive-p (car data))
                        (cmd (map 'string 'code-char (nbutlast (cdr data)))))
                   (declare (ignore interactive-p))
                   (interactive-command cmd)
                   (xlib:change-property win :rp_command_result (map 'list 'char-code "0TODO") :string 8)
                   (xlib:display-finish-output *display*)))
               bytes-after)))
    (loop while (> (one-cmd) 0))))

(defun handle-stumpwm-commands (root)
  "Handle a StumpWM style command request."
  (let* ((win root)
         (screen (find-screen root))
         (data (xlib:get-property win :stumpwm_command :delete-p t))
         (cmd (map 'string 'code-char data)))
    (let ((msgs (screen-last-msg screen))
          (hlts (screen-last-msg-highlights screen))
          (*executing-stumpwm-command* t))
      (setf (screen-last-msg screen) '()
            (screen-last-msg-highlights screen) '())
      (interactive-command cmd)
      (xlib:change-property win :stumpwm_command_result (map 'list 'char-code (format nil "~{~{~a~%~}~}" (nreverse (screen-last-msg screen)))) :string 8)
      (setf (screen-last-msg screen) msgs
            (screen-last-msg-highlights screen) hlts))
    (xlib:display-finish-output *display*)))

(defun update-window-properties (window atom)
  (case atom
    (:wm_name
     (setf (window-title window) (xwin-name (window-xwin window)))
     ;; Let the mode line know about the new name.
     (update-all-mode-lines))
    (:wm_normal_hints
     (setf (window-normal-hints window) (xlib:wm-normal-hints (window-xwin window))
           (window-type window) (xwin-type (window-xwin window)))
     (dformat 4 "new hints: ~s~%" (window-normal-hints window))
     (maximize-window window))
    (:wm_hints)
    (:wm_class
     (setf (window-class window) (xwin-class (window-xwin window))
           (window-res window) (xwin-res-name (window-xwin window))))
    (:wm_window_role
     (setf (window-role window) (xwin-role (window-xwin window))))
    (:wm_transient_for
     (setf (window-type window) (xwin-type (window-xwin window)))
     (maximize-window window))
    (:_NET_WM_STATE
     ;; Client is broken and sets this property itself instead of sending a
     ;; client request to the root window. Try to make do.
     (dolist (p (xlib:get-property (window-xwin window) :_NET_WM_STATE))
       (case (xlib:atom-name *display* p)
         (:_NET_WM_STATE_FULLSCREEN
          ;; FIXME: what about when properties are REMOVED?
          (update-fullscreen window 1)))))))

(define-stump-event-handler :property-notify (window atom state)
  (dformat 2 "property notify ~s ~s ~s~%" window atom state)
  (case atom
    (:rp_command_request
     ;; we will only find the screen if window is a root window, which
     ;; is the only place we listen for ratpoison commands.
     (let* ((screen (find-screen window)))
       (when (and (eq state :new-value)
                  screen)
         (handle-rp-commands window))))
    (:stumpwm_command
     ;; RP commands are too weird and problematic, KISS.
     (let* ((screen (find-screen window)))
       (when (and (eq state :new-value)
                  screen)
         (handle-stumpwm-commands window))))
    (t
     (let ((window (find-window window)))
       (when window
         (update-window-properties window atom))))))

(define-stump-event-handler :mapping-notify (request start count)
  ;; We could be a bit more intelligent about when to update the
  ;; modifier map, but I don't think it really matters.
  (xlib:mapping-notify *display* request start count)
  (update-modifier-map)
  (sync-keys))

(define-stump-event-handler :selection-request (requestor property selection target time)
  (send-selection requestor property selection target time))

(define-stump-event-handler :selection-clear ()
  (setf *x-selection* nil))

(defun find-message-window-screen (win)
  "Return the screen, if any, that message window WIN belongs."
  (dolist (screen *screen-list*)
    (when (xlib:window-equal (screen-message-window screen) win)
      (return screen))))

(defun draw-cross (screen window x y width height)
  (xlib:draw-line window
                  (screen-frame-outline-gc screen)
                  x y
                  width height
                  t)
  (xlib:draw-line window
                  (screen-frame-outline-gc screen)
                  x (+ y height)
                  (+ x width) y))

(define-stump-event-handler :exposure (window x y width height count)
  (let (screen ml)
    (when (zerop count)
      (cond
        ((setf screen (find-screen window))
         ;; root exposed
         (show-frame-outline (screen-current-group screen) nil))
        ((setf screen (find-message-window-screen window))
         ;; message window exposed
         (if (plusp (screen-ignore-msg-expose screen))
             (decf (screen-ignore-msg-expose screen))
             (redraw-current-message screen)))
        ((setf ml (find-mode-line-window window))
         (setf screen (mode-line-screen ml))
         (redraw-mode-line ml t)))
      ;; Show the area.
      (when (and *debug-expose-events* screen)
        (draw-cross screen window x y width height)))))


(define-stump-event-handler :reparent-notify (window parent)
  (let ((win (find-window window)))
    (when (and win
               (not (xlib:window-equal parent (window-parent win))))
      ;; reparent it back
      (unless (eq (xlib:window-map-state (window-xwin win)) :unmapped)
        (incf (window-unmap-ignores win)))
      (xlib:reparent-window (window-xwin win) (window-parent win) 0 0))))


;;; Fullscreen functions

(defun activate-fullscreen (window)
  (dformat 2 "client requests to go fullscreen~%")
  (add-wm-state (window-xwin window) :_NET_WM_STATE_FULLSCREEN)
  (setf (window-fullscreen window) t)
  (maximize-window window)
  (focus-window window))

(defun deactivate-fullscreen (window)
  (setf (window-fullscreen window) nil)
  (dformat 2 "client requests to leave fullscreen~%")
  (remove-wm-state (window-xwin window) :_NET_WM_STATE_FULLSCREEN)
  (setf (xlib:drawable-border-width (window-parent window)) (default-border-width-for-type (window-type window)))
  (maximize-window window)
  (update-window-border window)
  (update-mode-lines (current-screen)))

(defun update-fullscreen (window action)
  (let ((fullscreen-p (window-fullscreen window)))
    (case action
      (0                                ; _NET_WM_STATE_REMOVE
       (when fullscreen-p
         (deactivate-fullscreen window)))
      (1                                ; _NET_WM_STATE_ADD
       (unless fullscreen-p
         (activate-fullscreen window)))
      (2                                ; _NET_WM_STATE_TOGGLE
       (if fullscreen-p
           (deactivate-fullscreen window)
           (activate-fullscreen window))))))


(defun maybe-raise-window (window)
  (if (deny-request-p window *deny-raise-request*)
      (unless (or *suppress-deny-messages*
                  ;; don't mention windows that are already visible
                  (eq (frame-window (window-frame window)) window))
        (if (eq (window-group window) (current-group))
            (echo-string (window-screen window) (format nil "'~a' denied raise request" (window-name window)))
            (echo-string (window-screen window) (format nil "'~a' denied raise request in group ~a" (window-name window) (group-name (window-group window))))))
      (focus-all window)))

(define-stump-event-handler :client-message (window type #|format|# data)
  (dformat 2 "client message: ~s ~s~%" type data)
  (case type
    (:_NET_CURRENT_DESKTOP              ;switch desktop
     (let* ((screen (find-screen window))
            (n (elt data 0))
            (group (and screen
                        (< n (length (screen-groups screen)))
                        (elt (sort-groups screen) n))))
       (when group
         (switch-to-group group))))
    (:_NET_WM_DESKTOP                   ;move window to desktop
     (let* ((our-window (find-window window))
            (screen (when our-window
                      (window-screen our-window)))
            (n (elt data 0))
            (group (and screen
                        (< n (length (screen-groups screen)))
                        (elt (sort-groups screen) n))))
       (when (and our-window group)
         (move-window-to-group our-window group))))
    (:_NET_ACTIVE_WINDOW
     (let ((our-window (find-window window))
           (source (elt data 0)))
       (when our-window
         (if (= source 2)               ;request is from a pager
             (focus-all our-window)
             (maybe-raise-window our-window)))))
    (:_NET_CLOSE_WINDOW
     (let ((our-window (find-window window)))
       (when our-window
         (delete-window our-window))))
    (:_NET_WM_STATE
     (let ((our-window (find-window window)))
       (when our-window
         (let ((action (elt data 0))
               (p1 (elt data 1))
               (p2 (elt data 2)))
           (dolist (p (list p1 p2))
             (unless (= p 0)
               (case (xlib:atom-name *display* p)
                 (:_NET_WM_STATE_FULLSCREEN
                  (update-fullscreen our-window action)))))))))
    (:_NET_MOVERESIZE_WINDOW
     (let ((our-window (find-window window)))
       (when our-window
         (let ((x (elt data 1))
               (y (elt data 2)))
           (dformat 3 "!!! Data: ~S~%" data)
           (handle-window-move our-window x y :relative :root)))))
    (t
     (dformat 2 "ignored message~%"))))

(define-stump-event-handler :focus-out (window mode kind)
  (dformat 5 "~@{~s ~}~%" window mode kind))

;;; Mouse focus

(defun focus-all (win)
  "Focus the window, frame, group and screen belonging to WIN. Raise
the window in it's frame."
  (when (and win (window-frame win))
    (unmap-message-window (window-screen win))
    (switch-to-screen (window-screen win))
    (let ((frame (window-frame win))
          (group (window-group win)))
      (switch-to-group group)
      (frame-raise-window group frame win))))

(define-stump-event-handler :enter-notify (window mode)
  (when (and window (eq mode :normal) (eq *mouse-focus-policy* :sloppy))
    (let ((win (find-window window)))
      (when (and win (find win (top-windows)))
        (focus-all win)))))

(define-stump-event-handler :button-press (window code x y child time)
  ;; Pass click to client
  (xlib:allow-events *display* :replay-pointer time)
  (let (screen ml win)
    (cond
      ((and (setf screen (find-screen window)) (not child))
       (when (and (eq *mouse-focus-policy* :click)
                  *root-click-focuses-frame*)
         (let* ((group (screen-current-group screen))
                (frame (find-frame group x y)))
           (when frame
             (focus-frame group frame))))
       (run-hook-with-args *root-click-hook* screen code x y))
      ((setf ml (find-mode-line-window window))
       (run-hook-with-args *mode-line-click-hook* ml code x y))
      ((setf win (find-window-by-parent window (visible-windows)))
       (when (eq *mouse-focus-policy* :click)
         (focus-all win))))))

;; Handling event :KEY-PRESS
;; (:DISPLAY #<XLIB:DISPLAY :0 (The X.Org Foundation R60700000)> :EVENT-KEY :KEY-PRESS :EVENT-CODE 2 :SEND-EVENT-P NIL :CODE 45 :SEQUENCE 1419 :TIME 98761213 :ROOT #<XLIB:WINDOW :0 96> :WINDOW #<XLIB:WINDOW :0 6291484> :EVENT-WINDOW #<XLIB:WINDOW :0 6291484> :CHILD
;;  #<XLIB:WINDOW :0 6291485> :ROOT-X 754 :ROOT-Y 223 :X 753 :Y 222 :STATE 4 :SAME-SCREEN-P T)
;; H

(defun handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
  (dformat 1 ">>> ~S~%" event-key)
  (let ((eventfn (gethash event-key *event-fn-table*)))
    (when eventfn
      (handler-case
          (progn
            ;; This is not the stumpwm top level, but if the restart
            ;; is in the top level then it seems the event being
            ;; processed isn't popped off the stack and is immediately
            ;; reprocessed after restarting to the top level. So fake
            ;; it, and put the restart here.
            (with-simple-restart (top-level "Return to stumpwm's top level")
              (apply eventfn event-slots))
            (xlib:display-finish-output *display*))
        ((or xlib:window-error xlib:drawable-error) (c)
          ;; Asynchronous errors are handled in the error
          ;; handler. Synchronous errors like trying to get the window
          ;; hints on a deleted window are caught and ignored here. We
          ;; do this inside the event handler so that the event is
          ;; handled. If we catch it higher up the event will not be
          ;; flushed from the queue and we'll get ourselves into an
          ;; infinite loop.
          (dformat 4 "ignore synchronous ~a~%" c))))
    (dformat 2 "<<< ~S~%" event-key)
    t))

;;; Selection

(defun export-selection ()
  (let* ((screen (current-screen))
         (selwin (screen-focus-window (current-screen)))
         (root (screen-root screen)))
    (xlib:set-selection-owner *display* :primary selwin)
    (unless (eq (xlib:selection-owner *display* :primary) selwin)
      (error "Can't set selection owner"))
    ;; also set the cut buffer for completeness
    (xlib:change-property root :cut-buffer0 *x-selection* :string 8 :transform #'xlib:char->card8
                          :mode :replace)))

(defun set-x-selection (text)
  "Set the X11 selection string to @var{string}."
  (setf *x-selection* text)
  (export-selection))

(defun send-selection (requestor property selection target time)
  (dformat 1 "send-selection ~s ~s ~s ~s ~s~%" requestor property selection target time)
  (cond
    ;; they're requesting what targets are available
    ((eq target :targets)
     (xlib:change-property requestor property (list :targets :string) target 8 :mode :replace))
    ;; send them a string
    ((find target '(:string ))
     (xlib:change-property requestor property *x-selection* :string 8 :mode :replace :transform #'xlib:char->card8))
    ;; we don't know how to handle anything else
    (t
     (setf property nil)))
  (xlib:send-event requestor :selection-notify nil
                   :display *display*
                   :window requestor
                   :selection selection
                   :property property
                   :target target
                   :time time)
  (xlib:display-finish-output *display*))

(defun get-x-selection (&optional timeout)
  "Return the x selection no matter what client own it."
  (labels ((wait-for-selection (&rest event-slots &key display event-key &allow-other-keys)
             (declare (ignore display))
             (when (eq event-key :selection-notify)
               (destructuring-bind (&key window property &allow-other-keys) event-slots
                 (if property
                     (xlib:get-property window property :type :string :result-type 'string :transform #'xlib:card8->char :delete-p t)
                     "")))))
    (if *x-selection*
        *x-selection*
        (progn
          (xlib:convert-selection :primary :string (screen-input-window (current-screen)) :stumpwm-selection)
          ;; Note: this may spend longer than timeout in this loop but it will eventually return.
          (let ((time (get-internal-real-time)))
            (loop for ret = (xlib:process-event *display* :handler #'wait-for-selection :timeout timeout :discard-p nil)
                  when (or ret
                           (> (/ (- time (get-internal-real-time)) internal-time-units-per-second)
                              timeout))
                  ;; make sure we return a string
                  return (or ret "")))))))

;;; Top map push/popping

(defvar *top-map-list* nil)

(defun push-top-map (new-top)
  (push *top-map* *top-map-list*)
  (setf *top-map* new-top)
  (sync-keys))

(defun pop-top-map ()
  (when *top-map-list*
    (setf *top-map* (pop *top-map-list*))
    (sync-keys)
    t))

(defmacro save-frame-excursion (&body body)
  "Execute body and then restore the current frame."
  (let ((oframe (gensym "OFRAME"))
        (ogroup (gensym "OGROUP")))
    `(let ((,oframe (tile-group-current-frame (current-group)))
           (,ogroup (current-group)))
      (unwind-protect (progn ,@body)
        (focus-frame ,ogroup ,oframe)))))
