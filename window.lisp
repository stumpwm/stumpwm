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
;; Window functionality.
;;
;; Code:

(in-package #:stumpwm)

(export '(def-window-attr
          set-normal-gravity
          set-maxsize-gravity
          set-transient-gravity
          set-window-geometry))

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
                (if (eq *window-border-style* :thick)
                    c
                    (screen-win-bg-color screen))
                :none))
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
         (border (case *window-border-style*
                   (:none 0)
                   (t (default-border-width-for-type (window-type win)))))
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
               (xlib:window-priority (window-parent win)) :above))
       (return-from geometry-hints (values x y 0 0 width height 0 t)))
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
      (when (or center
                (find *window-border-style* '(:tight :none)))
        (setf x (+ wx (frame-x f))
              y (+ wy (frame-display-y (window-group win) f))
              wx 0 wy 0))
      ;; Now return our findings
      (values x y wx wy width height border center))))

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
  (multiple-value-bind (x y wx wy width height border stick)
      (geometry-hints win)
    (dformat 4 "maximize window ~a x: ~d y: ~d width: ~d height: ~d border: ~d stick: ~s~%" win x y width height border stick)
    ;; This is the only place a window's geometry should change
    (set-window-geometry win :x wx :y wy :width width :height height :border-width 0)
    (xlib:with-state ((window-parent win))
      ;; FIXME: updating the border doesn't need to be run everytime
      ;; the window is maximized, but only when the border style or
      ;; window type changes. The overhead is probably minimal,
      ;; though.
      (setf (xlib:drawable-x (window-parent win)) x
            (xlib:drawable-y (window-parent win)) y
            (xlib:drawable-border-width (window-parent win)) border)
      ;; the parent window should stick to the size of the window
      ;; unless it isn't being maximized to fill the frame.
      (if (or stick
              (find *window-border-style* '(:tight :none)))
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
                           :background (if (eq (window-type window) :normal)
                                           (screen-win-bg-color screen)
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
             (let* ((code (xlib:keysym->keycodes *display* (key-keysym key)))
                    (shift-sym (xlib:keycode->keysym *display* code 1)))
               ;; Some keysyms, such as upper case letters, need the
               ;; shift modifier to be set in order to grab properly.
               (when (eql (key-keysym key) shift-sym)
                 ;; don't butcher the caller's structure
                 (setf key (copy-structure key)
                       (key-shift key) t))
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
        (window-frame window) (or frame (pick-preferred-frame window)))
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

(defun pick-preferred-frame (window)
  (let* ((group (window-group window))
         (frames (group-frames group))
         (default (tile-group-current-frame group))
         (preferred-frame (or *new-window-preferred-frame* default)))
    (when (or (functionp *new-window-preferred-frame*)
              (and (symbolp *new-window-preferred-frame*)
                   (fboundp *new-window-preferred-frame*)))
      (setq preferred-frame
            (handler-case
                (funcall *new-window-preferred-frame* window)
              (error (c)
                (message "^1*^BError while calling ^b^3**new-window-preferred-frame*^1*^B: ^n~a" c)
                default))))
    (cond
      ;; If we already have a frame use it.
      ((frame-p preferred-frame)
       preferred-frame)
      ;; If `preferred-frame' is a list of keyword use it to determine the
      ;; frame.  The sanity check doesn't cover not recognized keywords.  We
      ;; simply fall back to the default then.
      ((and (listp preferred-frame)
            (every #'keywordp preferred-frame))
       (loop for i in preferred-frame
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
                    (t                  ; :focused or not recognized keyword
                     default))))
      ;; Not well formed `*new-window-preferred-frame*'.  Message an error and
      ;; return the default.
      (t (message "^1*^BInvalid ^b^3**new-window-preferred-frame*^1*^B: ^n~a"
                  preferred-frame)
         default))))

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
            (window-frame window) (or frame (pick-preferred-frame window))))
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
