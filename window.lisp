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
;; Window functionality.
;;
;; Code:

(in-package #:stumpwm)

(export '(*default-window-name*
          define-window-slot
          set-normal-gravity
          set-maxsize-gravity
          set-transient-gravity
          set-window-geometry
          find-wm-state
          add-wm-state
          remove-wm-state))

(export
  '(window window-xwin window-width window-height window-x window-y
    window-gravity window-group window-number window-parent window-title
    window-user-title window-class window-type window-res window-role
    window-unmap-ignores window-state window-normal-hints window-marked
    window-plist window-fullscreen window-screen
    ;; Window utilities
    update-configuration no-focus
    ;; Window management API
    update-decoration focus-window raise-window window-visible-p window-sync
    window-head really-raise-window))

(defvar *default-window-name* "Unnamed"
  "The name given to a window that does not supply its own name.")

(defclass window ()
  ((xwin    :initarg :xwin    :accessor window-xwin)
   (width   :initarg :width   :accessor window-width)
   (height  :initarg :height  :accessor window-height)
   ;; these are only used to hold the requested map location.
   (x       :initarg :x       :accessor window-x)
   (y       :initarg :y       :accessor window-y)
   (gravity :initform nil     :accessor window-gravity)
   (group   :initarg :group   :accessor window-group)
   (number  :initarg :number  :accessor window-number)
   (parent                    :accessor window-parent)
   (title   :initarg :title   :accessor window-title)
   (user-title :initform nil  :accessor window-user-title)
   (class   :initarg :class   :accessor window-class)
   (type    :initarg :type    :accessor window-type)
   (res     :initarg :res     :accessor window-res)
   (role    :initarg :role    :accessor window-role)
   (unmap-ignores :initarg :unmap-ignores :accessor window-unmap-ignores)
   (state   :initarg :state   :accessor window-state)
   (normal-hints :initarg :normal-hints :accessor window-normal-hints)
   (marked  :initform nil     :accessor window-marked)
   (plist   :initarg :plist   :accessor window-plist)
   (fullscreen :initform nil  :accessor window-fullscreen)))

(defmethod print-object ((object window) stream)
  (format stream "#S(~a ~s #x~x)" (type-of object) (window-name object) (window-id object)))

;;; Window Management API

(defgeneric update-decoration (window)
  (:documentation "Update the window decoration."))
(defgeneric focus-window (window &optional raise)
  (:documentation "Give the specified window keyboard focus and (optionally) raise."))
(defgeneric raise-window (window)
  (:documentation "Bring the window to the top of the window stack."))
(defgeneric window-visible-p (window)
  (:documentation "Return T if the window is visible"))
(defgeneric window-sync (window what-changed)
  (:documentation "Some window slot has been updated and the window
may need to sync itself. WHAT-CHANGED is a hint at what changed."))
(defgeneric window-head (window)
  (:documentation "Report what window the head is currently on."))
(defgeneric really-raise-window (window)
  (:documentation "Really bring the window to the top of the window stack in group"))


(defmethod window-group :around ((window window))
  (if (find window *always-show-windows*)
      (current-group)
      (call-next-method)))

;; Urgency / demands attention

(defun register-urgent-window (window)
  "Add WINDOW to its screen's list of urgent windows"
  (if (eq (screen-current-window (window-screen window)) window)
      ;; window is already current, clear the urgent state to let it know we know.
      (window-clear-urgency window)
      (progn
        (push window (screen-urgent-windows (window-screen window)))
        (update-mode-lines (window-screen window))
        (values t))))

(defun unregister-urgent-window (window)
  "Remove WINDOW to its screen's list of urgent windows"
  (setf (screen-urgent-windows (window-screen window))
        (delete window (screen-urgent-windows (window-screen window))))
  (update-mode-lines (window-screen window)))

(defun window-clear-urgency (window)
  "Clear the urgency bit and/or _NET_WM_STATE_DEMANDS_ATTENTION on
WINDOW"
  (let* ((hints (xlib:wm-hints (window-xwin window)))
         (flags (when hints (xlib:wm-hints-flags hints))))
    (when flags
      (setf (xlib:wm-hints-flags hints) (logand (lognot 256) flags)
            (xlib:wm-hints (window-xwin window)) hints)))
  (remove-wm-state (window-xwin window) :_NET_WM_STATE_DEMANDS_ATTENTION)
  (unregister-urgent-window window))

(defun window-urgent-p (window)
  "Returns T if WINDOW has the urgency bit and/or
_NET_WM_STATE_DEMANDS_ATTENTION set"
  (let* ((hints (xlib:wm-hints (window-xwin window)))
         (flags (when hints (xlib:wm-hints-flags hints))))
    (or (and flags (logtest 256 flags))
        (find-wm-state (window-xwin window) :_NET_WM_STATE_DEMANDS_ATTENTION))))

(defcommand next-urgent () ()
            "Jump to the next urgent window"
            (and (screen-urgent-windows (current-screen))
                 (focus-all (first (screen-urgent-windows (current-screen))))))

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
         (if w
             (append (list w) (transients-of w))
           '()))))))

(defun only-transients (windows)
  "Out of WINDOWS, return a list of those which are transient."
  (remove-if-not 'window-transient-p (copy-list windows)))

(defun all-windows ()
  (mapcan (lambda (s) (copy-list (screen-windows s))) *screen-list*))

(defun visible-windows ()
  "Return a list of visible windows (on all screens)"
  (loop for s in *screen-list*
        nconc (delete-if 'window-hidden-p (copy-list (group-windows (screen-current-group s))))))

(defun top-windows ()
  "Return a list of semantically visible windows (on all screens)"
  (loop for s in *screen-list*
        nconc (delete-if-not 'window-visible-p (copy-list (group-windows (screen-current-group s))))))

(defun window-name (window)
  (or (window-user-title window)
      (case *window-name-source*
        (:resource-name (window-res window))
        (:class (window-class window))
        (t (window-title window)))
      *default-window-name*))

(defun window-id (window)
  (xlib:window-id (window-xwin window)))

(defun window-in-current-group-p (window)
  (or
   (find window *always-show-windows*)
   (eq (window-group window)
       (screen-current-group (window-screen window)))))

(defun window-screen (window)
  (group-screen (window-group window)))

(defun send-client-message (window type &rest data)
  "Send a client message to a client's window."
  (xlib:send-event (window-xwin window)
                   :client-message nil
                   :window (window-xwin window)
                   :type type
                   :format 32
                   :data data))

(defun window-map-number (window)
  (princ-to-string
   (let ((num (window-number window)))
     (or (and (< num (length *window-number-map*))
              (elt *window-number-map* num))
         num))))

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
(defun escape-caret (str)
  "Escape carets by doubling them"
  (coerce (loop :for char :across str
                :collect char
                :when (char= char #\^)
                  :collect char)
          'string))

(defun get-normalized-normal-hints (xwin)
  (macrolet ((validate-hint (fn)
               (setf fn (intern1 (concatenate 'string (string '#:wm-size-hints-) (string fn)) :xlib))
               `(setf (,fn hints) (and (,fn hints)
                                       (plusp (,fn hints))
                                       (,fn hints)))))
    (let ((hints (xlib:wm-normal-hints xwin)))
      (when hints
        (validate-hint :min-width)
        (validate-hint :min-height)
        (validate-hint :max-width)
        (validate-hint :max-height)
        (validate-hint :base-width)
        (validate-hint :base-height)
        (validate-hint :width-inc)
        (validate-hint :height-inc)
        (validate-hint :min-aspect)
        (validate-hint :max-aspect))
      hints)))

(defun xwin-net-wm-name (win)
  "Return the netwm wm name"
  (when-let ((name (xlib:get-property win :_NET_WM_NAME)))
    (utf8-to-string name)))

(defun safely-decode-x11-string (string)
  (handler-case
      (map 'string 'xlib:card8->char string)
    (type-error () nil)))

(defun xwin-wm-name (win)
  (multiple-value-bind
        (name encoding)
      (xlib:get-property win :WM_NAME :result-type '(vector (unsigned-byte 8)))
    (when name
      (if (eq encoding :string)
          (safely-decode-x11-string name)
          (utf8-to-string name)))))

(defun xwin-name (win)
  (escape-caret (or (xwin-net-wm-name win)
                    (xwin-wm-name win)
                    "")))

(defun update-configuration (win)
  ;; Send a synthetic configure-notify event so that the window
  ;; knows where it is onscreen.
  (handler-case
      (xwin-send-configuration-notify (window-xwin win)
                                      (+ (xlib:drawable-x (window-parent win))
                                         (xlib:drawable-x (window-xwin win)))
                                      (+ (xlib:drawable-y (window-parent win))
                                         (xlib:drawable-y (window-xwin win)))
                                      (window-width win) (window-height win) 0)
    (xlib:drawable-error (c)
      (dformat 4 "ignore ~S in ~S on ~S" c 'update-configuration win))))

;; FIXME: should we raise the window or its parent?
(defmethod raise-window (win)
  "Map the window if needed and bring it to the top of the stack. Does not affect focus."
  (when (window-urgent-p win)
    (window-clear-urgency win))
  (when (window-hidden-p win)
    (unhide-window win)
    (update-configuration win))
  (when (window-in-current-group-p win)
    (let ((group (window-group win)))
      (unless (null (group-raised-window group))
        (setf (xlib:window-priority
               (window-parent win)
               (window-parent (group-raised-window group)))
              :above))
     (setf (group-raised-window group) win)))
  (raise-top-windows))
;; some handy wrappers

(defun raise-top-windows ()
  (mapc (lambda (w)
          (when (window-in-current-group-p w)
            (setf (xlib:window-priority (window-parent w)) :top-if)))
        (group-on-top-windows (current-group))))

(defun xwin-border-width (win)
  (xlib:drawable-border-width win))

(defun (setf xwin-border-width) (width win)
  (setf (xlib:drawable-border-width win) width))

(defun default-border-width-for-type (window)
  (or (and (window-maxsize-p window)
           *maxsize-border-width*)
      (ecase (window-type window)
        (:dock 0)
        (:normal *normal-border-width*)
        ((:transient :dialog) *transient-border-width*))))

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

(defmacro define-window-slot (attr)
  "Create a new window attribute and corresponding get/set functions."
  (let ((win (gensym))
        (val (gensym)))
    `(progn
      (defun ,(intern1 (format nil "WINDOW-~a" attr)) (,win)
        (gethash ,attr (window-plist ,win)))
      (defun (setf ,(intern1 (format nil "WINDOW-~a" attr))) (,val ,win)
        (setf (gethash ,attr (window-plist ,win)) ,val)))))

(defgeneric sort-windows-by-number (window-list-spec)
  (:documentation "Return a copy of the provided window list sorted by number."))

(defmethod sort-windows-by-number ((window-list list))
  "Return a copy of the screen's window list sorted by number."
  (sort1 window-list '< :key 'window-number))

(defmethod sort-windows-by-number ((group group))
  "Return a copy of the screen's window list sorted by number."
  (sort1 (group-windows group) '< :key 'window-number))


(defgeneric sort-windows-by-class (window-list-spec)
  (:documentation "Return a copy of the provided window list sortes by class
 then by numer."))

(defmethod sort-windows-by-class ((window-list list))
  "Return a copy of the provided window list sorted by class then by number."
  (sort1 window-list (lambda (w1 w2)
                       (let ((class1 (window-class w1))
                             (class2 (window-class w2)))
                         (if (string= class1 class2)
                           (< (window-number w1) (window-number w2))
                           (string< class1 class2))))))

(defmethod sort-windows-by-class (group)
  "Return a copy of the provided window list sorted by class then by number."
  (sort-windows-by-class (group-windows group)))


(defun sort-windows (group)
  "Return a copy of the screen's window list sorted by number."
    (sort-windows-by-number group))

(defun marked-windows (group)
  "Return the marked windows in the specified group."
  (loop for i in (sort-windows group)
        when (window-marked i)
        collect i))

(defun (setf xwin-state) (state xwin)
  "Set the state (iconic, normal, withdrawn) of a window."
  (xlib:change-property xwin
                        :WM_STATE
                        (list state)
                        :WM_STATE
                        32))

(defun xwin-state (xwin)
  "Get the state (iconic, normal, withdrawn) of a window."
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
        (group-lost-focus (window-group window))))))

(defun window-maxsize-p (win)
  "Returns T if WIN specifies maximum dimensions."
  (let ((hints (window-normal-hints win)))
    (and hints (or (xlib:wm-size-hints-max-width hints)
                   (xlib:wm-size-hints-max-height hints)
                   (xlib:wm-size-hints-min-aspect hints)
                   (xlib:wm-size-hints-max-aspect hints)))))

(defun xwin-type (win)
  "Return one of :desktop, :dock, :toolbar, :utility, :splash,
:dialog, :transient, and :normal.  Right now
only :dock, :dialog, :normal, and :transient are
actually returned; see +NETWM-WINDOW-TYPES+."
  (or (let ((net-wm-window-type (xlib:get-property win :_NET_WM_WINDOW_TYPE)))
        (when net-wm-window-type
          (dolist (type-atom net-wm-window-type)
            (let ((net-wm-window-type
                   (assoc (xlib:atom-name *display* type-atom) +netwm-window-types+)))
              (when net-wm-window-type
                (return (cdr net-wm-window-type)))))))
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
      (and (window-maxsize-p win) *maxsize-gravity*)
      (ecase (window-type win)
        (:dock *normal-gravity*)
        (:normal *normal-gravity*)
        ((:transient :dialog) *transient-gravity*))))

(defun window-width-inc (window)
  "Find out what is the correct step to change window width"
  (or (when-let ((window-hints (window-normal-hints window)))
        (xlib:wm-size-hints-width-inc (window-normal-hints window)))
      1))

(defun window-height-inc (window)
  "Find out what is the correct step to change window height"
    (or (when-let ((window-hints (window-normal-hints window)))
          (xlib:wm-size-hints-height-inc (window-normal-hints window)))
      1))

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

(defun find-free-window-number (group)
  "Return a free window number for GROUP."
  (find-free-number (mapcar 'window-number (group-windows group))))

(defun reparent-window (screen window)
  ;; apparently we need to grab the server so the client doesn't get
  ;; the mapnotify event before the reparent event. that's what fvwm
  ;; says.
  (let* ((xwin (window-xwin window))
         (master-window (xlib:create-window
                         :parent (screen-root screen)
                         :x (xlib:drawable-x (window-xwin window))
                         :y (xlib:drawable-y (window-xwin window))
                         :width (window-width window)
                         :height (window-height window)
                         :background (if (eq (window-type window) :normal)
                                         (screen-win-bg-color screen)
                                         :none)
                         :border (screen-unfocus-color screen)
                         :border-width (default-border-width-for-type window)
                         :event-mask *window-parent-events*
                         :depth (xlib:drawable-depth xwin)
                         :visual (xlib:window-visual-info xwin)
                         :colormap (xlib:window-colormap xwin))))
    (unless (eq (xlib:window-map-state (window-xwin window)) :unmapped)
      (incf (window-unmap-ignores window)))
    (xlib:reparent-window (window-xwin window) master-window 0 0)
    (xwin-grab-buttons master-window)
    ;;     ;; we need to update these values since they get set to 0,0 on reparent
    ;;     (setf (window-x window) 0
    ;;          (window-y window) 0)
    (xlib:add-to-save-set (window-xwin window))
    (setf (window-parent window) master-window)))

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
                    (xlib:with-server-grabbed (*display*)
                      (process-mapped-window screen win)))))))))
  (dolist (w (screen-windows screen))
    (setf (window-state w) +normal-state+)
    (xwin-hide w)))

(defun xwin-grab-key (w key)
  (labels ((add-shift-modifier (key)
             ;; don't butcher the caller's structure
             (let ((key (copy-structure key)))
               (setf (key-shift key) t)
               key))
           (key-modifiers-exist-p (key)
             (and
              (or (not (key-meta key)) (modifiers-meta *modifiers*))
              (or (not (key-alt key)) (modifiers-alt *modifiers*))
              (or (not (key-hyper key)) (modifiers-hyper *modifiers*))
              (or (not (key-super key)) (modifiers-super *modifiers*)))))
    (loop for code in (multiple-value-list (xlib:keysym->keycodes *display* (key-keysym key)))
       ;; some keysyms aren't mapped to keycodes so just ignore them.
       when (and code (key-modifiers-exist-p key))
       do
       ;; Some keysyms, such as upper case letters, need the
       ;; shift modifier to be set in order to grab properly.
         (let ((key
                (if (and (not (eql (key-keysym key) (xlib:keycode->keysym *display* code 0)))
                         (eql (key-keysym key) (xlib:keycode->keysym *display* code 1)))
                    (add-shift-modifier key)
                    key)))
           (xlib:grab-key w code
                          :modifiers (x11-mods key) :owner-p t
                          :sync-pointer-p nil :sync-keyboard-p nil)
           ;; Ignore capslock and numlock by also grabbing the
           ;; keycombos with them on.
           (xlib:grab-key w code :modifiers (x11-mods key nil t) :owner-p t
                          :sync-keyboard-p nil :sync-keyboard-p nil)
           (when (modifiers-numlock *modifiers*)
             (xlib:grab-key w code
                            :modifiers (x11-mods key t nil) :owner-p t
                            :sync-pointer-p nil :sync-keyboard-p nil)
             (xlib:grab-key w code :modifiers (x11-mods key t t) :owner-p t
                            :sync-keyboard-p nil :sync-keyboard-p nil))))))

(defun xwin-grab-keys (win group)
  (dolist (map (dereference-kmaps (top-maps group)))
    (dolist (i (kmap-bindings map))
      (xwin-grab-key win (binding-key i)))))

(defun grab-keys-on-window (win)
  (xwin-grab-keys (window-xwin win) (window-group win)))

(defun xwin-ungrab-keys (win)
  (xlib:ungrab-key win :any :modifiers :any))

(defun ungrab-keys-on-window (win)
  (xwin-ungrab-keys (window-xwin win)))

(defun xwin-grab-buttons (win)
  ;; FIXME: Why doesn't grabbing button :any work? We have to
  ;; grab them one by one instead.
  (xwin-ungrab-buttons win)
  (loop for i from 1 to 32
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
                 do (xwin-grab-keys j (window-group (find-window j))))
        do (xwin-grab-keys (screen-focus-window i) (screen-current-group i)))
  (when (current-window)
    (remap-keys-grab-keys (current-window)))
  (xlib:display-finish-output *display*))

(defun netwm-remove-window (window)
  (xlib:delete-property (window-xwin window) :_NET_WM_DESKTOP))

(defun process-mapped-window (screen xwin)
  "Add the window to the screen's mapped window list and process it as
needed."
  (let ((window (xwin-to-window xwin)))
    (screen-add-mapped-window screen xwin)
    ;; windows always have border width 0. Their parents provide the
    ;; border.
    (set-window-geometry window :border-width 0)
    (setf (xlib:window-event-mask (window-xwin window)) *window-events*)
    (register-window window)
    (reparent-window screen window)
    (netwm-set-allowed-actions window)
    (let ((placement-data (place-window screen window)))
      (apply 'group-add-window (window-group window) window placement-data)
      ;; If the placement rule matched then either the window's group
      ;; is the current group or the rule's :lock attribute was
      ;; on. Either way the window's group should become the current
      ;; one (if it isn't already) if :raise is T.
      (when placement-data
        (if (getf placement-data :raise)
          (switch-to-group (window-group window))
          (unless *suppress-window-placement-indicator*
            (message "Placing window ~a in group ~a." (window-name window) (group-name (window-group window)))))
        (apply 'run-hook-with-args *place-window-hook* window (window-group window) placement-data)))
    ;; must call this after the group slot is set for the window.
    (grab-keys-on-window window)
    ;; quite often the modeline displays the window list, so update it
    (update-all-mode-lines)
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
  (let* ((screen (window-screen window))
         (group (get-window-placement screen window)))
    (unless (find (window-group window)
                  (screen-groups screen))
      (setf (window-group window) (or group (screen-current-group screen))))
    ;; FIXME: somehow it feels like this could be merged with group-add-window
    (setf (window-title window) (xwin-name (window-xwin window))
          (window-class window) (xwin-class (window-xwin window))
          (window-res window) (xwin-res-name (window-xwin window))
          (window-role window) (xwin-role (window-xwin window))
          (window-type window) (xwin-type (window-xwin window))
          (window-normal-hints window) (get-normalized-normal-hints (window-xwin window))
          (window-number window) (find-free-window-number (window-group window))
          (window-state window) +iconic-state+
          (xwin-state (window-xwin window)) +iconic-state+
          (screen-withdrawn-windows screen) (delete window (screen-withdrawn-windows screen))
          ;; put the window at the end of the list
          (group-windows (window-group window)) (append (group-windows (window-group window)) (list window)))
    (screen-add-mapped-window screen (window-xwin window))
    (register-window window)
    (group-add-window (window-group window) window)
    (netwm-set-group window)
    ;; It is effectively a new window in terms of the window list.
    (run-hook-with-args *new-window-hook* window)
    ;; FIXME: only called frame-raise-window instead of this function
    ;; which will likely call focus-all.
    (group-raise-request (window-group window) window :map)))

(defun withdraw-window (window)
  "Withdrawing a window means just putting it in a list til we get a destroy event."
  (declare (type window window))
  ;; This function cannot request info about WINDOW from the xserver as it may not exist anymore.
  (let ((group (window-group window))
        (screen (window-screen window)))
    (dformat 1 "withdraw window ~a~%" screen)
    ;; Save it for later since it is only withdrawn, not destroyed.
    (push window (screen-withdrawn-windows screen))
    (setf (window-state window) +withdrawn-state+
          (xwin-state (window-xwin window)) +withdrawn-state+)
    (xlib:unmap-window (window-parent window))
    ;; Clean up the window's entry in the screen and group
    (setf (group-windows group)
          (delete window (group-windows group)))
    (screen-remove-mapped-window screen (window-xwin window))
    (when (window-in-current-group-p window)
      ;; since the window doesn't exist, it doesn't have focus.
      (setf (screen-focus screen) nil))
    (netwm-remove-window window)
    (group-delete-window group window)
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
    (setf (screen-urgent-windows screen)
          (delete window (screen-urgent-windows screen)))
    (dformat 1 "destroy window ~a~%" screen)
    (dformat 3 "destroying parent window~%")
    (dformat 7 "parent window is ~a~%" (window-parent window))
    (xlib:destroy-window (window-parent window))))

(defun move-window-to-head (group window)
  "Move window to the head of the group's window list."
  (declare (type group group))
  (declare (type window window))
                                        ;(assert (member window (screen-mapped-windows screen)))
  (move-to-head (group-windows group) window)
  (netwm-update-client-list-stacking (group-screen group)))

(defun no-focus (group last-win)
  "don't focus any window but still read keyboard events."
  (dformat 3 "NO-FOCUS called~%")
  (let* ((screen (group-screen group)))
    (setf (group-current-window group) nil)
    ;; lame workaround to fix bug where non-focused window doesn't
    ;; listen unless an event is caught by the listener. In this case
    ;; a fake click is sent.
    (xlib-fake-click (screen-root screen) (screen-focus-window screen) 1)
    (when (eq group (screen-current-group screen))
      (xlib:set-input-focus *display* (screen-focus-window screen) :POINTER-ROOT)
      (setf (screen-focus screen) nil)
      (move-screen-to-head screen))
    (when last-win
      (update-decoration last-win))))

(defmethod focus-window (window &optional (raise t))
  "Make the window visible and give it keyboard focus. If raise is t, raise the window."
  (dformat 3 "focus-window: ~s~%" window)
  (let* ((group (window-group window))
         (screen (group-screen group))
         (cw (screen-focus screen))
         (xwin (window-xwin window)))
    (when raise
      (raise-window window))
    (cond
      ((eq window cw)
       ;; If window to focus is already focused then our work is done.
       )
      ;; If a WM_TAKE_FOCUS client message is not sent to the window,
      ;; widgets in Java applications tend to lose focus when the
      ;; window gets focused. This is hopefully the right way to
      ;; handle this.
      ((member :WM_TAKE_FOCUS (xlib:wm-protocols xwin) :test #'eq)
       (let ((hints (xlib:wm-hints xwin)))
         (when (or (null hints) (eq (xlib:wm-hints-input hints) :on))
           (screen-set-focus screen window)))
       (setf (group-current-window group) window)
       (update-decoration window)
       (when cw
         (update-decoration cw))
       (move-window-to-head group window)
       (send-client-message window :WM_PROTOCOLS
                            (xlib:intern-atom *display* :WM_TAKE_FOCUS)
                            ;; From reading the ICCCM spec, it's not
                            ;; entirely clear that this is the correct
                            ;; value for time that we send here.
                            (or *current-event-time* 0))
       (update-mode-lines (window-screen window))
       (run-hook-with-args *focus-window-hook* window cw))
      (t
       (screen-set-focus screen window)
       (setf (group-current-window group) window)
       (update-decoration window)
       (when cw
         (update-decoration cw))
       ;; Move the window to the head of the mapped-windows list
       (move-window-to-head group window)
       (update-mode-lines (window-screen window))
       (run-hook-with-args *focus-window-hook* window cw)))))

(defun xwin-kill (window)
  "Kill the client associated with window."
  (dformat 3 "Kill client~%")
  (xlib:kill-client *display* (xlib:window-id window)))

(defun default-window-menu-filter (item-string item-object user-input)
  "The default filter predicate for window menus."
  (or (menu-item-matches-regexp item-string item-object user-input)
      (match-all-regexps user-input (window-title item-object)
                         :case-insensitive t)))

(defvar *window-menu-filter* #'default-window-menu-filter
  "The filter predicate used to filter menu items in window menus
  created by SELECT-WINDOW-FROM-MENU. The interface for filter
  predicates is described in the docstring for SELECT-FROM-ITEM.")

(defun select-window-from-menu (windows fmt &optional prompt
                                              (filter-pred *window-menu-filter*))
  "Allow the user to select a window from the list passed in @var{windows}.  The
@var{fmt} argument specifies the window formatting used.  Returns the window
selected."
  (second (select-from-menu (current-screen)
                            (mapcar (lambda (w)
                                      (list (format-expand *window-formatters* fmt w) w))
                                    windows)
                            prompt
                            (or (position (current-window) windows) 0)  ; Initial selection
                            nil  ; Extra keymap
                            filter-pred)))

;;; Window commands

(defcommand delete-window (&optional (window (current-window))) ()
  "Delete a window. By default delete the current window. This is a
request sent to the window. The window's client may decide not to
grant the request or may not be able to if it is unresponsive."
  (when (find window *always-show-windows*)
    (disable-always-show-window window (current-screen)))
  (when window
    (send-client-message window :WM_PROTOCOLS (xlib:intern-atom *display* :WM_DELETE_WINDOW))))

(defcommand-alias delete delete-window)

(defcommand kill-window (&optional (window (current-window))) ()
  "Tell X to disconnect the client that owns the specified
window. Default to the current window. if
@command{delete-window} didn't work, try this."
  (when window
    (xwin-kill (window-xwin window))))

(defun kill-windows (windows)
  "Kill all windows @var{windows}"
  (dolist (window windows)
    (xwin-kill (window-xwin window))))

(defun kill-windows-in-group (group)
   "Kill all windows in group @var{group}"
  (kill-windows (group-windows group)))

(defcommand kill-windows-current-group () ()
  "Kill all windows in the current group."
  (kill-windows-in-group (current-group)))

(defcommand kill-windows-other () ()
  "Kill all windows in current group except the current-window"
  (let ((target-windows (remove (current-window)
                                (group-windows (current-group)))))
    (kill-windows target-windows)))

(defcommand-alias kill kill-window)

(defcommand title (title) ((:rest "Set window's title to: "))
  "Override the current window's title."
  (if (current-window)
      (setf (window-user-title (current-window)) title)
      (message "No Focused Window.")))

(defcommand select-window (query) ((:window-name "Select: "))
  "Switch to the first window that starts with @var{query}."
  (let (match)
    (labels ((match (win)
               (let* ((wname (window-name win))
                      (end (min (length wname) (length query))))
                 (string-equal wname query :end1 end :end2 end))))
      (unless (null query)
        (setf match (find-if #'match (group-windows (current-group)))))
      (when match
        (group-focus-window (current-group) match)))))

(defcommand-alias select select-window)

(defcommand select-window-by-name (name) ((:window-name "Select: "))
  "Switch to the first window whose name is exactly @var{name}."
  (let ((win (find name (group-windows (current-group))
                   :test #'string= :key #'window-name)))
    (when win
      (group-focus-window (current-group) win))))

(defcommand select-window-by-number (num &optional (group (current-group)))
                                    ((:window-number "Select: "))
  "Find the window with the given number and focus it in its frame."
  (labels ((match (win)
             (= (window-number win) num)))
    (let ((win (find-if #'match (group-windows group))))
      (when win
        (group-focus-window group win)))))

(defgeneric group-windows-for-cycling (group &key sorting)
  (:documentation "Return a list of windows in the group that can be selected with
 Next, Prev and Other command."))

(defmethod group-windows-for-cycling (group &key (sorting nil))
  (if sorting
      (sort-windows group)
      (group-windows group)))

(defgeneric focus-other-window (group)
  (:documentation "Focus the window in the group last focused"))

(defmethod focus-other-window (group)
  (let* ((wins (group-windows-for-cycling group))
         ;; the frame could be empty
         (win (if (group-current-window group)
                  (second wins)
                  (first wins))))
    (if win
        (group-focus-window group win)
        (echo-string (group-screen group) "No other window."))))

(defgeneric focus-next-window (group)
  (:documentation "Focus the next window in the windows list of the group"))

(defgeneric focus-prev-window (group)
  (:documentation "Focus the previous window in the windows list of the group"))

(defmethod focus-next-window (group)
  (let* ((w (group-current-window group))
         (wins (group-windows-for-cycling group :sorting t))
         (nw (or (cadr (member w wins)) (first wins))))
    (if (and nw
             (not (eq w nw)))
        (group-focus-window group nw)
        (message "No other window."))))

(defmethod focus-prev-window (group)
  (let* ((w (group-current-window group))
         (wins (reverse (group-windows-for-cycling group :sorting t)))
         (nw (or (cadr (member w wins)) (first wins))))
    (if (and nw
             (not (eq w nw)))
        (group-focus-window group nw)
        (message "No other window."))))

(defcommand other-window (&optional (group (current-group))) ()
  "Switch to the window last focused."
  (focus-other-window group))

(defcommand-alias other other-window)

(defcommand next () ()
  "Go to the next window in the window list."
  (let ((group (current-group)))
    (if (group-current-window group)
        (focus-next-window group)
        (other-window group))))

(defcommand prev () ()
  "Go to the previous window in the window list."
  (let ((group (current-group)))
    (if (group-current-window group)
        (focus-prev-window group)
        (other-window group))))

(defcommand renumber (nt &optional (group (current-group))) ((:number "Number: "))
  "Change the current window's number to the specified number. If another window
is using the number, then the windows swap numbers. Defaults to current group."
  (let ((nf (window-number (group-current-window group)))
        (win (find-if #'(lambda (win)
                          (= (window-number win) nt))
                      (group-windows group))))
    ;; Is it already taken?
    (if win
        (progn
          ;; swap the window numbers
          (setf (window-number win) nf)
          (setf (window-number (group-current-window group)) nt))
        ;; Just give the window the number
        (setf (window-number (group-current-window group)) nt))))

(defcommand-alias number renumber)

(defcommand repack-window-numbers (&optional preserved) ()
  "Ensure that used window numbers do not have gaps; ignore PRESERVED window numbers."
  (let* ((group (current-group))
         (windows (sort-windows group)))
    (loop for w in windows
          do (unless (find (window-number w) preserved)
               (setf
                 (window-number w)
                 (find-free-number
                   (remove
                     (window-number w)
                     (mapcar 'window-number windows))
                   0))))))

;; It would make more sense that the window-list argument was before the fmt one
;; but window-list was added latter and I didn't want to break other's code.
(defcommand windowlist (&optional (fmt *window-format*)
                                  window-list) (:rest)
  "Allow the user to select a window from the list of windows and focus the
selected window. For information of menu bindings see @ref{Menus}. The optional
 argument @var{fmt} can be specified to override the default window formatting.
The optional argument @var{window-list} can be provided to show a custom window
list (see @command{windowlist-by-class}). The default window list is the list of
all window in the current group. Also note that the default window list is sorted
by number and if the @var{windows-list} is provided, it is shown unsorted (as-is)."
  ;; Shadowing the window-list argument.
  (if-let ((window-list (or window-list
                          (sort-windows-by-number
                           (group-windows (current-group))))))
    (if-let ((window (select-window-from-menu window-list fmt)))
      (group-focus-window (current-group) window)
      (throw 'error :abort))
    (message "No Managed Windows")))

(defcommand windowlist-by-class (&optional (fmt *window-format-by-class*)) (:rest)
  "Allow the user to select a window from the list of windows (sorted by class)
 and focus the selected window. For information of menu bindings see @ref{Menus}.
The optional argument @var{fmt} can be specified to override the default window
formatting. This is a simple wrapper around the command @command{windowlist}."
  (windowlist fmt (sort-windows-by-class (group-windows (current-group)))))

(defcommand window-send-string (string &optional (window (current-window))) ((:rest "Insert: "))
  "Send the string of characters to the current window as if they'd been typed."
  (when window
    (map nil (lambda (ch)
               ;; exploit the fact that keysyms for ascii characters
               ;; are the same as their ascii value.
               (let ((sym (cond ((<= 32 (char-code ch) 127)
                                 (char-code ch))
                                ((char= ch #\Tab)
                                 (stumpwm-name->keysym "TAB"))
                                ((char= ch #\Newline)
                                 (stumpwm-name->keysym "RET"))
                                (t (first (xlib:character->keysyms ch *display*))))))
                 (when sym
                   (send-fake-key window
                                  (make-key :keysym sym)))))
         string)))

(defcommand-alias insert window-send-string)

(defcommand mark () ()
"Toggle the current window's mark."
  (let ((win (current-window)))
    (when win
      (setf (window-marked win) (not (window-marked win)))
      (message (if (window-marked win)
                   "Marked!"
                   "Unmarked!")))))

(defcommand clear-window-marks (&optional (group (current-group)) (windows (group-windows group))) ()
"Clear all marks in the current group."
  (dolist (w windows)
    (setf (window-marked w) nil)))

(defcommand-alias clear-marks clear-window-marks)

(defcommand echo-windows (&optional (fmt *window-format*) (group (current-group)) (windows (group-windows group))) (:rest)
  "Display a list of managed windows. The optional argument @var{fmt} can
be used to override the default window formatting."
  (let* ((wins (sort1 windows '< :key 'window-number))
         (highlight (position (group-current-window group) wins))
         (names (mapcar (lambda (w)
                          (format-expand *window-formatters* fmt w)) wins)))
    (if (null wins)
        (echo-string (group-screen group) "No Managed Windows")
        (echo-string-list (group-screen group) names highlight))))

(defcommand-alias windows echo-windows)

(defcommand info (&optional (fmt *window-info-format*)) (:rest)
  "Display information about the current window."
  (if (current-window)
      (message "~a" (format-expand *window-formatters* fmt (current-window)))
      (message "No Current Window.")))

(defcommand refresh () ()
  "Refresh current window without changing its size."
  (when-let* ((window (current-window))
              (w (window-width window))
              (h (window-height window)))
    (set-window-geometry window
                         :width (- w (window-width-inc window))
                         :height (- h (window-height-inc window)))
    ;; make sure the first one goes through before sending the second
    (xlib:display-finish-output *display*)
    (set-window-geometry window
                         :width w
                         :height h)))

(defcommand toggle-always-on-top () ()
  "Toggle whether the current window always appears over other windows.
The order windows are added to this list determines priority."
  (let ((w (current-window))
        (windows (group-on-top-windows (current-group))))
    (when w
      (if (find w windows)
          (setf (group-on-top-windows (current-group)) (remove w windows))
          (push (current-window) (group-on-top-windows (current-group)))))))

(defcommand fullscreen () ()
  "Toggle the fullscreen mode of the current widnow. Use this for clients
with broken (non-NETWM) fullscreen implementations, such as any program
using SDL."
  (update-fullscreen (current-window) 2))
