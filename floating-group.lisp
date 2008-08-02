;;; implementation of a floating style window management group

(in-package :stumpwm)

(defclass float-group (group)
  ((current-window :accessor float-group-current-window))
  )

(defmethod group-add-window ((group float-group) window)
  (change-class window 'window)
  (float-window-align window)
  (focus-window window))

(defun &float-focus-next (group)
  (if (group-windows group)
      (focus-window (first (group-windows group)))
      (no-focus group nil)))

(defmethod group-delete-window ((group float-group) window)
  (&float-focus-next group))

(defmethod group-wake-up ((group float-group))
  (&float-focus-next group))

(defmethod group-suspend ((group float-group))
  )

(defmethod group-current-window ((group float-group))
  (screen-focus (group-screen group)))

(defmethod group-current-head ((group float-group))
  ;; FIXME: it'd be good not to copy them each time.
  (first (copy-heads (group-screen group))))

(defun float-window-align (window)
  (with-slots (parent xwin width height) window
    (let ((border (xlib:drawable-border-width parent)))
      (xlib:with-state (parent)
        (setf (xlib:drawable-x xwin) border
              (xlib:drawable-y xwin) (+ border 10)
              (xlib:drawable-width parent) (+ width
                                              (* 2 border))
              (xlib:drawable-height parent) (+ height 10
                                               (* 2 border))
              (xlib:window-background parent) (xlib:alloc-color (xlib:screen-default-colormap (screen-number (window-screen window)))
                                                               "Orange")))
      (xlib:clear-area (window-parent window)))))
  
(defmethod group-resize-request ((group float-group) window width height)
  (with-slots (parent xwin) window
    (xlib:with-state (parent)
      (setf (xlib:drawable-width parent) (+ width
                                            (* 2 (xlib:drawable-border-width parent)))
            (xlib:drawable-height parent) (+ height 10
                                             (* 2 (xlib:drawable-border-width parent))))
      (xlib:with-state (xwin)
        (setf (xlib:drawable-width xwin) width
              (xlib:drawable-height xwin) height)))))

(defmethod group-move-request ((group float-group) window x y relative-to)
  (with-slots (parent) window
    (xlib:with-state (parent)
      (setf (xlib:drawable-x parent) x
            (xlib:drawable-y parent) y))))

(defmethod group-raise-request ((group float-group) window type)
  (focus-window window))

(defmethod group-lost-focus ((group float-group))
  (&float-focus-next group))

(defmethod group-indicate-focus ((group float-group))
  )

(defmethod group-focus-window ((group float-group) window)
  (when (group-current-window group)
    (setf (xlib:window-background (window-parent (group-current-window group)))
          (xlib:alloc-color (xlib:screen-default-colormap (screen-number (window-screen window)))
                            "SteelBlue"))
    (xlib:clear-area (window-parent (group-current-window group))))
  (setf (xlib:window-background (window-parent (group-current-window group))) (xlib:alloc-color (xlib:screen-default-colormap (screen-number (window-screen window)))
                                                                                                "Orange"))
  (xlib:clear-area (window-parent window))
  (focus-window window))

(defmethod group-window-visible-p ((group float-group) win)
  (eql (window-state win) +normal-state+))

(defmethod group-root-exposure ((group float-group))
  )

(defmethod group-add-head ((group float-group))
  )

(defmethod group-button-press ((group float-group) x y window)
  (let ((screen (group-screen group)))
    (when (or (< x (xlib:drawable-x (window-xwin window)))
              (> x (+ (xlib:drawable-width (window-xwin window))
                      (xlib:drawable-x (window-xwin window))))
              (< y (xlib:drawable-y (window-xwin window)))
              (> y (+ (xlib:drawable-height (window-xwin window))
                      (xlib:drawable-y (window-xwin window)))))
      (message "Move window! ~@{~a ~}" x y window)
      (multiple-value-bind (relx rely) (xlib:query-pointer (window-parent window))
        (labels ((move-window-event-handler (&rest event-slots &key event-key &allow-other-keys)
                   (case event-key
                     (:button-release
                      :done)
                     (:motion-notify
                      (with-slots (parent) window
                        (xlib:with-state (parent)
                          (setf (xlib:drawable-x parent) (- (getf event-slots :x) relx)
                                (xlib:drawable-y parent) (- (getf event-slots :y) rely))))
                      t)
                     (t
                      nil))))
          (xlib:grab-pointer (screen-root screen) '(:button-release :pointer-motion))
          (unwind-protect
               (loop for ev = (xlib:process-event *display* :handler #'move-window-event-handler :timeout nil)
                  until (eq ev :done))
            (ungrab-pointer))
          (message "Done."))))))

(defcommand newfloat (name) ((:rest "Name: "))
  (add-group (current-screen) name 'float-group))


;;; floating window

(defclass float-window (window)
  ())

(defmethod update-decoration ((window float-window))
  (xlib:clear-area (window-parent window)))
