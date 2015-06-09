;;; implementation of a floating style window management group
(defpackage #:stumpwm.floating-group
  (:use :cl :stumpwm)
  (:export #:float-group))

(in-package :stumpwm.floating-group)

;;; floating window

(defclass float-window (window)
  ((last-width :initform 0 :accessor float-window-last-width)
   (last-height :initform 0 :accessor float-window-last-height)
   (last-x :initform 0 :accessor float-window-last-x)
   (last-y :initform 0 :accessor float-window-last-y)))

(defvar *float-window-border* 1)
(defvar *float-window-title-height* 10)

;; some book keeping functions
(defmethod (setf window-x) :before (val (window float-window))
  (setf (float-window-last-x window) (window-x window)))

(defmethod (setf window-y) :before (val (window float-window))
  (setf (float-window-last-y window) (window-y window)))

(defmethod (setf window-width) :before (val (window float-window))
  (setf (float-window-last-width window) (window-width window)))

(defmethod (setf window-height) :before (val (window float-window))
  (setf (float-window-last-height window) (window-height window)))

(defun float-window-move-resize (win &key x y width height (border *float-window-border*))
  ;; x and y position the parent window while width, height resize the
  ;; xwin (meaning the parent will have a larger width).
  (with-accessors ((xwin window-xwin)
                   (parent window-parent))
      win
    (xlib:with-state (parent)
      (xlib:with-state (xwin)
        (when x
          (setf (xlib:drawable-x parent) x
                (window-x win) x))
        (when y
          (setf (xlib:drawable-y parent) y
                (window-y win) y))
        (when width
          (setf (xlib:drawable-width parent) (+ (xlib:drawable-x xwin) width border)
                (xlib:drawable-width xwin) width
                (window-width win) width))
        (when height
          (setf (xlib:drawable-height parent) (+ (xlib:drawable-y xwin) height border)
                (xlib:drawable-height xwin) height
                (window-height win) height))))))

(defmethod update-decoration ((window float-window))
  (let ((group (window-group window)))
    (setf (xlib:window-background (window-parent window))
          (if (eq (group-current-window group) window)
              (screen-float-focus-color (window-screen window))
              (screen-float-unfocus-color (window-screen window))))
    (xlib:clear-area (window-parent window))))

(defmethod window-sync ((window float-window) hint)
  (declare (ignore hint)))

(defmethod window-head ((window float-window))
  (let ((left (window-x window))
        (right (+ (window-x window) (window-width window)))
        (top (window-y window))
        (bottom (+ (window-y window) (window-height window)))
        (heads (screen-heads (group-screen (window-group window)))))
    (flet ((within-frame-p (y x head)
             (and (>= x (frame-x head))
                  (< x (+ (frame-x head) (frame-width head)))
                  (>= y (frame-y head))
                  (< y (+ (frame-y head) (frame-height head))))))
      (or (find-if (lambda (head)
                     (or (within-frame-p top left head)
                         (within-frame-p top right head)
                         (within-frame-p bottom left head)
                         (within-frame-p bottom right head)))
                   heads)
          ;; Didn't find any head, so give up and return the first one
          ;; in the list.
          (first heads)))))

(defmethod window-visible-p ((win float-window))
  (eql (window-state win) +normal-state+))

(defmethod (setf window-fullscreen) :after (val (window float-window))
  (with-accessors ((last-x float-window-last-x)
                   (last-y float-window-last-y)
                   (last-width float-window-last-width)
                   (last-height float-window-last-height)
                   (parent window-parent))
      window
    (if val
        (let ((head (window-head window)))
          (with-accessors ((x window-x)
                           (y window-y)
                           (width window-width)
                           (height window-height))
              window
            (format t "major on: ~a ~a ~a ~a~%" x y width height))
          (set-window-geometry window :x 0 :y 0)
          (float-window-move-resize window
                                    :x (frame-x head)
                                    :y (frame-y head)
                                    :width (frame-width head)
                                    :height (frame-height head)
                                    :border 0)
          (format t "loot after: ~a ~a ~a ~a~%" last-x last-y last-width last-height))
        (progn
          (format t "fullscreenage: ~a ~a ~a ~a~%" last-x last-y last-width last-height)
          ;; restore the position
          (set-window-geometry window :x *float-window-border* :y *float-window-title-height*)
          (float-window-move-resize window
                                    :x last-x
                                    :y last-y
                                    :width last-width
                                    :height last-height)))))

;;; floating group

(defclass float-group (group)
  ((current-window :accessor float-group-current-window)))

(defmethod group-startup ((group float-group)))

(defmethod group-add-window ((group float-group) window &key &allow-other-keys)
  (change-class window 'float-window)
  (float-window-align window)
  (focus-window window))

(defun &float-focus-next (group)
  (if (group-windows group)
      (focus-window (first (group-windows group)))
      (no-focus group nil)))

(defmethod group-delete-window ((group float-group) window)
  (declare (ignore window))
  (&float-focus-next group))

(defmethod group-wake-up ((group float-group))
  (&float-focus-next group))

(defmethod group-suspend ((group float-group)))

(defmethod group-current-window ((group float-group))
  (screen-focus (group-screen group)))

(defmethod group-current-head ((group float-group))
  (if (group-current-window group)
      (window-head (group-current-window group))
      (first (screen-heads (group-screen group)))))

(defun float-window-align (window)
  (with-accessors ((parent window-parent)
                   (xwin window-xwin)
                   (width window-width)
                   (height window-height))
      window
    (set-window-geometry window :x *float-window-border* :y *float-window-title-height*)
    (xlib:with-state (parent)
      (setf (xlib:drawable-width parent) (+ width (* 2 *float-window-border*))
            (xlib:drawable-height parent) (+ height *float-window-title-height* *float-window-border*)
            (xlib:window-background parent) (xlib:alloc-color (xlib:screen-default-colormap (screen-number (window-screen window)))
                                                              "Orange")))
    (xlib:clear-area (window-parent window))))

(defmethod group-resize-request ((group float-group) window width height)
  (float-window-move-resize window :width width :height height))

(defmethod group-move-request ((group float-group) window x y relative-to)
  (declare (ignore relative-to))
  (float-window-move-resize window :x x :y y))

(defmethod group-raise-request ((group float-group) window type)
  (declare (ignore type))
  (focus-window window))

(defmethod group-lost-focus ((group float-group))
  (&float-focus-next group))

(defmethod group-indicate-focus ((group float-group))
  )

(defmethod group-focus-window ((group float-group) window)
  (focus-window window))

(defmethod group-root-exposure ((group float-group))
  )

(defmethod group-add-head ((group float-group) head)
  (declare (ignore head)))

(defmethod group-remove-head ((group float-group) head)
  (declare (ignore head)))

(defmethod group-resize-head ((group float-group) oh nh)
  (declare (ignore oh nh)))

(defmethod group-sync-all-heads ((group float-group))
  )

(defmethod group-sync-head ((group float-group) head)
  (declare (ignore head))
  )

(defmethod group-button-press ((group float-group) x y (window float-window))
  (let ((screen (group-screen group))
        (initial-width (xlib:drawable-width (window-parent window)))
        (initial-height (xlib:drawable-height (window-parent window))))
    (when (eq *mouse-focus-policy* :click)
      (focus-window window))

    ;; When in border
    (multiple-value-bind (relx rely same-screen-p child state-mask)
        (xlib:query-pointer (window-parent window))
      (declare (ignore relx rely same-screen-p child))
      (when (or (< x (xlib:drawable-x (window-xwin window)))
                (> x (+ (xlib:drawable-width (window-xwin window))
                        (xlib:drawable-x (window-xwin window))))
                (< y (xlib:drawable-y (window-xwin window)))
                (> y (+ (xlib:drawable-height (window-xwin window))
                        (xlib:drawable-y (window-xwin window))))
                (intersection (modifiers-super *modifiers*) (xlib:make-state-keys state-mask)))

        ;; When resizing warp pointer to left-right corner
        (when (find :button-3 (xlib:make-state-keys state-mask))
          (xlib:warp-pointer (window-parent window) initial-width initial-height))

        (multiple-value-bind (relx rely same-screen-p child state-mask)
            (xlib:query-pointer (window-parent window))
          (declare (ignore same-screen-p child))
          (labels ((move-window-event-handler
                       (&rest event-slots &key event-key &allow-other-keys)
                     (case event-key
                       (:button-release :done)
                       (:motion-notify
                        (with-accessors ((parent window-parent))
                            window
                          (xlib:with-state (parent)
                            ;; Either move or resize the window
                            (cond
                              ((find :button-1 (xlib:make-state-keys state-mask))
                               (setf (xlib:drawable-x parent) (- (getf event-slots :x) relx)
                                     (xlib:drawable-y parent) (- (getf event-slots :y) rely)))
                              ((find :button-3 (xlib:make-state-keys state-mask))
                               (let ((w (- (getf event-slots :x)
                                           (xlib:drawable-x parent)))
                                     (h (- (getf event-slots :y)
                                           (xlib:drawable-y parent)
                                           *float-window-title-height*)))
                                 ;; Don't let the window become too small
                                 (float-window-move-resize window
                                                           :width (max w *min-frame-width*)
                                                           :height (max h *min-frame-height*)))))))
                        t)
                       ;; We need to eat these events or they'll ALL
                       ;; come blasting in later. Also things start
                       ;; lagging hard if we don't (on clisp anyway).
                       (:configure-notify t)
                       (:exposure t)
                       (t nil))))
            (xlib:grab-pointer (screen-root screen) '(:button-release :pointer-motion))
            (unwind-protect
                 ;; Wait until the mouse button is released
                 (loop for ev = (xlib:process-event *display*
                                                    :handler #'move-window-event-handler
                                                    :timeout nil
                                                    :discard-p t)
                       until (eq ev :done))
              (ungrab-pointer))
            (update-configuration window)
            ;; don't forget to update the cache
            (setf (window-x window) (xlib:drawable-x (window-parent window))
                  (window-y window) (xlib:drawable-y (window-parent window)))))))))

(defmethod group-button-press ((group float-group) x y where)
  (declare (ignore x y where)))

;;; Bindings

(pushnew '(float-group *float-group-top-map*) *group-top-maps*)
(defvar *float-group-top-map* (make-sparse-keymap))
(defvar *float-group-root-map* (make-sparse-keymap))


(in-package :stumpwm)
(defcommand gnew-float (name) ((:rest "Group Name: "))
  "Create a floating window group with the specified name and switch to it."
  (add-group (current-screen) name :type 'stumpwm.floating-group:float-group))

(defcommand gnewbg-float (name) ((:rest "Group Name: "))
  "Create a floating window group with the specified name, but do not switch to it."
  (add-group (current-screen) name :background t :type 'stumpwm.floating-group:float-group))
