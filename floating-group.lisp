;;; implementation of a floating style window management group

(in-package #:stumpwm)

;;; floating window

(defclass float-window (window)
  ((last-width :initform 0 :accessor float-window-last-width)
   (last-height :initform 0 :accessor float-window-last-height)
   (last-x :initform 0 :accessor float-window-last-x)
   (last-y :initform 0 :accessor float-window-last-y)))

(defvar *float-window-border* 1)
(defvar *float-window-title-height* 10)
(defvar *float-window-modifier* :super
  "The keyboard modifier to use for resize and move floating windows without
  clicking on the top border. Valid values are :META :ALT :HYPER :SUPER, :ALTGR
  and :NUMLOCK.")


(defun float-window-p (window)
  (typep window 'float-window))

(defun float-window-modifier ()
  "Convert the *FLOAT-WINDOW-MODIFIER* to its corresponding X11."
  (when-let ((fn (find-symbol (concat "MODIFIERS-" (symbol-name *float-window-modifier*))
                              (find-package "STUMPWM"))))
    (funcall fn *modifiers*)))

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
                  (< x (+ (frame-x head) (1- (frame-width head))))
                  (>= y (frame-y head))
                  (< y (+ (frame-y head) (1- (frame-height head)))))))
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

(defmethod really-raise-window ((window float-window))
  (raise-window window))

;;; floating group

(defclass float-group (group)
  ((current-window :accessor float-group-current-window)))

(defmethod group-startup ((group float-group)))

(flet ((add-float-window (group window raise)
         (change-class window 'float-window)
         (float-window-align window)
         (when raise
           (group-focus-window group window))))
  (defmethod group-add-window ((group float-group) window &key raise &allow-other-keys)
    (add-float-window group window raise))
  (defmethod group-add-window (group (window float-window) &key raise &allow-other-keys)
    (add-float-window group window raise)))

(defun %float-focus-next (group)
  (let ((windows (remove-if 'window-hidden-p (group-windows group))))
    (if windows
        (group-focus-window group (first windows))
        (no-focus group nil))))

(defmethod group-delete-window ((group float-group) (window float-window))
  (declare (ignore window))
  (%float-focus-next group))

(defmethod group-wake-up ((group float-group))
  (%float-focus-next group))

(defmethod group-suspend ((group float-group)))

(defmethod group-current-head ((group float-group))
  (if-let ((current-window (group-current-window group)))
    (window-head current-window)
    (multiple-value-bind (x y)
        (xlib:global-pointer-position *display*)
      (find-head-by-position (group-screen group) x y))))

(defun float-window-align (window)
  (with-accessors ((parent window-parent)
                   (screen window-screen)
                   (width window-width)
                   (height window-height))
      window
    (set-window-geometry window :x *float-window-border* :y *float-window-title-height*)
    (xlib:with-state (parent)
      (setf (xlib:drawable-width parent) (+ width (* 2 *float-window-border*))
            (xlib:drawable-height parent) (+ height *float-window-title-height* *float-window-border*)
            (xlib:window-background parent) (xlib:alloc-color (xlib:screen-default-colormap (screen-number (window-screen window)))
                                                              "Orange")))
    (xlib:clear-area (window-parent window))
    (let ((parent-x (xlib:drawable-x parent))
          (parent-y (xlib:drawable-y parent))
          (parent-width (xlib:drawable-width parent))
          (parent-height (xlib:drawable-height parent))
          (border (xlib:drawable-border-width parent))
          (screen-width (screen-width screen))
          (screen-height (screen-height screen)))
      ;; Resize window when borders outside screen
      (let ((diff-width (- (+ parent-x parent-width) (- screen-width (* 2 border))))
            (diff-height (- (+ parent-y parent-height) (- screen-height (* 2 border)))))
        (when (or (> parent-x 0) (> parent-y 0))
          (float-window-move-resize window :x parent-x :y parent-y))
        (when (> diff-width 0)
          (float-window-move-resize window :width (- width diff-width)))
        (when (> diff-height 0)
          (float-window-move-resize window :height (- height diff-height)))))))

(defmethod group-resize-request ((group float-group) window width height)
  (float-window-move-resize window :width width :height height))

(defmethod group-move-request ((group float-group) window x y relative-to)
  (declare (ignore relative-to))
  (float-window-move-resize window :x x :y y))

(defmethod group-raise-request ((group float-group) window type)
  (declare (ignore type))
  (group-focus-window group window))

(defmethod group-lost-focus ((group float-group))
  (%float-focus-next group))

(defmethod group-indicate-focus ((group float-group)))

(defmethod group-focus-window ((group float-group) window)
  (focus-window window))

(defmethod group-root-exposure ((group float-group)))

(defmethod group-add-head ((group float-group) head)
  (declare (ignore head)))

(defmethod group-remove-head ((group float-group) head)
  (declare (ignore head)))

(defmethod group-resize-head ((group float-group) oh nh)
  (declare (ignore oh nh)))

(defmethod group-sync-all-heads ((group float-group)))

(defmethod group-sync-head ((group float-group) head)
  (declare (ignore head)))

(defvar *last-click-time* 0
  "Time since the last click occurred")

(defun window-display-height (window)
  "Returns maximum displayable height of window accounting for the mode-line"
  (let* ((head (window-head window))
         (ml (head-mode-line head))
         (ml-height (if (null ml) 0 (mode-line-height ml))))
    (- (head-height head) ml-height
       (* 2 *normal-border-width*)
       *float-window-border*
       *float-window-title-height*)))

(defun maximize-float (window &key horizontal vertical)
  (let* ((head (window-head window))
         (ml (head-mode-line head))
         (hx (head-x head))
         (hy (if (null ml) 0 (mode-line-height ml)))
         (w (- (head-width head)
               (* 2 *normal-border-width*)
               (* 2 *float-window-border*)))
         (h (window-display-height window)))
    (when horizontal
      (float-window-move-resize window :width w))
    (when vertical
      (float-window-move-resize window :y hy :height h))
    (when (and horizontal vertical)
      (float-window-move-resize window :x hx :y hy))))

(defmethod group-button-press (group button x y (window float-window))
  (declare (ignore button))
  (let ((screen (group-screen group))
        (initial-width (xlib:drawable-width (window-parent window)))
        (initial-height (xlib:drawable-height (window-parent window)))
        (initial-x (xlib:drawable-x (window-parent window)))
        (initial-y (xlib:drawable-y (window-parent window)))
        (xwin (window-xwin window)))
    (when (member *mouse-focus-policy* '(:click :sloppy))
      (group-focus-window group window))

    ;; When in border
    (multiple-value-bind (relx rely same-screen-p child state-mask)
        (xlib:query-pointer (window-parent window))
      (declare (ignore same-screen-p child))
      (when (or (< x (xlib:drawable-x xwin))
                (> x (+ (xlib:drawable-width xwin)
                        (xlib:drawable-x xwin)))
                (< y (xlib:drawable-y xwin))
                (> y (+ (xlib:drawable-height xwin)
                        (xlib:drawable-y xwin)))
                (intersection (float-window-modifier)
                              (xlib:make-state-keys state-mask)))
        (when (find :button-1 (xlib:make-state-keys state-mask))
          (let* ((current-time (/ (get-internal-real-time)
                                  internal-time-units-per-second))
                 (delta-t (- current-time *last-click-time*))
                 (win-focused-p (eq window (screen-focus screen))))
            (setf *last-click-time* current-time)
            (when (< delta-t 0.25)
              (cond ((and (not (eq (window-height window)
                                   (window-display-height window)))
                          win-focused-p)
                     (maximize-float window :vertical t))
                    (win-focused-p (maximize-float window :vertical t :horizontal t))
                    (t (focus-window window t))))))

        (multiple-value-bind (relx rely same-screen-p child state-mask)
            (xlib:query-pointer (window-parent window))
          (declare (ignore same-screen-p child))

          (let ((left-quadrant (< relx (floor initial-width 2)))
                (top-quadrant (< rely (floor initial-height 2))))
            (dformat 4 "corner: left: ~a top: ~a~%" left-quadrant top-quadrant)

            ;; When resizing warp pointer to closest corner
            (when (find :button-3 (xlib:make-state-keys state-mask))
              (xlib:warp-pointer (window-parent window)
                                 (if left-quadrant 0 initial-width)
                                 (if top-quadrant 0 initial-height)))

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
                                 ;; if button-1 on the sides (left,
                                 ;; right, bottom) then we resize that
                                 ;; direction

                                 ;; if button-1 on the top, then we move the window
                                 (float-window-move-resize window
                                                           :x (- (getf event-slots :x) relx)
                                                           :y (- (getf event-slots :y) rely)))
                                ((find :button-3 (xlib:make-state-keys state-mask))
                                 (let ((w (if left-quadrant
                                              (- initial-width
                                                 (- (getf event-slots :x)
                                                    initial-x))
                                              (- (getf event-slots :x)
                                                 (xlib:drawable-x parent))))
                                       (h (if top-quadrant
                                              (- initial-height
                                                 (- (getf event-slots :y)
                                                    initial-y))
                                              (- (getf event-slots :y)
                                                 (xlib:drawable-y parent)
                                                 *float-window-title-height*)))
                                       ;; also move window when in top and/or left quadrant
                                       (x (if left-quadrant
                                              (getf event-slots :x)
                                              initial-x))
                                       (y (if top-quadrant
                                              (getf event-slots :y)
                                              initial-y)))

                                   ;; Don't let the window become too small
                                   (float-window-move-resize window
                                                             :x x
                                                             :y y
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
                    (window-y window) (xlib:drawable-y (window-parent window)))))))
      ;; restore the mouse to its original position
      (xlib:warp-pointer (window-parent window) relx rely))))

(defmethod group-button-press ((group float-group) button x y where)
  (declare (ignore button x y where))
  (when (next-method-p)
    (call-next-method)))

;;; Bindings

(defvar *float-group-top-map* nil)
(defvar *float-group-root-map* nil
  "Commands specific to a floating group context hang from this keymap.
It is available as part of the @dnf{prefix map} when the active group
is a float group.")

(fill-keymap *float-group-top-map*
  *escape-key* '*float-group-root-map*)

(fill-keymap *float-group-root-map*
  (kbd "n")  "next"
  (kbd "p")  "prev")

(pushnew '(float-group *float-group-top-map*) *group-top-maps*)

(defcommand gnew-float (name) ((:rest "Group Name: "))
  "Create a floating window group with the specified name and switch to it."
  (add-group (current-screen) name :type 'float-group))

(defcommand gnewbg-float (name) ((:rest "Group Name: "))
  "Create a floating window group with the specified name, but do not switch to it."
  (add-group (current-screen) name :background t :type 'float-group))
