(in-package :stumpwm)

(define-condition dynamic-group-too-many-windows (error) ())

(defclass dynamic-group (tile-group)
  ((master-window :initarg :master-window :initform nil
                  :accessor dynamic-group-master-window)
   (window-stack :initarg :window-stack :initform nil
                 :accessor dynamic-group-window-stack)
   (superfluous-window :initform nil
                       :accessor moving-superfluous-window)))

(defclass dynamic-window (tile-window) ())

(defparameter *maximum-dynamic-group-windows* 7
  "the maximum number of windows that can be placed in a dynamic-group before we 
attempt to place them in the next dynamic-group (or generate a new one)")

(defparameter *dynamic-group-master-split-ratio* "2/3"
  "The ratio with which to split when adding a second window to a dynamic group.")

(defun dyn-split-frame (group frame how &optional (ratio 1/2))
  "Split the specified frame into 2 frames. Return new frame number, if
it succeeded. NIL otherwise. RATIO is a fraction of the screen to
allocate to the new split window. If ratio is an integer then the
number of pixels will be used. This can be handy to setup the
desktop when starting."
  (check-type how (member :row :column))
  (let ((head (frame-head group frame)))
    ;; don't create frames smaller than the minimum size
    (when (or (and (eq how :row)
                   (>= (frame-height frame) (* *min-frame-height* 2)))
              (and (eq how :column)
                   (>= (frame-width frame) (* *min-frame-width* 2))))
      (multiple-value-bind (f1 f2) (funcall (if (eq how :column)
                                                'split-frame-h
                                                'split-frame-v)
                                            group frame ratio)
        (setf (tile-group-frame-head group head)
              (if (atom (tile-group-frame-head group head))
                  (list f1 f2)
                  (funcall-on-node (tile-group-frame-head group head)
                                   (lambda (tree)
                                     (substitute (list f1 f2) frame tree))
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
        (frame-number f2)))))

(defun dyn-split-frame-in-dir-with-frame (group frame dir &optional (ratio 1/2))
  (if (> (length (group-frames group)) *maximum-dynamic-group-windows*)
      (progn (message "Too many windows in group")
             (error 'dynamic-group-too-many-windows))
      (if (or (dyn-split-frame group frame dir ratio))
          (progn
            (when (frame-window frame)
              (update-decoration (frame-window frame)))
            (show-frame-indicator group))
          (error 'dynamic-group-too-many-windows))))

(defun dyn-hsplit-frame (frame &optional (ratio "1/2"))
  (dyn-split-frame-in-dir-with-frame (current-group) frame :column (read-from-string ratio)))

(defun dyn-vsplit-frame (frame &optional (ratio "1/2"))
  (dyn-split-frame-in-dir-with-frame (current-group) frame :row (read-from-string ratio)))

(defun dyn-balance-stack-tree (&optional (group (current-group)))
  "Balance only frames in the stack tree, ensuring they are the same size."
  (let ((tree (tile-group-frame-head group (current-head))))
    (when (listp tree)
      (balance-frames-internal group (cadr tree)))))

(defun dyn-find-superfluous-windows (group)
  (let ((frames (group-frames group)))
    (flatten
     (loop for frame in frames
           when (> (length (frame-windows group frame)) 1)
             collect (remove (frame-window frame) (frame-windows group frame))))))

(defun find-empty-frames (&optional (group (current-group)))
  (loop for frame in (group-frames group)
        when (null (frame-windows group frame))
          collect frame))

(defmethod group-add-window ((group dynamic-group) window &key frame raise &allow-other-keys)
  (cond ((typep window 'float-window)
         (call-next-method)
         (message "Floating windows in dynamic-groups is currently not supported"))
        ((eq frame :float)
         (change-class window 'float-window)
         (float-window-align window)
         (when raise (group-focus-window group window))
         (message "Floating windows in dynamic-groups is currently not supported"))
        (t
         ;; this is undefined behavior i think, but its done in the other
         ;; group-add-window method so i think its ok, at least on sbcl. 
         (change-class window 'dynamic-window)
         (setf (window-frame window) (frame-by-number group 0))
         (case (length (group-windows group))
           (1
            (setf (dynamic-group-master-window group) window))
           (2
            (let ((frame (frame-by-number group 0)))
              (dyn-hsplit-frame frame *dynamic-group-master-split-ratio*))
            (let* ((prev-win (dynamic-group-master-window group))
                   (prev-win-new-frame (car (remove (frame-by-number group 0)
                                                    (group-frames group)))))
              (push prev-win (dynamic-group-window-stack group))
              (setf (window-frame prev-win) prev-win-new-frame
                    (frame-window prev-win-new-frame) prev-win
                    (window-frame window) (frame-by-number group 0)
                    (frame-window (frame-by-number group 0)) window
                    (dynamic-group-master-window group) window)))
           (otherwise
            (let* ((master-frame (frame-by-number group 0))
                   (old-master (dynamic-group-master-window group))
                   (frames-no-master (remove master-frame (group-frames group)))
                   (frame-to-split (car frames-no-master)))
              (handler-case
                  (progn
                    (dyn-vsplit-frame frame-to-split)
                    (push (dynamic-group-master-window group)
                          (dynamic-group-window-stack group))
                    (setf (dynamic-group-master-window group) window)
                    (setf (window-frame window) (frame-by-number group 0)
                          (window-frame old-master)
                          (or (car (find-empty-frames group))
                              (error "No Empty Frames in group ~S! Something has gone terribly wrong!" group))
                          (frame-window (window-frame old-master)) old-master)
                    (focus-frame group (frame-by-number group 0))
                    (dyn-balance-stack-tree group))
                (dynamic-group-too-many-windows ()
                  (setf (moving-superfluous-window group) window)
                  (let ((new-group (or (find-group (current-screen) ".Overflow")
                                       (gnewbg ".Overflow"))))
                    (move-window-to-group window new-group)))))))
         (loop for frame in (group-frames group)
               do (sync-frame-windows group frame))
         (when (null (frame-window (window-frame window)))
           (frame-raise-window (window-group window) (window-frame window)
                               window nil)))))

(defmethod group-delete-window ((group dynamic-group) (window dynamic-window))
  (cond ((equal window (moving-superfluous-window group))
         (setf (moving-superfluous-window group) nil))
        ((equal window (dynamic-group-master-window group))
         ;; (message "group-windows: ~S" (group-windows group))
         (let* ((new-master (pop (dynamic-group-window-stack group))))
           (if new-master
               (let* ((new-masters-old-frame (window-frame new-master))
                      (master-frame (frame-by-number group 0))
                      (head (current-head group))
                      (tree (tile-group-frame-head group head)))
                 (cond 
                   ((not (dynamic-group-window-stack group)) ; we got 1 window left
                    (setf (window-frame new-master) master-frame
                          (frame-window master-frame) new-master
                          (dynamic-group-master-window group) new-master)
                    (loop for remframe in (remove (frame-by-number group 0)
                                                  (group-frames group))
                          do (setf (tile-group-frame-head group head)
                                   (remove-frame tree remframe)))
                    (setf (tile-group-current-frame group) master-frame)
                    (focus-frame group master-frame)
                    (update-decoration (frame-window master-frame))
                    (loop for frame in (group-frames group)
                          do (sync-frame-windows group frame)))
                   (t ; we have a stack left and a new master window
                    (setf (tile-group-frame-head group head)
                          (remove-frame tree new-masters-old-frame)
                          (window-frame new-master) master-frame
                          (frame-window master-frame) new-master
                          (dynamic-group-master-window group) new-master)
                    (dyn-balance-stack-tree group)
                    (loop for frame in (group-frames group)
                          do (sync-frame-windows group frame))
                    (focus-frame group master-frame))))
               (let ((f (window-frame window))) ; taken from tile-group/window
                 ;; maybe pick a new window for the old frame
                 (when (eq (frame-window f) window)
                   (frame-raise-window group f
                                       (first (frame-windows group f)) nil))))))
        ((member window (dynamic-group-window-stack group))
         (let* ((new-stack (remove window (dynamic-group-window-stack group)))
                (frame-to-remove (window-frame window))
                (head (frame-head group frame-to-remove))
                (tree (tile-group-frame-head group head)))
           (setf (dynamic-group-window-stack group) new-stack)
           (cond (new-stack
                  (setf (tile-group-frame-head group head)
                        (remove-frame tree frame-to-remove))
                  (tree-iterate tree (lambda (leaf)
                                       (sync-frame-windows group leaf)))
                  (focus-frame group (frame-by-number group 0))
                  (dyn-balance-stack-tree group)
                  (loop for frame in (group-frames group)
                        do (sync-frame-windows group frame)))
                 (t
                  (setf (tile-group-frame-head group head)
                        (remove-frame tree frame-to-remove))
                  (focus-frame group (frame-by-number group 0))
                  (loop for frame in (group-frames group)
                        do (sync-frame-windows group frame)))))))
  (loop for frame in (group-frames group)
        do (sync-frame-windows group frame)))



;;; Dynamic group commands

(defcommand gnew-dynamic (name) ((:rest "Group Name: "))
  (unless name 
    (throw 'error :abort))
  (add-group (current-screen) name :type 'dynamic-group))

(defcommand gnewbg-dynamic (name) ((:rest "Group Name: "))
  (unless name
    (throw 'error :abort))
  (add-group (current-screen) name :type 'dynamic-group :background t))

(defgeneric swap-window-with-master (group window-or-number)
  (:documentation "Swap a window with the master window."))

(defmethod swap-window-with-master (group window-or-number)
  (declare (ignore group window-or-number))
  (message "No master window in non-dynamic groups"))

(defmethod swap-window-with-master ((group dynamic-group) window-or-number)
  (let* ((stack (dynamic-group-window-stack group))
         (win (if (numberp window-or-number)
                  (member window-or-number stack :key 'window-number)
                  (member (window-number window-or-number) stack
                          :key 'window-number)))
         (old-master (dynamic-group-master-window group)))
    (if win
        (progn
          (exchange-windows (dynamic-group-master-window group) (car win))
          (setf (dynamic-group-master-window group) (car win))
          (setf (dynamic-group-window-stack group)
                (remove (car win) (dynamic-group-window-stack group)))
          (push old-master (dynamic-group-window-stack group))
          (focus-all (car win)))
        (message "Window not a member of the stack"))))

(defun dyn-cycle-windows (direction &optional (group (current-group)))
  (check-type group dynamic-group)
  (check-type direction (member :up :down))
  (let* ((sorted (sort (copy-seq (group-windows group))
                       (if (eq direction :down) '< '>)
                       :key 'window-number))
         (cycle-to (cadr (member (dynamic-group-master-window group) sorted))))
    (when sorted
      (swap-window-with-master group (window-number (if cycle-to
                                                        cycle-to
                                                        (car sorted)))))))

(define-stumpwm-type :up/down (input prompt)
  (let* ((values '(("up" :up)
                   ("down" :down)))
         (string (argument-pop-or-read input prompt (mapcar 'first values)))
         (dir (second (assoc string values :test 'string-equal))))
    (or dir
        (throw 'error "No matching direction."))))

(defcommand dyn-cycle (direction) ((:up/down "Enter up or down: "))
  (dyn-cycle-windows direction))

(defcommand dyn-switch (number) ((:number "Window Number: "))
  (when number
    (labels ((match (win)
               (= (window-number win) number)))
      (let ((win (find-if #'match (group-windows (current-group)))))
        (if win
            (swap-window-with-master (current-group) win)
            (message "No window of number ~S" number))))))

(defun window-number-as-char (window)
  (char (prin1-to-string (window-number window)) 0))

(defun draw-window-numbers (group)
  "like draw-frame-numbers, but draws the window number of the frames window"
  (let ((screen (group-screen group)))
    (mapcar (lambda (frame)
              (let* ((w (xlib:create-window
                         :parent (screen-root screen)
                         :x (frame-x frame) :y (frame-display-y group frame)
                         :width 1 :height 1
                         :background (screen-fg-color screen)
                         :border (screen-border-color screen)
                         :border-width 1
                         :event-mask '())))
                (xlib:map-window w)
                (setf (xlib:window-priority w) :above)
                (echo-in-window w (screen-font screen)
                                (screen-fg-color screen)
                                (screen-bg-color screen)
                                (string
                                 (window-number-as-char (frame-window frame))))
                (xlib:display-finish-output *display*)
                (dformat 3 "mapped ~S~%" (window-number (frame-window frame)))
                w))
            (group-frames group))))

(defun choose-window-by-number (group)
  (let ((wins (progn (draw-frame-outlines group)
                     (draw-window-numbers group)))
        winner num)
    (unwind-protect
         (multiple-value-bind (has-click ch x y)
             (read-one-char-or-click group)
           (cond (has-click
                  (dolist (f (group-frames group))
                    (when (and (> x (frame-x f)) (> y (frame-y f)))
                      (if winner
                          (when (or (> (frame-x f) (frame-x winner))
                                    (> (frame-y f) (frame-y winner)))
                            (setf winner f))
                          (setf winner f))))
                  (ungrab-pointer)
                  (frame-window winner))
                 (ch
                  (setf num (read-from-string (string ch) nil nil))
                  (dformat 3 "read ~S ~S~%" ch num)
                  (find num (group-windows group) :test '= :key 'window-number))))
      (mapc #'xlib:destroy-window wins)
      (clear-frame-outlines group))))

(defcommand dyn-switch-prompt-for-window () ()
  (when-let ((window (choose-window-by-number (current-group))))
    (swap-window-with-master (current-group) window)))

(defcommand dyn-switch-prompt-for-frame () ()
  (when-let ((frame (choose-frame-by-number (current-group))))
    (swap-window-with-master (current-group) (frame-window frame))))

(defcommand dyn-focus-current-window () ()
  (if (equal (current-window) (dynamic-group-master-window (current-group)))
      (message "Focused window is already master window")
      (swap-window-with-master (current-group) (current-window))))

(defcommand dyn-focus-master-window () ()
  (focus-frame (current-group) (frame-by-number (current-group) 0)))


;;; Dynamic group keybindings

(defvar *dynamic-group-top-map* nil)
(defvar *dynamic-group-root-map* nil
  "Commands specific to a dynamic group context hang from this keymap.
It is available as part of the @dnf{prefix map} when the active group
is a dynamic group.")

(fill-keymap *dynamic-group-top-map*
  *escape-key* '*dynamic-group-root-map*)

(fill-keymap *dynamic-group-root-map*
  (kbd "n")   "dyn-cycle down"
  (kbd "p")   "dyn-cycle up"
  (kbd "W")   "dyn-switch"  
  (kbd "w")   "dyn-switch-prompt-for-window"
  (kbd "SPC") "dyn-focus-master-window"
  (kbd "RET") "dyn-focus-current-window"
  (kbd "f")   "dyn-focus-master-window")

(pushnew '(dynamic-group *dynamic-group-top-map*) *group-top-maps*)


