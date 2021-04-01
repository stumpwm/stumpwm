(in-package :stumpwm)

(define-condition dynamic-group-too-many-windows (error) ()
  (:documentation "This condition is used to control overflowing windows in dynamic groups."))

(defclass dynamic-group (tile-group)
  ((master-location :accessor dynamic-group-master-location
                    :initform :left
                    :allocation :class)
   (master-location-override :accessor dynamic-group-master-location-override
                             :initform nil)
   (master-frame :accessor dynamic-group-master-frame
                 :initform nil)
   (master-window :initarg :master-window :initform nil
                  :accessor dynamic-group-master-window)
   (window-stack :initarg :window-stack :initform nil
                 :accessor dynamic-group-window-stack)
   (superfluous-window :initform nil
                       :accessor moving-superfluous-window)))

(defclass dynamic-window (tile-window) ())

(defvar *dynamic-group-overflow-policy* :least-important
  "Controls which window gets sent to the overflow group when a dynamic group 
overflows. Default value is :LEAST-IMPORTANT. The other possible values are 
:MOST-IMPORTANT, :MASTER, and :NEW-WINDOW. :LEAST-IMPORTANT will take the window 
at the end of the stack, :MOST-IMPORTANT from the beginning, :MASTER takes the 
master window, and :NEW-WINDOW moves the window being introduced to the group.")

(defun dynamic-group-master-location-dwim (set &optional (extent :all))
  "change the location of the master window for (a) dynamic group(s). When SET is 
nil, the current location is returned based on EXTENT. EXTENT is either a list of 
groups or one of the keywords :ALL or :CURRENT. EXTENT controls which groups SET 
applies to. 

When SET is one of :LEFT :RIGHT :TOP or :BOTTOM, EXTENT groups have their location 
overrides set to SET, unless EXTENT is :ALL, in which case the default location is
set to SET. 

When SET is :REVERT, EXTENT groups have their location overrides removed. 

When SET is null, the location for EXTENT groups is returned. If EXTENT is :ALL, 
the default location is returned, while if EXTENT is a list, a list is returned of
the groups locations."
  (declare (type (member :left :right :top :bottom :revert nil) set)
           (type (or cons (member :all :current)) extent))
  (case set
    ((:left :right :top :bottom)
     (case extent
       (:all
        (mapc (let ((trig t))
                (lambda (g)
                  (when (typep g 'dynamic-group)
                    (when trig
                      (setf (dynamic-group-master-location g) set
                            trig nil))
                    (retile g))))
              (screen-groups (current-screen))))
       (:current
        (let ((group (current-group)))
          (when (typep group 'dynamic-group)
            (setf (dynamic-group-master-location-override group) set)
            (retile group))))
       (otherwise
        (mapc (lambda (group)
                (when (typep group 'dynamic-group)
                  (setf (dynamic-group-master-location-override group) set)
                  (retile group)))
              extent))))
    (:revert
     (case extent
       (:all
        (mapc (lambda (group)
                (when (typep group 'dynamic-group)
                  (setf (dynamic-group-master-location-override group) nil)
                  (retile group)))
              (screen-groups (current-screen))))
       (:current
        (let ((group (current-group)))
          (when (typep group 'dynamic-group)
            (setf (dynamic-group-master-location-override group) nil))))
       (otherwise
        (mapc (lambda (group)
                (when (typep group 'dynamic-group)
                  (setf (dynamic-group-master-location-override group) nil)
                  (retile group)))
              extent))))
    (otherwise
     (case extent
       (:all
        (loop for group in (screen-groups (current-screen))
              when (typep group 'dynamic-group)
                return (dynamic-group-master-location group)))
       (:current
        (let ((group (current-group)))
          (when (typep group 'dynamic-group)
            (or (dynamic-group-master-location-override group)
                (dynamic-group-master-location group)))))
       (otherwise
        (if (= (length extent) 1)
            (or (dynamic-group-master-location-override (car extent))
                (dynamic-group-master-location (car extent)))
            (mapcar (lambda (group)
                      (or (dynamic-group-master-location-override group)
                          (dynamic-group-master-location group)))
                    extent)))))))

(defvar *dynamic-group-master-split-ratio* 2/3
  "The ratio with which to split when adding a second window to a dynamic group.
Defaults to 2/3. Must be less than 1.")

(defvar *dynamic-overflow-group* ".Overflow"
  "The group to which windows will be sent when a dynamic group can no longer 
split the window stack.")

(defun dynamic-merge-overflow-group (group to-group)
  (let ((*dynamic-overflow-group* (concat *dynamic-overflow-group* " °2")))
    (merge-groups group to-group)))

(defun dyn-split-frame (group frame how &optional (ratio 1/2))
  "Split FRAME in 2 and return the new frame number if successful. Otherwise, 
return NIL. RATIO is a fraction to split by."
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
        (when (eq (tile-group-current-frame group)
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
  "Splits FRAME by RATIO, or signals an error."
  (let ((fnum (dyn-split-frame group frame dir ratio)))
    (if fnum
        (progn
          (when (frame-window frame)
            (update-decoration (frame-window frame)))
          (show-frame-indicator group)
          fnum)
        (error 'dynamic-group-too-many-windows))))

(defun dyn-hsplit-frame (group frame &optional (ratio "1/2"))
  (dyn-split-frame-in-dir-with-frame group frame :column
                                     (if (stringp ratio)
                                         (read-from-string ratio)
                                         ratio)))

(defun dyn-vsplit-frame (group frame &optional (ratio "1/2"))
  (dyn-split-frame-in-dir-with-frame group frame :row
                                     (if (stringp ratio)
                                         (read-from-string ratio)
                                         ratio)))

(defun dyn-balance-stack-tree (&optional (group (current-group)))
  "Balance only frames in the stack tree, ensuring they are the same size."
  (let ((tree (tile-group-frame-head group (current-head group)))
        (how (dynamic-group-master-location-dwim nil (list group))))
    (when (listp tree)
      (balance-frames-internal group
                               (case how
                                 ((:left :top) (cadr tree))
                                 ((:right :bottom) (car tree)))))))

(defmethod group-repack-frame-numbers ((group dynamic-group))
  (setf (frame-number (window-frame (dynamic-group-master-window group))) 0)
  (loop for i from 1
        for window in (dynamic-group-window-stack group)
        do (setf (frame-number (window-frame window)) i)))

(defun find-empty-frames (&optional (group (current-group)))
  "Returns a list of all frames in GROUP with no window."
  (loop for frame in (group-frames group)
        when (null (frame-windows group frame))
          collect frame))

(defun dyn-handle-overflow (group window)
  "Move WINDOW from GROUP to the dynamic overflow group."
  (let ((to-group (or (find-group (current-screen) *dynamic-overflow-group*)
                      (gnewbg *dynamic-overflow-group*))))
    (move-window-to-group (case *dynamic-group-overflow-policy*
                            (:least-important
                             (lastcar (dynamic-group-window-stack group)))
                            (:most-important
                             (car (dynamic-group-window-stack group)))
                            (:master
                             (dynamic-group-master-window group))
                            (:new-window
                             (setf (moving-superfluous-window group) window)
                             window))
                          to-group)
    (if (eql *dynamic-group-overflow-policy* :new-window)
        (focus-all window)
        (dynamic-group-place-window group window))))

(defun dynamic-group-place-window (group window &optional retiling)
  "The logic to place a window in a dynamic group. If unable to split the stack 
further, send the least important window (bottom of the stack) to a overflow group"
  (case (or retiling (length (group-windows group)))
    (1 (setf (dynamic-group-master-window group) window))
    (2
     (case (dynamic-group-master-location-dwim nil (list group))
       (:left
        (dyn-hsplit-frame group
                          (frame-by-number group 0)
                          *dynamic-group-master-split-ratio*)
        (setf (dynamic-group-master-frame group) (frame-by-number group 0)))
       (:right
        (let ((fnum (dyn-hsplit-frame group
                                      (frame-by-number group 0)
                                      (- 1 *dynamic-group-master-split-ratio*))))
          (setf (dynamic-group-master-frame group) (frame-by-number group fnum))))
       (:top
        (dyn-vsplit-frame group
                          (frame-by-number group 0)
                          *dynamic-group-master-split-ratio*)
        (setf (dynamic-group-master-frame group) (frame-by-number group 0)))
       (:bottom
        (let ((fnum (dyn-vsplit-frame group
                                      (frame-by-number group 0)
                                      (- 1 *dynamic-group-master-split-ratio*))))
          (setf (dynamic-group-master-frame group) (frame-by-number group fnum)))))
     (let* ((prev-win (dynamic-group-master-window group))
            (prev-win-new-frame (car (remove (dynamic-group-master-frame group)
                                             (group-frames group)))))
       (push prev-win (dynamic-group-window-stack group))
       (setf (window-frame prev-win) prev-win-new-frame
             (frame-window prev-win-new-frame) prev-win
             (window-frame window) (dynamic-group-master-frame group)
             (frame-window (dynamic-group-master-frame group)) window
             (dynamic-group-master-window group) window
             (group-current-window group) window)
       (mapcar 'update-decoration (group-windows group))
       (focus-frame group (dynamic-group-master-frame group))))
    (otherwise
     (let* ((master-frame (dynamic-group-master-frame group))
            (old-master (dynamic-group-master-window group))
            (frame-to-split
              (window-frame (car (dynamic-group-window-stack group)))))
       (handler-case
           (progn
             (funcall (case (dynamic-group-master-location-dwim nil (list group))
                        ((:top :bottom) 'dyn-hsplit-frame)
                        ((:right :left) 'dyn-vsplit-frame))
                      group
                      frame-to-split)
             (push (dynamic-group-master-window group)
                   (dynamic-group-window-stack group))
             (setf (dynamic-group-master-window group) window)
             (setf (window-frame window) master-frame
                   (window-frame old-master)
                   (or (car (find-empty-frames group))
                       (error "No Empty Frames in group ~S! Something has gone terribly wrong!" group))
                   (frame-window (window-frame old-master)) old-master)
             (focus-frame group master-frame)
             (group-focus-window group window)
             (mapc 'update-decoration (group-windows group))
             (dyn-balance-stack-tree group))
         (dynamic-group-too-many-windows ()
           (dyn-handle-overflow group window)))))))

(defun dynamic-group-add-window (group window)
  "Add WINDOW to GROUP, making WINDOW the master of group."
  (change-class window 'dynamic-window)
  (setf (window-frame window) (frame-by-number group 0))
  (dynamic-group-place-window group window)
  (loop for frame in (group-frames group)
        do (sync-frame-windows group frame))
  (when (null (frame-window (window-frame window)))
    (frame-raise-window (window-group window) (window-frame window)
                        window nil)))

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
         (dynamic-group-add-window group window)
         (group-repack-frame-numbers group))))

(defun dynamic-group-delete-master-window (group window)
  "The logic to handle deletion of the master window of a dynamic group"
  (let* ((new-master (pop (dynamic-group-window-stack group))))
    (if new-master
        (let* ((new-masters-old-frame (window-frame new-master))
               (master-frame (dynamic-group-master-frame group))
               (head (current-head group))
               (tree (tile-group-frame-head group head)))
          (cond 
            ((not (dynamic-group-window-stack group)) 
             (setf (window-frame new-master) master-frame
                   (frame-window master-frame) new-master
                   (dynamic-group-master-window group) new-master)
             (loop for remframe in (remove (dynamic-group-master-frame group)
                                           (group-frames group))
                   do (setf (tile-group-frame-head group head)
                            (remove-frame tree remframe)))
             (setf (tile-group-current-frame group) master-frame)
             (focus-frame group master-frame)
             (update-decoration (frame-window master-frame)))
            (t 
             (setf (tile-group-frame-head group head)
                   (remove-frame tree new-masters-old-frame)
                   (window-frame new-master) master-frame
                   (frame-window master-frame) new-master
                   (dynamic-group-master-window group) new-master)
             (dyn-balance-stack-tree group))))
        (let ((f (window-frame window)))
          (when (eq (frame-window f) window)
            (frame-raise-window group f
                                (first (frame-windows group f)) nil))))))

(defun dynamic-group-delete-stack-window (group window)
  "Logic to handle deletion of a stack window from a dynamic group"
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
           (focus-frame group (dynamic-group-master-frame group))
           (dyn-balance-stack-tree group))
          (t
           (setf (tile-group-frame-head group head)
                 (remove-frame tree frame-to-remove))
           (focus-frame group (dynamic-group-master-frame group))))))

(defmethod group-delete-window ((group dynamic-group) (window dynamic-window))
  (cond ((equal window (moving-superfluous-window group))
         (setf (moving-superfluous-window group) nil))
        ((equal window (dynamic-group-master-window group))
         (dynamic-group-delete-master-window group window)
         (group-repack-frame-numbers group)
         (focus-frame group (dynamic-group-master-frame group)))
        ((member window (dynamic-group-window-stack group))
         (let ((location (frame-number (window-frame window))))
           (dynamic-group-delete-stack-window group window )
           (group-repack-frame-numbers group)
           (if-let ((frame (frame-by-number group location)))
             (focus-frame group frame)
             (if-let ((stack (dynamic-group-window-stack group)))
               (focus-frame group (window-frame (car (last stack))))
               (focus-frame group (dynamic-group-master-frame group)))))))
  (loop for frame in (group-frames group)
        do (sync-frame-windows group frame)))

(defun retile (group)
  "Retile GROUP, preserving focus and window positions."
  (declare (type dynamic-group group))
  (let ((master (dynamic-group-master-window group))
        (stack (dynamic-group-window-stack group))
        (win (group-current-window group))
        (new-frame (copy-frame (current-head group))))
    (when master
      (setf (tile-group-frame-head group (current-head group)) new-frame
            (tile-group-current-frame group) new-frame
            (dynamic-group-master-window group) nil
            (dynamic-group-window-stack group) nil)
      (focus-frame group new-frame)
      (loop for window in (append (reverse stack) (list master))
            for x from 1
            do (dynamic-group-place-window group window x))
      (loop for frame in (group-frames group)
            do (sync-frame-windows group frame))
      (focus-window win)
      (focus-frame group (window-frame win))
      (mapc 'update-decoration (group-windows group)))))

(defun dynamic-group-float-window (window group)
  "float windows in a dynamic group"
  (cond ((equal window (dynamic-group-master-window group))
         (dynamic-group-delete-master-window group window))
        ((member window (dynamic-group-window-stack group))
         (dynamic-group-delete-stack-window group window))
        (t (message "This window doesnt appear to be tracked by this group.")))
  (change-class window 'float-window)
  (float-window-align window)
  (loop for frame in (group-frames group)
        do (sync-frame-windows group frame))
  (focus-all window))

(defun dynamic-group-unfloat-window (window group)
  "Unfloat windows in a dynamic group"
  (dynamic-group-add-window group window)
  (loop for frame in (group-frames group)
        do (sync-frame-windows group frame))
  (update-decoration window)
  (frame-raise-window group
                      (dynamic-group-master-frame group)
                      (car (frame-windows group (dynamic-group-master-frame group)))))

;;; Dynamic group commands

(defvar *dynamic-group-blacklisted-commands* nil
  "A blacklist of commands for dynamic groups specifically. Needed due to group 
class hierarchy.")

(defun dyn-blacklist-command (cmd &aux (command (get-command-structure cmd nil)))
  "Add CMD to the command blacklist for dynamic groups"
  (unless (member command *dynamic-group-blacklisted-commands*)
    (push command *dynamic-group-blacklisted-commands*)))

(defun dyn-unblacklist-command (cmd &aux (command (get-command-structure cmd nil)))
  "Remove CMD to the command blacklist for dynamic groups"
  (setf *dynamic-group-blacklisted-commands*
        (remove command *dynamic-group-blacklisted-commands*)))

(flet ((bl (&rest cmds)
         (loop for cmd in cmds
               do (dyn-blacklist-command cmd))))
  ;; Due to the group class hierarchy the following tile-group commands must be
  ;; explicitly disabled for dynamic groups. 
  (bl "expose"
      "hsplit"
      "vsplit"
      "hsplit-equally"
      "vsplit-equally"
      "remove-split"
      "remove"
      "only"
      "pull-window-by-number"
      "pull"
      "pull-marked"))

(defcommand gnew-dynamic (name) ((:rest "Group Name: "))
  "Create a new dynamic group named NAME."
  (unless name 
    (throw 'error :abort))
  (add-group (current-screen) name :type 'dynamic-group))

(defcommand gnewbg-dynamic (name) ((:rest "Group Name: "))
  "Create a new dynamic group named NAME in the background."
  (unless name
    (throw 'error :abort))
  (add-group (current-screen) name :type 'dynamic-group :background t))

(defun swap-window-with-master (group window-or-number &optional preserve-location)
  "Swap WINDOW-OR-NUMBER with the master window of GROUP. Only applicable to 
dynamic groups."
  (check-type group dynamic-group)
  (check-type window-or-number (or window number))
  (let* ((location (tile-group-current-frame group))
         (stack (dynamic-group-window-stack group))
         (win (if (numberp window-or-number)
                  (member window-or-number stack :key 'window-number)
                  (member (window-number window-or-number) stack
                          :key 'window-number)))
         (old-master (dynamic-group-master-window group)))
    (when win
      (exchange-windows (dynamic-group-master-window group) (car win))
      (setf (dynamic-group-master-window group) (car win))
      (setf (car win) old-master)
      (if preserve-location
          (focus-frame group location)
          (focus-frame group (dynamic-group-master-frame group))))))

(defun dyn-rotate-stack (group old-master direction)
  "Used by dyn-rotate-windows to handle the stack rotation."
  (case direction
    (:ccl
     (setf (dynamic-group-window-stack group)
           (cons old-master (butlast (dynamic-group-window-stack group)))))
    (:cl
     (setf (dynamic-group-window-stack group)
           (append (cdr (dynamic-group-window-stack group)) (list old-master))))))

(defun dyn-rotate-windows (direction &optional (group (current-group)))
  "Rotate windows in GROUP clockwise or counterclockwise."
  (check-type group dynamic-group)
  (check-type direction (member :cl :ccl))
  (let* ((master-w (dynamic-group-master-window group))
         (master-f (dynamic-group-master-frame group))
         (frames (funcall (if (eq direction :ccl) 'reverse 'identity)
                          (remove master-f (group-frames group))))
         (windows (loop for f in frames collect (frame-window f))))
    (cond ((and (> (length windows) 1)
                (> (length frames) 1))
           (loop for (w1 w2) on windows
                 for (f1 f2) on frames
                 if (and w1 w2 f1 f2)
                   do (pull-window w1 f2)
                 else
                   do (pull-window w1 master-f)
                      (setf (dynamic-group-master-window group) w1)
                      (dyn-rotate-stack group master-w direction)
                      (pull-window master-w (car frames))))
          ((and (= (length windows) 1)
                (= (length frames) 1))
           (swap-window-with-master group
                                    (car (remove master-w
                                                 (group-windows group)))))
          (t (message "Only one window in group")))))

(defun dyn-cycle-windows (direction &key (group (current-group)) (focus :master))
  "Rotate windows in GROUP, with FOCUS either following the current window, always
focusing the master window, or remaining where it is."
  (check-type focus (member :remain :follow :master))
  (let* ((w (group-current-window group))
         (f (and w (window-frame w))))
    (when f
      (dyn-rotate-windows direction group)
      (case focus (:master (focus-window (dynamic-group-master-window group) t))
            (:follow (focus-window w t))
            (:remain (focus-frame group f))))))

(define-stumpwm-type :rotation (input prompt)
  (let* ((values '(("cl" :cl)
                   ("ccl" :ccl)
                   ("clockwise" :cl)
                   ("counterclockwise" :ccl)))
         (string (argument-pop-or-read input prompt (mapcar 'first values)))
         (dir (second (assoc string values :test 'string-equal))))
    (or dir
        (throw 'error "No matching direction."))))

(define-stumpwm-type :dynamic-cycle-focus (input prompt)
  (let* ((values '(("remain" :remain)
                   ("master" :master)
                   ("follow" :follow)))
         (string (argument-pop-or-read input prompt (mapcar 'first values)))
         (dir (second (assoc string values :test 'string-equal))))
    (or dir
        (throw 'error "No matching direction."))))

(defcommand (dyn-cycle dynamic-group) (dir &optional (focus :remain))
    ((:rotation "Direction: ") (:dynamic-cycle-focus))
  (dyn-cycle-windows dir :focus focus))

(defun stack-window-p (window &optional (group (current-group)))
  "Check if WINDOW is a member of GROUPs stack"
  (check-type group dynamic-group)
  (member window (dynamic-group-window-stack group)))

(defun master-window-p (window &optional (group (current-group)))
  "check if WINDOW is GROUPs master window"
  (check-type group dynamic-group)
  (eql window (dynamic-group-master-window group)))

(defun swap-stack-windows (group w1 w2)
  "swaps two stack windows within a dynamic group"
  (check-type group dynamic-group)
  (let ((f1 (window-frame w1))
        (f2 (window-frame w2)))
    (pull-window w1 f2 nil)
    (pull-window w2 f1 nil)
    (rotatef (car (member w1 (dynamic-group-window-stack group)))
             (car (member w2 (dynamic-group-window-stack group))))))

(defun exchange-dynamic-windows (w1 w2)
  "Similar to exchange-window, but tracks state for dynamic groups"
  (assert (eql (window-group w1) (window-group w2)))
  (let ((group (window-group w1)))
    (check-type group dynamic-group)
    (cond ((and (stack-window-p w1 group)
                (stack-window-p w2 group))
           (swap-stack-windows group w1 w2))
          ((and (stack-window-p w1 group)
                (master-window-p w2 group))
           (swap-window-with-master group w1))
          ((and (master-window-p w1 group)
                (stack-window-p w2 group))
           (swap-window-with-master group w2)))
    (focus-all w1)))

(defcommand (dyn-switch dynamic-group) (number) ((:number "Window Number: "))
  (when number
    (labels ((match (win)
               (= (window-number win) number)))
      (let ((win (find-if #'match (group-windows (current-group)))))
        (if win
            (swap-window-with-master (current-group) win t)
            (message "No window of number ~S" number))))))

(defun window-number-as-char (window)
  "return the window number of window as a character"
  (check-type window window)
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
  "Select a window by drawing its number and reading a click or keypress from the 
user"
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

(defcommand (dyn-switch-prompt-for-window dynamic-group) () ()
  "choose a window number by its number and swap it with master window"
  (when-let ((window (choose-window-by-number (current-group)))
             (current-position (window-frame (current-window))))
    (swap-window-with-master (current-group) window t)))

(defcommand (dyn-switch-prompt-for-frame dynamic-group) () ()
  "choose a window number by its frame number and swap it with master window"
  (when-let ((frame (choose-frame-by-number (current-group)))
             (current-position (window-frame (current-window))))
    (swap-window-with-master (current-group) (frame-window frame) t)))

(defcommand (dyn-focus-current-window dynamic-group) (&optional preserve-focus)
    ((:rest))
  "Swap the current window with the master window"
  (if (equal (current-window) (dynamic-group-master-window (current-group)))
      (message "Focused window is already master window")
      (swap-window-with-master (current-group) (current-window) preserve-focus)))

(defcommand (dyn-focus-master-window dynamic-group) () ()
  "focus the master window"
  (focus-frame (current-group) (dynamic-group-master-frame (current-group))))


;;; Dynamic group keybindings

(defvar *dynamic-group-top-map* nil)
(defvar *dynamic-group-root-map* nil
  "Commands specific to a dynamic group context hang from this keymap.
It is available as part of the @dnf{prefix map} when the active group
is a dynamic group.")

(fill-keymap *dynamic-group-top-map*
  *escape-key* '*dynamic-group-root-map*)

(fill-keymap *dynamic-group-root-map*
  (kbd "n")     "dyn-cycle cl remain"
  (kbd "N")     "dyn-cycle cl follow"
  (kbd "M-n")   "dyn-cycle cl master"
  
  (kbd "p")     "dyn-cycle ccl remain"
  (kbd "P")     "dyn-cycle ccl follow"
  (kbd "M-p")   "dyn-cycle ccl master"
  
  (kbd "W")     "dyn-switch"  
  (kbd "w")     "dyn-switch-prompt-for-window"
  (kbd "RET")   "dyn-focus-current-window"
  (kbd "C-RET") "dyn-focus-current-window t"
  (kbd "f")     "dyn-focus-master-window"
  (kbd "o")     "fnext")

(pushnew '(dynamic-group *dynamic-group-top-map*) *group-top-maps*)


