;;;; DYNAMIC TILING GROUPS

;;; Maintainer: szos at posteo dot net

;; This file is part of stumpwm.
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

;;; Commentary:

;;; Overview
;; This file implements dynamic tiling à la DWM, where windows are organized
;; into the master window and the window stack. There exists one master window
;; per head within a group. When a new window is added to a head within the
;; group, that heads master window is pushed onto that heads stack, and the new
;; window becomes the master window.

;;; Window Placement Policy
;; When a window is added to a dynamic group it must be determined which head to
;; place the window upon. This is controlled by the class allocated slot
;; head-placement-policy. It can either be a keyword of :FIRST through :FIFTH,
;; in which case the window is placed on that head, or the keyword
;; :CURRENT-FRAME, in which case the head of the current frame is selected.

;;; Overflow Policy
;; In the event that the head a window is to be placed upon is full, a window,
;; head, and group are chosen as backups. The chosen window is then moved to the
;; chosen head or, if that head is itself full, to the chosen group. The window
;; can be chosen by one of four keywords, :NEW-WINDOW, :MASTER-WINDOW,
;; :STACK-BEG, and :STACK-END. The head can be chosen by one of the keywords
;; :FIRST through :FIFTH, or :ANY and :ORDERED. :ANY uses the first empty head
;; it can find, while :ORDERED looks for the lowest numbered head. The group can
;; be chosen by any valid string. The group will be created if it does not
;; exist.

;;; Window Layouts
;; The location of the master window can be chosen on a global or per head
;; basis. The location can be chosen by one of four keywords, :LEFT :RIGHT :TOP
;; and :BOTTOM. Likewise, the amount of space given to the master window can
;; chosen on a global or per head basis. This is given as a number (fraction)
;; between zero and one exclusive representing the amount of the screen to give
;; to the master window. In practice it must be large enough that the master
;; window is not smaller than the minimum frame width/height and small enough
;; that the window stack is not smaller than the minimum frame width/height.

;;; Code:

(in-package :stumpwm)

(defmacro swap (a b)
  "Swap the values of A and B using PSETF."
  `(psetf ,a ,b
          ,b ,a))

;; The window definition remains unchanged, as at its core it is a tile
;; window. All we do is add a single tag.

(defclass dynamic-window (tile-window)
  ((superfluous :initform nil
                :accessor superfluous-window-tag)))

(defmethod superfluous-window-p ((window dynamic-window))
  (superfluous-window-tag window))

(defmethod superfluous-window-p ((window window))
  nil)

;; Class definition is greatly changed. We track more things at the class level
;; instead of at the object level, and consolidate our layout information and
;; whatnot into an alist with heads as the keys. We also expand our overflow
;; policy to live at the class level and add a head placement policy to
;; determine where new windows should be placed. 

(defclass dynamic-group (tile-group)
  (;; Class allocated slots
   (head-placement-policy
    :reader dynamic-group-head-placement-policy
    :initform :current-frame
    :allocation :class
    :documentation "Control which head new windows are placed upon. Valid values
are :current-frame :first :second :third :fourth and :fifth")
   (overflow-policy
    :reader dynamic-group-overflow-policy
    :initform (list :stack-end :ordered ".Overflow")
    :allocation :class
    :documentation "Control which window goes where when a head/group cannot
hold more windows. 

The CAR is which window to remove from the group. Possible values are 
:new-window :master-window :stack-end and :stack-beg

The CADR is which head to move the window being removed to. Possible values are
:any :ordered :first :second :third :fourth and :fifth. 

The CADDR is what group to move the window being removed to in the event that it
cannot be placed on a head in the group. Possible values are any and all strings.")
   (master-layout
    :reader dynamic-group-master-layout
    :initform :left
    :allocation :class
    :documentation "The default layout of the master window and window
stack. Valid values are :left :right :top and :bottom")
   (split-ratio
    :reader dynamic-group-default-split-ratio
    :initform 2/3
    :allocation :class
    :documentation "The default ratio for the split between the master window
and the window stack. Valid values are any number between zero and one exclusive.")
   ;; Object allocated slots
   (head-info-alist
    ;; TODO: Potentially update this slot to be an alist whose values is an
    ;; array. This may make access faster. However given the size of the lists,
    ;; this is likely to have no impact. This is an implementation specific
    ;; question, look at sbcl specifics and decide. 
    
    ;; TODO: Add superfluous window tracking to this alist. This should always
    ;; be nil, unless a window ... WAIT! Do we even need to track "superfluous"
    ;; windows? Or, because we changed how we add windows to a group to no
    ;; longer use condition signalling (instead calculating the max number of
    ;; stack windows before trying to place it) can we just... idk, not track
    ;; it? I need to do some digging and find out...
    :accessor dynamic-group-head-info-alist
    :documentation "Alist with heads as keys containing information for each
head.  Calling ASSOC on this alist returns a list whose CAR is the head, CADR is
the layout of the frames, CADDR is the master frame, CADDDR is the the master
window, CADDDDR is the window stack frames, CADDDDDR is the window stack
windows, and CADDDDDDR is the major split ratio."))
  (:documentation "A group type that implements dynamic tiling à la DWM with a
single master window and a window stack."))

(defun dynamic-group-p (thing)
  (typep thing 'dynamic-group))

;; We need an method after initialization in order to set up our head alist with
;; the heads present when the group is created.

(defmethod initialize-instance :after ((group dynamic-group)
                                       &key &allow-other-keys)
  "Initialize information for all present heads for dynamic groups."
  (let ((heads (group-heads group)))
    (setf (dynamic-group-head-info-alist group)
          (loop for head in heads
                collect (list head ; key for the alist
                              (dynamic-group-master-layout group) ; frame layout
                              (car (head-frames group head)) ; default master frame
                              nil ; no master window yet
                              nil ; no window stack frames yet
                              nil ; no window stack windows yet
                              (dynamic-group-default-split-ratio group))))))

;; We create some basic wrappers to get the information for a specific head and
;; to add the information for a new head all in one go.

(defmethod dynamic-group-head-info ((group dynamic-group) head)
  "Return the list of information for HEAD in GROUP. This list contains in order
the layout, master frame, the master window, and the window stack."
  (assoc head (dynamic-group-head-info-alist group)))

;; An anaphoric macro that exposes members of the information list for a
;; specific head. Specific names can be provided through the key arguments, and
;; key arguments are themselves the default names. 
(defmacro with-group-head-info ((group head &key layout split-ratio
                                              master-frame master-window
                                              stack-frames stack-windows)
                                &body body)
  (with-gensyms (head-info)
    `(let ((,head-info (dynamic-group-head-info ,group ,head)))
       (symbol-macrolet ((,(or layout 'layout)
                           (cadr ,head-info))
                         (,(or split-ratio 'split-ratio)
                           (caddr (cddddr ,head-info)))
                         (,(or master-frame 'master-frame)
                           (caddr ,head-info))
                         (,(or master-window 'master-window)
                           (cadddr ,head-info))
                         (,(or stack-frames 'stack-frames)
                           (car (cddddr ,head-info)))
                         (,(or stack-windows 'stack-windows)
                           (cadr (cddddr ,head-info))))
         
         ,@body))))

;; We also need a writer method for a couple of the class allocated slots. These
;; should have the same name as our slot reader and should include a keyarg to
;; update all heads, and update all groups. If updating all heads we map over
;; the head alist. If updating all group heads we map over every group and
;; update their head alist. as such providing the keyarg update-all-group-heads
;; implies update-all-heads

;; Setf methods for the layout and split ratio slots, both head local and
;; global. The global methods take an optional argument specifying which heads
;; to update to the new value. All updated heads are retiled to immediately
;; reflect the changes.

(defmethod (setf dynamic-group-master-layout)
    (new (group dynamic-group) &optional (update-heads :unset))
  ;; Possible values for update-heads are :unset, :all, or :none
  (if (typep new 'keyword)
      (let ((old (slot-value group 'master-layout)))
        (setf (slot-value group 'master-layout) new)
        (unless (eql update-heads :none)
          (labels ((update-group (g)
                     (if (eql update-heads :unset)
                         (loop for info in (slot-value g 'head-info-alist)
                               when (eql old (cadr info))
                                 do (setf (cadr info) new)
                                    (dynamic-group-retile-head g (car info)))
                         (loop for info in (slot-value g 'head-info-alist)
                               do (setf (cadr info) new)
                                  (dynamic-group-retile-head g (car info))))))
            (mapc #'update-group
                  (remove-if-not #'dynamic-group-p
                                 (screen-groups (group-screen group)))))))
      (error "Expected a keyword but recieved ~A" new)))

(defmethod (setf dynamic-group-default-split-ratio)
    (new (group dynamic-group) &optional (update-heads :unset))
  (if (> 1 new 0)
      (let ((old (dynamic-group-default-split-ratio group)))
        (setf (slot-value group 'split-ratio) new)
        (unless (eql update-heads :none)
          (labels ((update-group (g)
                     (if (eql update-heads :unset)
                         (loop for info in (slot-value g 'head-info-alist)
                               when (= old (caddr (cddddr info)))
                                 do (setf (caddr (cddddr info)) new)
                                    (dynamic-group-retile-head g (car info)))
                         (loop for info in (slot-value g 'head-info-alist)
                               do (setf (caddr (cddddr info)) new)
                                  (dynamic-group-retile-head g (car info))))))
            (mapc #'update-group
                  (remove-if-not #'dynamic-group-p
                                 (screen-groups (group-screen group)))))))
      (error "Expected a ratio between zero and one exclusive, but got ~A" new)))

(defmethod (setf dynamic-group-head-layout) (new (group dynamic-group) head)
  (if (typep new 'keyword)
      (with-group-head-info (group head)
        (setf layout new)
        (dynamic-group-retile-head group head))
      (error "Expected a keyword but recieved ~A" new)))

(defmethod (setf dynamic-group-head-split-ratio) (new (group dynamic-group) head)
  (if (> 1 new 0)
      (with-group-head-info (group head)
        (setf split-ratio new)
        (dynamic-group-retile-head group head))
      (error "Expected a ratio between zero and one exclusive, but got ~A" new)))

(defmethod (setf dynamic-group-overflow-policy) (new (group dynamic-group))
  (if (and
       (member (car new) '(:stack-end :stack-beg :new-window :master-window))
       (member (cadr new) '(:any :ordered :first :second :third :fourth :fifth))
       (stringp (caddr new)))
      (setf (slot-value group 'overflow-policy) new)
      (error "The list ~A is not a valid overflow policy." new)))

(defmethod (setf dynamic-group-head-placement-policy) (new (group dynamic-group))
  (if (member new '(:current-frame :first :second :third :fourth :fifth))
      (setf (slot-value group 'head-placement-policy) new)
      (error "The value ~A is not a valid head placement policy." new)))

(defmethod master-window-p ((group dynamic-group) head (window dynamic-window))
  (with-group-head-info (group head)
    (eql window master-window)))

(defmethod master-window-p (g h w)
  (declare (ignore g h w))
  nil)

(defmethod stack-window-p ((group dynamic-group) head (window dynamic-window))
  (with-group-head-info (group head)
    (member window stack-windows)))

(defmethod stack-window-p (g h w)
  (declare (ignore g h w))
  nil)

;; Create methods for adding and removing heads from a group. These are needed
;; in order to make sure our head alist tracks new/removed heads. 

(defmethod group-add-head ((group dynamic-group) head)
  (let ((new-frame-num (find-free-frame-number group)))
    (setf (tile-group-frame-tree group)
          (insert-before (tile-group-frame-tree group)
                         (copy-frame head)
                         (head-number head)))
    ;; Set up the new association
    (let ((frame (tile-group-frame-head group head)))
      (setf (frame-number frame) new-frame-num)
      (push (list head
                  (dynamic-group-master-layout group)
                  frame
                  nil
                  nil
                  nil
                  (dynamic-group-default-split-ratio group))
            (dynamic-group-head-info-alist group)))))

(defmethod group-remove-head ((group dynamic-group) head)
  (group-sync-all-heads group)
  (let* ((windows (head-windows group head))
         (frames-to-delete (tile-group-frame-head group head))
         (list-of-frames-to-delete (if (atom frames-to-delete)
                                       (list frames-to-delete)
                                       (flatten frames-to-delete)))
         (group-frame-tree (tile-group-frame-tree group))
         (new-frame? (member (tile-group-current-frame group)
                             list-of-frames-to-delete))
         (old-frame? (member (tile-group-last-frame group)
                             list-of-frames-to-delete)))
    ;; Remove the current heads frames
    (setf (tile-group-frame-tree group) (delete frames-to-delete group-frame-tree))
    ;; When the head removed holds the current frame, update it. 
    (when new-frame?
      (setf (tile-group-current-frame group) (first (group-frames group))))
    ;; When the head removed holds the last frame, update it. 
    (when old-frame?
      (setf (tile-group-last-frame group) nil))
    ;; Loop over all heads and attempt to place orphaned windows. 
    (do ((heads (remove head (group-heads group)) (cdr heads)))
        ((not (and heads windows))) 
      ;; place windows until none are left or head is full. 
      (loop until (or (dynamic-group-head-full-p group (car heads))
                      (not windows))
            do (dynamic-group-place-window group (car heads) (pop windows))))
    ;; If any windows remain, move them to the overflow group.
    (let* ((g (third (dynamic-group-overflow-policy group)))
           (overflow (or (find-group (group-screen group) g)
                         (gnewbg g))))
      (do ((win windows (cdr win)))
          ((not win))
        (move-window-to-group (car win) overflow)))
    ;; Finally, remove the head from the groups alist entry.
    (let ((alist (dynamic-group-head-info-alist group)))
      (setf (dynamic-group-head-info-alist group)
            (remove (assoc head alist) alist)))))

(defun dynamic-group-head-full-p (group head)
  "Calculate the total number of frames a head can hold, and compare that with
the number of windows to see if there is space for another window. "
  (assert (typep head 'head))
  (with-group-head-info (group head)
    (let ((wincount (length stack-windows ;; (head-windows group head)
                            )))
      (>= wincount ; one window will be used for the master, so not >=
          (case layout ; (dynamic-group-head-layout group head)
            ;; Calculate minimum width and heigth of frames, use that to find how
            ;; many frames can fit in the window stack. Err on the side of caution
            ((:top :bottom)
             (floor (/ (frame-width head)
                       (* 2 *min-frame-width*))))
            ((:right :left)
             (floor (/ (frame-height head)
                       (* 2 *min-frame-height*)))))))))

(defun dynamic-group-head-main-split (group head)
  "Return the stack tree and the master frame for GROUP and HEAD."
  (let* ((fh (tile-group-frame-head group head)))
    (if (frame-p fh)
        (values nil fh)
        (with-group-head-info (group head) 
          (case layout
            ((:top :left)
             (values (cadr fh) 
                     (car fh)))
            ((:bottom :right)
             (values (car fh)
                     (cadr fh))))))))

(defun dynamic-group-head-final-frame (group head)
  "Return the last frame in the stack tree. for GROUP and HEAD."
  (let ((stack-tree (dynamic-group-head-main-split group head)))
    (labels ((get-final-frame (tree)
               (when tree
                 (or (and (frame-p tree) tree)
                     (get-final-frame (cadr tree))))))
      (get-final-frame stack-tree))))

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
        (when (eq (tile-group-current-frame group)
                  frame)
          (setf (tile-group-current-frame group) f1))
        (setf (tile-group-last-frame group) f2)
        (values (frame-number f2)
                f1
                f2)))))

(define-condition dynamic-group-too-many-windows (error)
  ((dgtmw-group :initform nil :initarg :group :reader dgtmw-group))
  (:report (lambda (c s)
             (format s "To many splits made in group ~A."
                     (group-name (dgtmw-group c))))))

(defun dyn-split-frame-in-dir-with-frame (group frame dir &optional (ratio 1/2))
  "Splits FRAME by RATIO, or signals an error."
  (multiple-value-bind (fnum f1 f2) (dyn-split-frame group frame dir ratio)
    (if fnum
        (progn
          (when (frame-window frame)
            (update-decoration (frame-window frame)))
          (show-frame-indicator group)
          (values fnum f1 f2))
        (error 'dynamic-group-too-many-windows :group group))))

(defmethod group-add-window ((group dynamic-group) window &key frame raise
                             &allow-other-keys)
  (cond ((typep window 'float-window)
         (call-next-method)) 
        ((eq frame :float)
         (change-class window 'float-window)
         (float-window-align window)
         (when raise (group-focus-window group window)))
        (t ; if were not dealing with a floating window
         (let ((head (choose-head-from-placement-policy group)))
           ;; keep all calls to change-class in the same place.x
           (change-class window 'dynamic-window) 
           (dynamic-group-add-window group head window)))))

(defmethod group-delete-window ((group dynamic-group) (window dynamic-window))
  "Delete a dynamic window from a dynamic group. For floating windows we fall
back to the behavior defined for tile groups."
  (let* ((head (window-head window))
         (final-frame (dynamic-group-head-final-frame group head)))
    (labels ((dyn-remove-split (frame)
               ;; Remove the split without updating windows to the new size, as
               ;; thats done by SYNCHRONIZE-FRAMES-AND-WINDOWS
               (let ((tree (tile-group-frame-head group head)))
                 (setf (tile-group-frame-head group head)
                       (remove-frame tree frame)))))
      (with-group-head-info (group head)
        (cond ((superfluous-window-p window)
               ;; window was never placed and is going straight to the overflow
               ;; group.
               (setf (superfluous-window-tag window) nil))
              ((eql window master-window) 
               (cond ((cadr stack-windows) ; two+ stack windows
                      (setf master-window (car stack-windows)
                            stack-windows (cdr stack-windows))
                      (dyn-remove-split final-frame)
                      (balance-frames-internal group
                                               (dynamic-group-head-main-split group
                                                                              head)
                                               nil)
                      (synchronize-frames-and-windows group head)
                      (focus-frame group master-frame))
                     ((car stack-windows) ; one stack window
                      (let ((final-frame
                              (dynamic-group-head-final-frame group head)))
                        (setf master-window (car stack-windows)
                              stack-windows nil)
                        (dyn-remove-split final-frame)
                        (setf master-frame (tile-group-frame-head group head))
                        (synchronize-frames-and-windows group head)
                        (focus-frame group master-frame)))
                     (t ; No stack windows
                      (psetf master-window nil
                             (frame-window master-frame) nil)
                      (synchronize-frames-and-windows group head)
                      (focus-frame group master-frame))))
              ((member window stack-windows)
               ;; Because theres a stack window, we are assured that we have at
               ;; least two frames, and FINAL-FRAME will always return the stack
               ;; frame.  
               (let ((fnum (frame-number (window-frame window))))
                 (setf stack-windows (remove window stack-windows))
                 (dyn-remove-split final-frame)
                 (when-let ((tree (dynamic-group-head-main-split group head)))
                   ;; Only balance the stack tree if theres a stack.
                   (balance-frames-internal group tree nil))
                 (synchronize-frames-and-windows group head)
                 (labels ((find-closest-frame (number frames &optional dif closest)
                            ;; Find the frame with the closest number to NUMBER.
                            (if frames
                                (if (or (not dif)
                                        (> dif
                                           (- number (frame-number (car frames)))))
                                    (find-closest-frame number
                                                        (cdr frames)
                                                        (- number
                                                           (frame-number
                                                            (car frames)))
                                                        (car frames))
                                    (find-closest-frame number (cdr frames)
                                                        dif closest))
                                closest)))
                   (let ((frames (head-frames group head)))
                     ;; Try to focus the most recently focused frame, unless its
                     ;; been removed in which case find the closest frame number. 
                     (focus-frame group
                                  (or (find fnum frames :key #'frame-number)
                                      (find-closest-frame fnum frames)
                                      master-frame))))))
              (t
               (error "Group ~A desynchronized on removal of window ~A"
                      group window)))))))

(defmethod choose-head-from-placement-policy ((group dynamic-group))
  "Return the head to place new windows into according to the head placement
policy of GROUP"
  (case (dynamic-group-head-placement-policy group)
    ((:current-frame)
     (frame-head group (tile-group-current-frame group)))
    ((:first :second :third :fourth :fifth)
     (if-let ((head (funcall (intern (symbol-name
                                      (dynamic-group-head-placement-policy group)))
                             (group-heads group))))
       head
       ;; If it doesnt exist, just give the final head, cause theyve specified a
       ;; head position beyond the end of the list of head. 
       (lastcar (group-heads group))))
    (otherwise
     (error "~A is not a valid head placement policy."
            (dynamic-group-head-placement-policy group)))))

(defun dynamic-group-add-window (group head window)
  ;; Add a window to a dynamic group, on a specific head. This should only be
  ;; called with a dynamic group and dynamic window. 
  (if (dynamic-group-head-full-p group head)
      (progn (message "Head ~A in group ~A is full" head group)
             (handle-head-overflow group head window))
      (dynamic-group-place-window group head window))
  ;; The LOOP and WHEN forms here could maybe be removed...? I think the syncing
  ;; of the frame windows is done by synchronize-frames-and-windows
  ;; (specifically by the call to maximize). And the frame-window of the
  ;; window-frame of the window will always be set, and the window should always
  ;; be raised. 
  (loop for frame in (group-frames group)
        do (sync-frame-windows group frame))
  (when (null (frame-window (window-frame window)))
    (frame-raise-window (window-group window) (window-frame window)
                        window nil)))

(labels
    ((initialize-group-head-master-stack-split (group head)
       ;; Create a split, setting the master and stack frame values for the
       ;; group and head appropriately. return the stack and master frames.
       (let ((frame (tile-group-frame-head group head)))
         (assert (frame-p frame))
         (with-group-head-info (group head :split-ratio ratio)
           (multiple-value-bind (fnum f1 f2)
               (dyn-split-frame-in-dir-with-frame group frame
                                                  (case layout
                                                    ((:left :right) :column)
                                                    ((:top :bottom) :row))
                                                  (case layout
                                                    ((:left :top) ratio)
                                                    ((:right :bottom)
                                                     (- 1 ratio))))
             (declare (ignore fnum))
             ;; Ensure that the master frame always has the lowest frame number.
             (when (or (eql layout :right)
                       (eql layout :bottom))
               (swap (frame-number f1) (frame-number f2)))
             (macrolet ((select-frame (right-and-bottom left-and-top)
                          ;; Because f1 and f2 can both be the master frame
                          ;; depending upon our layout, we need a way of
                          ;; consistently selecting the master frame and stack
                          ;; frame here. So we use this local macro.
                          `(if (or (eql layout :right)
                                   (eql layout :bottom))
                               ,right-and-bottom
                               ,left-and-top)))
               (psetf master-frame (select-frame f2 f1)
                      stack-frames (list (select-frame f1 f2)))
               ;; Return (values stack-frame master-frame)
               (select-frame (values f1 f2) (values f2 f1)))))))
     (add-stack-frame (group head)
       ;; Add a frame to the stack. We always add the frame to the end of the
       ;; stack, which effectively turns the frame tree into a list. 
       (labels ((get-final-frame (tree)
                  ;; run through the tree until we get a frame.
                  (or (and (frame-p tree) tree)
                      (get-final-frame (cadr tree)))))
         (with-group-head-info (group head)
           (let* ((fh (tile-group-frame-head group head)) 
                  (tree (case layout ((:top :left) (cadr fh)) ; get stack tree
                                     ((:bottom :right) (car fh))))
                  (frame-to-split (get-final-frame tree)))
             (dyn-split-frame-in-dir-with-frame group
                                                frame-to-split
                                                (case layout
                                                  ((:left :right) :row)
                                                  ((:top :bottom) :column))
                                                split-ratio)))))
     (add-window-to-stack (group head window)
       ;; Push WINDOW onto the stack. This assumes there already is a windows
       ;; stack. 
       (with-group-head-info (group head)
         (push window stack-windows))))
  (defun dynamic-group-place-window (group head window)
    ;; This function should only be called when HEAD can accept WINDOW. This
    ;; function DOES NOT check for or protect against head/group overflow.
    (with-group-head-info (group head :layout head-layout :split-ratio ratio)
      (let ((head-frame-tree (tile-group-frame-head group head)))
        (if (frame-p head-frame-tree)
            ;; Then theres only one frame, and we need to check the number of
            ;; windows to see if we are adding the initial window or moving the
            ;; initial window to the stack.
            
            ;; TODO: This could be rewritten to not use case, and not depend on
            ;; the number of windows. 
            (case (or (and master-window
                           (length (cons master-window stack-windows)))
                      0)
              (0 ;; Initialize master window
               (psetf master-frame head-frame-tree
                      master-window window
                      ;; set up the single window and frame
                      (window-frame window) head-frame-tree
                      (frame-window head-frame-tree) window
                      (group-current-window group) window)
               (update-decoration window)
               (raise-window window)
               (focus-frame group master-frame))
              (1
               (multiple-value-bind (stack master)
                   ;; Create the master/stack split, set up the head info
                   ;; alist.
                   (initialize-group-head-master-stack-split group head)
                 (declare (ignorable stack master))
                 (psetf stack-windows (list master-window)
                        master-window window
                        (group-current-window group) window)
                 (synchronize-frames-and-windows group head)
                 (raise-window window)
                 (focus-frame group master-frame)))
              (otherwise
               (error "Group ~A head ~A has desynchronized." group head)))
            ;; Otherwise we already have a stack, so move master to the stack and
            ;; make WINDOW the new master. 
            (progn
              (add-stack-frame group head)
              (add-window-to-stack group head master-window)
              (setf master-window window)
              (synchronize-frames-and-windows group head)
              (raise-window window)
              (let* ((fh (tile-group-frame-head group head)) 
                     (tree (case head-layout
                             ((:top :left) (cadr fh)) ; get stack tree
                             ((:bottom :right) (car fh)))))
                (balance-frames-internal group tree))))))))

(labels
    ((only-one (group head)
       ;; This is just a clone of the command ONLY, but it takes a group and a
       ;; head to work with instead of using the current ones. 
       (with-group-head-info (group head)
         (let ((win master-window)
               (frame (copy-frame head)))
           (if (only-one-frame-p)
               (message "There's only one frame.")
               (progn
                 (mapc (lambda (w)
                         ;; windows in other frames disappear
                         (unless (eq (window-frame w) 
                                     (tile-group-current-frame group))
                           (hide-window w))
                         (setf (window-frame w) frame))
                       (remove-if (lambda (w) (typep w 'float-window))
                                  (head-windows group head)))
                 (setf (frame-window frame) win
                       (tile-group-frame-head group head) frame
                       (tile-group-current-frame group) frame)
                 (focus-frame group frame)
                 (if (frame-window frame)
                     (update-decoration (frame-window frame))
                     (show-frame-indicator group))
                 (sync-frame-windows group (tile-group-current-frame group))))))))
  (defun dynamic-group-retile-head (group head &optional retile-floats)
    "Retile a specific head within a group. If RETILE-FLOATS is T then place all
floating windows onto the stack."
    (with-group-head-info (group head)
      (only-one group head)
      (let ((windows (reverse
                      (cons master-window
                            (if retile-floats
                                (append
                                 (loop for w in (head-windows group head)
                                       when (float-window-p w)
                                         collect (change-class w 'dynamic-window))
                                 stack-windows)
                                stack-windows)))))
        (setf master-window nil
              stack-windows nil)
        (loop for window in windows
              do (dynamic-group-place-window group head window))
        (focus-frame group (window-frame master-window))))))

;;; Handle overflow of both heads and groups

(defun head-overflow-generate-new-head-placement-list (group head)
  "Return a list of heads to try to place window(s) into, excluding HEAD."
  (destructuring-bind (w head-to-move-to g)
      (dynamic-group-overflow-policy group)
    (declare (ignore w g))
    (case head-to-move-to
      ((:any) (remove head (copy-list (group-heads group))))
      ((:ordered) (sort (remove head (copy-list (group-heads group)))
                        #'< :key #'frame-number))
      ((:first :second  :third  :fourth :fifth)
       (let* ((fn (intern (symbol-name head-to-move-to)))
              (new-head (funcall fn (copy-list (group-heads group)))))
         ;; Return the head as a list.
         (and head
              (not (eql head new-head))
              (list new-head))))
      (otherwise (error "Invalid head overflow policy for heads ~A"
                        head-to-move-to)))))

(defun handle-head-overflow (group head window)
  ;; This function should only be called when HEAD is full.
  ;; It should be called with the group were working in, the head we attempted
  ;; to place on, and the window we attempted to place. It is important that
  ;; WINDOW has not been placed.
  (let ((potential-heads
          (head-overflow-generate-new-head-placement-list group head))
        (unplaced t))
    (destructuring-bind (window-to-move h g)
        (dynamic-group-overflow-policy group)
      (declare (ignore h g))
      (if potential-heads
          (with-group-head-info (group head)
            (loop for new-head in potential-heads
                  unless (dynamic-group-head-full-p group new-head)
                    return (progn
                             (setf unplaced nil)
                             (case window-to-move
                               ((:new-window)
                                (dynamic-group-place-window group new-head window))
                               ((:master-window)
                                (let ((m master-window))
                                  (group-delete-window group m)
                                  (dynamic-group-place-window group new-head m)))
                               ((:stack-end)
                                (let ((e (lastcar stack-windows)))
                                  (group-delete-window group e)
                                  (dynamic-group-place-window group new-head e)))
                               ((:stack-beg)
                                (let ((b (car stack-windows)))
                                  (group-delete-window group b)
                                  (dynamic-group-place-window group new-head b)))
                               (otherwise
                                (error
                                 "Invalid window section of overflow policy: ~A"
                                 window-to-move)))))
            (when unplaced (handle-group-overflow group head window)))
          (handle-group-overflow group head window)))))

(defun handle-group-overflow (group head window)
  ;; Should be called with the group were working in, the head we attempted to
  ;; place on, and the window we attempted to place. 
  (destructuring-bind (who-to-move h group-to-move-to)
      (dynamic-group-overflow-policy group)
    (declare (ignore h))
    (let ((to-group (or (find-group (group-screen group) group-to-move-to)
                        (gnewbg group-to-move-to))))
      (with-group-head-info (group head)
        (case who-to-move
          ((:new-window)
           (setf (superfluous-window-tag window) t)
           (move-window-to-group window to-group))
          ((:master-window)
           (move-window-to-group master-window to-group)
           (dynamic-group-place-window group head window))
          ((:stack-end)
           (move-window-to-group (lastcar stack-windows) to-group)
           (dynamic-group-place-window group head window))
          ((:stack-beg)
           (move-window-to-group (car stack-windows) to-group)
           (dynamic-group-place-window group head window))
          (otherwise
           (error
            "Invalid window section of overflow policy: ~A"
            who-to-move)))))))


;;; General functions for managing windows

;; We need a function to synchronize the frame and window list. This function
;; should ensure that the window in position 0 resides in the frame in position
;; zero. 

(defmethod synchronize-frames-and-windows ((group dynamic-group) head)
  "Synchronize the frames and windows within a dynamic group. "
  (with-group-head-info (group head)
    (multiple-value-bind (l-stack-tree l-master-frame)
        (dynamic-group-head-main-split group head)
      (macrolet ((pop-frame (tree) ; We want to walk the tree but immitate
                   (with-gensyms (a) ;  popping off of a list. 
                     `(let ((,a ,tree))
                        (if (frame-p ,a)
                            (prog1 ,a (setf ,tree nil))
                            (prog1 (car ,a) (setf ,tree (cadr ,tree))))))))
        (let ((stack (list l-master-frame l-stack-tree))
              (windows (cons master-window stack-windows)))
          ;; Loop through all windows and frames (master and stack) and
          ;; synchronize them.
          (if (and (car stack) (not (car windows)))
              (focus-frame group (car stack))
              (do ((frame (pop-frame stack) (pop-frame stack))
                   (window (pop windows) (pop windows)))
                  ((not (and frame window))
                   (and frame window))
                (setf (frame-window frame) window
                      (window-frame window) frame)
                (maximize-window window)
                (update-decoration window))))))))

;; We need a function to swap a stack window with the master window, regardless
;; of its location in the stack.

(defun swap-window-with-master (group head window)
  "exchange a window with the master window for a specific group and head."
  (with-group-head-info (group head)
    (unless (eq window master-window)
      (let ((mf (window-frame master-window)))
        (psetf master-window window ; set a new master window
               ;; put master in the same position in the window stack list
               (car (member window stack-windows)) master-window)
        (synchronize-frames-and-windows group head)
        (focus-frame group mf)))))

;; We need functions to rotate the windows within a group head. These should
;; move the first/last element of the stack to be the master, and move the
;; master to the last/first element.

(defvar *rotation-focus-policy* :master-or-follow
  "A keyword determining what frame to focus after rotating the windows in a
dynamic group. Valid values are:
:PRESERVE, meaning to stay on the same frame
:FOLLOW, meaning to follow the current window as it rotates
:MASTER, meaning to always stay to the master
:MASTER-OR-FOLLOW, meaning to stay on the master, or if initiating the rotation 
while focused on a stack window to follow that window.")

(defmethod rotate-windows-forward ((group dynamic-group) head)
  "Rotate all windows forward, placing the master window on top of the stack."
  (with-group-head-info (group head)
    (when stack-windows ; only when theres a stack
      (let* ((slw (last stack-windows 2))
             (lw (cdr slw))
             (curframe (tile-group-current-frame group))
             (curwin (group-current-window group))
             (curwin-master-p (eq curwin master-window)))
        (if lw
            (progn
              (push master-window stack-windows) ; put master on the stack
              (setf (cdr slw) nil ; trim the final window from the stack
                    master-window (car lw))) ; make the final window the master.
            (psetf (car slw) master-window ; otherwise exchange master and stack
                   master-window (car slw)))
        (synchronize-frames-and-windows group head)
        (focus-frame group (case *rotation-focus-policy*
                             ((:preserve) curframe)
                             ((:follow) (window-frame curwin))
                             ((:master) (window-frame master-window))
                             ((:master-or-follow)
                              (if curwin-master-p
                                  curframe
                                  (window-frame curwin))))))))) 

(defmethod rotate-windows-backward ((group dynamic-group) head)
  "Rotate all windows backwards, placing the master window at the end of the
stack."
  (with-group-head-info (group head)
    (when stack-windows ; only when theres a stack
      (let* ((lw (last stack-windows))
             (curframe (tile-group-current-frame group))
             (curwin (group-current-window group))
             (curwin-master-p (eq curwin master-window)))
        (setf (cdr lw) (list master-window) ; put master at the end of the stack
              master-window (pop stack-windows)) ; make the the stack top master
        (synchronize-frames-and-windows group head)
        (focus-frame group (case *rotation-focus-policy*
                             ((:preserve) curframe)
                             ((:follow) (window-frame curwin))
                             ((:master) (window-frame master-window))
                             ((:master-or-follow)
                              (if curwin-master-p
                                  curframe
                                  (window-frame curwin)))))))))

(defmethod rotate-stack-forward ((group dynamic-group) head)
  "Rotate the stack windows, moving the top of the stack to the bottom."
  (with-group-head-info (group head)
    (when (cdr stack-windows)
      (let* ((slw (last stack-windows 2))
             (lw (cdr slw))
             (curframe (tile-group-current-frame group))
             (curwin (group-current-window group))
             (curwin-master-p (eq curwin master-window)))
        (setf (cdr slw) nil
              stack-windows (cons (car lw) stack-windows))
        (synchronize-frames-and-windows group head)
        (focus-frame group (case *rotation-focus-policy*
                             ((:preserve) curframe)
                             ((:follow) (window-frame curwin))
                             ((:master) (window-frame master-window))
                             ((:master-or-follow)
                              (if curwin-master-p
                                  curframe
                                  (window-frame curwin)))))))))

(defmethod rotate-stack-backward ((group dynamic-group) head)
  "Rotate the stack windows, moving the bottom of the stack to the top."
  (with-group-head-info (group head)
    (when (cdr stack-windows)
      (let* ((lw (last stack-windows))
             (curframe (tile-group-current-frame group))
             (curwin (group-current-window group))
             (curwin-master-p (eq curwin master-window)))
        (psetf (cdr lw) (list (car stack-windows))
                 stack-windows (cdr stack-windows))
        (synchronize-frames-and-windows group head)
        (focus-frame group (case *rotation-focus-policy*
                             ((:preserve) curframe)
                             ((:follow) (window-frame curwin))
                             ((:master) (window-frame master-window))
                             ((:master-or-follow)
                              (if curwin-master-p
                                  curframe
                                  (window-frame curwin)))))))))

(defmethod exchange-windows ((w1 dynamic-window) (w2 dynamic-window))
  "Exchange dynamic windows in their respective frames. Does not move windows
between groups."
  (let ((g1 (window-group w1))
        (g2 (window-group w2))
        (h1 (window-head w1))
        (h2 (window-head w2)))
    (when (eq g1 g2)
      (if (eq h1 h2) 
          ;; This is just a simple exchange of windows within a head
          (with-group-head-info (g1 h1)
            ;; Find which of the windows is master and which is stack, if any.
            (let* ((master (car (member master-window (list w1 w2))))
                   (stack (or (and master
                                   (member (car (remove master (list w1 w2)))
                                           stack-windows))
                              ;; If no master, then both windows are stack.
                              ;; Track their locations so we can swap them
                              (cons (member w1 stack-windows)
                                    (member w2 stack-windows)))))
              (if master
                  (psetf master-window (car stack)
                         (car stack) master-window)
                  ;; otherwise neither of the windows is master
                  (psetf (caar stack) (cadr stack)
                         (cadr stack) (caar stack)))
              (synchronize-frames-and-windows g1 h1)))
          ;; We need to handle moving these between heads
          (with-group-head-info (g1 h1 :master-window m1 :stack-windows s1)
            (with-group-head-info (g2 h2 :master-window m2 :stack-windows s2)
              (if (eq m1 w1)
                  (if (eq m2 w2)
                      ;; Swapping both heads master windows
                      (progn (psetf m1 w2
                                    m2 w1)
                             (pull-window w1 (window-frame w2))
                             (pull-window w2 (window-frame w1)))
                      ;; Swapping master of head 1 with a stack window of head 2
                      (let ((stack (member w2 s2)))
                        (psetf (car stack) w1
                               m1 (car stack))
                        (pull-window w1 (window-frame w2))
                        (pull-window w2 (window-frame w1))))
                  (if (eq m2 w2)
                      ;; Swapping master of head 2 with a stack window of head 1
                      (let ((stack (member w1 s1)))
                        (psetf (car stack) w2
                               m2 (car stack))
                        (pull-window w1 (window-frame w2))
                        (pull-window w2 (window-frame w1)))
                      ;; Swapping a stack window of head 1 with stack window of
                      ;; head 2
                      (let ((st1 (member w1 s1))
                            (st2 (member w2 s2)))
                        (psetf (car st1) w2
                               (car st2) w1)
                        (pull-window w1 (window-frame w2))
                        (pull-window w2 (window-frame w1)))))
              (synchronize-frames-and-windows g1 h1)
              (synchronize-frames-and-windows g2 h2)))))))

(defun dynamic-group-float-window (window group)
  "Make WINDOW into a floating window.  Stop managing it as a dynamic tiling 
window. "
  (if (typep window 'float-window)
      (message "Window ~A is already a floating window." window)
      (progn
        (group-delete-window group window)
        (change-class window 'float-window)
        (float-window-align window)
        (focus-all window))))

(defun dynamic-group-unfloat-window (window group)
  "Make WINDOW into a dynamic window. "
  (if (typep window 'dynamic-window)
      (message "Window ~A is already a dynamic window." window)
      (progn
        (let ((head (window-head window)))
          (change-class window 'dynamic-window)
          (dynamic-group-add-window group head window)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dynamic Group Commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *dynamic-group-blacklisted-commands* nil
  "A blacklist of commands for dynamic groups specifically.")

;; The above needed due to the class hierarchy. dynamic groups inherit from
;; tiling groups. Because the hierarchy is group -> tile-group -> dynamic-group,
;; all commands defined for tiling groups are valid in dynamic groups, even when
;; they shouldnt be. Changing the class hierarchy is a large change that could
;; break peoples configs, so it was decided to implement a blacklist instead of
;; changing the hierarchy to group -> tile-group -> manual-tile-group
;;                                               |> dynamic-tile-group

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

(defcommand gnew-dynamic (name) ((:rest "Group name: "))
  "Create a new dynamic group named NAME."
  (unless name 
    (throw 'error :abort))
  (add-group (current-screen) name :type 'dynamic-group))

(defcommand gnewbg-dynamic (name) ((:rest "Group name: "))
  "Create a new dynamic group named NAME in the background."
  (unless name
    (throw 'error :abort))
  (add-group (current-screen) name :type 'dynamic-group :background t))

(define-stumpwm-type :rotation-direction (input prompt)
  (let* ((values '(("Forward" :f)
                   ("Backward" :b)))
         (string (argument-pop-or-read input prompt (mapcar 'first values)))
         (dir (second (assoc string values :test 'string-equal))))
    (or dir
        (throw 'error (format nil "no direction matching ~A" string)))))

(defcommand (rotate-windows dynamic-group) (direction)
    ((:rotation-direction "Direction: "))
  "Rotate all windows in the current group and head forward (clockwise) or
backward (counterclockwise)"
  (let* ((g (current-group))
         (h (current-head g)))
    (case direction
      ((:f) (rotate-windows-forward g h))
      ((:b) (rotate-windows-backward g h)))))

(defcommand (rotate-stack dynamic-group) (direction)
    ((:rotation-direction "Direction: "))
  "Rotate the stack windows in current group and head forward (clockwise) or
backward (counterclockwise)"
  (let* ((g (current-group))
         (h (current-head g)))
    (case direction
      ((:f) (rotate-stack-forward g h))
      ((:b) (rotate-stack-backward g h)))))

(defcommand (swap-windows tile-group) () ()
  (let* ((f1 (progn (message "Select Window One")
                    (choose-frame-by-number (current-group))))
         (f2 (progn (message "Select Window Two")
                    (choose-frame-by-number (current-group)))))
    (when (and f1 f2)
      (let ((w1 (frame-window f1))
            (w2 (frame-window f2)))
        (if (and w1 w2)
            (exchange-windows w1 w2)
            (throw 'error (format nil "Frame ~A has no window"
                                  (or (and w1 f2) (and w2 f1)))))))))

(define-stumpwm-type :dynamic-layout (input prompt)
  (let* ((values '(("Top" :top)
                   ("Left" :left)
                   ("Right" :right)
                   ("Bottom" :bottom)))
         (string (argument-pop-or-read input prompt (mapcar #'first values)))
         (layout (second (assoc string values :test 'string-equal))))
    (or layout
        (throw 'error (format nil "No layout matching ~A" string)))))

(defcommand (change-layout dynamic-group) (layout) ((:dynamic-layout "Layout: "))
  "Change the layout of the current head and group."
  (setf (dynamic-group-head-layout (current-group) (current-head)) layout))

(defcommand (change-split-ratio dynamic-group) (ratio) ((:number "Ratio: "))
  "Change the size of the master window of the current head and group."
  (setf (dynamic-group-head-split-ratio (current-group) (current-head)) ratio))

(defcommand (change-default-layout dynamic-group)
    (layout &optional (update-heads :unset)) ((:dynamic-layout "Layout: "))
  "Change the default layout for dynamic groups."
  (setf (dynamic-group-master-layout (current-group) update-heads) layout))

(defcommand (change-default-split-ratio dynamic-group)
    (ratio &optional (update-heads :unset)) ((:number "Ratio: "))
  "Change the default size of the master window for dynamic groups."
  (setf (dynamic-group-default-split-ratio (current-group) update-heads) ratio))

(defcommand (retile dynamic-group) (&optional (retile-floats t))
    ((:y-or-n "Retile floating windows? "))
  "Force a retile of all windows."
  (dynamic-group-retile-head (current-group) (current-head) retile-floats))


(defcommand select-floating-window (&optional (fmt *window-format*) window-list)
    ((:rest))
  "Select a floating window from a menu."
  (if-let ((windows (remove-if-not #'float-window-p
                                   (or window-list
                                       (sort-windows-by-number
                                        (group-windows (current-group)))))))
    (if-let ((window (select-window-from-menu windows fmt)))
      (group-focus-window (current-group) window)
      (throw 'error :abort))
    (message "No Managed Floating Windows")))

(defcommand (exchange-with-master dynamic-group) () ()
  (swap-window-with-master (current-group) (current-head) (current-window)))

(defcommand (hnext dynamic-group) () ()
  "Move focus to the next head in a dynamic group"
  (let* ((group (current-group))
         (head (current-head))
         (info-alist (dynamic-group-head-info-alist group))
         (head-list (member (current-head) info-alist :key #'car))
         (next-head (if (cdr head-list) ; get the next head to focus on
                        (caadr head-list) 
                        (unless (eql head (caar head-list))
                          (caar head-list)))))
    (when next-head
      (if (head-windows group next-head)
          (focus-frame group (window-frame (with-group-head-info (group next-head)
                                             master-window)))
          (focus-frame group next-head)))))

(defcommand (hprev dynamic-group) () ()
  (let* ((group (current-group))
         (head (current-head))
         (info-alist (reverse (dynamic-group-head-info-alist group)))
         (head-list (member (current-head) info-alist :key #'car))
         (next-head (if (cdr head-list) ; get the next head to focus on
                        (caadr head-list) 
                        (unless (eql head (caar head-list))
                          (caar head-list)))))
    (when next-head
      (if (head-windows group next-head)
          (focus-frame group (window-frame (with-group-head-info (group next-head)
                                             master-window)))
          (focus-frame group next-head)))))

(defcommand (fnext-in-head dynamic-group) () ()
  (let ((group (current-group)))
    (focus-frame-after group (head-frames group (current-head)))))

(defcommand (fprev-in-head dynamic-group) () ()
  (let ((group (current-group)))
    (focus-frame-after group (reverse (head-frames group (current-head))))))

;;; Dynamic group keybindings

(defvar *dynamic-group-top-map* nil)
(defvar *dynamic-group-root-map* nil
  "Commands specific to a dynamic group context hang from this keymap.
It is available as part of the @dnf{prefix map} when the active group
is a dynamic group.")

(fill-keymap *dynamic-group-top-map*
  *escape-key* '*dynamic-group-root-map*)

(fill-keymap *dynamic-group-root-map*
             (kbd "n") "rotate-windows forward"
             (kbd "p") "rotate-windows backward"
             (kbd "N") "rotate-stack forward"
             (kbd "P") "rotate-stack backward"
             (kbd "C-n") "fnext-in-head"
             (kbd "C-p") "fprev-in-head"
             (kbd "M-n") "hnext"
             (kbd "M-p") "hprev"
             (kbd "f") "fselect"
             (kbd "F") "curframe"
             (kbd "s") "swap-windows"
             (kbd "RET") "exchange-with-master")

(pushnew '(dynamic-group *dynamic-group-top-map*) *group-top-maps*)
