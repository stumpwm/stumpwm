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
;; Frame functions
;;
;; Code:

(in-package #:stumpwm)

(export '(save-frame-excursion))

(defclass tile-group (group)
  ((frame-tree :accessor tile-group-frame-tree)
   (last-frame :initform nil :accessor tile-group-last-frame)
   (current-frame :accessor tile-group-current-frame)))

(defmethod initialize-instance :after ((group tile-group) &key &allow-other-keys)
  (let* ((heads (copy-heads (group-screen group))))
    (setf (tile-group-frame-tree group) heads
          (tile-group-current-frame group) (first heads))))

(defmethod group-startup ((group tile-group))
  (let* ((window (first (group-tile-windows group)))
         (frame (if (typep window 'tile-window)
                    (window-frame window)
                    (tile-group-current-frame group))))
    (focus-frame group frame)))

(defmethod group-wake-up ((group tile-group))
  (focus-frame group (tile-group-current-frame group))
  ;; doesn't get called by focus-frame
  (show-frame-indicator group))

(defmethod group-delete-window ((group tile-group) (window tile-window))
  (let ((f (window-frame window)))
    ;; maybe pick a new window for the old frame
    (when (eq (frame-window f) window)
      (frame-raise-window group f (first (frame-windows group f)) nil))))

(defmethod group-delete-window ((group tile-group) (window float-window))
  (let* ((windows (group-windows group))
         (float-w (some (lambda (w) (when (typep w 'float-window) w))
                        windows))
         (tile-w (some (lambda (w) (when (typep w 'tile-window) w))
                       windows)))
    (cond (float-w (group-focus-window group float-w))
          (tile-w (frame-raise-window group (window-frame tile-w) tile-w))
          (t (no-focus group nil)))))

(defmethod group-add-window ((group tile-group) window &key frame raise &allow-other-keys)
  ;; This is important to get the frame slot
  (cond ((typep window 'float-window)
         (call-next-method))
        ((eq frame :float)
         (change-class window 'float-window)
         (float-window-align window)
        (when raise
          (group-focus-window group window)))
        (t
         (change-class window 'tile-window)
         ;; Try to put the window in the appropriate frame for the group.
         (setf (window-frame window)
               (or frame
                   (when *processing-existing-windows*
                     (find-frame group (xlib:drawable-x (window-parent window))
                                 (xlib:drawable-y (window-parent window))))
                   (pick-preferred-frame window)))
         (when *processing-existing-windows*
           (setf (frame-window (window-frame window)) window))
         (when (and frame raise)
           (setf (tile-group-current-frame group) frame
                 (frame-window frame) nil))
         (sync-frame-windows group (window-frame window))
         (when (null (frame-window (window-frame window)))
           (frame-raise-window (window-group window) (window-frame window)
                               window nil)))))

(defmethod group-current-head ((group tile-group))
  (if-let ((current-window (group-current-window group)))
    (window-head current-window)
    (frame-head group (tile-group-current-frame group))))

(defmethod group-move-request ((group tile-group) (window tile-window) x y relative-to)
  (when *honor-window-moves*
    (dformat 3 "Window requested new position ~D,~D relative to ~S~%" x y relative-to)
    (let* ((pointer-pos (multiple-value-list (xlib:global-pointer-position *display*)))
           (pos  (if (eq relative-to :parent)
                     (list
                      (+ (xlib:drawable-x (window-parent window)) x)
                      (+ (xlib:drawable-y (window-parent window)) y))
                     (list (first pointer-pos) (second pointer-pos))))
           (frame (apply #'find-frame group pos)))
      (when frame
        (pull-window window frame)))))

(defmethod group-resize-request ((group tile-group) (window tile-window) width height)
  ;; it's important to grant the resize request first so that resize
  ;; increment hints use the proper base size to resize from.
  (set-window-geometry window :width width :height height)
  (maximize-window window))

(defmethod group-resize-request ((group tile-group) (window float-window) width height)
  (float-window-move-resize window :width width :height height))

(defmethod group-move-request ((group tile-group) (window float-window) x y relative-to)
  (declare (ignore relative-to))
  (float-window-move-resize window :x x :y y))

(defmethod group-raise-request ((group tile-group) window stack-mode)
  (when (window-in-current-group-p window)
    (case stack-mode
      (:map
       (maybe-map-window window))
      (:above
       (maybe-raise-window window)))))

(defmethod group-lost-focus ((group tile-group))
  ;; If this window had the focus, try to avoid losing it.
  (let ((frame (tile-group-current-frame group)))
    (setf (frame-window frame)
          (first (remove-if 'window-hidden-p (frame-windows group frame))))
    (focus-frame group frame)))

(defmethod group-indicate-focus ((group tile-group))
  (show-frame-indicator group))

(defmethod group-focus-window ((group tile-group) (win tile-window))
  (frame-raise-window group (window-frame win) win))

(defmethod group-focus-window ((group tile-group) (window float-window))
  (focus-window window))

(defmethod group-button-press ((group tile-group) button x y (where (eql :root)))
  (when *root-click-focuses-frame*
    (when-let ((frame (find-frame group x y)))
      (focus-frame group frame)
      (unless (or (eq *mouse-focus-policy* :click)
                  (scroll-button-keyword-p button))
        (update-all-mode-lines)))))

(defmethod group-button-press ((group tile-group) button x y (where window))
  (declare (ignore x y))
  (when (typep where 'float-window)
    (call-next-method))
  (when (member *mouse-focus-policy* '(:click :sloppy))
    (focus-all where)
    (unless (scroll-button-keyword-p button)
      (update-all-mode-lines))))

(defmethod group-root-exposure ((group tile-group))
  (show-frame-outline group nil))

(defmethod group-add-head ((group tile-group) head)
  (let ((new-frame-num (find-free-frame-number group)))
    (setf (tile-group-frame-tree group)
          (insert-before (tile-group-frame-tree group)
                         (copy-frame head)
                         (head-number head)))
    ;; Try to put something in the new frame and give it an unused number
    (let ((frame (tile-group-frame-head group head)))
      (setf (frame-number frame) new-frame-num)
        (choose-new-frame-window frame group)
        (when (frame-window frame)
          (unhide-window (frame-window frame))))))

;; TODO: This method has not been updated for floating windows
(defmethod group-remove-head ((group tile-group) head)
  ;; first ensure the data is up to date
  (group-sync-all-heads group)
  (let ((windows (head-windows group head))
        (frames-to-delete (tile-group-frame-head group head))
        (group-frame-tree (tile-group-frame-tree group)))
    ;; Remove this head's frames from the frame tree.
    (setf (tile-group-frame-tree group) (delete frames-to-delete group-frame-tree))
    ;; Just set current frame to whatever.
    (let ((frame (first (group-frames group))))
      (setf (tile-group-current-frame group) frame
            (tile-group-last-frame group) nil)
      ;; Hide its windows.
      (dolist (window windows)
        (hide-window window)
        (setf (window-frame window) frame))))
  ;; Try to do something with the orphaned windows
  (populate-frames group))

(defmethod group-resize-head ((group tile-group) oh nh)
  (resize-tree (tile-group-frame-head group oh) (head-width nh) (head-height nh) (head-x nh) (head-y nh)))

(defmethod group-sync-all-heads ((group tile-group))
  (sync-all-frame-windows group))

(defmethod group-sync-head ((group tile-group) head)
  (dolist (f (head-frames group head))
    (sync-frame-windows group f)))

;;;;;
;; (defun tile-group-frame-head (group head)
;;   (let ((index (position head (group-heads group)))
;;         (frame-tree (tile-group-frame-tree group)))
;;     (when (> index (length frame-tree))
;;       (elt frame-tree index))))

(defun group-tile-windows (group)
  (only-tile-windows (group-windows group)))

(defmethod group-windows-for-cycling ((group tile-group) &key sorting)
  (declare (ignore sorting))
  (only-tile-windows (call-next-method)))

(defmethod focus-next-window ((group tile-group))
  (focus-forward group (group-windows-for-cycling group :sorting t)))

(defmethod focus-prev-window ((group tile-group))
  (focus-forward group
                 (reverse
                  (group-windows-for-cycling group :sorting t))))

(defun tile-group-frame-head (group head)
  (elt (tile-group-frame-tree group) (position head (group-heads group))))

(defun (setf tile-group-frame-head) (frame group head)
  (setf (elt (tile-group-frame-tree group) (position head (group-heads group))) frame))

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
             (<= fy y fhy)
             (<= fx x fwx))
        (return f)))))


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

(defun frame-display-y (group frame)
  "Return a Y for frame that doesn't overlap the mode-line."
  (let* ((head (frame-head group frame))
         (ml (head-mode-line head))
         (head-y (frame-y head))
         (rel-frame-y (- (frame-y frame) head-y)))
    (if (and ml (not (eq (mode-line-mode ml) :hidden)))
        (case (mode-line-position ml)
          (:top
           (+ head-y
              (+ (mode-line-height ml) (round (* rel-frame-y (mode-line-factor ml))))))
          (:bottom
           (+ head-y
              (round (* rel-frame-y (mode-line-factor ml))))))
        (frame-y frame))))

(defun frame-display-height (group frame)
  "Return a HEIGHT for frame that doesn't overlap the mode-line."
  (let* ((head (frame-head group frame))
         (ml (head-mode-line head)))
    (if (and ml (not (eq (mode-line-mode ml) :hidden)))
        (round (* (frame-height frame) (mode-line-factor ml)))
        (frame-height frame))))

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
  (check-type f1 frame)
  (check-type f2 frame)
  (and (and (frame-p f1) (frame-p f2))
       (let ((frame (frame-intersect f1 f2)))
         (values (and (plusp (frame-width frame))
                      (plusp (frame-height frame)))))))

(defun frame-raise-window (g f w &optional (focus t))
  "Raise the window w in frame f in group g. if FOCUS is
T (default) then also focus the frame."
  (let ((oldw (frame-window f)))
    ;; nothing to do when W is nil
    (setf (frame-window f) w)
    (unless (and w (eq oldw w))
      (if w
          (raise-window w)
          (mapc 'hide-window
                (reverse (frame-windows g f)))))
    ;; If raising a window in the current frame we must focus it or
    ;; the group and screen will get out of sync.
    (when (or focus
              (eq (tile-group-current-frame g) f))
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
    (if show-indicator
        (show-frame-indicator group)
        (show-frame-outline group))))

(defun frame-windows (group f)
  (remove-if-not (lambda (w) (eq (window-frame w) f))
                 (group-tile-windows group)))

(defun frame-sort-windows (group f)
  (remove-if-not (lambda (w) (and (typep w 'tile-window)
                                  (eq (window-frame w) f)))
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
  (find-free-number (mapcar 'frame-number (group-frames group))))

(defun choose-new-frame-window (frame group)
  "Find out what window should go in a newly created frame."
  (let ((win (case *new-frame-action*
               (:last-window (other-hidden-window group))
               (t nil))))
    (setf (frame-window frame) win)
    (when win
      (setf (window-frame win) frame))))

(defun split-frame-h (group p ratio)
  "Return 2 new frames. The first one stealing P's number and window"
  (let* ((w (ratio-or-pixel (frame-width p) ratio))
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
    (run-hook-with-args *split-frame-hook* p f1 f2)
    (run-hook-with-args *new-frame-hook* f2)
    (values f1 f2)))

(defun split-frame-v (group p ratio)
  "Return 2 new frames. The first one stealing P's number and window"
  (let* ((w (frame-width p))
         (h (ratio-or-pixel (frame-height p) ratio))
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
    (run-hook-with-args *split-frame-hook* p f1 f2)
    (run-hook-with-args *new-frame-hook* f2)
    (values f1 f2)))

(defun ratio-or-pixel (length ratio)
  "Return a ratio of length unless ratio is an integer.
If ratio is an integer return the number of pixel desired."
  (if (integerp ratio)
      ratio
      (truncate (* length ratio))))

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
        (group-tile-windows group)))

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
                                  collect (floor (* amount old-sz) total)))
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

(defun resize-tree (tree w h &optional (x (tree-x tree)) (y (tree-y tree)))
  "Scale TREE to width W and height H, ignoring aspect. If X and Y are
  provided, reposition the TREE as well."
  (let* ((tw (tree-width tree))
         (th (tree-height tree))
         (tx (tree-x tree))
         (ty (tree-y tree))
         (wf (/ w tw))
         (hf (/ h th)))
    (tree-iterate tree (lambda (f)
                         (setf (frame-height f) (round (* (frame-height f) hf))
                               (frame-y f) (+ (round (* (- (frame-y f) ty) hf)) y)
                               (frame-width f) (round (* (frame-width f) wf))
                               (frame-x f) (+ (round (* (- (frame-x f) tx) wf)) x))))
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
        (group-tile-windows group)))

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
             (let ((right-sibling (cadr (member node parent)))
                   (left-sibling (cadr (member node (reverse parent)))))

               (dformat 10 "max ~@{~a~^ ~}~%" parent node min dim-fn right-sibling left-sibling)
               (if parent
                   (cond (right-sibling
                          (max 0 (- (funcall dim-fn right-sibling) min)))
                         (left-sibling
                          (max 0 (- (funcall dim-fn left-sibling) min)))
                         (t 0))
                   ;; no parent means the frame can't get any bigger.
                   0))))
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
  "Fully balance all the frames contained in tree."
  (labels
      ((balance (tree x y width height)
         (etypecase tree
           (frame (balance-frame tree x y width height))
           (list (balance-tree tree x y width height))))
       (balance-frame (frame x y width height)
         (setf (frame-x frame) x
               (frame-y frame) y
               (frame-width frame) width
               (frame-height frame) height)
         (sync-frame-windows group frame))
       (count-splits (tree split-type)
         "Count the number of top-level splits of split-type in tree."
         (cond ((frame-p tree) 1)
               ((eql split-type (tree-split-type tree))
                (+ (count-splits (first tree) split-type)
                   (count-splits (second tree) split-type)))
               (t 1)))
       (divide-dimension (value first-splits second-splits)
         "Divide a width or height between two sides of a binary tree.

         Returns two values: the number of pixels to give to the first and
         second child, respectively.

         For example: (divide-dimension 500 3 2) will divide 500 pixels into 300
         for the first 3 splits and 200 for the second 2 splits.

         "
         ;; First divide the two groups as evenly as possible.
         (multiple-value-bind (base remainder)
             (truncate value (+ first-splits second-splits))
           ;; We may have up to TOTAL-1 pixels left.  Divide those evenly
           ;; between the two sides.  If there's 1 odd pixel left just give it
           ;; to the first side.
           (multiple-value-bind (extra maybe-one-extra)
               (truncate remainder 2)
             (values (+ (* base first-splits) extra maybe-one-extra)
                     (+ (* base second-splits) extra)))))
       (balance-tree (tree x y width height)
         "Balance the binary tree to fit the given dimensions."
         (let* ((split-type (tree-split-type tree))
                (first-splits (count-splits (first tree) split-type))
                (second-splits (count-splits (second tree) split-type)))
           (ecase split-type
             (:row (multiple-value-bind (top-height bottom-height)
                       (divide-dimension height first-splits second-splits)
                     (balance (first tree)
                              x y
                              width top-height)
                     (balance (second tree)
                              x (+ y top-height)
                              width bottom-height)))
             (:column (multiple-value-bind (left-width right-width)
                          (divide-dimension width first-splits second-splits)
                        (balance (first tree)
                                 x y
                                 left-width height)
                        (balance (second tree)
                                 (+ x left-width) y
                                 right-width height)))))))
    (balance tree (tree-x tree) (tree-y tree) (tree-width tree) (tree-height tree))))

(defun split-frame (group how &optional (ratio 1/2))
  "Split the current frame into 2 frames. Return new frame number, if
it succeeded. NIL otherwise. RATIO is a fraction of the screen to
allocate to the new split window. If ratio is an integer then the
number of pixels will be used. This can be handy to setup the
desktop when starting."
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



(defun draw-frame-outline (group f tl br)
  "Draw an outline around FRAME."
  (let* ((screen (group-screen group))
         (win (if (frame-window f) (window-xwin (frame-window f)) (screen-root screen)))
         (width (screen-frame-outline-width screen))
         (gc (screen-frame-outline-gc screen))
         (halfwidth (/ width 2)))
    (when (> width 0)
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
                          x (+ y (- h halfwidth)) w 0 t))))))

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

(defmacro save-frame-excursion (&body body)
  "Execute body and then restore the current frame."
  (let ((oframe (gensym "OFRAME"))
        (ogroup (gensym "OGROUP")))
    `(let ((,oframe (tile-group-current-frame (current-group)))
           (,ogroup (current-group)))
      (unwind-protect (progn ,@body)
        (focus-frame ,ogroup ,oframe)))))

;;; Frame commands

(defun split-frame-in-dir (group dir &optional (ratio 1/2))
  (let ((f (tile-group-current-frame group)))
    (if (split-frame group dir ratio)
        (progn
          (when (frame-window f)
            (update-decoration (frame-window f)))
          (show-frame-indicator group))
        (message "Cannot split smaller than minimum size."))))

(defcommand (hsplit tile-group) (&optional (ratio "1/2")) (:string)
"Split the current frame into 2 side-by-side frames."
  (split-frame-in-dir (current-group) :column (read-from-string ratio)))

(defcommand (vsplit tile-group) (&optional (ratio "1/2")) (:string)
"Split the current frame into 2 frames, one on top of the other."
  (split-frame-in-dir (current-group) :row (read-from-string ratio)))

(defun split-frame-eql-parts* (group dir amt)
  (when (> amt 1)
    (when-let ((new-frame (split-frame group dir (/ (- amt 1) amt))))
      (cons new-frame (split-frame-eql-parts* group dir (- amt 1))))))

(defun split-frame-eql-parts (group dir amt)
  "Splits frame in equal parts defined by amt."
  (assert (> amt 1))
  (let ((f (tile-group-current-frame group))
        (new-frame-numbers (split-frame-eql-parts* group dir amt)))
    (if (= (list-length new-frame-numbers) (- amt 1))
        (progn
          (when (frame-window f)
            (update-decoration (frame-window f)))
          (show-frame-indicator group))
        (let ((head (frame-head group f)))
          (setf (tile-group-frame-head group head)
                (reduce (lambda (tree num)
                          (remove-frame tree
                                        (frame-by-number group num)))
                        new-frame-numbers
                        :initial-value (tile-group-frame-head group head)))
          (message "Cannot split. Maybe current frame is too small.")))))

(defcommand (hsplit-equally tile-group) (amt)
    ((:number "Enter the number of frames: "))
"Split current frame in n rows of equal size."
  (split-frame-eql-parts (current-group) :row amt))

(defcommand (vsplit-equally tile-group) (amt)
    ((:number "Enter the number of frames: "))
"Split current frame in n columns of equal size."
  (split-frame-eql-parts (current-group) :column amt))

(defcommand (remove-split tile-group)
    (&optional (group (current-group))
               (frame (tile-group-current-frame group))) ()
"Remove the specified frame in the specified group (defaults to current group,
current frame). Windows in the frame are migrated to the frame taking up its
space."
  (let* ((head (frame-head group frame))
         (current (tile-group-current-frame group))
         (tree (tile-group-frame-head group head))
         (s (closest-sibling (list tree) frame))
         ;; grab a leaf of the siblings. The siblings doesn't have to be
         ;; a frame.
         (l (tree-accum-fn s
                           (lambda (&rest siblings)
                             (car siblings))
                           #'identity)))
    ;; Only remove the current frame if it has a sibling
    (if (atom tree)
        (message "No more frames!")
        (when s
          (when (frame-is-head group frame)
            (setf (frame-number l) (frame-number frame)))
          ;; Move the windows from the removed frame to its sibling
          (migrate-frame-windows group frame l)
          ;; If the frame has no window, give it the current window of
          ;; the current frame.
          (unless (frame-window l)
            (setf (frame-window l)
                  (frame-window frame)))
          ;; Unsplit
          (setf (tile-group-frame-head group head) (remove-frame tree frame))
          ;; update the current frame and sync all windows
          (when (eq frame current)
            (setf (tile-group-current-frame group) l))
          (tree-iterate tree
                        (lambda (leaf)
                          (sync-frame-windows group leaf)))
          (frame-raise-window group l (frame-window l) nil)
          (when (frame-window l)
            (update-decoration (frame-window l)))
          (when (eq frame current)
            (show-frame-indicator group))
          (run-hook-with-args *remove-split-hook* l frame)))))

(defcommand-alias remove remove-split)

(defun only-one-frame-p ()
  "T if there is only one maximized frame in the current head.
This can be used around a the \"only\" command to avoid the warning message."
  (let* ((group (screen-current-group (current-screen)))
         (head (current-head group)))
    (atom (tile-group-frame-head group head))))

(defcommand (only tile-group) () ()
  "Delete all the frames but the current one and grow it to take up the entire head."
  (let* ((screen (current-screen))
         (group (screen-current-group screen))
         (win (group-current-window group))
         (head (current-head group))
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
          (sync-frame-windows group (tile-group-current-frame group))))))

(defcommand (curframe tile-group) () ()
"Display a window indicating which frame is focused."
  (show-frame-indicator (current-group) t))

(defun focus-frame-next-sibling (group)
  (let* ((sib (next-sibling (tile-group-frame-tree group)
                            (tile-group-current-frame group))))
    (when sib
      (focus-frame group (tree-accum-fn sib
                                        (lambda (x y)
                                          (declare (ignore y))
                                          x)
                                        'identity))
      (show-frame-indicator group))))

(defun focus-last-frame (group)
  ;; make sure the last frame still exists in the frame tree
  (let ((last-frame (tile-group-last-frame group)))
    (when (and last-frame
               (find last-frame (group-frames group)))
      (focus-frame group last-frame))))

(defun focus-frame-after (group frames)
  "Given a list of frames focus the next one in the list after
the current frame."
  (let ((rest (cdr (member (tile-group-current-frame group) frames :test 'eq))))
    (if (= (length frames) 1)
        (message "No other frames.")
        (focus-frame group
                     (if (null rest)
                         (car frames)
                         (car rest))))))

(defun focus-next-frame (group)
  (focus-frame-after group (group-frames group)))

(defun focus-prev-frame (group)
  (focus-frame-after group (nreverse (group-frames group))))

(defcommand (fnext tile-group) () ()
"Cycle through the frame tree to the next frame."
  (focus-next-frame (current-group)))

(defcommand (fprev tile-group) () ()
  "Cycle through the frame tree to the previous frame."
  (focus-prev-frame (current-group)))

(defcommand (sibling tile-group) () ()
"Jump to the frame's sibling. If a frame is split into two frames,
these two frames are siblings."
  (focus-frame-next-sibling (current-group)))

(defcommand (fother tile-group) () ()
"Jump to the last frame that had focus."
  (focus-last-frame (current-group)))

(defun choose-frame-by-number (group)
  "show a number in the corner of each frame and wait for the user to
select one. Returns the selected frame or nil if aborted."
  (let ((wins (progn
                (draw-frame-outlines group)
                (draw-frame-numbers group))))
    (unwind-protect
         (multiple-value-bind (has-click ch x y)
             (read-one-char-or-click group)
           (if has-click
               (let ((winner))
                 ;; frame-width and frame-height are not updated in this
                 ;; context, so we need to loop through all of them until
                 ;; we find the most satisfying one.
                 (dolist (f (group-frames group))
                   (when (and (> x (frame-x f)) (> y (frame-y f)))
                     (if winner
                         (when (or (> (frame-x f) (frame-x winner))
                                   (> (frame-y f) (frame-y winner)))
                           (setf winner f))
                         (setf winner f))))
                 (ungrab-pointer)
                 winner)
               (when ch
                 (let ((num (read-from-string (string ch) nil nil)))
                   (dformat 3 "read ~S ~S~%" ch num)
                   (find ch (group-frames group)
                         :test 'char=
                         :key 'get-frame-number-translation)))))
      (mapc #'xlib:destroy-window wins)
      (clear-frame-outlines group))))


(defcommand (fselect tile-group) (frame-number) ((:frame t))
"Display a number in the corner of each frame and let the user to
select a frame by number or click. If @var{frame-number} is specified,
just jump to that frame."
  (let ((group (current-group)))
    (focus-frame group frame-number)))

(defcommand (resize tile-group) (width height) ((:number "+ Width: ")
                                                (:number "+ Height: "))
  "Resize the current frame by @var{width} and @var{height} pixels"
  (let* ((group (current-group))
         (f (tile-group-current-frame group)))
    (if (atom (tile-group-frame-tree group))
        (message "No more frames!")
        (progn
          (clear-frame-outlines group)
          (resize-frame group f width :width)
          (resize-frame group f height :height)
          (draw-frame-outlines group (current-head))))))

(defun clear-frame (frame group)
  "Clear the given frame."
  (frame-raise-window group frame nil (eq (tile-group-current-frame group) frame)))

(defcommand (fclear tile-group) () ()
"Clear the current frame."
  (clear-frame (tile-group-current-frame (current-group)) (current-group)))

(defun get-edge (frame edge)
  "Returns the specified edge of FRAME.  Valid values for EDGE are :TOP, :BOTTOM, :LEFT, and :RIGHT.
  An edge is a START, END, and OFFSET. For horizontal edges, START is the left coordinate, END is
  the right coordinate, and OFFSET is the Y coordinate.  Similarly, for vertical lines, START is
  top, END is bottom, and OFFSET is X coordinate."
  (let* ((x1 (frame-x frame))
         (y1 (frame-y frame))
         (x2 (+ x1 (frame-width frame)))
         (y2 (+ y1 (frame-height frame))))
    (ecase edge
      (:top
       (values x1 x2 y1))
      (:bottom
       (values x1 x2 y2))
      (:left
       (values y1 y2 x1))
      (:right
       (values y1 y2 x2)))))

(defun neighbour (direction frame frameset)
  "Returns the best neighbour of FRAME in FRAMESET on the DIRECTION edge.
   Valid directions are :UP, :DOWN, :LEFT, :RIGHT.
   eg: (NEIGHBOUR :UP F FS) finds the frame in FS that is the 'best'
   neighbour above F."
  (let ((src-edge (ecase direction
                    (:up :top)
                    (:down :bottom)
                    (:left :left)
                    (:right :right)))
        (opposite (ecase direction
                    (:up :bottom)
                    (:down :top)
                    (:left :right)
                    (:right :left)))
        (best-frame nil)
        (best-overlap 0))
    (multiple-value-bind (src-s src-e src-offset)
        (get-edge frame src-edge)
      (dolist (f frameset)
        (multiple-value-bind (s e offset)
            (get-edge f opposite)
          (let ((overlap (- (min src-e e)
                            (max src-s s))))
            ;; Two edges are neighbours if they have the same offset and their starts and ends
            ;; overlap.  We want to find the neighbour that overlaps the most.
            (when (and (= src-offset offset)
                       (> overlap best-overlap))
              (setf best-frame f)
              (setf best-overlap overlap))))))
    best-frame))

(defun move-focus-and-or-window (dir &optional win-p)
  (declare (type (member :up :down :left :right) dir))
  (let* ((group (current-group))
         (new-frame (neighbour dir (tile-group-current-frame group) (group-frames group)))
         (window (current-window)))
    (when new-frame
      (if (and win-p window)
          (pull-window window new-frame)
          (focus-frame group new-frame)))))

(defcommand (move-focus tile-group) (dir) ((:direction "Direction: "))
"Focus the frame adjacent to the current one in the specified
direction. The following are valid directions:
@table @asis
@item up
@item down
@item left
@item right
@end table"
  (move-focus-and-or-window dir))

(defcommand (move-window tile-group) (dir) ((:direction "Direction: "))
"Just like move-focus except that the current is pulled along."
  (move-focus-and-or-window dir t))

(defcommand (next-in-frame tile-group) () ()
"Go to the next window in the current frame."
  (let ((group (current-group)))
    (if (group-current-window group)
        (focus-forward group (frame-sort-windows group (tile-group-current-frame group)))
        (other-window-in-frame group))))

(defcommand (prev-in-frame tile-group) () ()
"Go to the previous window in the current frame."
  (let ((group (current-group)))
    (if (group-current-window group)
        (focus-forward group (reverse (frame-sort-windows group (tile-group-current-frame group))))
        (other-window-in-frame group))))

(defcommand (other-in-frame tile-group) () ()
"Go to the last accessed window in the current frame."
  (other-window-in-frame (current-group)))

(defcommand (balance-frames tile-group) (&aux (group (current-group))) ()
  "Make frames the same height or width in the current frame's subtree."
  (let ((tree (tile-group-frame-head group (current-head))))
    (if (frame-p tree)
        (message "There's only one frame.")
        (balance-frames-internal group tree))))

(defun window-centroid (win)
  "Return the centroid of WIN."
  (let ((x (window-x win))
        (y (window-y win))
        (w (window-width win))
        (h (window-height win)))
    (cons (+ x (/ w 2))
          (+ y (/ h 2)))))

(defun frame-centroid (frame)
  "Return the centroid of frame, excluding the borders."
  (let ((x (frame-x frame))
        (y (frame-y frame))
        (w (frame-width frame))
        (h (frame-height frame)))
    (cons (+ x (/ w 2))
          (+ y (/ h 2)))))

(defun closest-frame (win group)
  "Returns the frame closet to the window, WIN."
  (flet ((square (n) (* n n)))
    (let (shortest)
      (destructuring-bind (win-x . win-y) (window-centroid win)
        (loop :for frame :in (group-frames group)
	      :for (frame-x . frame-y) := (frame-centroid frame)
	      :for distance := (sqrt (+ (square (- win-x frame-x))
					(square (- win-y frame-y))))
	      :unless shortest
                :do (setf shortest (cons distance frame))
	      :when (> (car shortest) distance)
                :do (setf shortest (cons distance frame))))
      (cdr shortest))))

(defun unfloat-window (window group)
  (let ((frame (closest-frame window group)))
    (change-class window 'tile-window :frame frame)
    (setf (window-frame window) frame
          (frame-window frame) window
          (tile-group-current-frame group) frame)
    (update-decoration window)
    (sync-frame-windows group frame)))

(defun float-window (window group)
  (let ((frame (tile-group-current-frame group)))
    (change-class window 'float-window)
    (float-window-align window)
    (funcall-on-node (tile-group-frame-tree group)
                     (lambda (f) (setf (slot-value f 'window) nil))
                     (lambda (f) (eq frame f)))))

(defcommand (float-this tile-group) () ()
  "Transforms a tile-window into a float-window"
  (float-window (current-window) (current-group)))

(defcommand (unfloat-this tile-group) () ()
  "Transforms a float-window into a tile-window"
  (unfloat-window (current-window) (current-group)))

(defcommand flatten-floats () ()
  "Transform all floating windows in this group to tiled windows.
Puts all tiled windows in the first frame of the group. "
  (let ((group (current-group)))
    (mapc (lambda (w)
          (when (typep w 'float-window)
            (unfloat-window w group)))
          (head-windows group (current-head)))))
