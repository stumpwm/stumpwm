;; Copyright (C) 2021 Jin-Cheng Guu
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

(in-package :stumpwm-dfg)

;;; Parameters

(defparameter *default-master-ratio* (/ 2 (+ 1 (sqrt 5))))
(defparameter *supported-layouts* '(left-vertical fullscreen horizontal))
(defparameter *default-layout* (car *supported-layouts*)
  "Currently supported layouts are: 'left-vertical 'horizontal
  'fullscreen. See the body of #'RE-TILE for their details.")

;;; Classes

(defclass window+ (window)
  ((window :initarg :window
           :accessor window+-window)
   (drift :initarg :drift
          :accessor window+-drift))
  (:documentation
   "An augmented window (window+) is a window with some other
information. Usually, in a dynamic floating group, the
information will be used during the tiling process (compare
#'re-tile).

All windows in a dynamic floating group are floating windows. A
drifting window is a window+ with :DRIFT slot being T. It is not
affected by the core tiling function #'RE-TILE. A staying window
is a window+ with :DRIFT slot being NIL. They are subject to
#'RE-TILE."))

(defclass dyn-float-group (stumpwm::float-group)
  ((dyn-order :initform nil
              :accessor dyn-float-group-dyn-order
              :documentation
              "The list of augmented windows (window+) that a
              dynamic-floating group holds. The dyn-float-group
              should tile automatically according to its
              dyn-order.")
   (layout-hist :initform (list *default-layout*)
                :accessor dyn-float-group-layout-hist
                :documentation
                "The list of layout histories, where the first
                element is interpreted as the current layout.")
   (master-ratio :initform *default-master-ratio*
                 :accessor dyn-float-group-master-ratio
                 :documentation
                 "The ratio of the master window takes.")))

(defun dyn-float-group-p (group)
  "The predicate of dyn-float-group. It is used frequently in
this code to ensure correct type."
  (eq (type-of group) 'dyn-float-group))

;;;

(defmethod stumpwm:group-add-window
    ((group dyn-float-group)
     window
     &key &allow-other-keys)
  (stumpwm::add-float-window group window)
  (alexandria:appendf (dyn-float-group-dyn-order group)
                      (list (make-instance 'window+ :window window :drift nil)))
  (re-tile group))

(defmethod stumpwm:group-delete-window
    ((group dyn-float-group)
     (window stumpwm::float-window))
  (declare (ignore window))
  (stumpwm::%float-focus-next group)
  (sync-dyn-order group)
  (re-tile group))

(defmethod stumpwm:group-button-press
    ((group dyn-float-group)
     button x y (window stumpwm::float-window))
  ;; Free the window if it's pressed at the boarder or with
  ;; *float-window-modifier*.
  (let ((xwin (stumpwm:window-xwin window)))
    (multiple-value-bind (relx rely same-screen-p child state-mask)
        (xlib:query-pointer (stumpwm::window-parent window))
      (declare (ignore relx rely same-screen-p child))
      (when (or
             (< x (xlib:drawable-x xwin))
             (> x (+ (xlib:drawable-width xwin)
                     (xlib:drawable-x xwin)))
             (< y (xlib:drawable-y xwin))
             (> y (+ (xlib:drawable-height xwin)
                     (xlib:drawable-y xwin)))
             (intersection (stumpwm::float-window-modifier)
                           (xlib:make-state-keys state-mask)))
        (free-window window group))))
  (call-next-method))

;;;

(defun sync-dyn-order (&optional (group (stumpwm:current-group)))
  "Recall that a window+ is a window with some extra data.
Dynamic-floating-group works by operating on the DYN-ORDER, i.e.
the list of window+ of the group.

This function ensures that both the ordinary window list and the
window+ list are in sync. It then ensures that the drifting windows
stay in the beginning of the DYN-ORDER. After doing so, it
ensures the ordinary window list respects the order of
DYN-ORDER."
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (symbol-macrolet ((dyn-order (dyn-float-group-dyn-order group)))

    ;; Sync (w+) according to (w).
    ;;
    ;; If window W does not have a corresponding W+ in the
    ;; dyn-order, make one for it.
    (loop for w in (stumpwm::group-windows group)
          unless (member w (mapcar #'window+-window dyn-order))
            do (push (make-instance 'window+ :window w :drift nil) dyn-order))
    ;; If window W+ does not correspond to a window of GROUP,
    ;; delete W+ from the dyn-order.
    (loop for w+ in dyn-order
          unless (member (window+-window w+) (stumpwm::group-windows group))
            do (alexandria:deletef dyn-order w+))

    ;; Make the drifting windows on top of the stack.
    (flet ((drifting-first (win-a win-b)
             (declare (ignore win-b))
             (window+-drift win-a)))
      (setf dyn-order
            (sort (copy-list dyn-order) #'drifting-first)))

    ;; Let the (group-windows group) respect the order of
    ;; dyn-order.
    (setf (stumpwm::group-windows group)
          (mapcar #'window+-window dyn-order))))

(defun current-window+ (&optional (group (stumpwm:current-group)))
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (let ((gcw (stumpwm::group-current-window group)))
    (find-if (lambda (x)
               (equal gcw (window+-window x)))
             (dyn-float-group-dyn-order group))))

(defun next-window+ (&optional (N 1)
                       (group (stumpwm:current-group))
                       (window (current-window+ group)))
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (let ((dyno (dyn-float-group-dyn-order group)))
    (nth (mod (+ N (position window dyno))
              (length dyno))
         dyno)))

(defcommand focus-next-window (&optional (N 1) (group (stumpwm:current-group))) ()
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (stumpwm::group-focus-window group
                               (window+-window (next-window+ N group))))

(defcommand focus-last-window (&optional (group (stumpwm:current-group))) ()
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (focus-next-window -1 group))

(defun current-window-position (&optional (group (stumpwm:current-group)))
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (position (current-window+ group)
            (dyn-float-group-dyn-order group)
            :test #'equal))

(defun free-all (&optional (group (stumpwm:current-group)))
  "Make all windows in GROUP drift."
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (progn (loop for w+ in (dyn-float-group-dyn-order group)
               do (setf (window+-drift w+) t))
         (re-tile group)))

(defcommand unfree-all
    (&optional (group (stumpwm:current-group)))
  ()
  "Make all windows in GROUP stay. It forces re-tiling all
windows in GROUP."
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (progn
    (loop for w+ in (dyn-float-group-dyn-order group)
          do (setf (window+-drift w+) nil))
    (re-tile group)))

(defun free-window (&optional
                      (window (stumpwm:current-window))
                      (group (stumpwm:current-group)))
  "Make WINDOW drift."
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (progn
    (loop for w+ in (dyn-float-group-dyn-order group)
          if (equal window (window+-window w+))
            do (setf (window+-drift w+) t))
    (re-tile group)))

(defcommand unfree-window
    (&optional
     (window (stumpwm:current-window))
     (group (stumpwm:current-group)))
  ()
  "Make WINDOW stay."
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (progn
    (symbol-macrolet ((dyno (dyn-float-group-dyn-order group)))
      (loop for w+ in dyno
            do (when (equal window (window+-window w+))
                 (progn
                   (alexandria:deletef dyno w+)
                   (setf (window+-drift w+) nil)
                   (if (null dyno)
                       (setf dyno (list w+))
                       (push w+ (cdr (last dyno))))))))
    (re-tile group)))

(defun toggle-drift-current-window
    (&optional
       (window (stumpwm:current-window))
       (group (stumpwm:current-group)))
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (progn
    (if (eq (window+-drift (current-window+ group)) t)
        (unfree-window window group)
        (free-window window group))
    (re-tile group)))

(defun staying-windows+ (&optional (group (stumpwm:current-group)))
  "Return the list of window+s whose :DRIFT slot is nil."
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (remove-if 'window+-drift (dyn-float-group-dyn-order group)))


(defun re-tile (&optional (group (stumpwm:current-group)))
  "The core function that does the retiling. It operates on the
list WL of staying windows, and tile the members according to
the parameter MASTER-RATIO and CURRENT-LAYOUT."
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (progn
    (sync-dyn-order group)
    (let* ((cs (slot-value (stumpwm:current-screen) 'number))
           (sw (xlib:screen-width cs))
           (sh (xlib:screen-height cs))
           (wl (mapcar #'window+-window (staying-windows+ group)))
           (N (length wl))

           (master-ratio (dyn-float-group-master-ratio group))
           (current-layout (car (dyn-float-group-layout-hist group))))

      ;; Waiting for the fix for a related issue for general floating group.
      ;; https://github.com/stumpwm/stumpwm/issues/864
      (setf sw (- sw 2)) ; Adhoc hack to respect boarder width FIXME.
      (setf sh (- sh 18)) ; Adhoc hack to respect modeline FIXME.

      (case N
        (0 nil)
        (1 (stumpwm::float-window-move-resize
            (car wl)
            :x 0 :y 0 :width sw :height sh))
        (t (case current-layout
             (fullscreen
              (loop for k from 0 to (- N 1)
                    do (stumpwm::float-window-move-resize
                        (nth k wl)
                        :x 0 :y 0 :width sw :height sh)))
             (left-vertical
              (progn
                (stumpwm::float-window-move-resize
                 (car wl)
                 :x 0 :y 0 :width (round (* sw master-ratio)) :height sh)
                (loop for k from 1 to (- N 1)
                      do (stumpwm::float-window-move-resize
                          (nth k wl)
                          :x (round (* sw master-ratio))
                          :y (* (round (/ sh (- N 1))) (- k 1))
                          :width (round (* sw (- 1 master-ratio)))
                          :height (round (/ sh (- N 1)))))))
           ; (right-vertical "TODO")
             (horizontal
              (progn
                (stumpwm::float-window-move-resize
                 (car wl)
                 :x 0 :y 0 :width sw :height (round (* sh master-ratio)))
                (loop for k from 1 to (- N 1)
                      do (stumpwm::float-window-move-resize
                          (nth k wl)
                          :x (* (round (/ sw (- N 1))) (- k 1))
                          :y (round (* sh master-ratio))
                          :width (round (/ sw (- N 1)))
                          :height (round (* sh (- 1 master-ratio)))))))
           ; (fibonacci "TODO")
             (otherwise (progn (warn "Layout is not supported. Fall back to the default layout.")
                               (symbol-macrolet ((layout-hist (dyn-float-group-layout-hist (current-group))))
                                 (push *default-layout* layout-hist))
                               (re-tile)))))))))

(defcommand rotate-window-list
    (&optional
     (group (stumpwm:current-group))
     opposite)
  ()
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (flet ((rotate-list (xs &optional opposite)
           "An adhoc pure function that rotates the list."
           (if opposite
               (append (cdr xs) (list (car xs)))
               (append (last xs) (butlast xs)))))
    (symbol-macrolet ((dyno (dyn-float-group-dyn-order group)))
      (setf dyno (rotate-list dyno opposite))
      (re-tile group))))

(defcommand permute-window-list
    (&optional
     opposite
     (group (stumpwm:current-group))
     (n (current-window-position group)))
  ()
  "Permute the window at point with the next one."
  (assert (dyn-float-group-p group) ()
          "Expected GROUP ~A to be of type DYN-FLOAT-GROUP." group)
  (flet ((permute-at (ring n)
           "A pure function that permutes the nth and
the (n+1)th element of RING."
           ;; ((0 1 2 3 4 5) 3) => (0 1 2 4 3 5)
           ;; ((0 1 2 3 4 5) 5) => (5 1 2 3 4 0)
           (when (and (listp ring) (not (null ring)))
             (let* ((l (length ring))
                    (n (mod n l)))
               (when (>= l 2)
                 (if (= n (- l 1))
                     (append (last ring)
                             (butlast (cdr ring))
                             (list (car ring)))
                     (append (subseq ring 0 n)
                             (list (nth (mod (+ n 1) l) ring))
                             (list (nth (mod (+ n 0) l) ring))
                             (subseq ring (+ n 2)))))))))
    (progn
      (when opposite (setf n (- n 1)))
      (symbol-macrolet
          ((dyno (dyn-float-group-dyn-order group)))
        (setf dyno (permute-at dyno n)))
      (re-tile group))))

(defcommand permute-window-list--reverse () ()
  "Permute the window at point with the last one."
  (permute-window-list t))

(defcommand gnew-dyn-float
    (name &optional bg) ((:rest "Group Name: "))
  "Create a new dynamic floating group named NAME."
  (unless name (throw 'error :abort))
  (add-group (stumpwm:current-screen) name
             :type 'dyn-float-group
             :background bg))

(defcommand gnew-dyn-float-bg
    (name) ((:rest "Group Name: "))
  "Create a new dynamic floating group named NAME in the
background."
  (gnew-dyn-float name t))

(defcommand %print-devel-stat () ()
  "A command that helps development. Should not be exported."
  (echo (prin1-to-string
         (list (dyn-float-group-dyn-order (stumpwm:current-group))
               ""
               (group-windows (stumpwm:current-group))))))

(defcommand increase-master-ratio () ()
  "Increase the master ratio of the current dyn-float-group."
  (symbol-macrolet ((master-ratio (dyn-float-group-master-ratio (current-group))))
    (setf master-ratio (* 1.05 master-ratio))
    (re-tile)))
(defcommand decrease-master-ratio () ()
  "Decrease the master ratio of the current dyn-float-group."
  (symbol-macrolet ((master-ratio (dyn-float-group-master-ratio (current-group))))
    (setf master-ratio (* (/ 1 1.05) master-ratio))
    (re-tile)))
(defcommand set-default-master-ratio () ()
  "Set the master ratio of the current dyn-float-group to the
default value *DEFAULT-MASTER-RATIO*."
  (symbol-macrolet ((master-ratio (dyn-float-group-master-ratio (current-group))))
    (setf master-ratio *default-master-ratio*)
    (re-tile)))

(defcommand select-layout (&optional layout) ()
  "Prompt a menu for the user to select a layout, if LAYOUT is
NIL. Push LAYOUT to the history list of layouts. Call the
function #'re-tile, which regards the first member in the history
list as the current layout."
  (symbol-macrolet ((layout-hist (dyn-float-group-layout-hist (current-group))))
    (when (null layout)
      (progn
        (setf layout
              (select-from-menu (current-screen)
                                (mapcar #'string *supported-layouts*)
                                "Select layout: "))
        ;; TODO Anyway to fix this adhoc solution?
        (setf layout (concatenate 'string "stumpwm-dfg::" (car layout)))
        (setf layout (read-from-string layout))))
    (push layout layout-hist)
    (stumpwm::echo (format nil "LAYOUT: ~a" layout))
    (re-tile)))

(defcommand select-next-layout () ()
  "Select the next layout in *SUPPORTED-LAYOUTS*."
  (symbol-macrolet ((layout-hist (dyn-float-group-layout-hist (current-group))))
    (let ((current-layout (car layout-hist))
          next-layout)
      (setf next-layout (second (member current-layout *supported-layouts*)))
      (when (null next-layout)
        (setf next-layout (car *supported-layouts*)))
      (select-layout next-layout))))

(defmacro define-toggle-layout (layout)
  `(defcommand
       ,(read-from-string (concatenate 'string
                                       "toggle-"
                                       (string (eval layout))
                                       "-layout"))
       () ()
     (symbol-macrolet ((layout-hist (dyn-float-group-layout-hist (current-group))))
       (let ((current-layout (car layout-hist)))
         (if (eq current-layout ,layout)
             (select-layout (second layout-hist))
             (select-layout ,layout))))))

;; TODO Implement this nicely into a loop.
;;      Note that the operator is a macro.
(define-toggle-layout 'horizontal)
(define-toggle-layout 'fullscreen)
(define-toggle-layout 'left-verticle)



;;;; Developmental Notes

;; 1. Create for types of window+s: :pin-top, :pin-bottom, :tiled,
;; :unmanaged. The last one will replace :free .

;; 2. Cooperate with the modeline : how to read where it is, whether
;; it is active.. etc.

;; 3. Resizing and moving the floating windows (with keybinding).

;; 4. Drop down window! (:pin-top)

;; 5. Gap support.

;; 6. More distant plan : multilayer support.
