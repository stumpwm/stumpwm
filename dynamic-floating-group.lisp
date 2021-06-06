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

(defparameter *default-layout* 'left-vertical)
(defparameter *default-master-ratio* (/ 2 (+ 1 (sqrt 5))))

(defstruct window+
  "An augmented window (window+) is a window with some other
information. Usually, in a dynamic floating group, the
information will be used when the group re-tiles it."
  ;; access example: (window+-free (make-window+ :window 1 :free t))
  :window :free)

;; A dyn-order, or a dynamic order, is a list of window+.
(defclass dyn-float-group (stumpwm::float-group)
  ((dyn-order :initform nil
              :accessor dyn-float-group-dyn-order
              :documentation
              "The list of augmented windows (window+) that the
              group holds. A dyn-float-group should tile
              automatically according to it.")
   (layout-hist :initform (list *default-layout*)
                :accessor dyn-float-group-layout-hist
                :documentation
                "The list of layout histories. The first element
                is interpreted as the current layout.")
   (master-ratio :initform *default-master-ratio*
                 :accessor dyn-float-group-master-ratio
                 :documentation
                 "The ratio that the master window will take
                 apart.")))

(defun dyn-float-group-p (group)
  (eq (type-of group) 'dyn-float-group))

(flet ((add-float-window (group window)
         ;; not sure if needed
         (change-class window 'stumpwm::float-window)
         (stumpwm::float-window-align window)
         (stumpwm::group-focus-window group window)))
  (defmethod stumpwm:group-add-window
      ((group dyn-float-group)
       window
       &key &allow-other-keys)
    (add-float-window group window)
    (nconc (dyn-float-group-dyn-order group)
           (list (make-window+ :window window :free nil)))
    (re-tile group)))

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
      (declare (ignore same-screen-p child))
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

(defun sync-dyn-order (&optional (group (stumpwm:current-group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (symbol-macrolet ((dyn-order (dyn-float-group-dyn-order group)))
        ;; If window W does not have a corresponding W+ in the
        ;; dyn-order, make one for it.
        (loop for w in (stumpwm::group-windows group)
              do (unless (member w (mapcar #'window+-window dyn-order))
                   (push (make-window+ :window w :free nil) dyn-order)))
        ;; If window W+ does not correspond to a window of GROUP,
        ;; delete W+ from the dyn-order.
        (loop for w+ in dyn-order
              do (unless (member (window+-window w+) (stumpwm::group-windows group))
                   (alexandria:deletef dyn-order w+)))
        ;; Make the free windows on top of the stack.
        (setf dyn-order
              (append (remove-if (lambda (dyno)
                                   (equal nil (window+-free dyno)))
                                 dyn-order)
                      (remove-if (lambda (dyno)
                                   (equal t (window+-free dyno)))
                                 dyn-order)))
        ;; Let the (group-windows group) respect the order of
        ;; dyn-order
        (setf (stumpwm::group-windows group)
              (mapcar #'window+-window dyn-order)))))

(defun current-window+ (&optional (group (stumpwm:current-group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (let ((gcw (stumpwm::group-current-window group)))
        (find-if (lambda (x)
                   (equal gcw (window+-window x)))
                 (dyn-float-group-dyn-order group)))))

(defun next-window+ (&optional (N 1)
                       (group (stumpwm:current-group))
                       (window (current-window+ group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (let ((dyno (dyn-float-group-dyn-order group)))
        (nth (mod (+ N (position window dyno))
                  (length dyno))
             dyno))))

(defcommand focus-next-window (&optional (N 1) (group (stumpwm:current-group))) ()
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (stumpwm::group-focus-window group
                                   (window+-window (next-window+ N group)))))

(defcommand focus-last-window (&optional (group (stumpwm:current-group))) ()
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (focus-next-window -1 group)))

(defun current-window-position (&optional (group (stumpwm:current-group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (position (current-window+ group)
                (dyn-float-group-dyn-order group)
                :test #'equal)))

(defun free-all (&optional (group (stumpwm:current-group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      ;; alias: un-tile-all
      (progn (loop for w+ in (dyn-float-group-dyn-order group)
                   do (setf (window+-free w+) t))
             (re-tile group))))

;; This will effectively force re-tile all windows in this group.
(defcommand unfree-all
    (&optional (group (stumpwm:current-group)))
    ()
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (progn
        ;; alias: tile-all
        (loop for w+ in (dyn-float-group-dyn-order group)
              do (setf (window+-free w+) nil))
        (re-tile group))))

(defun free-window (&optional
                      (window (stumpwm:current-window))
                      (group (stumpwm:current-group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (progn
        (loop for w+ in (dyn-float-group-dyn-order group)
              if (equal window (window+-window w+))
                do (setf (window+-free w+) t))
        (re-tile group))))

(defcommand unfree-window
    (&optional
     (window (stumpwm:current-window))
     (group (stumpwm:current-group)))
    ()
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (progn
        (symbol-macrolet ((dyno (dyn-float-group-dyn-order group)))
          (loop for w+ in dyno
                do (when (equal window (window+-window w+))
                     (progn
                       (alexandria:deletef dyno w+)
                       (setf (window+-free w+) nil)
                       (if (null dyno)
                           (setf dyno (list w+))
                           (push w+ (cdr (last dyno))))))))
        (re-tile group))))

(defun toggle-freeness-current-window
    (&optional
       (window (stumpwm:current-window))
       (group (stumpwm:current-group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (progn
        (if (eq (window+-free (current-window+ group)) t)
            (unfree-window window group)
            (free-window window group))
        (re-tile group))))

(defun unfloating-windows+ (&optional (group (stumpwm:current-group)))
  "Return the list of window+s whose :FREE slot is nil."
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (remove-if-not
       (lambda (w+)
         (eq (window+-free w+) nil))
       (dyn-float-group-dyn-order group))))

(defun re-tile (&optional (group (stumpwm:current-group)))
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (progn
        (sync-dyn-order group)
        (let* ((cs (slot-value (stumpwm:current-screen) 'number))
               (sw (xlib:screen-width cs))
               (sh (xlib:screen-height cs))
               (wl (mapcar #'window+-window (unfloating-windows+ group)))
               (N (length wl))

               (master-ratio (dyn-float-group-master-ratio group))
               (current-layout (car (dyn-float-group-layout-hist group))))

          ;; Waiting for the fix for a related issue for general floating group.
          ;; https://github.com/stumpwm/stumpwm/issues/864
          (setf sw (- sw 2))  ; Adhoc hack to respect boarder width FIXME.
          (setf sh (- sh 18)) ; Adhoc hack to respect modeline FIXME.

          (case N
            (0 nil)
            (1 (stumpwm::float-window-move-resize
                (car wl)
                :x 0 :y 0 :width sw :height sh))
            (t (case current-layout
               ('left-vertical
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
               ('horizontal
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
               ('fullscreen
                (loop for k from 0 to (- N 1)
                      do (stumpwm::float-window-move-resize
                          (nth k wl)
                          :x 0 :y 0 :width sw :height sh)))
               ;; ('right-vertical "TODO")
               ;; ('fibonacci "TODO")
               (otherwise (error "Layout isn't supported.")))))))))

(defcommand rotate-window-list
    (&optional
     (group (stumpwm:current-group))
     opposite)
    ()
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
      (flet ((rotate-list (xs &optional opposite)
               "An adhoc pure function that rotates the list."
               (if opposite
                   (append (cdr xs) (list (car xs)))
                   (append (last xs) (butlast xs)))))
        (symbol-macrolet ((dyno (dyn-float-group-dyn-order group)))
          (setf dyno (rotate-list dyno opposite))
          (re-tile group)))))

(defcommand permute-window-list
    (&optional
     opposite
     (group (stumpwm:current-group))
     (n (current-window-position group)))
    ()
  (if (not (dyn-float-group-p group))
      (error "GROUP must be of type DYN-FLOAT-GROUP.")
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
             (re-tile group)))))

(defcommand gnew-dyn-float
    (name) ((:rest "Group Name: "))
  "Create a new dynamic floating group named NAME."
  (unless name (throw 'error :abort))
  (add-group (stumpwm:current-screen) name
             :type 'dyn-float-group))

(defcommand gnew-dyn-float-bg
    (name) ((:rest "Group Name: "))
  "Create a new dynamic floating group named NAME in the background."
  (unless name (throw 'error :abort))
  (add-group (stumpwm:current-screen) name
             :type 'dyn-float-group
             :background t))

(defcommand print-devel-stat () ()
  "A command that helps development. Should not be exported."
  (echo (prin1-to-string
         (list (dyn-float-group-dyn-order (stumpwm:current-group))
               ""
               (group-windows (stumpwm:current-group))))))

(defcommand increase-master-ratio () ()
  (symbol-macrolet ((master-ratio (dyn-float-group-master-ratio (current-group))))
    (setf master-ratio (* 1.05 master-ratio))
    (re-tile)))
(defcommand decrease-master-ratio () ()
  (symbol-macrolet ((master-ratio (dyn-float-group-master-ratio (current-group))))
    (setf master-ratio (* (/ 1 1.05) master-ratio))
    (re-tile)))
(defcommand default-master-ratio () ()
  (symbol-macrolet ((master-ratio (dyn-float-group-master-ratio (current-group))))
    (setf master-ratio *default-master-ratio*)
    (re-tile)))

;; TODO Learn how to use stumpwm's menu and implement this.
;; (defcommand select-layout () ())

(defcommand toggle-left-vertical-layout () ()
  (symbol-macrolet ((layout-hist (dyn-float-group-layout-hist (current-group))))
    (if (eq (car layout-hist) 'left-vertical)
        (push (nth 1 layout-hist) layout-hist)
        (push 'left-vertical layout-hist))
    (re-tile)))

(defcommand toggle-horizontal-layout () ()
  (symbol-macrolet ((layout-hist (dyn-float-group-layout-hist (current-group))))
    (if (eq (car layout-hist) 'horizontal)
        (push (nth 1 layout-hist) layout-hist)
        (push 'horizontal layout-hist))
    (re-tile)))

(defcommand toggle-fullscreen-layout () ()
  (symbol-macrolet ((layout-hist (dyn-float-group-layout-hist (current-group))))
    (if (eq (car layout-hist) 'fullscreen)
        (push (nth 1 layout-hist) layout-hist)
        (push 'fullscreen layout-hist))
    (re-tile)))
