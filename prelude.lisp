;; This file contains out-of-place type definitions that would logically fit
;; better elsewhere but has to come early in order to avoid compiler warnings.

(in-package :stumpwm)

(define-swm-class tile-window (window)
  ((frame   :initarg :frame   :accessor window-frame :type frame)
   (normal-size :initform nil :accessor window-normal-size)))

(define-swm-class dynamic-group (tile-group)
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
    :accessor dynamic-group-head-info-alist
    :documentation "Alist with heads as keys containing information for each
head.  Calling ASSOC on this alist returns a list whose FIRST element is the
head, SECOND is the layout of the frames, THIRD is the master frame, FOURTH is the
the master window, FIFTH is the window stack frames, SIXTH is the window
stack windows, and SEVENTH is the major split ratio."))
  (:documentation "A group type that implements dynamic tiling à la DWM with a
single master window and a window stack."))
