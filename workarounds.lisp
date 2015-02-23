;;; workarounds for bugs in clx

(in-package :xlib)

;;; CLISP can't handle non-compliant (and even compliant) wm-class strings. See
;;; test-wm-class in test-wm.lisp.

;; This redefines decod-wm-size-hints in clisp because "It seems clisp
;; tries to be sneaky and represent the min and max aspect ratios as a
;; ratio number, which works except when the 0/0 is how you specify
;; that there is no aspect ratio, as mplayer/mpv/mplayer2 does."
;; http://lists.gnu.org/archive/html/stumpwm-devel/2009-08/msg00025.html
#+clisp
(defun decode-wm-size-hints (vector)
  (declare (type (or null (simple-vector *)) vector))
  (declare (values (or null wm-size-hints)))
  (when vector
    (let ((flags (aref vector 0))
          (hints (make-wm-size-hints)))
      (declare (type card16 flags)
               (type wm-size-hints hints))
      (setf (wm-size-hints-user-specified-position-p hints) (logbitp 0 flags))
      (setf (wm-size-hints-user-specified-size-p hints) (logbitp 1 flags))
      (setf (wm-size-hints-program-specified-position-p hints)
            (logbitp 2 flags))
      (setf (wm-size-hints-program-specified-size-p hints) (logbitp 3 flags))
      (when (logbitp 4 flags)
        (setf (wm-size-hints-min-width hints) (aref vector 5)
              (wm-size-hints-min-height hints) (aref vector 6)))
      (when (logbitp 5 flags)
        (setf (wm-size-hints-max-width hints) (aref vector 7)
              (wm-size-hints-max-height hints) (aref vector 8)))
      (when (logbitp 6 flags)
        (setf (wm-size-hints-width-inc hints) (aref vector 9)
              (wm-size-hints-height-inc hints) (aref vector 10)))
      (when (logbitp 7 flags)
        (setf (wm-size-hints-min-aspect hints) (ignore-errors (/ (aref
                                                                  vector 11) (aref vector 12)))
              (wm-size-hints-max-aspect hints) (ignore-errors (/ (aref
                                                                  vector 13) (aref vector 14)))))
      (when (> (length vector) 15)
        ;; This test is for backwards compatibility since old Xlib programs
        ;; can set a size-hints structure that is too small.  See ICCCM.
        (when (logbitp 8 flags)
          (setf (wm-size-hints-base-width hints) (aref vector 15)
                (wm-size-hints-base-height hints) (aref vector 16)))
        (when (logbitp 9 flags)
          (setf (wm-size-hints-win-gravity hints)
                (decode-type (member :unmap :north-west :north :north-east :west
                                     :center :east :south-west :south
                                     :south-east :static)
                             (aref vector 17)))))
      ;; Obsolete fields
      (when (or (logbitp 0 flags) (logbitp 2 flags))
        (setf (wm-size-hints-x hints) (aref vector 1)
              (wm-size-hints-y hints) (aref vector 2)))
      (when (or (logbitp 1 flags) (logbitp 3 flags))
        (setf (wm-size-hints-width hints) (aref vector 3)
              (wm-size-hints-height hints) (aref vector 4)))
      hints)))
