;; Copyright (C) 2016 Joram Schrijver
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

(in-package :stumpwm)

(export '(gravity-coords))

(defgeneric gravity-coords (gravity width height minx miny maxx maxy)
  (:documentation "Get the X and Y coordinates to place something of width
WIDTH and height HEIGHT within an area defined by MINX MINY MAXX and MAXY,
guided by GRAVITY."))

(defmacro define-simple-gravity (name x y)
  "Define a simple gravity calculation of name NAME, where X and Y are one of
:MIN, :MAX or :CENTER."
  `(defmethod gravity-coords ((gravity (eql ,name))
                              (width number) (height number)
                              (minx number) (miny number)
                              (maxx number) (maxy number))
     (declare (ignorable gravity width height minx miny maxx maxy))
     (values ,(ecase x
                (:min 'minx)
                (:max '(- maxx width))
                (:center '(+ minx (truncate (- maxx minx width) 2))))
             ,(ecase y
                (:min 'miny)
                (:max '(- maxy height))
                (:center '(+ miny (truncate (- maxy miny height) 2)))))))

(define-simple-gravity :top-right :max :min)
(define-simple-gravity :top-left :min :min)
(define-simple-gravity :bottom-right :max :max)
(define-simple-gravity :bottom-left :min :max)
(define-simple-gravity :right :max :center)
(define-simple-gravity :left :min :center)
(define-simple-gravity :top :center :min)
(define-simple-gravity :bottom :center :max)
(define-simple-gravity :center :center :center)

(defun gravitate-xwin (screen frame xwin gravity)
  "Position the x, y of the window according to its gravity. This
function expects to be wrapped in a with-state for win."
  (xlib:with-state ((screen-root screen))
    (let* ((w (+ (xlib:drawable-width xwin)
                 (* (xlib:drawable-border-width xwin) 2)))
           (h (+ (xlib:drawable-height xwin)
                 (* (xlib:drawable-border-width xwin) 2)))
           (frame-x (frame-x frame))
           (frame-y (frame-y frame))
           (frame-maxx (+ frame-x (frame-width frame)))
           (frame-maxy (+ frame-y (frame-height frame))))
      (multiple-value-bind (x y)
          (gravity-coords gravity w h frame-x frame-y frame-maxx frame-maxy)
        (setf (xlib:drawable-y xwin) (max frame-y y)
              (xlib:drawable-x xwin) (max frame-x x))))))
