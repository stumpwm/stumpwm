;; Copyright (C) 2007-2008 Jonathan Moore Liles
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
;; This simplified implementation of the the C color code is as follows:
;;
;; ^B bright
;; ^b dim
;; ^n normal (sgr0)
;;
;; ^00 black black
;; ^10 red black
;; ^01 black red
;; ^1* red clear
;;
;; and so on.
;;
;; I won't explain here the many reasons that C is better than ANSI, so just
;; take my word for it.

(in-package :stumpwm)

(export '(*colors* update-color-map adjust-color update-screen-color-context))

(defvar *colors*
  '("black"
    "red"
    "green"
    "yellow"
    "blue"
    "magenta"
    "cyan"
    "white")
  "Eight colors by default. You can redefine these to whatever you like and
then call (update-color-map).")

(defvar *color-map* nil)
(defvar *foreground* nil)
(defvar *background* nil)
(defvar *reverse* nil)
(defvar *color-stack* '())

(defun adjust-color (color amt)
  (labels ((max-min (x y) (max 0 (min 1 (+ x y)))))
    (setf (xlib:color-red color) (max-min (xlib:color-red color) amt)
          (xlib:color-green color) (max-min (xlib:color-green color) amt)
          (xlib:color-blue color) (max-min (xlib:color-blue color) amt))))

(defun alloc-color (screen color)
  (xlib:alloc-color (xlib:screen-default-colormap (screen-number screen)) color))

(defun lookup-color (screen color)
  (cond
    ((typep color 'xlib:color) color)
    (t (xlib:lookup-color (xlib:screen-default-colormap (screen-number screen)) color))))

;; Normal colors are dimmed and bright colors are intensified in order
;; to more closely resemble the VGA pallet.
(defun update-color-map (screen)
  "Read *colors* and cache their pixel colors for use when rendering colored text."
  (let ((scm (xlib:screen-default-colormap (screen-number screen))))
    (labels ((map-colors (amt)
               (loop for c in *colors*
                     as color = (lookup-color screen c)
                     do (adjust-color color amt)
                     collect (xlib:alloc-color scm color))))
      (setf (screen-color-map-normal screen) (apply #'vector (map-colors -0.25))
            (screen-color-map-bright screen) (apply #'vector (map-colors 0.25))))))

(defun update-screen-color-context (screen)
  (let* ((cc (screen-message-cc screen))
         (bright (lookup-color screen *text-color*)))
    (setf
     (ccontext-default-fg cc) (screen-fg-color screen)
     (ccontext-default-bg cc) (screen-bg-color screen))
    (adjust-color bright 0.25)
    (setf (ccontext-default-bright cc) (alloc-color screen bright))))

(defun get-bg-color (screen cc color)
  (setf *background* color)
  (if color
      (svref (screen-color-map-normal screen) color)
      (ccontext-default-bg cc)))

(defun get-fg-color (screen cc color)
  (setf *foreground* color)
  (if color
      (svref *color-map* color)
      (if (eq *color-map* (screen-color-map-bright screen))
          (ccontext-default-bright cc)
          (ccontext-default-fg cc))))

(defun set-color (screen cc s i)
  (let* ((gc (ccontext-gc cc))
         (l (- (length s) i))
         (r 2)
         (f (subseq s i (1+ i)))
         (b (if (< l 2) "*" (subseq s (1+ i) (+ i 2)))))
    (labels
        ((set-fg-bg (fg bg)
           (if *reverse*
               (setf
                (xlib:gcontext-foreground gc) bg
                (xlib:gcontext-background gc) fg)
               (setf
                (xlib:gcontext-foreground gc) fg
                (xlib:gcontext-background gc) bg)))
         (update-colors ()
           (set-fg-bg (get-fg-color screen cc *foreground*)
                      (get-bg-color screen cc *background*))))
      (case (elt f 0)
        (#\n                            ; normal
         (setf f "*" b "*" r 1
               *color-map* (screen-color-map-normal screen)
               *reverse* nil)
         (get-fg-color screen cc nil)
         (get-bg-color screen cc nil))
        (#\b                            ; bright off
         (setf *color-map* (screen-color-map-normal screen))
         (update-colors)
         (return-from set-color 1))
        (#\B                            ; bright on
         (setf *color-map* (screen-color-map-bright screen))
         (update-colors)
         (return-from set-color 1))
        (#\R
         (setf *reverse* t)
         (update-colors)
         (return-from set-color 1))
        (#\r
         (setf *reverse* nil)
         (update-colors)
         (return-from set-color 1))
        (#\[
         (push (list *foreground* *background* *color-map*) *color-stack*)
         (return-from set-color 1))
        (#\]
         (let ((colors (pop *color-stack*)))
           (when colors
             (setf *foreground* (first colors)
                   *background* (second colors)
                   *color-map* (third colors))))
         (update-colors)
         (return-from set-color 1))
        (#\^                            ; circumflex
         (return-from set-color 1)))
      (handler-case
          (let ((fg (if (equal f "*") (progn (get-fg-color screen cc nil) (ccontext-default-fg cc)) (get-fg-color screen cc (parse-integer f))))
                (bg (if (equal b "*") (progn (get-bg-color screen cc nil) (ccontext-default-bg cc)) (get-bg-color screen cc (parse-integer b)))))
            (set-fg-bg fg bg))
        (error (c) (dformat 1 "Invalid color code: ~A" c))))
    r))

(defun render-strings (screen cc padx pady strings highlights &optional (draw t))
  (let* ((height (font-height (screen-font screen)))
         (width 0)
         (gc (ccontext-gc cc))
         (win (ccontext-win cc))
         (px (ccontext-px cc))
         (*foreground* nil)
         (*background* nil)
         (*reverse* nil)
         (*color-stack* '())
         (*color-map* (screen-color-map-normal screen)))
    (when draw
      (when (or (not px)
                (/= (xlib:drawable-width px) (xlib:drawable-width win))
                (/= (xlib:drawable-height px) (xlib:drawable-height win)))
        (when px (xlib:free-pixmap px))
        (setf px (xlib:create-pixmap :drawable win
                                     :width (xlib:drawable-width win)
                                     :height (xlib:drawable-height win)
                                     :depth (xlib:drawable-depth win))
              (ccontext-px cc) px))
      (xlib:with-gcontext (gc :foreground (xlib:gcontext-background gc))
        (xlib:draw-rectangle px gc 0 0 (xlib:drawable-width px) (xlib:drawable-height px) t)))
    (loop for s in strings
          ;; We need this so we can track the row for each element
          for i from 0 to (length strings)
          do (let ((x 0) (off 0) (len (length s)))
               (loop
                for st = 0 then (+ en (1+ off))
                as en = (position #\^ s :start st)
                do (progn
		     (let ((en (cond ((and en (= (1+ en) len)) nil)
				     ((and en (char= #\^ (char s (1+ en)))) (1+ en))
				     (t en))))
		       (when draw
                         (draw-image-glyphs px gc (screen-font screen)
                                            (+ padx x)
                                            (+ pady (* i height)
                                               (font-ascent (screen-font screen)))
                                            (subseq s st en)
                                            :translate #'translate-id
                                            :size 16))
		       (setf x (+ x (text-line-width (screen-font screen) (subseq s st en) :translate #'translate-id))
			     width (max width x)))
		     (when (and en (< (1+ en) len))
		       ;; right-align rest of string?
		       (if (char= #\> (char s (1+ en)))
			   (progn
			     (when draw
			       (setf x (- (xlib:drawable-width px) (* 2 padx)
					  ;; get width of rest of s
					  (render-strings screen cc padx pady
							  (list (subseq s (+ en 2)))
							  '() nil))
				     width (- (xlib:drawable-width px) (* 2 padx))))
			     (setf off 1))
			   (setf off (set-color screen cc s (1+ en))))))
		  while en))
          when (find i highlights :test 'eql)
          do (when draw (invert-rect screen px
                                     0 (* i height)
                                     (xlib:drawable-width px)
                                     height)))
    (when draw
      (xlib:copy-area px gc 0 0 (xlib:drawable-width px) (xlib:drawable-height px) win 0 0))
    (set-color screen cc "n" 0)
    width))

;;; FIXME: It would be nice if the output of this parser was used to
;;; draw the text, but the current drawing implementation is probably
;;; faster.
(defun parse-color (s i)
  (let ((l (- (length s) i)))
    (when (zerop l)
      (return-from parse-color (values `("^") 0)))
    (let ((f (subseq s i (1+ i)))
          (b (if (< l 2) "*" (subseq s (1+ i) (+ i 2)))))
      (case (elt f 0)
        (#\n                            ; normal
         (values
          `((:background "*")
            (:foreground "*")
            (:reverse nil))
          1))
        (#\b                            ; bright off
         (values
          `((:bright nil))
          1))
        (#\B                            ; bright on
         (values 
          `((:bright t))
          1))
        (#\R
         (values
          `((:reverse t))
          1))
        (#\r
         (values
          `((:reverse nil))
          1))
        (#\[
         (values
          `((:push))
          1))
        (#\]
         (values
          `((:pop))
          1))
        (#\^                            ; circumflex
         (values `("^") 1))
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (values
          `((:background ,(if (string= f "*")
                              "*"
                              (parse-integer f)))
            (:foreground ,(if (string= b "*")
                              "*"
                              (parse-integer b))))
          2))
        (t
         (values `(,(format nil "^~a" f)) 1))))))

(defun parse-color-string (string)
  "parse a color coded string into a list of strings and color codes"
  (loop
     with color = nil
     with off = 0
     for st = 0 then (min (+ en (1+ off)) (length string))
     as en = (position #\^ string :start st)
     ;; avoid empty strings at the beginning and end
     unless (or (eql en st)
                (eql st (length string)))
     collect (subseq string st en)
     while en
     append (progn
              (multiple-value-setq (color off) (parse-color string (1+ en)))
              color)))

(defun uncolorify (string)
  "Remove any color markup in STRING"
  (format nil "~{~a~}" (remove-if-not 'stringp (parse-color-string string))))
