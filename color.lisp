;; Copyright (C) 2007-2008 Jonathan Moore Liles
;; Copyright (C) 2014 Joram Schrijver
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
;; A change in color, or presentation in general, is started by a ^. If that
;; ^ is followed by a single number, that's taken as the index into the color
;; map to be set as the foreground color. If the ^ is followed by two numbers,
;; the first is taken as the index of the foreground color, and the second as
;; the index of the background color. Either of those can also be *, which
;; means the value should be set to default.
;;
;; ^n resets the foreground and background color back to default.
;; ^A B turns bright colors on, and b turns them off.
;; ^R turns reverse colors on and r turns them off.
;; ^[ pushes the current settings onto the color stack. The current settings
;;    remain unchanged.
;; ^] pops color settings off the stack.
;; ^> aligns the rest of the string to the right of the window.
;; ^f<n> switches to the font at index n in the screen's font stack.
;; ^^ prints a regular caret
;; ^(<modifier> &rest arguments) allows for more complicated color settings:
;;    <modifier> can be one of :fg, :bg, :reverse, :bright, :push, :pop, :font
;;    and :>.
;;    The arguments for each modifier differ:
;;    - :fg and :bg take a color as an argument, which can either be a numeric
;;      index into the color map or a hexadecimal color in the form of "#fff"
;;      or "#ffffff".
;;    - :reverse and :bright take either t or nil as an argument. A t enables
;;      the setting and nil disables it.
;;    - :push and :pop take no arguments. :push pushes the current settings onto
;;      the color stack, leaving the current settings intact. :pop pops color
;;      settings off the stack, updating the current settings.
;;    - :font takes an integer that represents an index into the screen's list
;;      of fonts, or, possibly, a literal font object that can immediately be
;;      used. In a string you'll probably only want to specify an integer.
;;    - :> takes no arguments. It triggers right-alignment for the rest of the
;;      line.

(in-package :stumpwm)

(export '(*colors*
          update-color-map
          adjust-color
          update-screen-color-context
          lookup-color))

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

(defun adjust-color (color amt)
  (labels ((max-min (x y) (max 0 (min 1 (+ x y)))))
    (setf (xlib:color-red color) (max-min (xlib:color-red color) amt)
          (xlib:color-green color) (max-min (xlib:color-green color) amt)
          (xlib:color-blue color) (max-min (xlib:color-blue color) amt))))

(defun hex-to-xlib-color (color)
  (cond
   ((= 4 (length color))
    (let ((red (/ (parse-integer (subseq color 1 2) :radix 16) 255.0))
          (green (/ (parse-integer (subseq color 2 3) :radix 16) 255.0))
          (blue (/ (parse-integer (subseq color 3 4) :radix 16) 255.0)))
      (xlib:make-color :red (+ red (* 16 red))
                       :green (+ green (* 16 green))
                       :blue (+ blue (* 16 blue)))))
   ((= 7 (length color))
    (let ((red (/ (parse-integer (subseq color 1 3) :radix 16) 255.0))
          (green (/ (parse-integer (subseq color 3 5) :radix 16) 255.0))
          (blue (/ (parse-integer (subseq color 5 7) :radix 16) 255.0)))
      (xlib:make-color :red red :green green :blue blue)))))

(defun lookup-color (screen color)
  (cond
    ((typep color 'xlib:color) color)
    ((and (stringp color)
          (or (= 7 (length color))
              (= 4 (length color)))
          (char= #\# (elt color 0)))
     (hex-to-xlib-color color))
    (t (xlib:lookup-color (xlib:screen-default-colormap (screen-number screen))
                          color))))

(defun alloc-color (screen color)
  (xlib:alloc-color 
   (xlib:screen-default-colormap (screen-number screen))
   (lookup-color screen color)))

;; Normal colors are dimmed and bright colors are intensified in order
;; to more closely resemble the VGA pallet.
(defun update-color-map (screen)
  "Read *colors* and cache their pixel colors for use when rendering colored text."
  (labels ((map-colors (amt)
             (loop for c in *colors*
                   as color = (lookup-color screen c)
                   do (adjust-color color amt)
                   collect (alloc-color screen color))))
    (setf (screen-color-map-normal screen) (apply #'vector (map-colors -0.25))
          (screen-color-map-bright screen) (apply #'vector (map-colors 0.25)))))

(defun update-screen-color-context (screen)
  (let* ((cc (screen-message-cc screen))
         (bright (lookup-color screen *text-color*)))
    (setf
     (ccontext-default-fg cc) (screen-fg-color screen)
     (ccontext-default-bg cc) (screen-bg-color screen))
    (adjust-color bright 0.25)
    (setf (ccontext-default-bright cc) (alloc-color screen bright))))

;;; Parser for color strings

(defun parse-color (color)
  "Parse a possible colorcode into a list of the appropriate modifiers.
If COLOR isn't a colorcode a list containing COLOR is returned."
  (if (and (> (length color) 1)
           (char= (char color 0) #\^))
      (let ((foreground (char color 1))
            (background (if (> (length color) 2)
                            (char color 2)
                            :reset)))
        (case foreground
          ;; Normalize colors
          (#\n '((:bg :reset)
                 (:fg :reset)
                 (:reverse nil)))
          (#\R '((:reverse t)))
          (#\r '((:reverse nil)))
          (#\B '((:bright t)))
          (#\b '((:bright nil)))
          (#\[ '((:push)))
          (#\] '((:pop)))
          (#\> '((:>)))
          (#\f `((:font ,(or (parse-integer (string background)
                                            :junk-allowed t)
                             0))))
          (#\^ '("^"))
          (#\( (list (read-from-string (subseq color 1))))
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\*)
           `((:bg ,(or (parse-integer (string background)
                                      :junk-allowed t)
                       :reset))
             (:fg ,(or (parse-integer (string foreground)
                                      :junk-allowed t)
                       :reset))
             (:reverse nil)))))
      (list color))) ; this isn't a colorcode 

(defun parse-color-string (string)
  "Parse a color-coded string into a list of strings and color modifiers"
  (let ((substrings
          (remove-if
           (lambda (str) (zerop (length str)))
           (ppcre:split
            "(\\^[nrRbB>\\[\\]^]|\\^[0-9*]{1,2}|\\^f[0-9]|\\^\\(.*?\\))"
            string :with-registers-p t))))
    (loop for substring in substrings append (parse-color substring))))

(defun uncolorify (string)
  "Remove any color markup in STRING"
  (format nil "~{~a~}" (remove-if-not 'stringp (parse-color-string string))))


;;; Color modifiers and rendering code

(defun find-color (color default cc &aux (screen (ccontext-screen cc)))
  (cond ((or (null color)
             (eq :reset color))
         default)
        ((integerp color)
         (svref (if (ccontext-brightp cc)
                    (screen-color-map-bright screen)
                    (screen-color-map-normal screen))
                color))
        (t (nth-value 0 (alloc-color screen color)))))

(defun find-font (cc specified-font &aux (font (or specified-font 0)))
  (if (integerp font)
      (nth font (screen-fonts (ccontext-screen cc)))
      font))

(defgeneric apply-color (ccontext modifier &rest arguments))

(defmethod apply-color :around ((cc ccontext) modifier &rest arguments)
  (declare (ignorable ccontext modifier arguments))
  (when *draw-in-color*
    (call-next-method)))

(defmethod apply-color ((cc ccontext) (modifier (eql :fg)) &rest args)
  (setf (ccontext-fg cc) (first args))
  (let* ((gcontext (ccontext-gc cc))
         (specified-color (first args))
         (color (find-color specified-color
                            (if (ccontext-brightp cc)
                                (ccontext-default-bright cc)
                                (ccontext-default-fg cc))
                            cc)))
    (if (ccontext-reversep cc)
        (setf (xlib:gcontext-background gcontext) color)
        (setf (xlib:gcontext-foreground gcontext) color))))

(defmethod apply-color ((cc ccontext) (modifier (eql :bg)) &rest args)
  (setf (ccontext-bg cc) (first args))
  (let* ((gcontext (ccontext-gc cc))
         (specified-color (first args))
         (color (find-color specified-color
                            (ccontext-default-bg cc)
                            cc)))
    (if (ccontext-reversep cc)
        (setf (xlib:gcontext-foreground gcontext) color)
        (setf (xlib:gcontext-background gcontext) color))))

(defmethod apply-color ((cc ccontext) (modifier (eql :reverse)) &rest args)
  (setf (ccontext-reversep cc) (first args))
  (let ((fg (ccontext-fg cc))
        (bg (ccontext-bg cc)))
    (apply-color cc :fg fg)
    (apply-color cc :bg bg)))

(defmethod apply-color ((cc ccontext) (modifier (eql :bright)) &rest args)
  (setf (ccontext-brightp cc) (first args))
  (let ((fg (ccontext-fg cc))
        (bg (ccontext-bg cc)))
    (apply-color cc :fg fg)
    (apply-color cc :bg bg)))

(defmethod apply-color ((cc ccontext) (modifier (eql :push)) &rest args)
  (declare (ignore args))
  (push (list (ccontext-fg cc)
              (ccontext-bg cc)
              (ccontext-brightp cc)
              (ccontext-reversep cc)
              (ccontext-font cc))
        (ccontext-color-stack cc)))

(defmethod apply-color ((cc ccontext) (modifier (eql :pop)) &rest args)
  (declare (ignore args))
  (let ((values (pop (ccontext-color-stack cc))))
    (apply-color cc :fg (first values))
    (apply-color cc :bg (second values))
    (apply-color cc :bright (third values))
    (apply-color cc :reverse (fourth values))
    (apply-color cc :font (fifth values))))

(defmethod apply-color ((cc ccontext) (modifier (eql :font)) &rest args)
  (let ((font (or (first args) 0)))
    (setf (ccontext-font cc) (find-font cc font))))

(defmethod apply-color ((cc ccontext) (modifier (eql :>)) &rest args)
  ;; This is a special case in RENDER-STRING and is thus only called when not
  ;; rendering. Since it doesn't otherwise have any effects, we just ignore it.
  (declare (ignore cc modifier args)))

(defun max-font-height (parts cc)
  "Return the biggest font height for all of the fonts occurring in PARTS in
the form of (:FONT ...) modifiers."
  (font-height
   (cons (ccontext-font cc)
         (loop for part in parts
               if (and (listp part)
                       (eq :font (first part)))
                 collect (find-font cc (second part))))))

(defun reset-color-context (cc)
  (apply-color cc :fg)
  (apply-color cc :bright)
  (apply-color cc :bg)
  (apply-color cc :reverse)
  (apply-color cc :font))

(defun rendered-string-size (string-or-parts cc &optional (resetp t))
  "Return the width and height needed to render STRING-OR-PARTS, a single-line
string."
  (let* ((parts (if (stringp string-or-parts)
                    (parse-color-string string-or-parts)
                    string-or-parts))
         (height (max-font-height parts cc))
         (width 0))
    (loop
      for part in parts
      if (stringp part)
        do (incf width (text-line-width (ccontext-font cc)
                                        part
                                        :translate #'translate-id))
      else if (not (member (car part) '(:on-click :on-click-end)))
             do (apply #'apply-color cc (first part) (rest part)))
    (if resetp (reset-color-context cc))
    (values width height)))

(defun rendered-size (strings cc)
  "Return the width and height needed to render STRINGS"
  (loop for string in strings
        for (width line-height) = (multiple-value-list
                                   (rendered-string-size string cc nil))
        maximizing width into max-width
        summing line-height into height
        finally (progn
                  (reset-color-context cc)
                  (return (values max-width height)))))

(defun render-string (string-or-parts cc x y &aux (draw-x x))
  "Renders STRING-OR-PARTS to the pixmap in CC. Returns the height and width of
the rendered line as two values. The returned width is the value of X plus the
rendered width."
  (let* ((parts (if (stringp string-or-parts)
                    (parse-color-string string-or-parts)
                    string-or-parts))
         (height (max-font-height parts cc)))
    (loop
       for (part . rest) on parts
       for font-height-difference = (- height
                                       (font-height (ccontext-font cc)))
       for y-to-center = (floor (/ font-height-difference 2))
       if (stringp part)
       do (draw-image-glyphs
           (ccontext-px cc)
           (ccontext-gc cc)
           (ccontext-font cc)
           draw-x (+ y y-to-center (font-ascent (ccontext-font cc)))
           part
           :translate #'translate-id
           :size 16)
         (incf draw-x (text-line-width (ccontext-font cc)
                                       part
                                       :translate #'translate-id))
       else
       do (if (eq :> (first part))
              (progn (render-string rest cc
                                    (- (xlib:drawable-width (ccontext-px cc))
                                       x
                                       (rendered-string-size rest cc))
                                    y)
                     (loop-finish))
              (apply #'apply-color cc (first part) (rest part))))
    (values height draw-x)))

(defun render-strings (cc padx pady strings highlights)
  (let* ((gc (ccontext-gc cc))
         (xwin (ccontext-win cc))
         (px (ccontext-px cc))
         (strings (mapcar (lambda (string)
                            (if (stringp string)
                                (parse-color-string string)
                                string))
                          strings))
         (y 0))
    ;; Create a new pixmap if there isn't one or if it doesn't match the
    ;; window
    (when (or (not px)
              (/= (xlib:drawable-width px) (xlib:drawable-width xwin))
              (/= (xlib:drawable-height px) (xlib:drawable-height xwin)))
      (if px (xlib:free-pixmap px))
      (setf px (xlib:create-pixmap :drawable xwin
                                   :width (xlib:drawable-width xwin)
                                   :height (xlib:drawable-height xwin)
                                   :depth (xlib:drawable-depth xwin))
            (ccontext-px cc) px))
    ;; Clear the background
    (xlib:with-gcontext (gc :foreground (xlib:gcontext-background gc))
      (xlib:draw-rectangle px gc 0 0
                           (xlib:drawable-width px)
                           (xlib:drawable-height px) t))
    (loop for parts in strings
       for row from 0 to (length strings)
       for line-height = (max-font-height parts cc)
       if (find row highlights :test 'eql)
       do (xlib:draw-rectangle px gc 0 (+ pady y) (xlib:drawable-width px) line-height t)
         (xlib:with-gcontext (gc :foreground (xlib:gcontext-background gc)
                                 :background (xlib:gcontext-foreground gc))
           ;; If we don't switch the default colors, a color operation
           ;; resetting either color to its default value would undo the
           ;; switch.
           (rotatef (ccontext-default-fg cc) (ccontext-default-bg cc))
           (render-string parts cc (+ padx 0) (+ pady y))
           (rotatef (ccontext-default-fg cc) (ccontext-default-bg cc)))
       else
       do (render-string parts cc (+ padx 0) (+ pady y))
       end
       do (incf y line-height))
    (xlib:copy-area px gc 0 0
                    (xlib:drawable-width px)
                    (xlib:drawable-height px) xwin 0 0)
    (reset-color-context cc)
    (values)))


;;; Mode line rendering to allow on-click events to make sense.

;; The idea here is to let every mode line formatter register a function with an
;; ID. These functions must accept at least one argument, the button code for
;; the press. Any other arguments will be read (but not evaluated) from the
;; string when rendering the mode line. This is done by the color formatters
;; :on-click and :on-click-end. The former takes an id and an arbitrary number
;; of arguments, while the latter takes nothing. The text enclosed may contain
;; any formatter aside from the right alignment formatter (:>). 

#|
"^(:on-click :ml-on-click-focus-window 1034924)arbitrary text^(:on-click-end)"
|#

;; When rendering the above string, the bounds of "arbitrary text" are
;; calculated and saved in the mode line being rendered. Then when clicked, the
;; position of the click is checked against the saved bounds, and if one is
;; found its id is looked up and its function is dispatched with the button code
;; and the arguments provided. Before rendering the bounds registered in the
;; mode line are cleared.

;; Functions are registered to ids by the function #'REGISTER-ML-ON-CLICK-ID. An
;; ID can only have one function associated with it. 

(defvar *mode-line-on-click-functions* nil)

(defun register-ml-on-click-id (id fn)
  (let ((present (assoc id *mode-line-on-click-functions*)))
    (if present
        (setf (cdr present) fn)
        (push (cons id fn) *mode-line-on-click-functions*))))

(defun register-ml-boundaries-with-id (ml xbeg xend id arg)
  ;; TODO: check if these bounds already exist.
  (push (list xbeg xend id arg) (mode-line-on-click-bounds ml)))

(defun check-for-ml-press (ml code x y)
  "A function to hang on the mode line click hook which dispatches the
appropriate mode line click function."
  (declare (ignore y))
  ;; TODO: Should Y be ignored? currently only the x axis is checked, but if the
  ;; user has a multi line modeline this wont function correctly... 
  (let ((registered-ids *mode-line-on-click-functions*)
        (bounds-list (mode-line-on-click-bounds ml)))
    (dformat 3 "In mode line click: x=~A~&~2Tregistered ids: ~S~&~2Tbounds: ~S~&"
             x registered-ids bounds-list)
    (loop for (beg end id arg) in bounds-list
          do (when (< beg x end)
               (let ((fn (assoc id registered-ids)))
                 (when fn
                   (dformat 3 "Mode line click, calling ~A" (cdr fn))
                   (apply (cdr fn) code arg)
                   (loop-finish)))))))

(add-hook *mode-line-click-hook* 'check-for-ml-press)

;; Here is an example of a mode line click-to-focus-window function. It takes a
;; window id and focuses it. 

(defun ml-on-click-focus-window (code id &rest rest)
  (declare (ignore code rest))
  (when-let ((window (window-by-id id)))
    (focus-all window)))

(register-ml-on-click-id :ml-on-click-focus-window #'ml-on-click-focus-window)

;; This formatter wraps every window in an :on-click formatter which calls the
;; above function with the appropriate window id. 
(add-screen-mode-line-formatter #\i 'fmt-head-window-list-clickable)
(defun fmt-head-window-list-clickable (ml)
  "Using *window-format*, return a 1 line list of the windows, space seperated."
  (format nil "~{~a~^ ~}"
          (mapcar
           (lambda (w)
             (format nil
                     "^(:on-click :ml-on-click-focus-window ~A)~A^(:on-click-end)"
                     (window-id w)
                     (let ((str (format-expand *window-formatters*
                                               *window-format*
                                               w)))
                       (if (eq w (current-window))
                           (fmt-highlight str)
                           str))))
           (sort1 (head-windows (mode-line-current-group ml) (mode-line-head ml))
                  #'< :key #'window-number))))


;;;; Click-enabled mode line 

;; The implementation of click-enabled mode line depends on defining mode line
;; specific render-string(s) functions which check for the :on-click/end
;; formatters. 

(defun render-ml-string (string-or-parts cc ml x y &aux (draw-x x))
  "Renders STRING-OR-PARTS to the pixmap in CC. Returns the height and width of
the rendered line as two values. The returned width is the value of X plus the
rendered width."
  (let* ((parts (if (stringp string-or-parts)
                    (parse-color-string string-or-parts)
                    string-or-parts))
         (height (max-font-height parts cc))
         (current-on-click-beg nil)
         (current-on-click-id nil)
         (current-on-click-args nil))
    (labels ((actually-draw (thing x y)
               (draw-image-glyphs (ccontext-px cc)
                                  (ccontext-gc cc)
                                  (ccontext-font cc)
                                  x y
                                  thing
                                  :translate #'translate-id
                                  :size 16))
             (draw-string (thing x y)
               (actually-draw thing x y)
               (incf draw-x (text-line-width (ccontext-font cc)
                                             thing
                                             :translate #'translate-id))))
      (loop
        for (part . rest) on parts
        for font-height-difference = (- height
                                        (font-height (ccontext-font cc)))
        for y-to-center = (floor (/ font-height-difference 2))
        do (cond ((stringp part)
                  (draw-string part
                               draw-x
                               (+ y
                                  y-to-center
                                  (font-ascent (ccontext-font cc)))))
                 ((eql (car part) :on-click)
                  (setf current-on-click-beg  draw-x
                        current-on-click-id   (cadr part)
                        current-on-click-args (cddr part)))
                 ((eql (car part) :on-click-end)
                  (register-ml-boundaries-with-id ml
                                                  current-on-click-beg
                                                  draw-x
                                                  current-on-click-id
                                                  current-on-click-args)
                  (setf current-on-click-beg  nil
                        current-on-click-id   nil
                        current-on-click-args nil))
                 ((eql (car part) :>)
                  (render-ml-string rest cc ml
                                    (- (xlib:drawable-width (ccontext-px cc))
                                       x
                                       (rendered-string-size rest cc))
                                    y)
                  (loop-finish))
                 (t
                  (apply #'apply-color cc (first part) (rest part))))))
    (values height draw-x)))

(defun render-ml-strings (ml cc padx pady strings highlights)
  ;; cribbed from render-strings, theres parts here that arent needed.
  (let* ((gc (ccontext-gc cc))
         (xwin (ccontext-win cc))
         (px (ccontext-px cc))
         (strings (mapcar (lambda (string)
                            (if (stringp string)
                                (parse-color-string string)
                                string))
                          strings))
         (y 0))
    ;; Create a new pixmap if there isn't one or if it doesn't match the
    ;; window
    (when (or (not px)
              (/= (xlib:drawable-width px) (xlib:drawable-width xwin))
              (/= (xlib:drawable-height px) (xlib:drawable-height xwin)))
      (if px (xlib:free-pixmap px))
      (setf px (xlib:create-pixmap :drawable xwin
                                   :width (xlib:drawable-width xwin)
                                   :height (xlib:drawable-height xwin)
                                   :depth (xlib:drawable-depth xwin))
            (ccontext-px cc) px))
    ;; Clear the background
    (xlib:with-gcontext (gc :foreground (xlib:gcontext-background gc))
      (xlib:draw-rectangle px gc 0 0
                           (xlib:drawable-width px)
                           (xlib:drawable-height px) t))
    (loop for parts in strings
          for row from 0 to (length strings)
          for line-height = (max-font-height parts cc)
          if (find row highlights :test 'eql)
            do (xlib:draw-rectangle px gc 0 (+ pady y) (xlib:drawable-width px) line-height t)
               (xlib:with-gcontext (gc :foreground (xlib:gcontext-background gc)
                                       :background (xlib:gcontext-foreground gc))
                 ;; If we don't switch the default colors, a color operation
                 ;; resetting either color to its default value would undo the
                 ;; switch.
                 (rotatef (ccontext-default-fg cc) (ccontext-default-bg cc))
                 (render-ml-string parts cc ml (+ padx 0) (+ pady y))
                 (rotatef (ccontext-default-fg cc) (ccontext-default-bg cc)))
          else
            do (render-ml-string parts cc ml (+ padx 0) (+ pady y))
          end
          do (incf y line-height))
    (xlib:copy-area px gc 0 0
                    (xlib:drawable-width px)
                    (xlib:drawable-height px) xwin 0 0)
    (reset-color-context cc)
    (values)))
