
(in-package :stumpwm)

(defgeneric font-exists-p (font))

(defgeneric open-font (display font))

(defgeneric close-font (font))

(defgeneric font-ascent (font))

(defgeneric font-descent (font))

(defgeneric font-height (font))

(defgeneric text-lines-height (font string))


(defgeneric text-line-width (font text &rest keys &key start end translate))


(defgeneric draw-image-glyphs (drawable gcontext
                               font
                               x y
                               sequence &rest keys &key start end translate width size))

;;;; X11 fonts
(defmethod font-exists-p ((font-name string))
  ;; if we can list the font then it exists
  (xlib:list-font-names *display* font-name :max-fonts 1))

(defmethod open-font ((display xlib:display) (font-name string))
  (xlib:open-font display (first (xlib:list-font-names display font-name :max-fonts 1))))

(defmethod close-font ((font xlib:font))
  (xlib:close-font font))

(defmethod font-ascent ((font xlib:font))
  (xlib:font-ascent font))

(defmethod font-descent ((font xlib:font))
  (xlib:font-descent font))

(defmethod font-height ((font xlib:font))
  (+ (font-ascent font)
     (font-descent font)))

(defmethod text-line-width ((font xlib:font) text &rest keys &key (start 0) end translate)
  (declare (ignorable start end translate))
  (apply 'xlib:text-width font text keys))

(defmethod draw-image-glyphs (drawable
                              gcontext
                              (font xlib:font)
                              x y
                              sequence &rest keys &key (start 0) end translate width size)
  (declare (ignorable start end translate width size))
  (setf (xlib:gcontext-font gcontext) font)
  (apply 'xlib:draw-image-glyphs drawable
         gcontext
         x y
         sequence keys))

(defmethod font-height ((fonts cons))
  (loop for font in fonts
        maximizing (font-height font)))
