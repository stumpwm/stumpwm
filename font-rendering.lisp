
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
(defmethod font-exists-p ((font string))
  ;; if we can list the font then it exists
  (plusp (length (xlib:list-font-names *display* font :max-fonts 1))))

(defmethod open-font ((display xlib:display) (font string))
  (xlib:open-font display (first (xlib:list-font-names display font :max-fonts 1))))

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
  (apply 'xlib:text-width font text keys)
  )

(defmethod draw-image-glyphs (drawable 
                              gcontext
                              (font xlib:font)
                              x y
                              sequence &rest keys &key (start 0) end translate width size) 
  (apply 'xlib:draw-image-glyphs drawable 
         gcontext
         x y
         sequence keys))

;;;; TTF fonts
(defmethod font-exists-p ((font xft:font))
  ;; if we can list the font then it exists
  t)

(defmethod open-font ((display xlib:display) (font xft:font))
  font)

(defmethod close-font ((font xft:font))
  )

(defmethod font-ascent ((font xft:font))
  (xft:font-ascent (screen-number (current-screen)) font))

(defmethod font-descent ((font xft:font))
  (xft:font-descent (screen-number (current-screen)) font))

(defmethod font-height ((font xft:font))
  (+ (font-ascent font)
     (- (font-descent font))))

(defmethod text-line-width ((font xft:font) text &rest keys &key (start 0) end translate)
  (declare (ignorable start end translate))
  (apply 'xft:text-line-width (screen-number (current-screen)) font text
         :allow-other-keys t keys))

(defmethod draw-image-glyphs (drawable 
                              gcontext
                              (font xft:font)
                              x y
                              sequence &rest keys &key (start 0) end translate width size)
  (declare (ignorable start end translate width size))
  (apply 'xft:draw-text-line 
         drawable
         gcontext
         font
         sequence
         x y
         :draw-background-p t
         :allow-other-keys t
         keys))
