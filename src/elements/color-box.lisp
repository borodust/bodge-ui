(cl:in-package :bodge-ui)

;;;
;;; COLOR BOX
;;;
(defclass color-box (%layout widget)
  ((label :initarg :label :initform "")
   (color :initarg :color :initform nil)
   (height :initarg :height :initform 400f0)))


(defmethod compose ((this color-box))
  (with-slots (height (this-color color) label) this
    (c-with ((size (:struct %nuklear:vec2)))
      (setf (size :x) (%nuklear:widget-width *handle*)
            (size :y) (float height 0f0))
      (flet ((combo-begin ()
               (cond
                 (this-color
                  (c-with ((color (:struct %nuklear:colorf)))
                    (setf (color :r) (x this-color)
                          (color :g) (y this-color)
                          (color :b) (z this-color)
                          (color :a) (w this-color))
                    (%nuklear:combo-begin-color *handle* (%nuklear:rgb-cf color color) size)))
                 (t (%nuklear:combo-begin-label *handle* label size)))))
        (unless (= (combo-begin) 0)
          (unwind-protect
               (call-next-method)
            (%nuklear:combo-end *handle*)))))))
