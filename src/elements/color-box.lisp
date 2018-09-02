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
    (claw:c-with ((size (:struct (%nk:vec2))))
      (setf (size :x) (%nk:widget-width *handle*)
            (size :y) (float height 0f0))
      (flet ((combo-begin ()
               (cond
                 (this-color
                  (claw:c-with ((color (:struct (%nk:colorf))))
                    (setf (color :r) (x this-color)
                          (color :g) (y this-color)
                          (color :b) (z this-color)
                          (color :a) (w this-color))
                    (%nk:combo-begin-color *handle* (%nk:rgb-cf color color) size)))
                 (t (%nk:combo-begin-label *handle* label size)))))
        (unless (= (combo-begin) 0)
          (unwind-protect
               (call-next-method)
            (%nk:combo-end *handle*)))))))
