(cl:in-package :bodge-ui)

;;;
;;;
;;;
(defclass color-picker (widget disposable)
  ((color)))


(defmethod initialize-instance :after ((this color-picker) &key (color (vec4 1 1 1 1)))
  (with-slots ((this-color color)) this
    (setf this-color (claw:c-let ((color-f (:struct (%nk:colorf))))
                       (setf (color-f :r) (x color)
                             (color-f :g) (y color)
                             (color-f :b) (z color)
                             (color-f :a) (w color))
                       color-f))))


(define-destructor color-picker (color)
  (claw:free color))


(defmethod compose ((this color-picker))
  (with-slots (color) this
    (%nk:color-picker color *handle* color %nk:+rgba+)))
