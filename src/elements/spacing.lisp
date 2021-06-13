(cl:in-package :bodge-ui)

;;;
;;;
;;;
(defclass spacing (widget)
  ((columns :initform 1 :initarg :columns)))


(defmethod compose ((this spacing))
  (with-slots (columns) this
    (%nuklear:spacing *handle* (floor columns))))
