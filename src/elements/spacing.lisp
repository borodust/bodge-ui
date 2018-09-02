(cl:in-package :bodge-ui)

;;;
;;;
;;;
(defclass spacing (widget)
  ((columns :initform 1 :initarg :columns)))


(defmethod compose ((this spacing))
  (with-slots (columns) this
    (%nk:spacing *handle* (floor columns))))
