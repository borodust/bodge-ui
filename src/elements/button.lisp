(cl:in-package :bodge-ui)

;;;
;;;
;;;
(defclass button (widget)
  ((label :initarg :label :initform "")
   (click-listener :initarg :on-click :initform nil)))


(defmethod compose ((this button))
  (with-slots (label click-listener) this
    (unless (or (= (%nk:button-label *handle* label) 0) (null click-listener))
      (funcall click-listener *panel**))))
