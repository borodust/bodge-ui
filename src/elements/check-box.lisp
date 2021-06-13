(cl:in-package :bodge-ui)

;;;
;;;
;;;
(defclass check-box (widget)
  ((label :initarg :label :initform "")
   (checked-p :initarg :enabled-p :initform nil :accessor checked)
   (click-listener :initarg :on-click :initform nil)))


(defmethod compose ((this check-box))
  (with-slots ((this-checked-p checked-p) click-listener label) this
    (let ((checked-p (/= %nuklear:+false+
                         (%nuklear:check-label *handle* label
                                          (if this-checked-p %nuklear:+true+ %nuklear:+false+)))))
      (unless (eq this-checked-p checked-p)
        (setf this-checked-p checked-p)
        (when click-listener
          (funcall click-listener *panel*))))))
