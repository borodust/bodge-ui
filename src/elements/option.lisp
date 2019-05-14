(cl:in-package :bodge-ui)

;;;
;;;
;;;
(defclass option (widget)
  ((label :initarg :label :initform "")
   (enabled-p :initarg :enabled-p :initform nil)
   (click-listener :initarg :on-click :initform nil)))


(defmethod compose ((this option))
  (with-slots (enabled-p click-listener label) this
    (let ((return-value (%nk:option-label *handle* label (if enabled-p 1 0))))
      (unless (or (= return-value %nk:+false+) (null click-listener))
        (funcall click-listener *panel*)))))
