(cl:in-package :bodge-ui)


(defclass float-property (disposable widget)
  ((label :initarg :label :initform "")
   (min :initarg :start :initform 0f0)
   (max :initarg :end :initform 1f0)
   (step :initarg :step :initform 0f0)
   (increment :initarg :increment :initform 0.005f0)
   (value :initarg :value :initform 0f0)))


(defmethod compose ((this float-property))
  (with-slots (value min max step label increment) this
    (setf value (%nk:propertyf *handle* label
                               (float min 0f0) (float value 0f0) (float max 0f0)
                               (float step 0f0) (float increment 0f0)))))
