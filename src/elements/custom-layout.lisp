(cl:in-package :bodge-ui)


;;;
;;; CUSTOM LAYOUT
;;;
(defgeneric initialize-custom-layout (layout)
  (:method (layout) (declare (ignore layout))))


(defclass custom-layout (vertical-layout)
  ((redefined-p :initform nil)))


(defmethod initialize-instance :after ((this custom-layout) &key)
  (initialize-custom-layout this))


(defmethod compose ((this custom-layout))
  (with-slots (redefined-p) this
    (when redefined-p
      (abandon-all this)
      (initialize-custom-layout this)
      (setf redefined-p nil)))
  (call-next-method))


(defmethod update-instance-for-redefined-class :after ((this custom-layout)
                                                       added-slots
                                                       discarded-slots
                                                       property-list
                                                       &rest initargs)
  (declare (ignore added-slots discarded-slots property-list initargs))
  (with-slots (redefined-p) this
    (setf redefined-p t)))


(defmacro deflayout (name-and-opts &body layout)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (with-gensyms (this)
      `(progn
         (defclass ,name (custom-layout ,@(assoc-value opts :inherit)) ())
         (defmethod initialize-custom-layout ((,this ,name))
           (layout (,this)
             ,@layout))
         (make-instances-obsolete ',name)))))
