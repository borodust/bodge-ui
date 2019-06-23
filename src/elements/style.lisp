(cl:in-package :bodge-ui)


(defclass styled-group (behavior-element)
  ((style :initform nil))
  (:default-initargs :delegate (make-instance 'vertical-layout)))


(defmethod initialize-instance :after ((this styled-group) &rest args
                                       &key &allow-other-keys)
  (with-slots (style) this
    (setf style (apply #'make-style (loop for (name value) on args by #'cddr
                                          if (eq :style name)
                                            append value
                                          else
                                            append (unless (member name '(:name :delegate))
                                                       (list name value)))))))


(defmethod compose :around ((this styled-group))
  (with-slots (style) this
    (with-style (style)
      (call-next-method))))
