(cl:in-package :bodge-ui)

;;;
;;; COMBO BOX
;;;
(defclass combo-box (disposable %layout widget)
  ((selected :initform 0)
   (count :initform 1)
   (drop-height :initform nil :initarg :drop-height)
   (drop-width :initform nil :initarg :drop-width)
   (values :initform (list "") :initarg :values)
   (foreign-array :initform nil)))


(define-destructor combo-box (foreign-array count)
  (c-let ((array-ptr :pointer :from foreign-array))
    (loop for i from 0 below count
          do (cffi:foreign-string-free (array-ptr i)))
    (cffi:foreign-free array-ptr)))


(defmethod initialize-instance :after ((this combo-box) &key)
  (with-slots (values foreign-array count drop-height drop-width) this
    (c-let ((array-ptr :pointer :alloc t :count (length values)))
      (loop with max-width = 0
            for value in values
            for i from 0
            do (setf (array-ptr i) (cffi:foreign-string-alloc value)
                     max-width (max max-width (calculate-text-width (%renderer-of *context*) value)))
            finally (progn
                      (setf count (1+ i)
                            foreign-array (array-ptr &))
                      (unless drop-width
                        (setf drop-width (+ (float max-width 0f0) 24))))))))


(defmethod compose ((this combo-box))
  (with-slots (foreign-array count selected drop-width drop-height) this
    (c-with ((size (:struct %nk:vec2)))
      (setf (size :x) drop-width
            (size :y) (float (or drop-height (* count (+ *row-height* 8))) 0f0))
      (let ((new-value
              (%nk:combo *handle*
                         foreign-array
                         count
                         selected
                         (floor *row-height*)
                         (size &))))
        (unless (= new-value selected)
          (setf selected new-value))))))
