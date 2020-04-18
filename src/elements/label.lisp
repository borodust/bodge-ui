(cl:in-package :bodge-ui)

;;;
;;;
;;;
(defclass label (widget)
  ((text :initarg :text :initform "" :accessor text-of)
   (align :initarg :align)))


(defmethod initialize-instance :after ((this label) &key (align :left))
  (with-slots ((this-align align)) this
    (setf this-align align)))


(defmethod compose ((this label))
  (with-slots (text align) this
    (let ((text (if (functionp text)
                    (format nil "~A" (funcall text))
                    text)))
      (%nk:label *handle*
                 (or text "")
                 (cffi:foreign-enum-value '%nuklear:text-alignment align)))))
