(cl:in-package :bodge-ui)

;;;
;;;
;;;
(defun text-align->nk (align)
  (ecase align
    (:left %nk:+text-left+)
    (:right %nk:+text-right+)
    ((or :center :middle) %nk:+text-centered+)))


(defclass label (widget)
  ((text :initarg :text :initform "")
   (align :initarg :align)))


(defmethod initialize-instance :after ((this label) &key (align :left))
  (with-slots ((this-align align)) this
    (setf this-align (text-align->nk align))))


(defmethod compose ((this label))
  (with-slots (text align) this
    (let ((text (if (functionp text)
                    (format nil "~A" (funcall text))
                    text)))
      (%nk:label *handle* text align))))
