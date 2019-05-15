(cl:in-package :bodge-ui)

;;;
;;; SCROLL AREA
;;;
(defclass scroll-area (disposable basic-pane widget)
  ((nk-scroll :initform (claw:calloc '(:struct (%nk:scroll))))
   (layout :initform (make-instance 'vertical-layout))))


(define-destructor scroll-area (nk-scroll)
  (claw:free nk-scroll))


(defmethod children-of ((this scroll-area))
  (with-slots (layout) this
    (children-of layout)))


(defmethod adopt ((this scroll-area) child)
  (with-slots (layout) this
    (adopt layout child)))


(defmethod abandon ((this scroll-area) child)
  (with-slots (layout) this
    (abandon layout child)))


(defmethod abandon-all ((this scroll-area))
  (with-slots (layout) this
    (abandon-all layout)))


(defmethod calc-bounds ((this scroll-area))
  (with-slots (layout) this
    (multiple-value-bind (width height) (calc-bounds layout)
      (let ((width (if-let ((this-width (width-of this)))
                     this-width
                     width))
            (height (if-let ((this-height (height-of this)))
                      this-height
                      height)))
        (values width height)))))


(defun update-area-scroll-position (pane x y)
  (with-slots (nk-scroll) pane
    (claw:c-let ((scroll (:struct (%nk:scroll)) :from nk-scroll))
      (setf (scroll :x) (round x)
            (scroll :y) (round y)))
    (values)))


(defun %area-scroll-position (pane)
  (with-slots (nk-scroll) pane
    (claw:c-let ((scroll (:struct (%nk:scroll)) :from nk-scroll))
      (values (scroll :x) (scroll :y)))))


(defmacro with-area-scroll-position ((x y) pane &body body)
  `(multiple-value-bind (,x ,y) (%area-scroll-position ,pane)
     (declare (ignorable ,x ,y))
     ,@body))


(defun area-scroll-position (pane &optional (result (vec2)))
  (multiple-value-bind (x y) (%area-scroll-position pane)
    (setf (x result) x
          (y result) y)
    result))


(defmethod compose ((this scroll-area))
  (with-slots (nk-scroll layout) this
    (let ((begin-result (%nk:group-scrolled-begin *handle*
                                                  nk-scroll
                                                  (%pane-id-of this)
                                                  0)))
      (unless (= begin-result 0)
        (unwind-protect
             (multiple-value-bind (width height) (calc-bounds layout)
               (let ((height (default-row-height height)))
                 (if width
                     (%nk:layout-row-static *handle* height (round width) 1)
                     (%nk:layout-row-dynamic *handle* height 1)))
               (compose layout))
          (%nk:group-scrolled-end *handle*))))))
