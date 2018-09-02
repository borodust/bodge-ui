(cl:in-package :bodge-ui)

;;;
;;; NOTEBOOK
;;;
(defgeneric label-of (element)
  (:method (element) (declare (ignore element))))


(defclass tab (vertical-layout)
  ((label :initform "" :initarg :label :reader label-of)))


(defclass notebook (%layout)
  ((root :initform (make-instance 'vertical-layout))
   (tabbar :initform (make-instance 'horizontal-layout))
   (tab-index :initform 0)
   (tabs :initform (make-array 0 :adjustable t :fill-pointer 0))))


(defmethod initialize-instance :after ((this notebook) &key)
  (with-slots (root tabbar) this
    (adopt root tabbar)))


(defun select-tab (notebook new-tab-index)
  (with-slots (tab-index tabs root) notebook
    (when (or (< new-tab-index 0) (>= new-tab-index (length tabs)))
      (error "Tab index out of bounds"))
    (abandon root (aref tabs tab-index))
    (adopt root (aref tabs new-tab-index))
    (setf tab-index new-tab-index)))


(defmethod adopt ((this notebook) tab)
  (with-slots (tabs tabbar) this
    (adopt tabbar (make-instance 'button :label (label-of tab)))
    (vector-push-extend tab tabs)
    (when (= (length tabs) 1)
      (select-tab this 0)))
  (call-next-method))


(defmethod abandon ((this notebook) child)
  (with-slots (tabs tabbar tab-index) this
    (when-let ((child-index (position child tabs)))
      (deletef tabbar (nth child-index (children-of tabbar)))
      (deletef tabs child)
      (if (> (length tabs) 0)
          (select-tab this (min tab-index (1- (length tabs))))
          (setf tab-index 0))))
  (call-next-method))


(defmethod abandon-all ((this notebook))
  (with-slots (tabbar tab-index tabs) this
    (abandon-all tabbar)
    (setf (fill-pointer tabs) 0
          tab-index 0))
  (call-next-method))


(defmethod calc-bounds ((this notebook))
  (with-slots (root) this
    (calc-bounds root)))


(defmethod compose ((this notebook))
  (with-slots (root) this
    (compose root)))
