(cl:in-package :bodge-ui)

;;
(defgeneric item-status (item))
(defgeneric item-name-of (item))
(defgeneric item-selected-p (item))
(defgeneric select-item (item status))


(defclass list-select-text-item (disposable)
  ((text :initarg :text :reader item-name-of)
   (status-buf :initform (cffi:foreign-alloc :int) :reader item-status)))


(define-destructor list-select-text-item (status-buf)
  (cffi:foreign-free status-buf))


(defmethod item-selected-p ((this list-select-text-item))
  (/= 0 (c-ref (item-status this) :int)))


(defmethod select-item ((this list-select-text-item) status)
  (setf (c-ref (item-status this) :int) (if status 1 0)))


(defgeneric add-item (object item))
(defgeneric clear (object))

(defclass list-select (widget)
  ((items :initform nil)
   (item-height :initarg :item-height)))


(defun make-list-select (item-height &key name)
  (make-instance 'list-select :item-height item-height :name name))


(defmethod add-item ((this list-select) (text string))
  (with-slots (items) this
    (nconcf items (list (make-instance 'list-select-text-item :text text)))))


(defmethod clear ((this list-select))
  (with-slots (items) this
    (setf items nil)))


(defmethod compose ((this list-select))
  (with-slots (items item-height status-buf) this
    (%nuklear:layout-row-dynamic *handle* (float item-height) 1)
    (dolist (item items)
      (unless (= 0 (%nuklear:selectable-label *handle*
                                         (item-name-of item)
                                         (cffi:foreign-bitfield-value '%nuklear:text-align
                                                                      :left)
                                         (item-status item)))
        ;; todo: invoke listeners
        (dolist (other-item items)
          (unless (eq item other-item)
            (select-item other-item nil)))))))
