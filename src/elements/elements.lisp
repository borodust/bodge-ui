(cl:in-package :bodge-ui)

(defvar *radio-group* nil)

(defvar *zero-vec2* (make-instance '%vec2 :x 0f0 :y 0f0))
(defvar *one-vec2* (make-instance '%vec2 :x 1f0 :y 1f0))
(defvar *nk-buttons* (list %nk:+button-left+ :left
                           %nk:+button-right+ :right
                           %nk:+button-middle+ :middle))


(defgeneric compose (element))
(defgeneric text-of (element))
(defgeneric (setf text-of) (value element))


(defclass named ()
  ((name :initarg :name :initform nil :reader name-of)))

;;;
;;; LAYOUT
;;;
(defmacro layout ((parent-layout) &body elements)
  `(parent-tree (,parent-layout) ,@elements))


(defun max-reducer (property-supplier)
  (lambda (value element)
    (if (not value)
        (funcall property-supplier element)
        (when-let ((element-value (funcall property-supplier element)))
          (max value element-value)))))


(defgeneric calc-bounds (element)
  (:method (element) (declare (ignore element)) (values nil nil)))


(defgeneric expand-ratio-of (element)
  (:method (element) (declare (ignore element))))


(defgeneric expandablep (element)
  (:method (element) (declare (ignore element)) t))


(defclass expandable ()
  ((expand-ratio :initform nil :initarg :expand-ratio :reader expand-ratio-of)
   (expandable-p :initform t :initarg :expandable :reader expandablep)))


(defclass %layout (named parent) ())


(defmethod compose ((this %layout))
  (dochildren (element this)
    (compose element)))


(defun make-container-layout ()
  (make-instance '%layout))


(defmacro with-style (((&rest path) value) &body body)
  (with-gensyms (ctx)
    `(claw:c-let ((,ctx (:struct (%nk:context)) :from (%handle-of *context*)))
       (push-style *context* (,ctx :style ,@path &) ,value)
       (unwind-protect
            (progn ,@body)
         (pop-style *context*)))))


(defmacro with-styles ((&rest styles) &body body)
  (labels ((expand-next (rest-styles)
             (if-let ((path (first rest-styles))
                                 (value (second rest-styles)))
               `(with-style ((,@path) ,value)
                  ,(expand-next (cddr rest-styles)))
               `(progn ,@body))))
    (expand-next styles)))


;;;
;;; PANEL
;;;
(defclass basic-panel ()
  ((panel-id :initform (%next-panel-id) :reader %panel-id-of)))


(defclass panel (basic-panel)
  ())


(defgeneric compose-panel (element))


(defmethod compose ((this panel))
  (let ((begin-result (%nk:group-begin-titled *handle*
                                              (%panel-id-of this)
                                              ""
                                              (nk:panel-mask :no-scrollbar))))
    (unless (= begin-result 0)
      (unwind-protect
           (compose-panel this)
        (%nk:group-end *handle*)))))



;;;
;;; WIDGET
;;;
(defgeneric hide-widget (widget))
(defgeneric show-widget (widget))


(defclass widget (named expandable)
  ((hidden :initform nil :reader hiddenp)
   (width :initform nil :initarg :width :reader width-of)
   (height :initform nil :initarg :height :reader height-of)))


(defmethod hide-widget ((this widget))
  (with-slots (hidden) this
    (setf hidden t)))


(defmethod show-widget ((this widget))
  (with-slots (hidden) this
    (setf hidden nil)))


(defmethod calc-bounds ((this widget))
  (values (width-of this) (height-of this)))


(defmethod compose :around ((this widget))
  (unless (hiddenp this)
    (call-next-method)))


(defmethod children-of ((this widget))
  (declare (ignore this))
  nil)


;;;
;;; BEHAVIOR ELEMENT
;;;
(defclass behavior-element ()
  ((delegate :initarg :delegate :initform (error ":delegate missing"))))


(defmethod expand-ratio-of ((this behavior-element))
  (with-slots (delegate) this
    (expand-ratio-of delegate)))


(defmethod expandablep ((this behavior-element))
  (with-slots (delegate) this
    (expandablep delegate)))


(defmethod calc-bounds ((this behavior-element))
  (with-slots (delegate) this
    (calc-bounds delegate)))


(defmethod children-of ((this behavior-element))
  (with-slots (delegate) this
    (children-of delegate)))


(defmethod adopt ((this behavior-element) child)
  (with-slots (delegate) this
    (adopt delegate child)))


(defmethod abandon ((this behavior-element) child)
  (with-slots (delegate) this
    (abandon delegate child)))


(defmethod abandon-all ((this behavior-element))
  (with-slots (delegate) this
    (abandon-all delegate)))


(defmethod compose ((this behavior-element))
  (with-slots (delegate) this
    (compose delegate)))
