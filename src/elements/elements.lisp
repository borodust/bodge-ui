(cl:in-package :bodge-ui)

(defvar *radio-group* nil)

(defvar *zero-vec2* (vec2 0 0))
(defvar *one-vec2* (vec2 1 1))

(declaim (special *panel*
                  *row-height*))


(defparameter *nk-buttons* (list :left :right :middle))

(defgeneric text-of (element))
(defgeneric (setf text-of) (value element))


(defclass named ()
  ((name :initarg :name :initform nil :reader name-of)))


;;;
;;; STYLED
;;;
(defclass styled ()
  ((style :initform nil)))


(defmethod initialize-instance :after ((this styled) &key style)
  (with-slots ((this-style style)) this
    (setf this-style (apply #'make-style style))))


(defmethod compose :around ((this styled))
  (with-slots (style) this
    (if style
        (with-style (style)
          (call-next-method))
        (call-next-method))))

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


(defclass %layout (named styled parent) ())


(defmethod compose ((this %layout))
  (dochildren (element this)
    (compose element)))


(defun make-container-layout ()
  (make-instance '%layout))


;;;
;;; BASIC PANE
;;;
(defclass basic-pane ()
  ((pane-id :initform (%next-pane-id) :reader %pane-id-of)))

;;;
;;; PANE
;;;
(defclass pane (basic-pane) ())

(defgeneric compose-pane (element))


(defmethod compose ((this pane))
  (let ((begin-result (%nuklear:group-begin-titled *handle*
                                              (%pane-id-of this)
                                              ""
                                              (nk:panel-mask :no-scrollbar))))
    (unless (= begin-result 0)
      (unwind-protect
           (compose-pane this)
        (%nuklear:group-end *handle*)))))

;;;
;;; WIDGET
;;;
(defgeneric hide-widget (widget))
(defgeneric show-widget (widget))


(defclass widget (named styled expandable)
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
(defclass behavior-element (named)
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


(defun default-row-height (&optional child-height)
  (float (or child-height (style :row-height)) 0f0))


(defun calc-vertical-bounds (this)
  (let (width
        height
        (spacing (style :layout-spacing)))
    (dochildren (child this)
      (multiple-value-bind (child-width child-height) (calc-bounds child)
        (when child-width
          (setf width (+ (if width
                             (max width child-width)
                             child-width)
                         spacing)))
        (let ((child-height (default-row-height child-height)))
          (setf height (+ (if height
                              (+ height child-height)
                              child-height)
                          spacing)))))
    (values width height)))
