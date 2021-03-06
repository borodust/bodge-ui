(cl:in-package :bodge-ui)


(declaim (special *active-custom-widget*))
;;;
;;; CUSTOM WIDGET
;;;
(defgeneric custom-widget-width (widget)
  (:method (widget) (declare (ignore widget))))

(defgeneric custom-widget-height (widget)
  (:method (widget) (declare (ignore widget))))

(defgeneric custom-widget-on-hover (widget)
  (:method (widget) (declare (ignore widget))))

(defgeneric custom-widget-on-leave (widget)
  (:method (widget) (declare (ignore widget))))

(defgeneric custom-widget-on-click (widget button)
  (:method (widget button) (declare (ignore widget button))))

(defgeneric custom-widget-on-mouse-press (widget button)
  (:method (widget button) (declare (ignore widget button))))

(defgeneric custom-widget-on-mouse-release (widget button)
  (:method (widget button) (declare (ignore widget button))))

(defgeneric custom-widget-on-move (widget x y)
  (:method (widget x y) (declare (ignore widget x y))))

(defgeneric discard-custom-widget-state (state)
  (:method (state) (declare (ignore state))))

(defclass custom-widget (disposable widget)
  ((id :initform (%next-custom-widget-id) :reader %id-of)
   (root-panel :initform nil :reader %root-panel-of)
   (hovering-listener :initarg :on-hover :initform nil)
   (leaving-listener :initarg :on-leave :initform nil)
   (clicking-listener :initarg :on-click :initform nil)
   (pressing-listener :initarg :on-mouse-press :initform nil)
   (releasing-listener :initarg :on-mouse-release :initform nil)
   (bounds)
   (clicked-buttons :initform nil)
   (pressed-buttons :initform nil)
   (hovered-p :initform nil)
   (state :initform nil)))


(defmethod initialize-instance :around ((this custom-widget) &key)
  (let ((*active-custom-widget* this))
    (call-next-method)))


(defmethod initialize-instance :after ((this custom-widget) &key)
  (with-slots (bounds state) this
    (setf bounds (cffi:foreign-alloc '(:struct %nuklear:rect))
          ;; self-reference here should be safe enough - SBCL correctly collects the instance
          ;; maybe test on other implementations
          state this)))


(define-destructor custom-widget (bounds)
  (cffi:foreign-free bounds))


(defmethod render-custom-widget :around ((this custom-widget) origin width height)
  (let ((*panel* (%root-panel-of this)))
    (call-next-method)))


(defun transition-custom-widget-to (widget state-class &rest initargs &key &allow-other-keys)
  (with-slots (state) widget
    (let ((*active-custom-widget* widget))
      (discard-custom-widget-state state)
      (setf state (if state-class
                      (apply #'make-instance state-class initargs)
                      widget)))))


(defun custom-widget-instance ()
  *active-custom-widget*)


(defmethod calc-bounds ((this custom-widget))
  (values (custom-widget-width this) (custom-widget-height this)))


(defun update-widget (this x y width height)
  (declare (ignore width))
  (with-slots ((this-clicked-buttons clicked-buttons)
               (this-pressed-buttons pressed-buttons)
               (this-hovered-p hovered-p)
               hovering-listener
               leaving-listener
               clicking-listener
               pressing-listener
               releasing-listener
               bounds
               state
               root-panel)
      this
    (setf root-panel *panel*)
    (c-let ((ctx (:struct %nuklear:context) :from *handle*))
      (flet ((widget-hovered-p ()
               (= %nuklear:+true+ (%nuklear:input-is-mouse-hovering-rect (ctx :input &) bounds)))
             (widget-clicked-p (button)
               (= %nuklear:+true+ (%nuklear:input-is-mouse-click-in-rect (ctx :input &) button bounds)))
             (widget-pressed-p (button)
               (= %nuklear:+true+ (%nuklear:input-has-mouse-click-down-in-rect (ctx :input &)
                                                                     button
                                                                     bounds
                                                                     %nuklear:+true+))))
        (let ((*active-custom-widget* this)
              (hovered-p (widget-hovered-p))
              (clicked-buttons (loop for key in *nk-buttons* by #'cddr
                                     when (widget-clicked-p key)
                                       collect key))
              (pressed-buttons (loop for key in *nk-buttons* by #'cddr
                                     when (widget-pressed-p key)
                                       collect key)))
          (when (and (not this-hovered-p) hovered-p)
            (custom-widget-on-hover state)
            (when hovering-listener
              (funcall hovering-listener *panel*)))
          (when (and this-hovered-p (not hovered-p))
            (custom-widget-on-leave state)
            (when leaving-listener
              (funcall leaving-listener *panel*)))
          (when-let ((new-clicked-buttons (set-difference clicked-buttons
                                                          this-clicked-buttons)))
            (loop for button in new-clicked-buttons
                  do (custom-widget-on-click state button)
                     (when clicking-listener
                       (funcall clicking-listener *panel* :button button :allow-other-keys t))))
          (when-let ((new-pressed-buttons (set-difference pressed-buttons
                                                          this-pressed-buttons)))
            (loop for button in new-pressed-buttons
                  do (custom-widget-on-mouse-press state button)
                     (when pressing-listener
                       (funcall pressing-listener *panel* :button button :allow-other-keys t))))
          (when-let ((released-buttons (set-difference this-pressed-buttons
                                                       pressed-buttons)))
            (loop for button in released-buttons
                  do (custom-widget-on-mouse-release state button)
                     (when releasing-listener
                       (funcall releasing-listener *panel* :button button :allow-other-keys t))))
          (let ((mouse-x (ctx :input :mouse :pos :x))
                (mouse-y (ctx :input :mouse :pos :y))
                (prev-x (ctx :input :mouse :prev :x))
                (prev-y (ctx :input :mouse :prev :y)))
            (when (and (or (/= prev-x mouse-x) (/= prev-y mouse-y)) hovered-p)
              (custom-widget-on-move state (- mouse-x x) (- height (- mouse-y y)))))
          (setf this-hovered-p hovered-p
                this-clicked-buttons clicked-buttons
                this-pressed-buttons pressed-buttons))))))


(defun custom-widget-hovered-p (widget)
  (with-slots (hovered-p) widget
    hovered-p))


(defun custom-widget-clicked-p (widget button)
  (with-slots (clicked-buttons) widget
    (member button clicked-buttons)))


(defun custom-widget-pressed-p (widget button)
  (with-slots (pressed-buttons) widget
    (member button pressed-buttons)))


(defmethod compose ((this custom-widget))
  (with-slots (bounds) this
    (%nuklear:widget bounds *handle*)
    (c-val ((bounds (:struct %nuklear:rect)))
      (update-widget this (bounds :x) (bounds :y) (bounds :w) (bounds :h)))
    (setf (context-custom-widget (%id-of this)) this)
    (c-let ((ctx (:struct %nuklear:context) :from *handle*))
      (c-with ((id %nuklear:handle))
        (setf (id :id) (%id-of this))
        (%nuklear:push-custom (ctx :current * :buffer &) bounds nil (id &))))))
