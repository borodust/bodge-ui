(cl:in-package :bodge-ui)




;;;
;;; CUSTOM WIDGET
;;;
(defgeneric render-custom-widget (widget origin width height)
  (:method (widget origin width height) (declare (ignore widget origin width height))))

(defgeneric update-widget (widget)
  (:method (widget) (declare (ignore widget))))

(defgeneric custom-widget-width (widget)
  (:method (widget) (declare (ignore widget))))

(defgeneric custom-widget-height (widget)
  (:method (widget) (declare (ignore widget))))

(defclass custom-widget (disposable widget)
  ((id :initform (%next-custom-widget-id) :reader %id-of)
   (hovering-listener :initarg :on-hover :initform nil)
   (leaving-listener :initarg :on-leave :initform nil)
   (clicking-listener :initarg :on-click :initform nil)
   (pressing-listener :initarg :on-mouse-press :initform nil)
   (releasing-listener :initarg :on-mouse-release :initform nil)
   (bounds)
   (clicked-buttons :initform nil)
   (pressed-buttons :initform nil)
   (hovered-p :initform nil)))


(defmethod initialize-instance :after ((this custom-widget) &key)
  (with-slots (bounds) this
    (setf bounds (claw:calloc '(:struct (%nk:rect))))))


(define-destructor custom-widget (bounds)
  (claw:free bounds))


(defmethod calc-bounds ((this custom-widget))
  (values (custom-widget-width this) (custom-widget-height this)))


(defmethod update-widget ((this custom-widget))
  (with-slots ((this-clicked-buttons clicked-buttons)
               (this-pressed-buttons pressed-buttons)
               (this-hovered-p hovered-p)
               hovering-listener
               leaving-listener
               clicking-listener
               pressing-listener
               releasing-listener
               bounds)
      this
    (claw:c-let ((ctx (:struct (%nk:context)) :from *handle*))
      (flet ((widget-hovered-p ()
               (= %nk:+true+ (%nk:input-is-mouse-hovering-rect (ctx :input) bounds)))
             (widget-clicked-p (button)
               (= %nk:+true+ (%nk:input-is-mouse-click-in-rect (ctx :input) button bounds)))
             (widget-pressed-p (button)
               (= %nk:+true+ (%nk:input-has-mouse-click-down-in-rect (ctx :input) button bounds
                                                                     %nk:+true+))))
        (let ((hovered-p (widget-hovered-p))
              (clicked-buttons (loop for (nk-key key) on *nk-buttons* by #'cddr
                                     when (widget-clicked-p nk-key)
                                       collect key))
              (pressed-buttons (loop for (nk-key key) on *nk-buttons* by #'cddr
                                     when (widget-pressed-p nk-key)
                                       collect key)))
          (when (and hovering-listener (not this-hovered-p) hovered-p)
            (funcall hovering-listener *window*))
          (when (and leaving-listener this-hovered-p (not hovered-p))
            (funcall leaving-listener *window*))
          (when clicking-listener
            (when-let ((new-clicked-buttons (set-difference clicked-buttons
                                                            this-clicked-buttons)))
              (loop for button in new-clicked-buttons
                    do (funcall clicking-listener *window* :button button :allow-other-keys t))))
          (when pressing-listener
            (when-let ((new-pressed-buttons (set-difference pressed-buttons
                                                            this-pressed-buttons)))
              (loop for button in new-pressed-buttons
                    do (funcall pressing-listener *window* :button button :allow-other-keys t))))
          (when releasing-listener
            (when-let ((released-buttons (set-difference this-pressed-buttons
                                                         pressed-buttons)))
              (loop for button in released-buttons
                    do (funcall releasing-listener *window* :button button :allow-other-keys t))))
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
    (%nk:widget bounds *handle*)
    (update-widget this)
    (setf (context-custom-widget (%id-of this)) this)
    (claw:c-let ((ctx (:struct (%nk:context)) :from *handle*))
      (%nk:push-custom (ctx :current :buffer)
                       bounds nil (cffi:make-pointer (%id-of this))))))
