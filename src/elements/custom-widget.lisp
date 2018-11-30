(cl:in-package :bodge-ui)

;;;
;;; CUSTOM WIDGET
;;;
(defgeneric render-custom-widget (widget origin width height)
  (:method (widget origin width height) (declare (ignore widget origin width height))))

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


(defun update-widget (this x y width height)
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
               (= %nk:+true+ (%nk:input-has-mouse-click-down-in-rect (ctx :input)
                                                                     button
                                                                     bounds
                                                                     %nk:+true+))))
        (let ((hovered-p (widget-hovered-p))
              (clicked-buttons (loop for (nk-key key) on *nk-buttons* by #'cddr
                                     when (widget-clicked-p nk-key)
                                       collect key))
              (pressed-buttons (loop for (nk-key key) on *nk-buttons* by #'cddr
                                     when (widget-pressed-p nk-key)
                                       collect key)))
          (when (and (not this-hovered-p) hovered-p)
            (custom-widget-on-hover this)
            (when hovering-listener
              (funcall hovering-listener *window*)))
          (when (and this-hovered-p (not hovered-p))
            (custom-widget-on-leave this)
            (when leaving-listener
              (funcall leaving-listener *window*)))
          (when-let ((new-clicked-buttons (set-difference clicked-buttons
                                                          this-clicked-buttons)))
            (loop for button in new-clicked-buttons
                  do (custom-widget-on-click this button)
                     (when clicking-listener
                       (funcall clicking-listener *window* :button button :allow-other-keys t))))
          (when-let ((new-pressed-buttons (set-difference pressed-buttons
                                                          this-pressed-buttons)))
            (loop for button in new-pressed-buttons
                  do (custom-widget-on-mouse-press this button)
                     (when pressing-listener
                       (funcall pressing-listener *window* :button button :allow-other-keys t))))
          (when-let ((released-buttons (set-difference this-pressed-buttons
                                                       pressed-buttons)))
            (loop for button in released-buttons
                  do (custom-widget-on-mouse-release this button)
                     (when releasing-listener
                       (funcall releasing-listener *window* :button button :allow-other-keys t))))
          (let ((mouse-x (ctx :input :mouse :pos :x))
                (mouse-y (ctx :input :mouse :pos :y))
                (prev-x (ctx :input :mouse :prev :x))
                (prev-y (ctx :input :mouse :prev :y)))
            (when (and (or (/= prev-x mouse-x) (/= prev-y mouse-y)) hovered-p)
              (custom-widget-on-move this (- mouse-x x) (- height (- mouse-y y)))))
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
    (claw:c-val ((bounds (:struct (%nk:rect))))
      (update-widget this (bounds :x) (bounds :y) (bounds :w) (bounds :h)))
    (setf (context-custom-widget (%id-of this)) this)
    (claw:c-let ((ctx (:struct (%nk:context)) :from *handle*))
      (%nk:push-custom (ctx :current :buffer)
                       bounds nil (cffi:make-pointer (%id-of this))))))
