(cl:in-package :bodge-ui)


(declaim (special *active-custom-widget*))
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

(defgeneric discard-custom-widget-state (state)
  (:method (state) (declare (ignore state))))

(defclass custom-widget (disposable widget)
  ((id :initform (%next-custom-widget-id) :reader %id-of)
   (root-window :initform nil :reader %root-window-of)
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
    (setf bounds (claw:calloc '(:struct (%nk:rect)))
          ;; self-reference here should be safe enough - SBCL correctly collects the instance
          ;; maybe test on other implementations
          state this)))


(define-destructor custom-widget (bounds)
  (claw:free bounds))


(defmethod render-custom-widget :around ((this custom-widget) origin width height)
  (let ((*window* (%root-window-of this)))
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
               root-window)
      this
    (setf root-window *window*)
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
        (let ((*active-custom-widget* this)
              (hovered-p (widget-hovered-p))
              (clicked-buttons (loop for (nk-key key) on *nk-buttons* by #'cddr
                                     when (widget-clicked-p nk-key)
                                       collect key))
              (pressed-buttons (loop for (nk-key key) on *nk-buttons* by #'cddr
                                     when (widget-pressed-p nk-key)
                                       collect key)))
          (when (and (not this-hovered-p) hovered-p)
            (custom-widget-on-hover state)
            (when hovering-listener
              (funcall hovering-listener *window*)))
          (when (and this-hovered-p (not hovered-p))
            (custom-widget-on-leave state)
            (when leaving-listener
              (funcall leaving-listener *window*)))
          (when-let ((new-clicked-buttons (set-difference clicked-buttons
                                                          this-clicked-buttons)))
            (loop for button in new-clicked-buttons
                  do (custom-widget-on-click state button)
                     (when clicking-listener
                       (funcall clicking-listener *window* :button button :allow-other-keys t))))
          (when-let ((new-pressed-buttons (set-difference pressed-buttons
                                                          this-pressed-buttons)))
            (loop for button in new-pressed-buttons
                  do (custom-widget-on-mouse-press state button)
                     (when pressing-listener
                       (funcall pressing-listener *window* :button button :allow-other-keys t))))
          (when-let ((released-buttons (set-difference this-pressed-buttons
                                                       pressed-buttons)))
            (loop for button in released-buttons
                  do (custom-widget-on-mouse-release state button)
                     (when releasing-listener
                       (funcall releasing-listener *window* :button button :allow-other-keys t))))
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
    (%nk:widget bounds *handle*)
    (claw:c-val ((bounds (:struct (%nk:rect))))
      (update-widget this (bounds :x) (bounds :y) (bounds :w) (bounds :h)))
    (setf (context-custom-widget (%id-of this)) this)
    (claw:c-let ((ctx (:struct (%nk:context)) :from *handle*))
      (%nk:push-custom (ctx :current :buffer)
                       bounds nil (cffi:make-pointer (%id-of this))))))
