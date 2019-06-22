(cl:in-package :bodge-ui)

(declaim (special *panel*
                  *row-height*))


;;;
;;;
;;;
(defclass panel (disposable named basic-pane)
  ((x :initarg :x :initform 0.0)
   (y :initarg :y :initform 0.0)
   (width :initform nil)
   (height :initform nil)
   (max-width :initform nil :initarg :max-width)
   (max-height :initform nil :initarg :max-height)
   (min-width :initform nil :initarg :min-width)
   (min-height :initform nil :initarg :min-height)
   (title :initarg :title :initform "")
   (hidden-p :initform nil :reader hiddenp)
   (collapsed-p :initform nil :reader minimizedp)
   (option-mask :initarg :option-mask :initform '())
   (style :initform nil)
   (layout :initform (make-instance 'vertical-layout))
   (bounds :initform (claw:calloc '(:struct (%nk:rect))))
   (redefined-p :initform nil)
   (bounds-updated-p :initform nil)))


(defgeneric on-close (element)
  (:method ((this panel)) (declare (ignore this))))


(defgeneric on-minimize (element)
  (:method ((this panel)) (declare (ignore this))))


(defgeneric on-restore (element)
  (:method ((this panel)) (declare (ignore this))))


(defgeneric on-move (element)
  (:method ((this panel)) (declare (ignore this))))


(defun update-panel-position (panel x y)
  (with-slots ((this-x x) (this-y y) bounds-updated-p) panel
    (setf this-x x
          this-y y
          bounds-updated-p t)))


(defun update-panel-size (panel width height)
  (with-slots ((this-width width) (this-height height) bounds-updated-p) panel
    (setf this-width width
          this-height height
          bounds-updated-p t)))


(defun %panel-position (panel)
  (with-slots (x y) panel
    (values x y)))


(defmacro with-panel-position ((x y) panel &body body)
  `(multiple-value-bind (,x ,y) (%panel-position ,panel)
     (declare (ignorable ,x ,y))
     ,@body))


(defun panel-position (panel &optional (result-vec2 (vec2)))
  (with-panel-position (x y) panel
    (setf (x result-vec2) x
          (y result-vec2) y)
    result-vec2))


(defun %panel-dimensions (panel)
  (with-slots (width height) panel
    (values width height)))


(defmacro with-panel-dimensions ((width height) panel &body body)
  `(multiple-value-bind (,width ,height) (%panel-dimensions ,panel)
     (declare (ignorable ,width ,height))
     ,@body))


(defun panel-size (panel &optional (result-vec2 (vec2)))
  (with-panel-dimensions (width height) panel
    (setf (x result-vec2) width
          (y result-vec2) height)
    result-vec2))


(defun hide-panel (panel)
  (with-slots (hidden-p) panel
    (unless hidden-p
      (setf hidden-p t))))


(defun show-panel (panel)
  (with-slots (hidden-p) panel
    (when hidden-p
      (setf hidden-p nil))))


(defun minimize-panel (panel)
  (with-ui-access (*context*)
    (%nk:window-collapse *handle* (%pane-id-of panel) %nk:+minimized+)))


(defun restore-panel (panel)
  (with-ui-access (*context*)
    (%nk:window-collapse *handle* (%pane-id-of panel) %nk:+maximized+)))


(defun setup-panel (panel &key
                            (width 0)
                            (height 0)
                            (origin (vec2 0 0))
                            (title "") (background-color nil)
                            (hidden nil)
                            max-height
                            max-width
                            min-height
                            min-width
                            style
                    &allow-other-keys)
  (with-slots ((w width) (h height) (this-x x) (this-y y)
               option-mask (this-title title)
               (this-max-height max-height) (this-max-width max-width)
               (this-min-height min-height) (this-min-width min-width)
               (this-style style))
      panel
    (setf w (float width 0f0)
          h (float height 0f0)
          this-style (apply #'make-style
                            (append style
                                    (list :layout-spacing 4
                                          :row-height 26
                                          :panel-spacing (vec2 4 0)
                                          :panel-padding (vec2 4 0)
                                          :group-padding *zero-vec2*)
                                    (when background-color
                                      (list :panel-fixed-background
                                            (make-color-style-item background-color)))))
          this-x (x origin)
          this-y (y origin)
          this-title title
          this-max-height max-height
          this-max-width max-width
          this-min-height min-height
          this-min-width min-width)
    (when hidden
      (hide-panel panel))))


(define-destructor panel (bounds)
  (claw:free bounds))


(defmethod initialize-instance :after ((this panel) &key &allow-other-keys)
  (reinitialize-panel this))


(defmethod children-of ((this panel))
  (with-slots (layout) this
    (children-of layout)))


(defmethod adopt ((this panel) child)
  (with-slots (layout) this
    (adopt layout child)))


(defmethod abandon ((this panel) child)
  (with-slots (layout) this
    (abandon layout child)))


(defmethod abandon-all ((this panel))
  (with-slots (layout) this
    (abandon-all layout)))


(defun add-panel (ui panel-class &rest initargs &key &allow-other-keys)
  (with-ui (ui)
    (%add-panel ui (apply #'make-instance panel-class initargs))))


(defun remove-panel (ui panel)
  (%remove-panel ui panel))


(defun remove-all-panels (ui)
  (%remove-all-panels ui))


(defun find-element (name &optional (panel *panel*))
  (labels ((%find-element (root name)
             (if (equal (name-of root) name)
                 root
                 (loop for child in (children-of root)
                    thereis (%find-element child name)))))
    (%find-element panel name)))


(defun %ensure-panel-dimensions (win)
  (with-slots (width height
               max-width max-height
               min-width min-height
               bounds-updated-p)
      win
    (let ((min-width (or min-width 0))
          (min-height (or min-height 0))
          (max-width (or max-width width))
          (max-height (or max-height height)))
      (unless (and (<= min-width width max-width )
                   (<= min-height height max-height))
        (setf width (alexandria:clamp width min-width max-width)
              height (alexandria:clamp height min-height max-height)
              bounds-updated-p t)))))


(defun compose-panel (win)
  (with-slots (x y width height title option-mask layout bounds bounds-updated-p) win
    (%ensure-panel-dimensions win)
    (claw:c-val ((bounds (:struct (%nk:rect))))
      (setf (bounds :x) (float x 0f0)
            (bounds :y) (float (invert-y y height) 0f0)
            (bounds :w) (float width 0f0)
            (bounds :h) (float height 0f0))
      (when bounds-updated-p
        (%nk:window-set-bounds *handle* (%pane-id-of win) bounds)
        (setf bounds-updated-p nil))
      (let ((val (%nk:begin-titled *handle* (%pane-id-of win) title bounds option-mask)))
        (unwind-protect
             (unless (= 0 val)
               (%nk:window-get-bounds bounds *handle*)
               (let ((prev-x x)
                     (prev-y y))
                 (setf width (bounds :w)
                       height (bounds :h)
                       x (bounds :x)
                       y (invert-y (bounds :y) height))
                 (when (or (/= x prev-x)
                           (/= y prev-y))
                   (on-move win)))
               (multiple-value-bind (width height) (calc-bounds layout)
                 (declare (ignore width))
                 (%nk:layout-row-dynamic *handle* (default-row-height height) 1)
                 (compose layout)))
          (%nk:end *handle*))))))


(defmethod compose :around ((this panel))
  (with-slots (style) this
    (with-style (style)
      (call-next-method))))


(defmethod compose ((this panel))
  (with-slots (hidden-p redefined-p style collapsed-p bounds-updated-p) this
    (unless hidden-p
      (when redefined-p
        (reinitialize-panel this)
        (setf redefined-p nil
              bounds-updated-p t))
      (let* ((*panel* this)
             (*row-height* (style :row-height)))
        (compose-panel this))
      (when (or (/= %nk:+false+ (%nk:window-is-hidden *handle* (%pane-id-of this)))
                (/= %nk:+false+ (%nk:window-is-closed *handle* (%pane-id-of this))))
        (setf hidden-p t)
        (on-close this))
      (unless (eq (/= %nk:+false+
                      (%nk:window-is-collapsed *handle* (%pane-id-of this)))
                  collapsed-p)
        (setf collapsed-p (not collapsed-p))
        (if collapsed-p
            (on-minimize this)
            (on-restore this))))))


(defun root-panel ()
  *panel*)


(defmethod update-instance-for-redefined-class :after ((this panel)
                                                       added-slots
                                                       discarded-slots
                                                       property-list
                                                       &rest initargs)
  (declare (ignore added-slots discarded-slots property-list initargs))
  (with-slots (redefined-p) this
    (setf redefined-p t)))


(defgeneric reinitialize-panel (panel)
  (:method (panel) (declare (ignore panel))))


(defun update-panel-options (panel &rest opts)
  (with-slots (option-mask) panel
    (flet ((to-nuklear-opts (opts)
             (let ((updated-opts (list :title :no-scrollbar :border)))
               (loop for opt in opts
                     do (case opt
                          (:resizable (push :scalable updated-opts))
                          (:headerless (deletef updated-opts :title))
                          (:borderless (deletef updated-opts :border))
                          (:closable (push :closable updated-opts))
                          (:minimizable (push :minimizable updated-opts))
                          (:movable (push :movable updated-opts))
                          (:backgrounded (push :background updated-opts))
                          (:scrollable (deletef updated-opts :no-scrollbar))))
               updated-opts)))
      (setf option-mask (apply #'nk:panel-mask (to-nuklear-opts opts))))))


(defmacro defpanel (name-and-opts &body layout)
  (flet ((filter-panel-initargs (opts)
           (loop with special-keywords = '(:inherit :options)
                 for (key . value) in opts
                 unless (member key special-keywords)
                   append (case key
                            (:origin (list key `(vec2 ,(or (first value) 0)
                                                      ,(or (second value) 0))))
                            (:background-color
                             (list key `(vec4 ,(or (first value) 0)
                                              ,(or (second value) 0)
                                              ,(or (third value) 0)
                                              ,(or (fourth value) 1))))
                            (:style (list key `(list ,@value)))
                            (t (list key (first value)))))))
    (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
      (with-gensyms (layout-parent)
        (let ((initargs (filter-panel-initargs opts)))
          `(progn
             (defclass ,name (panel ,@(assoc-value opts :inherit)) ()
               (:default-initargs ,@initargs))
             (defmethod reinitialize-panel ((,layout-parent ,name))
               (setup-panel ,layout-parent ,@initargs)
               (update-panel-options ,layout-parent ,@(assoc-value opts :options))
               (abandon-all ,layout-parent)
               ,(when layout
                  `(layout (,layout-parent) ,@layout)))
             (make-instances-obsolete ',name)))))))
