(cl:in-package :bodge-ui)

(declaim (special *window*
                  *row-height*))


;;;
;;;
;;;
(defclass window (disposable basic-panel)
  ((x :initarg :x :initform 0.0)
   (y :initarg :y :initform 0.0)
   (width :initform nil)
   (height :initform nil)
   (background-style-item :initform nil)
   (title :initarg :title :initform "")
   (hidden-p :initform nil :reader hiddenp)
   (option-mask :initarg :option-mask :initform '())
   (style :initform (make-default-style))
   (layout :initform (make-instance 'vertical-layout))
   (bounds :initform (claw:alloc '(:struct (%nk:rect))))
   (panel-spacing :initform (make-instance '%vec2))
   (redefined-p :initform nil)))


(defgeneric on-window-close (window)
  (:method ((this window)) (declare (ignore this))))


(defun hide-window (window)
  (with-slots (hidden-p) window
    (unless hidden-p
      (setf hidden-p t))))


(defun show-window (window)
  (with-slots (hidden-p) window
    (when hidden-p
      (setf hidden-p nil))))


(defun setup-window (window &key
                              (width 0)
                              (height 0)
                              (origin (vec2 0 0))
                              (title "") (background-color nil)
                              (hidden nil)
                     &allow-other-keys)
  (with-slots ((w width) (h height) (this-x x) (this-y y) background-style-item
               option-mask (this-title title))
      window
    (setf w (float width 0f0)
          h (float height 0f0)
          this-x (x origin)
          this-y (y origin)
          this-title title)
    (when background-color
      (setf background-style-item (make-instance 'color-style-item :color background-color)))
    (when hidden
      (hide-window window))))


(define-destructor window (bounds panel-spacing)
  (claw:free bounds)
  (dispose panel-spacing))


(defmethod initialize-instance :after ((this window) &key &allow-other-keys)
  (reinitialize-window this))


(defmethod children-of ((this window))
  (with-slots (layout) this
    (children-of layout)))


(defmethod adopt ((this window) child)
  (with-slots (layout) this
    (adopt layout child)))


(defmethod abandon ((this window) child)
  (with-slots (layout) this
    (abandon layout child)))


(defmethod abandon-all ((this window))
  (with-slots (layout) this
    (abandon-all layout)))


(defun add-window (window-class &rest initargs &key (ui *context*) &allow-other-keys)
  (with-ui (ui)
    (%add-window ui (apply #'make-instance window-class initargs))))


(defun remove-window (window &key (ui *context*))
  (%remove-window ui window))


(defun remove-all-windows (&optional (ui *context*))
  (%remove-all-windows ui))


(defun find-element (name &optional (window *window*))
  (labels ((%find-element (root name)
             (if (equal (name-of root) name)
                 root
                 (loop for child in (children-of root)
                    thereis (%find-element child name)))))
    (%find-element window name)))


(defun compose-window (win)
  (with-slots (x y width height title option-mask layout bounds panel-spacing) win
    (claw:c-val ((bounds (:struct (%nk:rect))))
      (setf (bounds :x) (float x 0f0)
            (bounds :y) (float (- (renderer-canvas-height (%renderer-of *context*)) y height) 0f0)
            (bounds :w) (float width 0f0)
            (bounds :h) (float height 0f0))
      (let ((val (%nk:begin-titled *handle* (%panel-id-of win) title bounds option-mask)))
        (unwind-protect
             (unless (= 0 val)
               (setf (%x panel-spacing) (style :layout-spacing)
                     (%y panel-spacing) (style :layout-spacing))
               (with-styles ((:window :spacing) panel-spacing
                             (:window :padding) *zero-vec2*
                             (:window :group-padding) *zero-vec2*)
                 (multiple-value-bind (width height) (calc-bounds layout)
                   (declare (ignore width))
                   (%nk:layout-row-dynamic *handle* (default-row-height height) 1)
                   (compose layout))))
          (%nk:end *handle*))))))


(defmethod compose ((this window))
  (with-slots (background-style-item hidden-p redefined-p style)
      this
    (unless hidden-p
      (when redefined-p
        (reinitialize-window this)
        (setf redefined-p nil))
      (with-styles ((:window :fixed-background) background-style-item)
        (let* ((*window* this)
               (*style* style)
               (*row-height* (style :row-height)))
          (compose-window this)))
      (when (or (/= %nk:+false+ (%nk:window-is-hidden *handle* (%panel-id-of this)))
                (/= %nk:+false+ (%nk:window-is-closed *handle* (%panel-id-of this))))
        (setf hidden-p t)
        (on-window-close this)))))


(defmethod update-instance-for-redefined-class :after ((this window)
                                                       added-slots
                                                       discarded-slots
                                                       property-list
                                                       &rest initargs)
  (declare (ignore added-slots discarded-slots property-list initargs))
  (with-slots (redefined-p) this
    (setf redefined-p t)))


(defgeneric reinitialize-window (window)
  (:method (window) (declare (ignore window))))


(defun update-window-options (window &rest opts)
  (with-slots (option-mask) window
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


(defmacro defwindow (name-and-opts &body layout)
  (flet ((filter-window-initargs (opts)
           (loop with special-keywords = '(:inherit :options)
                 for (key . value) in opts
                 unless (member key special-keywords)
                   append (case key
                            (:origin (list key `(vec2 ,(or (first value) 0) ,(or (second value) 0))))
                            (t (list key (first value)))))))
    (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
      (with-gensyms (layout-parent)
        (let ((initargs (filter-window-initargs opts)))
          `(progn
             (defclass ,name (window ,@(assoc-value opts :inherit)) ()
               (:default-initargs ,@initargs))
             (defmethod reinitialize-window ((,layout-parent ,name))
               (setup-window ,layout-parent ,@initargs)
               (update-window-options ,layout-parent ,@(assoc-value opts :options))
               (abandon-all ,layout-parent)
               ,(when layout
                  `(layout (,layout-parent) ,@layout)))
             (make-instances-obsolete ',name)))))))
