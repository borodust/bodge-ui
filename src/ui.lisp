(cl:in-package :bodge-ui)


(declaim (special *context*
                  *handle*))


(defgeneric calculate-text-width (font string))

(defgeneric text-line-height (font))

(defgeneric font-handle (font))

(defgeneric renderer-default-font (renderer))

(defgeneric renderer-canvas-width (renderer))

(defgeneric renderer-canvas-height (renderer))

(defgeneric render-ui (renderer))

(defun invert-y (y &optional (height 0))
  (- (renderer-canvas-height (%renderer-of *context*)) y height))

(nuklear:define-text-width-callback calc-string-width (handle height string)
  (calculate-text-width (renderer-default-font (%renderer-of *context*)) string))


(defun make-nuklear-font (font)
  (nuklear:make-user-font (text-line-height font) 'calc-string-width))


(defun destroy-nuklear-font (nk-font)
  (nuklear:destroy-user-font nk-font))


(defclass custom-font (disposable)
  ((handle :reader font-handle)))


(defmethod initialize-instance :after ((this custom-font) &key)
  (with-slots (handle) this
    (setf handle (make-nuklear-font this))))


(define-destructor custom-font (handle)
  (destroy-nuklear-font handle))

;;;
;;;
;;;
(defclass nuklear-context (disposable)
  ((handle :initarg :handle :initform (error ":handle missing") :reader %handle-of)
   (renderer :initarg :renderer :reader %renderer-of)
   (compose-tasks :initform (mt:make-guarded-reference (list)))
   (input-source :initform nil :initarg :input-source :reader %input-source-of)
   (last-cursor-position :initform (vec2) :reader %last-cursor-position-of)
   (last-scroll :initform (vec2) :reader %last-scroll-of)
   (nuklear-font :initarg :nuklear-font)
   (last-panel-id :initform 0)
   (last-custom-widget-id :initform 0)
   (panels :initform nil :accessor %panels-of)
   (style :initform (make-style) :reader %style-of)
   (custom-widget-table :initform (make-hash-table))))


(define-destructor nuklear-context (handle)
  (nuklear:destroy-context handle))


(defmethod initialize-instance ((this nuklear-context) &rest keys &key renderer)
  (let* ((nk-font (font-handle (renderer-default-font renderer))))
    (apply #'call-next-method this
           :handle (nuklear:make-context nk-font)
           :renderer renderer
           :nuklear-font nk-font
           keys)))


(defun make-ui (renderer &key input-source)
  (unless renderer
    (error "Rrenderer must be provided"))
  (make-instance 'nuklear-context
                 :input-source input-source
                 :renderer renderer))


(defun %add-panel (ui panel)
  (push panel (%panels-of ui))
  panel)


(defun %remove-panel (ui panel)
  (alexandria:deletef (%panels-of ui) panel)
  (%nk:window-close (%handle-of ui) (%pane-id-of panel)))


(defun %remove-all-panels (ui)
  (let ((ui-handle (%handle-of ui)))
    (dolist (panel (%panels-of ui))
      (%nk:window-close ui-handle (%pane-id-of panel))))
  (setf (%panels-of ui) nil))


(defun %next-pane-id ()
  (with-slots (last-panel-id) *context*
    (format nil "~A" (incf last-panel-id))))


(defun %next-custom-widget-id ()
  (with-slots (last-custom-widget-id) *context*
    (incf last-custom-widget-id)))


(defun context-custom-widget (id &optional (ui *context*))
  (with-slots (custom-widget-table) ui
    (gethash id custom-widget-table)))


(defun (setf context-custom-widget) (value id &optional (ui *context*))
  (with-slots (custom-widget-table) ui
    (setf (gethash id custom-widget-table) value)))


(defun find-custom-widget-from-command (command &optional (ui *context*))
  (c-val ((command (:struct %nk:command-custom)))
    (context-custom-widget (cffi:pointer-address (command :callback-data :ptr)) ui)))


(defun push-compose-task (ctx fn)
  (with-slots (compose-tasks) ctx
    (mt:with-guarded-reference (compose-tasks)
      (alexandria:nconcf compose-tasks (list fn)))))


(defmacro with-ui-access ((ctx) &body body)
  `(push-compose-task ,ctx (lambda () ,@body)))


(defmacro with-ui ((ctx) &body body)
  `(let ((*context* ,ctx)
         (*handle* (%handle-of ,ctx)))
     ,@body))


(defun drain-compose-task-queue (ctx)
  (with-slots (compose-tasks) ctx
    (with-ui (ctx)
      (let ((tasks (mt:with-guarded-reference (compose-tasks)
                     (prog1 compose-tasks
                       (setf compose-tasks (list))))))
        (loop for task in tasks
              do (funcall task))))))


(defmacro with-ui-input ((ui) &body body)
  `(with-ui (,ui)
     (prog2
         (%nk:input-begin *handle*)
         (progn ,@body)
       (%nk:input-end *handle*))))


(defun clear-ui (&optional (ui *context*))
  (with-slots (custom-widget-table) ui
    (clrhash custom-widget-table))
  (%nk:clear (%handle-of ui)))


(defun register-cursor-position (x y)
  (%nk:input-motion *handle* (floor x)
                    (floor (- (renderer-canvas-height (%renderer-of *context*)) y))))


(defun register-character-input (character)
  (%nk:input-unicode *handle* (char-code character)))


(defun register-scroll-input (x y)
  (c-with ((vec (:struct %nk:vec2)))
    (setf (vec :x) (float x 0f0)
          (vec :y) (- (float y 0f0)))
    (%nk:input-scroll *handle* (vec &))))


(defun button-state->nk (state)
  (ecase state
    (:pressed %nk:+true+)
    (:released %nk:+false+)))


(defvar *nk-key-map*
  (alexandria:plist-hash-table
   (list :left-shift :shift
         :right-shift :shift
         :left-control :ctrl
         :right-control :ctrl
         :delete :del
         :enter :enter
         :tab :tab
         :backspace :backspace
         :up :up
         :down :down
         :left :left
         :right :right)))


(defun key->nk (key)
  (gethash key *nk-key-map* :none))


(defun register-keyboard-input (key state)
  (if (eq state :repeating)
      (progn
        (register-keyboard-input key :released)
        (register-keyboard-input key :pressed))
      (%nk:input-key *handle* (key->nk key) (button-state->nk state))))


(defun register-mouse-input (x y button state)
  (let ((nk-state (button-state->nk state)))
    (%nk:input-button *handle* button
                      (floor x) (floor (- (renderer-canvas-height (%renderer-of *context*)) y))
                      nk-state)))

;;;
;;;
;;;
(defclass %vec2 (disposable)
  ((handle :reader %handle-of)))


(define-destructor %vec2 (handle)
  (cffi:foreign-free handle))


(defmethod initialize-instance :after ((this %vec2) &key (x 0f0) (y 0f0))
  (with-slots (handle) this
    (c-let ((vec (:struct %nk:vec2) :alloc t))
      (setf (vec :x) (float x 0f0)
            (vec :y) (float y 0f0)
            handle (vec &)))))


(defmacro with-vec2-accessor ((value accessor %vec2) &body body)
  (alexandria:with-gensyms (vec)
    `(c-let ((,vec (:struct %nk:vec2) :from (%handle-of ,%vec2)))
       (symbol-macrolet ((,value (,vec ,accessor)))
         ,@body))))


(defun %x (vec2)
  (with-vec2-accessor (val :x vec2)
    val))


(defun (setf %x) (value vec2)
  (with-vec2-accessor (val :x vec2)
    (setf val (float value 0f0))))


(defun %y (vec2)
  (with-vec2-accessor (val :y vec2)
    val))


(defun (setf %y) (value vec2)
  (with-vec2-accessor (val :y vec2)
    (setf val (float value 0f0))))
