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
   (style-stack :initform nil)
   (last-cursor-position :initform (vec2) :reader %last-cursor-position-of)
   (last-scroll :initform (vec2) :reader %last-scroll-of)
   (nuklear-font :initarg :nuklear-font)
   (last-window-id :initform 0)
   (last-custom-widget-id :initform 0)
   (windows :initform nil :accessor %windows-of)
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


(defun %add-panel (ui window)
  (push window (%windows-of ui))
  window)


(defun %remove-panel (ui window)
  (alexandria:deletef (%windows-of ui) window)
  (%nk:window-close (%handle-of ui) (%panel-id-of window)))


(defun %remove-all-panels (ui)
  (let ((ui-handle (%handle-of ui)))
    (dolist (window (%windows-of ui))
      (%nk:window-close ui-handle (%panel-id-of window))))
  (setf (%windows-of ui) nil))


(defun %next-panel-id ()
  (with-slots (last-window-id) *context*
    (format nil "~A" (incf last-window-id))))


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
  (claw:c-val ((command (:struct (%nk:command-custom))))
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
  (claw:c-with ((vec (:struct (%nk:vec2))))
    (setf (vec :x) (float x 0f0)
          (vec :y) (float y 0f0))
    (%nk:input-scroll *handle* vec)))


(defun button-state->nk (state)
  (ecase state
    (:pressed %nk:+true+)
    (:released %nk:+false+)))


(defvar *nk-key-map*
  (alexandria:plist-hash-table
   (list :left-shift %nk:+key-shift+
         :right-shift %nk:+key-shift+
         :left-control %nk:+key-ctrl+
         :right-control %nk:+key-ctrl+
         :delete %nk:+key-del+
         :enter %nk:+key-enter+
         :tab %nk:+key-tab+
         :backspace %nk:+key-backspace+
         :up %nk:+key-up+
         :down %nk:+key-down+
         :left %nk:+key-left+
         :right %nk:+key-right+)))


(defun key->nk (key)
  (gethash key *nk-key-map* %nk:+key-none+))


(defun register-keyboard-input (key state)
  (if (eq state :repeating)
      (progn
        (register-keyboard-input key :released)
        (register-keyboard-input key :pressed))
      (%nk:input-key *handle* (key->nk key) (button-state->nk state))))


(defun register-mouse-input (x y button state)
  (let ((nk-button (ecase button
                     (:left %nk:+button-left+)
                     (:middle %nk:+button-middle+)
                     (:right %nk:+button-right+)))
        (nk-state (button-state->nk state)))
    (%nk:input-button *handle* nk-button
                      (floor x) (floor (- (renderer-canvas-height (%renderer-of *context*)) y))
                      nk-state)))

;;;
;;;
;;;
(defclass style-item (disposable)
  ((handle :initform (claw:alloc '(:struct (%nk:style-item))) :reader %handle-of)))


(define-destructor style-item (handle)
  (claw:free handle))


(defclass color-style-item (style-item) ())


(defun style-item-color (style-item r g b &optional (a 1f0))
  (claw:c-with ((color-v (:struct (%nk:color))))
    (%nk:style-item-color style-item
                          (%nk:rgba-f color-v
                                      (float r 0f0)
                                      (float g 0f0)
                                      (float b 0f0)
                                      (float a 0f0)))))


(defmethod initialize-instance :after ((this color-style-item)
                                       &key color)
  (style-item-color (%handle-of this) (x color) (y color) (z color) (w color)))


(defclass %vec2 (disposable)
  ((handle :reader %handle-of)))


(define-destructor %vec2 (handle)
  (claw:free handle))


(defmethod initialize-instance :after ((this %vec2) &key (x 0f0) (y 0f0))
  (with-slots (handle) this
    (claw:c-let ((vec (:struct (%nk:vec2))))
      (setf (vec :x) (float x 0f0)
            (vec :y) (float y 0f0)
            handle vec))))


(defmacro with-vec2-accessor ((value accessor %vec2) &body body)
  (alexandria:with-gensyms (vec)
    `(claw:c-with ((,vec (:struct (%nk:vec2)) :from (%handle-of ,%vec2)))
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

;;;
;;;
;;;
(defun push-style-popper (fu context)
  (with-slots (style-stack) context
    (push fu style-stack)))


(defun pop-style (context)
  (with-slots (style-stack) context
    (alexandria:when-let ((pop-fu (pop style-stack)))
      (funcall pop-fu (%handle-of context)))))


(defgeneric push-style (context destination source))

(defmethod push-style ((context nuklear-context) destination (vec %vec2))
  (%nk:style-push-vec2 (%handle-of context) destination (%handle-of vec))
  (push-style-popper #'%nk:style-pop-vec2 context))

(defmethod push-style ((context nuklear-context) destination (value single-float))
  (%nk:style-push-float (%handle-of context) destination value)
  (push-style-popper #'%nk:style-pop-float context))

(defmethod push-style ((context nuklear-context) destination (item style-item))
  (%nk:style-push-style-item (%handle-of context) destination (%handle-of item))
  (push-style-popper #'%nk:style-pop-style-item context))

(defun %nopper (handle)
  (declare (ignore handle)))

(defmethod push-style ((context nuklear-context) destination (item null))
  (push-style-popper #'%nopper context))
