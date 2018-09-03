(bodge-util:define-package :bodge-ui.renderer
    (:use :cl :bodge-ui)
  (:export #:make-nuklear-renderer
           #:destroy-nuklear-renderer))
(cl:in-package :bodge-ui.renderer)


(defclass nuklear-renderer-font ()
  ((handle :initarg :handle :reader %handle-of)))


(defclass nuklear-renderer ()
  ((handle :initarg :handle :reader %handle-of)
   (width :initarg :width :reader renderer-canvas-width)
   (height :initarg :height :reader renderer-canvas-height)
   (pixel-ratio :initarg :pixel-ratio)
   (font :reader renderer-default-font)))


(defmethod initialize-instance :after ((this nuklear-renderer) &key)
  (with-slots (font handle) this
    (setf font (make-instance 'nuklear-renderer-font :handle (nk:renderer-font handle)))))


(defun make-nuklear-renderer (width height &optional (pixel-ratio 1f0))
  (make-instance 'nuklear-renderer :handle (nuklear:make-renderer)
                                   :width width
                                   :height height
                                   :pixel-ratio (or pixel-ratio 1f0)))


(defun destroy-nuklear-renderer (renderer)
  (nuklear:destroy-renderer (%handle-of renderer)))


(defmethod calculate-text-width ((font nuklear-renderer-font) string)
  "Dummy method: we are using native nuklear font instead"
  (declare (ignore font string))
  0)


(defmethod text-line-height ((font nuklear-renderer-font))
  "Dummy method: we are using native nuklear font instead"
  (declare (ignore font))
  0)


(defmethod bodge-ui::font-handle ((font nuklear-renderer-font))
  (%handle-of font))


(defmethod render-ui ((renderer nuklear-renderer))
  (with-slots (width height pixel-ratio handle) renderer
    (claw:c-let ((nk-context (:struct (%nk:context)) :from bodge-ui::*handle*))
      (let ((default-font (nk-context :style :font)))
        (unwind-protect
             (nk:render-nuklear (%handle-of renderer) nk-context width height pixel-ratio)
          (%nk:style-set-font nk-context default-font))))))
