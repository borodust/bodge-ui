(bodge-util:define-package :bodge-ui.example
  (:use :cl :bodge-ui :bodge-ui.renderer))

(cl:in-package :bodge-ui.example)

(defvar *window-width* 800)
(defvar *window-height* 600)
(defvar *default-output* *standard-output*)

(defclass bodge-ui-app (bodge-host:application)
  ((context :initform nil)
   (renderer :initform nil)
   (enabled-p :initform t)
   (mouse-actions :initform (list))
   (cursor :initform (bodge-math:vec2)))
  (:default-initargs
   :opengl-version '(3 3)
   :viewport-title "Bodge UI Example"
   :viewport-width *window-width*
   :viewport-height *window-height*))

(cl:in-package :bodge-ui.example)


(defwindow (demo-window
            (:title "Hello Bodge UI")
            (:origin 200 200)
            (:width 100)
            (:height 100)
            (:options :scrollable t :movable t :resizable t))
  (label :text "YO")
  (label :text "YO"))

(cl:in-package :bodge-ui.example)


(defmethod bodge-host:on-mouse-action ((this bodge-ui-app) button action)
  (with-slots (mouse-actions) this
    (alexandria:nconcf mouse-actions (list (cons button action)))))


(defmethod next-mouse-interaction ((this bodge-ui-app))
  (with-slots (mouse-actions) this
    (let ((interaction (pop mouse-actions)))
      (values (car interaction) (cdr interaction)))))


(defmethod bodge-host:on-cursor-movement ((this bodge-ui-app) x y)
  (with-slots (cursor) this
    (setf (bodge-math:x cursor) x
          (bodge-math:y cursor) y)))


(defmethod last-cursor-position ((this bodge-ui-app) &optional result-vec2)
  (with-slots (cursor) this
    (if result-vec2
        (progn
          (setf (bodge-math:x result-vec2) (bodge-math:x cursor)
                (bodge-math:y result-vec2) (bodge-math:y cursor))
          result-vec2)
        cursor)))

(cl:in-package :bodge-ui.example)

(defun render-example-ui (app)
  (with-slots (context) app
    (gl:clear-color 0.8 0.8 0.8 1.0)
    (gl:clear :color-buffer-bit)
    (compose-ui context)
    (bodge-host:swap-buffers app)))

(cl:in-package :bodge-ui.example)

(defmethod bodge-host:on-init ((this bodge-ui-app))
  (with-slots (context renderer enabled-p) this
    (setf enabled-p t)
    (bodge-concurrency:in-new-thread ("rendering-thread")
      (unwind-protect
           (progn
             (bodge-host:bind-main-rendering-context this)
             (glad:init)
             (setf renderer (make-nuklear-renderer *window-width* *window-height*)
                   context (make-ui renderer :input-source this))
             (add-window 'demo-window :ui context)
             (loop while enabled-p
                   do (render-example-ui this)))
        (bodge-host:stop-application this)))))


(defmethod bodge-host:on-destroy ((this bodge-ui-app))
  (with-slots (context renderer) this
    (destroy-ui context)
    (destroy-nuklear-renderer renderer)))


(defmethod bodge-host:on-hide ((this bodge-ui-app))
  (with-slots (enabled-p) this
    (setf enabled-p nil)))


(export 'run)
(defun run ()
  (bodge-host:start-application (make-instance 'bodge-ui-app)))
