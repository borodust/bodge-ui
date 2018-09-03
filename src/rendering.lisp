(cl:in-package :bodge-ui)


(declaim (special *command*
                  *renderer*))


(defun clamp (r g b a)
  (flet ((c (v)
           (min (max (/ v 255.0) 0.0) 1.0)))
    (vec4 (c r) (c g) (c b) (c a))))


(defun bodge-color (nk-color)
  (claw:c-val ((nk-color (:struct (%nk:color))))
    (clamp (nk-color :r) (nk-color :g) (nk-color :b) (nk-color :a))))


(definline %invert (y &optional (h 0.0))
  (- (bodge-ui:renderer-canvas-height *renderer*) y h))


(defmacro as-command ((cmd-var type) &body body)
  `(claw:c-let ((,cmd-var (:struct (,type)) :from *command*))
     ,@body))


(defun scissor-origin ()
  (as-command (cmd %nk:command-scissor)
    (vec2 (cmd :x) (%invert (cmd :y) (cmd :h)))))


(defun scissor-width ()
  (as-command (cmd %nk:command-scissor)
    (cmd :w)))


(defun scissor-height ()
  (as-command (cmd %nk:command-scissor)
    (cmd :h)))


(defun line-origin ()
  (as-command (cmd %nk:command-line)
    (let ((x0 (cmd :begin :x))
          (y0 (cmd :begin :y)))
      (vec2 x0 (%invert y0)))))


(defun line-end ()
  (as-command (cmd %nk:command-line)
    (let ((x1 (cmd :end :x))
          (y1 (cmd :end :y)))
      (vec2 x1 (%invert y1)))))


(defun line-color ()
  (as-command (cmd %nk:command-line)
    (bodge-color (cmd :color))))


(defun line-thickness ()
  (as-command (cmd %nk:command-line)
    (cmd :line-thickness)))


(defun curve-origin ()
  (as-command (cmd %nk:command-curve)
    (let ((x0 (cmd :begin :x))
          (y0 (cmd :begin :y)))
      (vec2 x0 (%invert y0)))))


(defun curve-end ()
  (as-command (cmd %nk:command-curve)
    (let ((x1 (cmd :end :x))
          (y1 (cmd :end :y)))
      (vec2 x1 (%invert y1)))))


(defun curve-first-control-point ()
  (as-command (cmd %nk:command-curve)
    (let ((cx0 (cmd :ctrl 0 :x))
          (cy0 (cmd :ctrl 0 :y)))
      (vec2 cx0 (%invert cy0)))))


(defun curve-second-control-point ()
  (as-command (cmd %nk:command-curve)
    (let ((cx1 (cmd :ctrl 1 :x))
          (cy1 (cmd :ctrl 1 :y)))
      (vec2 cx1 (%invert cy1)))))


(defun curve-color ()
  (as-command (cmd %nk:command-curve)
    (bodge-color (cmd :color))))


(defun curve-thickness ()
  (as-command (cmd %nk:command-curve)
    (cmd :line-thickness)))


(defun rect-origin ()
  (as-command (cmd %nk:command-rect)
    (vec2 (cmd :x) (%invert (cmd :y) (cmd :h)))))


(defun rect-width ()
  (as-command (cmd %nk:command-rect)
    (cmd :w)))


(defun rect-height ()
  (as-command (cmd %nk:command-rect)
    (cmd :h)))


(defun rect-stroke-color ()
  (as-command (cmd %nk:command-rect)
    (bodge-color (cmd :color))))


(defun rect-stroke-thickness ()
  (as-command (cmd %nk:command-rect)
    (cmd :line-thickness)))


(defun rect-rounding ()
  (as-command (cmd %nk:command-rect)
    (cmd :rounding)))


(defun filled-rect-origin ()
  (as-command (cmd %nk:command-rect-filled)
    (vec2 (cmd :x) (%invert (cmd :y) (cmd :h)))))


(defun filled-rect-width ()
  (as-command (cmd %nk:command-rect-filled)
    (cmd :w)))


(defun filled-rect-height ()
  (as-command (cmd %nk:command-rect-filled)
    (cmd :h)))


(defun filled-rect-color ()
  (as-command (cmd %nk:command-rect-filled)
    (bodge-color (cmd :color))))


(defun filled-rect-rounding ()
  (as-command (cmd %nk:command-rect-filled)
    (cmd :rounding)))


(defun multi-color-rect-origin ()
  (as-command (cmd %nk:command-rect-multi-color)
    (vec2 (cmd :x) (%invert (cmd :y) (cmd :h)))))


(defun multi-color-rect-width ()
  (as-command (cmd %nk:command-rect-multi-color)
    (cmd :w)))


(defun multi-color-rect-height ()
  (as-command (cmd %nk:command-rect-multi-color)
    (cmd :h)))


(defun multi-color-rect-left-color ()
  (as-command (cmd %nk:command-rect-multi-color)
    (bodge-color (cmd :left))))


(defun multi-color-rect-top-color ()
  (as-command (cmd %nk:command-rect-multi-color)
    (bodge-color (cmd :top))))


(defun multi-color-rect-bottom-color ()
  (as-command (cmd %nk:command-rect-multi-color)
    (bodge-color (cmd :bottom))))


(defun multi-color-rect-right-color ()
  (as-command (cmd %nk:command-rect-multi-color)
    (bodge-color (cmd :right))))


(defun ellipse-origin ()
  (as-command (cmd %nk:command-circle)
    (let* ((x (cmd :x))
           (y (cmd :y))
           (w (cmd :w))
           (h (cmd :h))
           (rx (/ w 2))
           (ry (/ h 2)))
      (vec2 (+ x rx) (+ (%invert y h) ry)))))


(defun ellipse-radius-x ()
  (as-command (cmd %nk:command-circle)
    (/ (cmd :w) 2)))


(defun ellipse-radius-y ()
  (as-command (cmd %nk:command-circle)
    (/ (cmd :h) 2)))


(defun ellipse-stroke-color ()
  (as-command (cmd %nk:command-circle)
    (bodge-color (cmd :color))))


(defun ellipse-stroke-thickness ()
  (as-command (cmd %nk:command-circle)
    (cmd :line-thickness)))


(defun filled-ellipse-origin ()
  (as-command (cmd %nk:command-circle-filled)
    (let* ((x (cmd :x))
           (y (cmd :y))
           (w (cmd :w))
           (h (cmd :h))
           (rx (/ w 2))
           (ry (/ h 2)))
      (vec2 (+ x rx) (+ (%invert y h) ry)))))


(defun filled-ellipse-radius-x ()
  (as-command (cmd %nk:command-circle-filled)
    (/ (cmd :w) 2)))


(defun filled-ellipse-radius-y ()
  (as-command (cmd %nk:command-circle-filled)
    (/ (cmd :h) 2)))


(defun filled-ellipse-color ()
  (as-command (cmd %nk:command-circle-filled)
    (bodge-color (cmd :color))))


(defun arc-origin ()
  (as-command (cmd %nk:command-arc)
    (vec2 (cmd :cx) (%invert (cmd :cy)))))


(defun arc-radius ()
  (as-command (cmd %nk:command-arc)
    (cmd :r)))


(defun arc-start-angle ()
  (as-command (cmd %nk:command-arc)
    (cmd :a 0)))


(defun arc-end-angle ()
  (as-command (cmd %nk:command-arc)
    (cmd :a 1)))


(defun arc-stroke-color ()
  (as-command (cmd %nk:command-arc)
    (bodge-color (cmd :color))))


(defun arc-stroke-thickness ()
  (as-command (cmd %nk:command-arc)
    (cmd :line-thickness)))


(defun filled-arc-origin ()
  (as-command (cmd %nk:command-arc-filled)
    (vec2 (cmd :cx) (%invert (cmd :cy)))))


(defun filled-arc-radius ()
  (as-command (cmd %nk:command-arc-filled)
    (cmd :r)))


(defun filled-arc-start-angle ()
  (as-command (cmd %nk:command-arc-filled)
    (cmd :a 0)))


(defun filled-arc-end-angle ()
  (as-command (cmd %nk:command-arc-filled)
    (cmd :a 1)))


(defun filled-arc-color ()
  (as-command (cmd %nk:command-arc-filled)
    (bodge-color (cmd :color))))


(defun triangle-origin ()
  (as-command (cmd %nk:command-triangle)
    (vec2 (cmd :a :x) (%invert (cmd :a :y)))))


(defun triangle-second-vertex ()
  (as-command (cmd %nk:command-triangle)
    (vec2 (cmd :b :x) (%invert (cmd :b :y)))))


(defun triangle-third-vertex ()
  (as-command (cmd %nk:command-triangle)
    (vec2 (cmd :c :x) (%invert (cmd :c :y)))))


(defun triangle-stroke-color ()
  (as-command (cmd %nk:command-triangle)
    (bodge-color (cmd :color))))


(defun triangle-stroke-thickness ()
  (as-command (cmd %nk:command-triangle)
    (cmd :line-thickness)))


(defun filled-triangle-origin ()
  (as-command (cmd %nk:command-triangle-filled)
    (vec2 (cmd :a :x) (%invert (cmd :a :y)))))


(defun filled-triangle-second-vertex ()
  (as-command (cmd %nk:command-triangle-filled)
    (vec2 (cmd :b :x) (%invert (cmd :b :y)))))


(defun filled-triangle-third-vertex ()
  (as-command (cmd %nk:command-triangle-filled)
    (vec2 (cmd :c :x) (%invert (cmd :c :y)))))


(defun filled-triangle-color ()
  (as-command (cmd %nk:command-triangle-filled)
    (bodge-color (cmd :color))))


(defun polygon-vertices ()
  (as-command (cmd %nk:command-polygon)
    (loop for i from 0 below (cmd :point-count)
          collect (vec2 (cmd :points i :x) (%invert (cmd :points i :y))))))


(defun polygon-stroke-color ()
  (as-command (cmd %nk:command-polygon)
    (bodge-color (cmd :color))))


(defun polygon-stroke-thickness ()
  (as-command (cmd %nk:command-polygon)
    (cmd :line-thickness)))


(defun filled-polygon-vertices ()
  (as-command (cmd %nk:command-polygon-filled)
    (loop for i from 0 below (cmd :point-count)
          collect (vec2 (cmd :points i :x) (%invert (cmd :points i :y))))))


(defun filled-polygon-color ()
  (as-command (cmd %nk:command-polygon-filled)
    (bodge-color (cmd :color))))


(defun polyline-vertices ()
  (as-command (cmd %nk:command-polyline)
    (loop for i from 0 below (cmd :point-count)
          collect (vec2 (cmd :points i :x) (%invert (cmd :points i :y))))))


(defun polyline-color ()
  (as-command (cmd %nk:command-polyline)
    (bodge-color (cmd :color))))


(defun polyline-thickness ()
  (as-command (cmd %nk:command-polyline)
    (cmd :line-thickness)))


(defun text-box-origin ()
  (as-command (cmd %nk:command-text)
    (vec2 (cmd :x) (%invert (cmd :y) (cmd :height)))))


(defun text-background-color ()
  (as-command (cmd %nk:command-text)
    (bodge-color (cmd :background))))


(defun text-foreground-color ()
  (as-command (cmd %nk:command-text)
    (bodge-color (cmd :foreground))))


(defun text-box-width ()
  (as-command (cmd %nk:command-text)
    (bodge-color (cmd :w))))


(defun text-box-height ()
  (as-command (cmd %nk:command-text)
    (bodge-color (cmd :h))))


(defun text-string ()
  (as-command (cmd %nk:command-text)
    (cffi:foreign-string-to-lisp (cmd :string &) :count (cmd :length))))


(defun image-origin ()
  (as-command (cmd %nk:command-image)
    (vec2 (cmd :x) (%invert (cmd :y) (cmd :h)))))


(defun image-width ()
  (as-command (cmd %nk:command-image)
    (cmd :w)))


(defun image-height ()
  (as-command (cmd %nk:command-image)
    (cmd :h)))


(defun image-color ()
  (as-command (cmd %nk:command-image)
    (bodge-color (cmd :col))))


(defun custom-widget-origin ()
  (as-command (cmd %nk:command-custom)
    (vec2 (cmd :x) (%invert (cmd :y) (cmd :h)))))


(defun custom-widget-width ()
  (as-command (cmd %nk:command-custom)
    (cmd :w)))


(defun custom-widget-height ()
  (as-command (cmd %nk:command-custom)
    (cmd :h)))


(defun custom-widget-instance ()
  (as-command (cmd %nk:command-custom)
    (let ((widget-id (cffi:pointer-address (cmd :callback-data :ptr))))
      (context-custom-widget widget-id))))


(defun process-custom-widget ()
  (when-let ((widget (custom-widget-instance)))
    (render-custom-widget widget
                          (custom-widget-origin)
                          (custom-widget-width)
                          (custom-widget-height))))


(defun command-type ()
  (as-command (cmd %nk:command)
    (claw:enum-key '(:enum (%nk:command-type)) (cmd :type))))


(defmacro docommands (() &body body)
  (with-gensyms (cmd)
    `(nuklear:docommands (,cmd *handle*)
       (let ((*command* ,cmd))
         (if (eq (command-type) :custom)
             (process-custom-widget)
             (progn ,@body))))))


(defun compose-ui (context)
  (with-ui (context)
    (clear-ui)
    (drain-compose-task-queue context)
    (when-let ((input-source (%input-source-of context)))
      (with-ui-input (context)
        (loop (multiple-value-bind (key state) (next-keyboard-interaction input-source)
                (if key
                    (register-keyboard-input key state)
                    (return))))
        (when-let* ((cursor (last-cursor-position input-source
                                             (%last-cursor-position-of context)))
                    (x (x cursor))
                    (y (y cursor)))
          (loop (multiple-value-bind (button state) (next-mouse-interaction input-source)
                  (if button
                      (register-mouse-input x y button state)
                      (return))))
          (register-cursor-position x y))
        (loop for character = (next-character input-source)
              while character
              do (register-character-input character))
        (when-let ((scroll (next-scroll input-source (%last-scroll-of context))))
          (register-scroll-input (x scroll) (y scroll)))))
    (loop for win in (%windows-of context)
          do (compose win))
    (let ((*renderer* (%renderer-of context)))
      (render-ui *renderer*))))
