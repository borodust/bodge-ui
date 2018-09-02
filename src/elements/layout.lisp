(cl:in-package :bodge-ui)

;;;
;;;
;;;
(defclass stacking-layout (expandable panel %layout) ())


(defun default-row-height (child-height)
  (float (or child-height (style :row-height)) 0f0))


(defclass vertical-layout (stacking-layout) ())


(defmethod calc-bounds ((this vertical-layout))
  (let (width
        height
        (spacing (style :layout-spacing)))
    (dochildren (child this)
      (multiple-value-bind (child-width child-height) (calc-bounds child)
        (when child-width
          (setf width (+ (if width
                             (max width child-width)
                             child-width)
                         spacing)))
        (let ((child-height (default-row-height child-height)))
          (setf height (+ (if height
                              (+ height child-height)
                              child-height)
                          spacing)))))
    (values width height)))


(defmethod compose-panel ((this vertical-layout))
  (dochildren (child this)
    (multiple-value-bind (child-width child-height) (calc-bounds child)
      (let ((height (default-row-height child-height)))
        (if child-width
            (%nk:layout-row-static *handle* height (floor child-width) 1)
            (%nk:layout-row-dynamic *handle* height 1)))
      (compose child))))

;;;
;;;
;;;
(defclass horizontal-layout (stacking-layout) ())


(defmethod calc-bounds ((this horizontal-layout))
  (let (width
        height
        width-undefined
        (spacing (style :layout-spacing)))
    (dochildren (child this)
      (multiple-value-bind (child-width child-height) (calc-bounds child)
        ;; if at least one child has undefined width
        ;; make the whole container width undefined
        (unless width-undefined
          (if child-width
              (setf width (+ (if width
                                 (+ width child-width)
                                 child-width)
                             spacing))
              (setf width nil
                    width-undefined t)))
        (when child-height
          (setf height (+ (if height
                              (max height child-height)
                              child-height)
                          spacing)))))
    (values width height)))


(defun compose-horizontal-expand (this height expand-range child-count)
  (let ((normalizing-expand-multiplier (float (/ 1 (if (= expand-range 0) 1 expand-range)) 0f0)))
    (%nk:layout-row-begin *handle* %nk:+dynamic+ height child-count)
    (unwind-protect
         (dochildren (child this)
           (let ((expand-ratio (expand-ratio-of child)))
             (if expand-ratio
                 (%nk:layout-row-push *handle* (float (* expand-ratio normalizing-expand-multiplier)
                                                      0f0))
                 (%nk:layout-row-push *handle* normalizing-expand-multiplier)))
           (compose child))
      (%nk:layout-row-end *handle*))))


(defun compose-horizontal-flex (this height)
  (%nk:layout-row-template-begin *handle* height)
  (unwind-protect
       (dochildren (child this)
         (if-let ((width (calc-bounds child)))
           (if (expandablep child)
               (%nk:layout-row-template-push-variable *handle* (float width 0f0))
               (%nk:layout-row-template-push-static *handle* (float width 0f0)))
           (%nk:layout-row-template-push-dynamic *handle*)))
    (claw:with-float-traps-masked ()
      (%nk:layout-row-template-end *handle*)))
  (dochildren (child this)
    (compose child)))


(defmethod compose-panel ((this horizontal-layout))
  (flet ((height-max (value element)
           (multiple-value-bind (el-width el-height) (calc-bounds element)
             (declare (ignore el-width))
             (if value
                 (when el-height
                   (max el-height value))
                 el-height)))
         (add-expand-ratio (value element)
           (if-let ((ratio (expand-ratio-of element)))
             (progn
               (incf (car value) ratio)
               (unless (cdr value)
                 (setf (cdr value) t)))
             (incf (car value) 1.0))
           value))
    (let* ((children (children-of this))
           (child-count (length children))
           (height (float (or (reduce #'height-max children :initial-value nil)
                              (style :row-height))
                          0f0))
           (expand-range (reduce #'add-expand-ratio children :initial-value (cons 0.0 nil))))
      (if (cdr expand-range)
          (compose-horizontal-expand this height (car expand-range) child-count)
          (compose-horizontal-flex this height)))))
