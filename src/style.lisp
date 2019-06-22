(cl:in-package :bodge-ui)

(defgeneric style (style))
(defgeneric (setf style) (value style))


(declaim (special *style*))


(defclass style ()
  ((style-table :initarg :style-table :initform (make-hash-table))))


(defun make-style (&rest styles &key &allow-other-keys)
  (make-instance 'style :style-table (alexandria:plist-hash-table styles)))


(defun apply-style (style)
  (with-slots (style-table) style
    (loop with backup = (make-hash-table)
          for style-name being the hash-key of style-table
            using (hash-value style-value)
          do (setf (gethash style-name backup) (style style-name)
                   (style style-name) style-value)
          finally (return backup))))


(defun restore-style (backup)
  (loop for style-name being the hash-key of backup
          using (hash-value style-value)
        when style-value
          do (setf (style style-name) style-value)))


(defun set-style (style name value &key (ignore-if-exists t))
  (with-slots (style-table) style
    (if ignore-if-exists
        (setf (gethash name style-table) value)
        (unless (gethash name style-table)
          (setf (gethash name style-table) value)))))


(defun get-style (style name)
  (with-slots (style-table) style
    (gethash name style-table)))


(defmacro with-style ((style) &body body)
  (once-only (style)
    (with-gensyms (backup)
      `(let ((,backup (apply-style ,style)))
         (unwind-protect
              (progn ,@body)
           (restore-style ,backup))))))

;;
;; Bodge UI styles
;;
(defmethod style ((style (eql :row-height)))
  (with-slots (style-table) *style*
    (gethash style style-table)))


(defmethod (setf style) ((value number) (style (eql :row-height)))
  (with-slots (style-table) *style*
    (setf (gethash style style-table) value)))


(defmethod style ((style (eql :layout-spacing)))
  (with-slots (style-table) *style*
    (gethash style style-table)))


(defmethod (setf style) ((value number) (style (eql :layout-spacing)))
  (with-slots (style-table) *style*
    (setf (gethash style style-table) value)))


;;
;; Nuklear Style Item
;;
(defclass style-item (disposable)
  ((handle :initform (claw:alloc '(:struct (%nk:style-item))) :reader %handle-of)))


(define-destructor style-item (handle)
  (claw:free handle))


(defclass color-style-item (style-item) ())


(defmethod initialize-instance :after ((this color-style-item)
                                       &key color)
  (with-slots (handle) this
    (claw:c-with ((color-v (:struct (%nk:color))))
      (%nk:style-item-color handle
                            (%nk:rgba-f color-v
                                        (float (x color) 0f0)
                                        (float (y color) 0f0)
                                        (float (z color) 0f0)
                                        (float (w color) 0f0))))))


(defun make-color-style-item (color)
  (make-instance 'color-style-item :color color))


(defun nk->style-item (style-item)
  (claw:c-val ((style-item (:struct (%nk:style-item))))
    (switch ((style-item :type) :test #'=)
      (%nk:+style-item-color+
       (claw:c-let ((nk-color (:struct (%nk:color)) :from (style-item :data)))
         (make-color-style-item (clamp-vec4 (nk-color :r)
                                            (nk-color :g)
                                            (nk-color :b)
                                            (nk-color :a)))))
      (t (error "Unrecognized style item type")))))


(defun style-item->nk (style-item dest-ptr)
  (claw:memcpy dest-ptr (%handle-of style-item) 1 '(:struct (%nk:style-item))))



;;
;; Nuklear Styles
;;
(defmacro with-nk-style ((nk-style) &body body)
  `(claw:c-let ((,nk-style (:struct (%nk:style))
                           :from (claw:c-ref *handle* (:struct (%nk:context)) :style)))
     ,@body))


(defmacro with-nk-style-float ((value &rest fields) &body body)
  (with-gensyms (nk-style)
    `(with-nk-style (,nk-style)
       (symbol-macrolet ((,value (,nk-style ,@fields)))
         ,@body))))


(defmacro with-nk-style-color (((r g b a) &rest fields) &body body)
  (with-gensyms (nk-style)
    `(with-nk-style (,nk-style)
       (symbol-macrolet ((,r (,nk-style ,@fields :r))
                         (,g (,nk-style ,@fields :g))
                         (,b (,nk-style ,@fields :b))
                         (,a (,nk-style ,@fields :a)))
         (declare (ignorable ,r ,g ,b ,a))
         ,@body))))


(defmacro with-nk-style-vec2 (((x y) &rest fields) &body body)
  (with-gensyms (nk-style)
    `(with-nk-style (,nk-style)
       (symbol-macrolet ((,x (,nk-style ,@fields :x))
                         (,y (,nk-style ,@fields :y)))
         (declare (ignorable ,x ,y))
         ,@body))))


(defmacro define-float-style (name &rest path)
  (let ((style-name (make-keyword name)))
    (with-gensyms (value new-value style)
      `(progn
         (defmethod style ((,style (eql ,style-name)))
           (declare (ignore ,style))
           (with-nk-style-float (,value ,@path)
             ,value))
         (defmethod (setf style) ((,new-value number) (,style (eql ,style-name)))
           (declare (ignore ,style))
           (with-nk-style-float (,value ,@path)
             (setf ,value (float ,new-value 0f0))))))))


(defmacro define-color-style (name &rest path)
  (let ((style-name (make-keyword name)))
    (with-gensyms (r g b a new-value style)
      `(progn
         (defmethod style ((,style (eql ,style-name)))
           (declare (ignore ,style))
           (with-nk-style-color ((,r ,g ,b ,a) ,@path)
             (clamp-vec4 ,r ,g ,b ,a)))
         (defmethod (setf style) ((,new-value vec4) (,style (eql ,style-name)))
           (declare (ignore ,style))
           (with-nk-style-color ((,r ,g ,b ,a) ,@path)
             (setf ,r (unclamp (x ,new-value))
                   ,g (unclamp (y ,new-value))
                   ,b (unclamp (z ,new-value))
                   ,a (unclamp (w ,new-value))))
           ,new-value)))))


(defmacro define-vec2-style (name &rest path)
  (let ((style-name (make-keyword name)))
    (with-gensyms (x y new-value style)
      `(progn
         (defmethod style ((,style (eql ,style-name)))
           (declare (ignore ,style))
           (with-nk-style-vec2 ((,x ,y) ,@path)
             (clamp-vec2 ,x ,y)))
         (defmethod (setf style) ((,new-value vec2) (,style (eql ,style-name)))
           (declare (ignore ,style))
           (with-nk-style-vec2 ((,x ,y) ,@path)
             (setf ,x (float (x ,new-value) 0f0)
                   ,y (float (y ,new-value) 0f0)))
           ,new-value)))))


(defmacro define-item-style (name &rest path)
  (let ((style-name (make-keyword name)))
    (with-gensyms (new-value style nk-style)
      `(progn
         (defmethod style ((,style (eql ,style-name)))
           (declare (ignore ,style))
           (with-nk-style (,nk-style)
             (nk->style-item (,nk-style ,@path))))
         (defmethod (setf style) ((,new-value vec4) (,style (eql ,style-name)))
           (setf (style ,style) (make-color-style-item ,new-value)))
         (defmethod (setf style) ((,new-value style-item) (,style (eql ,style-name)))
           (declare (ignore ,style))
           (with-nk-style (,nk-style)
             (style-item->nk ,new-value (,nk-style ,@path))
             ,new-value))))))

;;;
;;; Text
;;;
(define-color-style text-color :text :color)
(define-vec2-style text-padding :text :padding)

;;;
;;; Button
;;;
(define-item-style button-normal :button :normal)
(define-item-style button-hover :button :hover)
(define-item-style button-active :button :active)
(define-color-style button-border-color :button :border-color)

(define-color-style button-text-background :button :text-background)
(define-color-style button-text-normal :button :text-normal)
(define-color-style button-text-hover :button :text-hover)
(define-color-style button-text-active :button :text-active)

(define-float-style button-border :button :border)
(define-float-style button-rounding :button :rounding)
(define-vec2-style button-padding :button :padding)
(define-vec2-style button-image-padding :button :image-padding)
(define-vec2-style button-touch-padding :button :touch-padding)

;;;
;;; Option
;;;
(define-item-style option-normal :option :normal)
(define-item-style option-hover :option :hover)
(define-item-style option-active :option :active)
(define-color-style option-border-color :option :border-color)

(define-item-style option-cursor-normal :option :cursor-normal)
(define-item-style option-cursor-hover :option :cursor-hover)

(define-color-style option-text-background :option :text-background)
(define-color-style option-text-normal :option :text-normal)
(define-color-style option-text-hover :option :text-hover)
(define-color-style option-text-active :option :text-active)

(define-vec2-style option-padding :option :padding)
(define-vec2-style option-touch-padding :option :touch-padding)
(define-float-style option-spacing :option :spacing)
(define-float-style option-border :option :border)


;;;
;;; Vertical Scrollbar
;;;
(define-item-style vertical-scrollbar-normal :scrollv :normal)
(define-item-style vertical-scrollbar-hover :scrollv :hover)
(define-item-style vertical-scrollbar-active :scrollv :active)
(define-color-style vertical-scrollbar-border-color :scrollv :border-color)

(define-item-style vertical-scrollbar-cursor-normal :scrollv :cursor-normal)
(define-item-style vertical-scrollbar-cursor-hover :scrollv :cursor-hover)
(define-item-style vertical-scrollbar-cursor-active :scrollv :cursor-active)
(define-color-style vertical-scrollbar-cursor-active :scrollv :cursor-border-color)

(define-float-style vertical-scrollbar-border :scrollv :border)
(define-float-style vertical-scrollbar-rounding :scrollv :rounding)
(define-float-style vertical-scrollbar-border-cursor :scrollv :border-cursor)
(define-float-style vertical-scrollbar-rounding-cursor :scrollv :rounding-cursor)
(define-vec2-style vertical-scrollbar-padding :scrollv :padding)


;;;
;;; Text Edit
;;;
(define-item-style text-edit-normal :edit :normal)
(define-item-style text-edit-hover :edit :hover)
(define-item-style text-edit-active :edit :active)
(define-color-style text-edit-border-color :edit :border-color)

(define-item-style text-edit-cursor-normal :edit :cursor-normal)
(define-item-style text-edit-cursor-hover :edit :cursor-hover)
(define-item-style text-edit-cursor-text-normal :edit :cursor-text-normal)
(define-item-style text-edit-cursor-text-hover :edit :cursor-text-hover)

(define-color-style text-edit-text-normal :edit :text-normal)
(define-color-style text-edit-text-hover :edit :text-hover)
(define-color-style text-edit-text-active :edit :text-active)

(define-color-style text-edit-selected-normal :edit :selected-normal)
(define-color-style text-edit-selected-hover :edit :selected-hover)

(define-color-style text-edit-selected-text-normal :edit :selected-text-normal)
(define-color-style text-edit-selected-text-hover :edit :selected-text-hover)

(define-float-style text-edit-border :edit :border)
(define-float-style text-edit-rounding :edit :rounding)
(define-float-style text-edit-cursor-size :edit :cursor-size)
(define-vec2-style text-edit-scrollbar-size :edit :scrollbar-size)
(define-vec2-style text-edit-padding :edit :padding)
(define-float-style text-edit-row-padding :edit :row-padding)


;;;
;;; Panel
;;;
(define-item-style panel-header-normal :window :header :normal)
(define-item-style panel-header-hover :window :header :hover)
(define-item-style panel-header-active :window :header :active)

(define-item-style panel-header-title-normal :window :header :label-normal)
(define-item-style panel-header-title-hover :window :header :label-hover)
(define-item-style panel-header-title-active :window :header :label-active)

(define-vec2-style panel-header-padding :window :header :padding)
(define-vec2-style panel-header-title-padding :window :header :label-padding)
(define-vec2-style panel-header-spacing :window :header :spacing)

(define-item-style panel-fixed-background :window :fixed-background)

(define-color-style panel-background :window :background)
(define-color-style panel-border-color :window :border-color)

(define-item-style panel-scaler :window :scaler)

(define-float-style panel-border :window :border)

(define-float-style panel-rounding :window :rounding)
(define-vec2-style panel-spacing :window :spacing)
(define-vec2-style panel-scrollbar-size :window :scrollbar-size)

(define-vec2-style panel-padding :window :padding)

;;;
;;; Groups
;;;
(define-float-style group-border :window :border)
(define-vec2-style group-padding :window :group-padding)
(define-color-style group-border-color :window :group-border-color)
