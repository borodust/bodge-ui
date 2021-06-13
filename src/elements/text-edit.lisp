(cl:in-package :bodge-ui)

;;;
;;;
;;;
(defclass text-edit (disposable widget)
  ((buffer :initform (cffi:foreign-alloc '(:struct %nuklear:text-edit)))))


(defmethod initialize-instance :after ((this text-edit) &key text)
  (with-slots (buffer) this
    (%nuklear:textedit-init-default buffer))
  (when text
    (setf (text-of this) text)))


(define-destructor text-edit (buffer)
  (cffi:foreign-free buffer))


(defun make-text-edit (&key name text)
  (make-instance 'text-edit :name name :text text))


(defmethod text-of ((this text-edit))
  (with-slots (buffer) this
    (c-let ((buf (:struct %nuklear:text-edit) :from buffer))
      (let* ((str-info (buf :string &))
             (len (%nuklear:str-len-char str-info)))
        (if-let ((ptr (%nuklear:str-get-const str-info)))
          (or (cffi:foreign-string-to-lisp ptr :count len
                                               :encoding :utf-8)
              "")
          "")))))


(defmethod (setf text-of) ((value string) (this text-edit))
  (with-slots (buffer) this
    (c-let ((buf (:struct %nuklear:text-edit) :from buffer))
      (let ((str-info (buf :string &)))
        (%nuklear:str-clear str-info)
        (unless (alexandria:emptyp value)
          (cffi:with-foreign-string (string-ptr value :encoding :utf-8)
            (%nuklear:str-append-text-utf8 str-info string-ptr (length value))))))
    value))


(defmethod compose ((this text-edit))
  (with-slots (buffer) this
    (%nuklear:edit-buffer *handle*
                     (cffi:foreign-enum-value '%nuklear:edit-types :simple)
                     buffer
                     (cffi:foreign-symbol-pointer "nk_filter_default"))))
