(cl:in-package :bodge-ui)

(defclass menu-bar (%layout) ())

(defun make-menu-bar ()
  (make-instance 'menu-bar))


(defmethod compose ((this menu-bar))
  (%nuklear:menubar-begin *handle*)
  (call-next-method)
  (%nuklear:menubar-end *handle*))
