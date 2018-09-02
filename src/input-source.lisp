(cl:in-package :bodge-ui)


(defgeneric next-keyboard-interaction (input-source)
  (:method (input-source) (declare (ignore input-source))))

(defgeneric next-mouse-interaction (input-source)
  (:method (input-source) (declare (ignore input-source))))

(defgeneric last-cursor-position (input-source &optional result-vec2)
  (:method (input-source &optional result-vec2) (declare (ignore input-source result-vec2))))

(defgeneric next-character (input-source)
  (:method (input-source) (declare (ignore input-source))))

(defgeneric next-scroll (input-source &optional result-vec2)
  (:method (input-source &optional result-vec2) (declare (ignore input-source result-vec2))))
