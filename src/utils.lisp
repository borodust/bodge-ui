(cl:in-package :bodge-ui)


(defun clamp (value)
  (min (max (/ value 255.0) 0.0) 1.0))


(defun unclamp (value)
  (the fixnum (round (* value 255))))


(defun clamp-vec4 (r g b a)
  (vec4 (clamp r) (clamp g) (clamp b) (clamp a)))


(defun clamp-vec2 (x y)
  (vec2 (clamp x) (clamp y)))
