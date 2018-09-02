(cl:in-package :bodge-ui.example)

(defwindow (demo-window
            (:title "Hello Bodge UI")
            (:origin 200 50)
            (:width 400) (:height 400)
            (:options :movable :resizable
                      :minimizable :scrollable
                      :closable))
  (label :text "Nested:")
  (horizontal-layout
   (radio-group
    (radio :label "Option 1")
    (radio :label "Option 2" :activated t))
   (vertical-layout
    (check-box :label "Check 1" :width 100)
    (check-box :label "Check 2"))
   (vertical-layout
    (label :text "Awesomely" :align :left)
    (label :text "Stacked" :align :middle)
    (label :text "Labels" :align :right)))
  (label :text "Expand by width:")
  (horizontal-layout
   (button :label "Dynamic")
   (button :label "Min-Width" :width 80)
   (button :label "Fixed-Width" :expandable nil :width 100))
  (label :text "Expand by ratio:")
  (horizontal-layout
   (button :label "1.0" :expand-ratio 1.0)
   (button :label "0.75" :expand-ratio 0.75)
   (button :label "0.5" :expand-ratio 0.5))
  (label :text "Rest:")
  (button :label "Top-Level Button"))

(cl:in-package :bodge-ui.example)

(defmethod next-mouse-interaction ((this bodge-ui-app))
  (with-slots (mouse-actions) this
    (let ((interaction (pop mouse-actions)))
      (values (car interaction) (cdr interaction)))))

(defmethod last-cursor-position ((this bodge-ui-app) &optional result-vec2)
  (with-slots (cursor) this
    (if result-vec2
        (progn
          (setf (bodge-math:x result-vec2) (bodge-math:x cursor)
                (bodge-math:y result-vec2) (bodge-math:y cursor))
          result-vec2)
        cursor)))
