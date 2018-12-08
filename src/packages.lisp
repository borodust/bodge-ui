(bodge-util:define-package :bodge-ui
  (:use :cl :bodge-memory :bodge-util :bodge-math)
  (:export make-ui

           push-compose-task
           with-ui-access
           compose-ui
           root-panel

           custom-font
           calculate-text-width
           text-line-height

           renderer-canvas-width
           renderer-canvas-height
           renderer-default-font
           render-ui

           defpanel
           find-element
           on-panel-close
           add-panel
           remove-panel
           remove-all-panels

           vertical-layout
           horizontal-layout
           button
           label
           text-edit
           combo-box
           color-box
           spacing
           color-picker
           float-property
           radio
           activated
           radio-group
           check-box
           checked
           notebook
           tab

           text-of

           deflayout
           custom-widget
           render-custom-widget
           initialize-custom-layout
           custom-widget-width
           custom-widget-height
           custom-widget-on-hover
           custom-widget-on-leave
           custom-widget-on-click
           custom-widget-on-move
           custom-widget-on-mouse-press
           custom-widget-on-mouse-release
           custom-widget-hovered-p
           custom-widget-clicked-p
           custom-widget-pressed-p
           discard-custom-widget-state
           transition-custom-widget-to
           custom-widget-instance

           next-keyboard-interaction
           next-mouse-interaction
           last-cursor-position
           next-character
           next-scroll

           docommands
           command-type
           scissor-origin
           scissor-width
           scissor-height
           line-origin
           line-end
           line-color
           line-thickness
           curve-origin
           curve-end
           curve-first-control-point
           curve-second-control-point
           curve-color
           curve-thickness
           rect-origin
           rect-width
           rect-height
           rect-stroke-color
           rect-stroke-thickness
           rect-rounding
           filled-rect-origin
           filled-rect-width
           filled-rect-height
           filled-rect-color
           filled-rect-rounding
           multi-color-rect-origin
           multi-color-rect-width
           multi-color-rect-height
           multi-color-rect-left-color
           multi-color-rect-top-color
           multi-color-rect-bottom-color
           multi-color-rect-right-color
           ellipse-origin
           ellipse-radius-x
           ellipse-radius-y
           ellipse-stroke-color
           ellipse-stroke-thickness
           filled-ellipse-origin
           filled-ellipse-radius-x
           filled-ellipse-radius-y
           filled-ellipse-color
           arc-origin
           arc-radius
           arc-start-angle
           arc-end-angle
           arc-stroke-color
           arc-stroke-thickness
           filled-arc-origin
           filled-arc-radius
           filled-arc-start-angle
           filled-arc-end-angle
           filled-arc-color
           triangle-origin
           triangle-second-vertex
           triangle-third-vertex
           triangle-stroke-color
           triangle-stroke-thickness
           filled-triangle-origin
           filled-triangle-second-vertex
           filled-triangle-third-vertex
           filled-triangle-color
           polygon-vertices
           polygon-stroke-color
           polygon-stroke-thickness
           filled-polygon-vertices
           filled-polygon-color
           polyline-vertices
           polyline-color
           polyline-thickness
           text-box-origin
           text-background-color
           text-foreground-color
           text-box-width
           text-box-height
           text-string
           image-origin
           image-width
           image-height
           image-color))
