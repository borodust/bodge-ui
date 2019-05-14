(asdf:defsystem :bodge-ui
  :description "Heavily lispified and opinionated wrapper over Nuklear immediate mode UI library"
  :version "1.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (bodge-utilities bodge-memory bodge-math
                               alexandria cl-muth nuklear-blob bodge-nuklear)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "ui")
               (:file "style")
               (:file "input-source")
               (:file "rendering")
               (:module elements
                :serial t
                :components ((:file "elements")
                             (:file "layout")
                             (:file "button")
                             (:file "check-box")
                             (:file "color-box")
                             (:file "color-picker")
                             (:file "combo-box")
                             (:file "label")
                             (:file "list-select")
                             (:file "menu-bar")
                             (:file "notebook")
                             (:file "option")
                             (:file "property")
                             (:file "radio")
                             (:file "spacing")
                             (:file "text-edit")
                             (:file "window")
                             (:file "custom-layout")
                             (:file "custom-widget")))))


(asdf:defsystem :bodge-ui/renderer
  :description "Default nuklear renderer for bodge-ui system"
  :version "1.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (bodge-glad glad-blob nuklear-blob bodge-nuklear bodge-ui cl-opengl)
  :pathname "renderer/"
  :serial t
  :components ((:file "renderer")))


(asdf:defsystem :bodge-ui/example
  :description "bodge-ui example"
  :version "1.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (bodge-host bodge-ui bodge-ui/renderer)
  :pathname "example/"
  :serial t
  :components ((:file "packages")
               (:file "app")
               (:file "ui")))
