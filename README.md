# bodge-ui

High-level immediate mode user interface library based on Nuklear IM GUI library.

## Example

Have a look at [example](bodge-ui-example.org) source.

## Install

```lisp
;; Add cl-bodge distribution into quicklisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt" :replace t :prompt nil)

;; Update main dist just in case
(ql:update-dist "quicklisp")

;; Load the example
(ql:quickload :bodge-ui/example)
;; And run it!
(bodge-ui.example:run)
```
