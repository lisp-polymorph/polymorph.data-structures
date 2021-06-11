(in-package #:polymorph.data-structures)

(defparameter *paramterize-name* (make-hash-table :test #'equalp))

(defparameter *unparamterize-name* (make-hash-table :test #'equalp))

;(define-polymorphic-function front (container))
;(define-polymorphic-function back (container))
;(define-polymorphic-function (setf front) (new container))
;(define-polymorphic-function (setf back) (new container))
(define-polymorphic-function push-front (data container))
(define-polymorphic-function push-back (data container))
(define-polymorphic-function pop-front (container))
(define-polymorphic-function pop-back (container))
;(define-polymorphic-function empty-p (container))


(define-polymorphic-function prev (node) :overwrite t)
(define-polymorphic-function data (node) :overwrite t)
(define-polymorphic-function next (node) :overwrite t)

(define-polymorphic-function (setf prev) (new node) :overwrite t)
(define-polymorphic-function (setf data) (data node) :overwrite t)
(define-polymorphic-function (setf next) (new node) :overwrite t)

(define-polymorphic-function anchor (container) :overwrite t)
;(define-polymorphic-function size (container) :overwrite t)
(define-polymorphic-function (setf size) (new container) :overwrite t)
