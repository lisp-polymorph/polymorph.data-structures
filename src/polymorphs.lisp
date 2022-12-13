(in-package #:polymorph.data-structures)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *paramterize-name* (make-hash-table :test #'equalp))

  (defparameter *unparamterize-name* (make-hash-table :test #'equalp))

  (defparameter *corresponding-ctype* (make-hash-table :test #'equalp)))
;;(define-polymorphic-function anchor (container) :overwrite t)

;(define-polymorphic-function size (container) :overwrite t
;  :documentation "Returns the number of elements in CONTAINER.")
;(define-polymorphic-function (setf size) (new container) :overwrite t)

;(define-polymorphic-function empty-p (container)
;  :documentation "Returns (zerop (size container)).")

;(define-polymorphic-function at (container &rest indices)
;  :documentation "Returns the element specified by INDICES in CONTAINER.")
;(define-polymorphic-function (setf at) (new container &rest indices) :overwrite t)

;(define-polymorphic-function at-safe (container &rest indices)
;  :documentation "Optionally returns the element specified by INDICES in CONTAINER.")
;(define-polymorphic-function (setf at-safe) (new container &rest indices) :overwrite t)


;;; list API

;(define-polymorphic-function front (container)
;  :documentation "Returns the first element in CONTAINER.")
;(define-polymorphic-function (setf front) (new container))

;(define-polymorphic-function front-safe (container)
;  :documentation "Returns the optional first element in CONTAINER.")
;(define-polymorphic-function (setf front-safe) (new container))


;(define-polymorphic-function back (container)
;  :documentation "Returns the last element in CONTAINER.")
;(define-polymorphic-function (setf back) (new container))

;(define-polymorphic-function back-safe (container)
;  :documentation "Returns the optional last element in CONTAINER.")
;(define-polymorphic-function (setf back-safe) (new container))


;(define-polymorphic-function push-front (data container) ;; TODO swap arguments for chaining
;  :documentation "Pushes DATA to the front of CONTAINER. Returns DATA.")
;(define-polymorphic-function push-back (data container)
;  :documentation "Pushes DATA to the back of CONTAINER. Returns DATA.")

;; TODO determine whether to signal an error
;(define-polymorphic-function pop-front (container)
;  :documentation "Removes the first element of CONTAINER and returns it. A SIMPLE-ERROR is
;signalled if the container is empty.")
;(define-polymorphic-function pop-back (container)
;  :documentation "Removes the last element of CONTAINER and returns it. A SIMPLE-ERROR is
;signalled if the container is empty.")
;(define-polymorphic-function pop-front-safe (container)
;  :documentation "Optionally removes the first element of CONTAINER and returns it.")
;(define-polymorphic-function pop-back-safe (container)
;  :documentation "Optionally removes the last element of CONTAINER and returns it.")


;;; tree API

(define-polymorphic-function prev-node (node) :overwrite t
  :documentation "Returns the predecessor of NODE (may be NIL).")
(define-polymorphic-function (setf prev-node) (new node) :overwrite t)

(define-polymorphic-function next-node (node) :overwrite t
  :documentation "Returns the successor of NODE (may be NIL).")
(define-polymorphic-function (setf next-node) (new node) :overwrite t)

(define-polymorphic-function data (node) :overwrite t
  :documentation "Returns the data stored in NODE.")
(define-polymorphic-function (setf data) (data node) :overwrite t)



(define-polymorphic-function insert (container &rest args) :overwrite t)
