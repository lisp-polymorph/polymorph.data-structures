(in-package #:polymorph.data-structures)

(defparameter *paramterize-name* (make-hash-table :test #'equalp))

(defparameter *unparamterize-name* (make-hash-table :test #'equalp))

;;(define-polymorphic-function anchor (container) :overwrite t)

(define-polymorphic-function size (container) :overwrite t
  :documentation "Returns the number of elements in CONTAINER.")
(define-polymorphic-function (setf size) (new container) :overwrite t)

(define-polymorphic-function empty-p (container)
  :documentation "Returns (zerop (size container)).")

(define-polymorphic-function at (container &rest indices)
  :documentation "Returns the element specified by INDICES in CONTAINER.
The last two elements INDICES may be (:error t) or (:error nil), specifiying whether to signal
an error or return NIL if INDICES are invalid.")
(define-polymorphic-function (setf at) (new container) :overwrite t)

;;; list API

(define-polymorphic-function front (container)
  :documentation "Returns the first element in CONTAINER.")
(define-polymorphic-function (setf front) (new container))

(define-polymorphic-function back (container)
  :documentation "Returns the last element in CONTAINER.")
(define-polymorphic-function (setf back) (new container))

(define-polymorphic-function push-front (data container)
  :documentation "Pushes DATA to the front of CONTAINER. Returns DATA.")
(define-polymorphic-function push-back (data container)
  :documentation "Pushes DATA to the back of CONTAINER. Returns DATA.")

;; TODO determine whether to signal an error
(define-polymorphic-function pop-front (container)
  :documentation "Removes the first element of CONTAINER and returns it. A SIMPLE-ERROR is
signalled if the container is empty.")
(define-polymorphic-function pop-back (container)
  :documentation "Removes the last element of CONTAINER and returns it. A SIMPLE-ERROR is
signalled if the container is empty.")

;;; tree API

(define-polymorphic-function prev (node) :overwrite t
  :documentation "Returns the predecessor of NODE (may be NIL).")
(define-polymorphic-function (setf prev) (new node) :overwrite t)

(define-polymorphic-function next (node) :overwrite t
  :documentation "Returns the successor of NODE (may be NIL).")
(define-polymorphic-function (setf next) (new node) :overwrite t)

(define-polymorphic-function data (node) :overwrite t
  :documentation "Returns the data stored in NODE.")
(define-polymorphic-function (setf data) (data node) :overwrite t)
