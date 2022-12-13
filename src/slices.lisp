(in-package #:polymorph.data-structures)

(defstruct slice)

(polymorph.macros::%def (slice-vector (:include slice)) ()
  (host (and vector (not simple-array)) (error "Supply a vector"))
  (from ind)
  (to ind))


(defpolymorph (slice :inline t) ((of (and vector (not simple-array))) &key ((from ind) 0) ((to ind) (length of))) (values slice-vector &optional)
  (when (> to (length of))
    (error "Slice limits cannot exceed vector length"))
  (when (< to from)
    (error "Lower slice limit cannot exceed upper slice limit"))
  (slice-vector :host of :from from :to to))


(polymorph.macros::%def (slice-vec (:include slice)) ()
  (host vec (error "Supply a vector"))
  (from ind)
  (to ind))


(defpolymorph (slice :inline t) ((of vec) &key ((from ind) 0) ((to ind) (size of))) (values slice-vec &optional)
  (when (> to (size of))
    (error "Slice limits cannot exceed vector length"))
  (when (< to from)
    (error "Lower slice limit cannot exceed upper slice limit"))
  (slice-vec :host of :from from :to to))

(polymorph.macros::%def (slice-array (:include slice)) ()
  (host simple-array (error "Supply an array"))
  (from ind)
  (to ind))


(defpolymorph (slice :inline t) ((of simple-array) &key ((from ind) 0) ((to ind) (size of))) (values slice-array &optional)
  (when (> to (size of))
    (error "Slice limits cannot exceed vector length"))
  (when (< to from)
    (error "Lower slice limit cannot exceed upper slice limit"))
  (slice-array :host of :from from :to to))



(defmacro slicef (vector &rest args)
  (assert (<= 1 (length args) 3))
  (case (length args)
    (1 (assert (eql (first args) '--))
       `(slice ,vector))
    (2 (destructuring-bind (a b) args
         (assert (or (eql a '--) (eql b '--)))
         (if (eql a '--)
             `(slice ,vector :to ,b)
             `(slice ,vector :from ,a))))
    (3 (assert (eql (second args) '--))
       `(slice ,vector :from ,(first args) :to ,(third args)))))

(declaim (inline check-slice))
(defun check-slice (slice)
  (declare (optimize speed)
           (type slice slice))
  (when (or (> (from slice) (size (host slice)))
            (> (to slice) (size (host slice))))
    (error "Slice is invalidated")))

(defpolymorph (size :inline t) ((s slice)) (values ind &optional)
  (check-slice s)
  (- (to s) (from s)))

(defpolymorph (empty-p :inline t) ((s slice)) (values boolean &optional)
  (check-slice s)
  (= 0 (- (to s) (from s))))

(defpolymorph (at :inline :maybe) ((s slice) (i ind)) (values t &optional)
  (check-slice s)
  (at (host s) (+ (from s) i)))


(defpolymorph (at-safe :inline :maybe) ((s slice) (i ind)) (values t &optional)
  (check-slice s)
  (at-safe (host s) (+ (from s) i)))


(defpolymorph ((setf at) :inline :maybe) ((new t) (s slice) (i ind)) (values t &optional)
  (check-slice s)
  (setf (at (host s) (+ (from s) i)) new))

(defpolymorph ((setf at-safe) :inline :maybe) ((new t) (s slice) (i ind)) (values t &optional)
  (check-slice s)
  (setf (at-safe (host s) (+ (from s) i)) new))


(defpolymorph (front :inline t) ((s slice)) (values t &optional)
  (check-slice s)
  (front (host s)))

(defpolymorph (front-safe :inline t) ((s slice)) (values t &optional)
  (check-slice s)
  (front-safe (host s)))

(defpolymorph ((setf front) :inline t) ((new t) (s slice)) (values t &optional)
  (check-slice s)
  (setf (front (host s)) new))

(defpolymorph ((setf front-safe) :inline t) ((new t) (s slice)) (values t &optional)
  (check-slice s)
  (setf (front-safe (host s)) new))


(defpolymorph (back :inline t) ((s slice)) (values t &optional)
  (check-slice s)
  (back (host s)))

(defpolymorph (back-safe :inline t) ((s slice)) (values t &optional)
  (check-slice s)
  (back-safe (host s)))

(defpolymorph ((setf back) :inline t) ((new t) (s slice)) (values t &optional)
  (check-slice s)
  (setf (back (host s)) new))

(defpolymorph ((setf back-safe) :inline t) ((new t) (s slice)) (values t &optional)
  (check-slice s)
  (setf (back-safe (host s)) new))


(polymorph.macros::%def (iter-slice (:include iter)) (:copy)
  (seq slice (error "Supply a slice"))
  (:mut cur ind)
  (%size ind))

(defpolymorph (next :inline t) ((s iter-slice)) (values t &optional)
  (if (< (cur s) (%size s))
      (prog1 (at (seq s) (cur s))
        (incf (cur s)))
      (iter-stop)))

(defpolymorph iter ((s slice)) (values iter-slice &optional)
  (iter-slice :seq s :%size (size s)))
