
(in-package #:polymorph.data-structures)


(define-polymorphic-function make (base-type &rest args))

(defpolymorph (make :inline t) ((base-type (eql simple-array))
                                &key ((size ind) 0)
                                     ((elem-type symbol) t)
                                     ((init-elem t) 0))
    (values simple-array &optional)
  (declare (ignorable base-type))
  (make-array size :element-type elem-type :initial-element init-elem))

(defpolymorph (make :inline t) ((base-type (eql simple-array))
                                &key ((init-seq list) '())
                                     ((elem-type symbol) t))
    (values simple-array &optional)
  (declare (ignorable base-type))
  (make-array (length init-seq) :element-type elem-type :initial-contents init-seq))


(defpolymorph (make :inline t) ((base-type (eql list))
                                &key ((size ind) 0)
                                ((init-elem t) 0))
    (values list &optional)
  (declare (ignorable base-type))
  (make-list size :initial-element init-elem))

(defpolymorph (make :inline t) ((base-type (eql list))
                                &key ((init-seq list) '()))
    (values list &optional)
  (declare (ignorable base-type))
  (copy-list init-seq))


(defpolymorph (make :inline t) ((base-type (eql vector))
                                &key ((size ind) 0)
                                ((elem-type symbol) t)
                                ((init-elem t) 0))
    (values vector &optional)
  (declare (ignorable base-type))
  (make-array size :element-type elem-type :initial-element init-elem
              :adjustable t :fill-pointer size))

(defpolymorph (make :inline t) ((base-type (eql vector))
                                &key ((init-seq list) '())
                                     ((elem-type symbol) t))
    (values vector &optional)
  (declare (ignorable base-type))
  (let ((l (length init-seq)))
    (make-array l :element-type elem-type :initial-contents init-seq
                :adjustable t :fill-pointer l)))



(defpolymorph (make :inline t) ((base-type (eql vec))
                                &key ((size ind) 0)
                                ((elem-type symbol) t)
                                ((init-elem t) 0))
    (values vec &optional)
  (declare (ignorable base-type))
  (let ((data (make-array size :initial-element init-elem :element-type elem-type)))
    (vec :data data :size size)))

(defpolymorph (make :inline t) ((base-type (eql vec))
                                &key ((init-seq list) '())
                                     ((elem-type symbol) t))
    (values vec &optional)
  (declare (ignorable base-type))
  (let ((data (make-array (length init-seq) :element-type elem-type :initial-contents init-seq)))
    (vec :data data :size (length data))))


(defpolymorph (make :inline t) ((base-type (eql hashset))
                                &key ((elem-type symbol) t)
                                     ((init-seq list) '()))
    (values hash-set &optional)
  (declare (ignorable base-type elem-type))
  (let ((s (hash-set :data (make-array (length init-seq)))))
    (loop :for x :in init-seq
          :do (insert s x))
    s))

(defpolymorph (make :inline t) ((base-type (eql hashmap))
                                &key ((elem-types list) (list t t))
                                     ((init-seq list) '()))
    (values hash-map &optional)
  (declare (ignorable base-type elem-types))
  (let ((m (hash-map :data (make-array (length init-seq)))))
    (loop :for (k v) :in init-seq
          :do (insert m k v))
    m))
