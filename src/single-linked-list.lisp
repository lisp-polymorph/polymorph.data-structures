(in-package #:polymorph.data-structures)




(def slnode (:eq :copy)
  (:mut val t)
  (:mut next-node (or null slnode) nil))


(def sllist ()
  (:mut head (or null slnode) nil)
  (:mut len ind)) ;; why did i call it len??????



(defpolymorph size ((container sllist)) (values ind &optional)
  (len container))

(defpolymorph capacity ((container sllist)) (values ind &optional)
  (len container))

(defpolymorph empty-p ((container sllist)) (values boolean &optional)
  (= 0 (len container)))


(defpolymorph front ((container sllist)) (values t &optional)
  (if (empty-p container)
      (error "Cannot show front of an empty list")
      (val (head container))))

(defpolymorph front-safe ((container sllist)) (values t boolean &optional)
  (if (empty-p container)
      (values nil nil)
      (values (val (head container)) t)))

(defpolymorph push-front ((container sllist) (new t)) (values t &optional)
  (setf (head container) (slnode :val new :next-node (head container)))
  (incf (len container))
  new)

(defpolymorph pop-front ((container sllist)) (values t &optional)
  (if (empty-p container)
      (error "Cannot pop from empty list")
      (let ((to-pop (val (head container))))
        (setf (head container) (next-node (head container)))
        (decf (len container))
        to-pop)))

(defpolymorph pop-front-safe ((container sllist)) (values t boolean &optional)
  (if (empty-p container)
      (values nil nil)
      (let ((to-pop (val (head container))))
        (setf (head container) (next-node (head container)))
        (decf (len container))
        (values to-pop t))))


(defpolymorph clear ((container sllist)) (values null &optional)
  (setf (head container) nil
        (len container) 0)
  nil)


;(defpolymorph insert-after ((container sllist)))
; TODO Needs iterators

(defpolymorph at ((container sllist) (index ind)) (values t &optional)
  (if (< index (len container))
      (let ((head (head container)))
        (loop :repeat index
              :do (setf head (next-node head)))
        (val head))
      (error 'simple-error :format-control "Index not in sllist bounds")))
   


(defpolymorph at-safe ((container sllist) (index ind)) (values t boolean &optional)
  (if (< index (len container))
      (let ((head (head container)))
        (loop :repeat index
              :do (setf head (next-node head)))
        (values (val head) t))
      (values nil nil)))
   

(defpolymorph (setf at) ((new t) (container sllist) (index ind)) (values t &optional)
  (if (< index (len container))
      (let ((head (head container)))
        (loop :repeat index
              :do (setf head (next-node head)))
        (setf (val head) new))
      (error 'simple-error :format-control "Index not in sllist bounds")))


(defpolymorph (setf at-safe) ((new t) (container sllist) (index ind)) (values t boolean &optional)
  (if (< index (len container))
      (let ((head (head container)))
        (loop :repeat index
              :do (setf head (next-node head)))
        (values (setf (val head) new) t))
      (values nil nil)))


(polymorph.macros::%def (iter-sllist (:include iter)) (:copy)
  (:mut seq (or null slnode) (error "Supply a list")))

(defpolymorph (next :inline t) ((l iter-sllist)) (values t &optional)
  (if (null (seq l))
      (iter-stop)
      (prog1 (val (seq l))
        (setf (seq l) (next-node (seq l))))))

(defpolymorph iter ((l sllist)) (values iter-sllist &optional)
  (iter-sllist :seq (head l)))



;; FIXME Think about how to do this better
(defpolymorph (collect :inline t) ((it iter) (type (eql sllist)) &optional ((combine function) #'identity))
    (values sllist &optional)
  (declare (ignorable type))
  (let ((res (sllist)))
    (setf (head res) (slnode :val nil)) ;;dirty hack, pffffff
    (let ((cur (head res)))
      (handler-case (loop (setf (next-node cur) (slnode :val (multiple-value-call combine (next it)))
                                (len res) (+ 1 (len res))
                                 cur (next-node cur)))
        (iterator-end (c)
          (declare (ignore c))
          (setf (head res) (next-node (head res)))
          res)))))
