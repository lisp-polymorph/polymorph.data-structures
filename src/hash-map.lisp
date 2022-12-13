(in-package #:polymorph.data-structures)


(def hash-set-node ()
  (:mut val t)
  (:mut next-node (or null hash-set-node) nil)
  (:mut hash (unsigned-byte 62)))



(def hash-set ()
  (:mut data (simple-array (or (eql 0) hash-set-node) (cl:*)) (make-array 0))
  (:mut size ind))

(defpolymorph capacity ((container hash-set)) (values ind &optional)
  (* 3 (truncate (length (data container)) 4)))

(declaim (inline adjust-hs))
(defun adjust-hs (hs newsize)
  (declare (hash-set hs)
           (ind newsize)
           (optimize speed))
  (let* ((newdata (make-array newsize :element-type '(or (eql 0) hash-set-node)))
         (olddata (data hs)))
    (loop :for ls :across olddata
          :when (hash-set-node-p ls)
            :do (loop :with node :of-type (or null hash-set-node) := ls
                      :while node
                      :do (let ((newplace (mod (hash node) newsize)))
                            (if (hash-set-node-p (aref newdata newplace))
                                (psetf (next-node node) (the hash-set-node (aref newdata newplace))
                                       (aref newdata newplace) node
                                       node (next-node node))
                                (psetf (next-node node) nil
                                       (aref newdata newplace) node
                                       node (next-node node))))))
    (setf (data hs) newdata)))


(defpolymorph hashset ((type symbol)) (values hash-set &optional)
  (declare (ignorable type))
  (hash-set :data (make-array 7 :element-type '(or (eql 0) hash-set-node))))


(defpolymorph insert ((container hash-set) (new t)) (values t &optional)
  (when (> (* 4 (size container)) (* 3 (length (data container))))
    (adjust-hs container (* 2 (length (data container)))))
  (let* ((hash (cl:sxhash new))
         (place (mod hash (length (data container))))
         (head (aref (data container) place)))
    (if (hash-set-node-p head)
        (loop :while head
              :do (if (= (val (the hash-set-node head)) new)
                      (return nil)
                      (if (next-node head)
                          (setf head (next-node head))
                          (progn (setf (next-node head) (hash-set-node :hash hash :val new :next-node nil)
                                       (size container) (the ind (+ 1 (size container))))
                                 (return t)))))
        (progn (setf (aref (data container) place) (hash-set-node :hash hash :val new :next-node nil)
                    (size container) (the ind (+ 1 (size container))))
               t))))


(defpolymorph contains ((container hash-set) (val t)) (values boolean &optional)
  (let* ((hash (cl:sxhash val))
         (place (mod hash (length (data container))))
         (head (aref (data container) place)))
    (when (hash-set-node-p head)
      (loop :while head
            :do (if (= (val (the hash-set-node head)) val)
                    (return t)
                    (if (next-node head)
                        (setf head (next-node head))
                        (return nil)))))))


(defpolymorph empty-p ((container hash-set)) (values boolean &optional)
  (= 0 (size container)))

(defpolymorph erase ((container hash-set) (elem t)) (values boolean &optional)
  (when (< (* 4 (size container)) (length (data container)))
    (adjust-hs container (* 2 (size container))))
  (let* ((hash (cl:sxhash elem))
         (place (mod hash (length (data container))))
         (head (aref (data container) place)))
    (when (hash-set-node-p head)
      (if (= elem (val (the hash-set-node head)))
          (progn (setf (aref (data container) place) (or (next-node head) 0)
                       (size container) (the ind (- (size container) 1)))
                 t)
          (loop :while (next-node head)
                :do (if (= elem (val (next-node head)))
                        (progn (setf (next-node head) (next-node (next-node head))
                                     (size container) (the ind (- (size container) 1)))
                               (return t))
                        (setf head (next-node head)))
                :finally (return nil))))))


(defpolymorph clear ((container hash-set)) (values null &optional)
  (setf (data container) (make-array 7 :element-type '(or (eql 0) hash-set-node))
        (size container) 0))


(defmacro doset ((var set) &body body)
  (alexandria:with-gensyms (v i bucket)
    `(let ((,v (data ,set)))
       (loop :for ,i :from 0 :below (length ,v)
             :for ,bucket := (aref ,v ,i)
             :unless (eql ,bucket 0)
               :do (loop :while ,bucket
                         :do (let ((,var (val ,bucket)))
                               ,@body)
                             (setf ,bucket (next-node ,bucket)))))))

(defpolymorph (subsetp :inline :maybe) ((first hash-set) (second hash-set)) (values boolean &optional)
  (and (<= (size first) (size second))
       (progn
         (doset (x first)
           (unless (contains second x)
             (return-from subsetp nil)))
         t)))

(defpolymorph (supersetp :inline :maybe) ((first hash-set) (second hash-set)) (values boolean &optional)
  (subsetp second first))



(polymorph.macros::%def (iter-hashset (:include iter)) ()
  (:mut seq (or (eql 0) hash-set-node) (error "Supply a hashset"))
  (:mut ind ind)
  (toplevel (simple-array (or (eql 0) hash-set-node) (cl:*)) (error "Supply a hashset")))


(defpolymorph (next :inline t) ((hs iter-hashset)) (values t &optional)
  (let ((top (toplevel hs)))
    (if (eql 0 (seq hs))
        (loop (if (< (ind hs) (length top))
                  (progn
                    (setf (seq hs) (aref top (incf (ind hs))))
                    (unless (eql 0 (seq hs))
                      (return (val (seq hs)))))
                  (iter-stop)))
        (if (null (next-node (seq hs)))
            (let ((p (cl:position-if (lambda (x) (not (eql 0 x))) top :start (+ 1 (ind hs)))))
              (if p
                  (prog1 (val (seq hs))
                    (setf (ind hs) p (seq hs) (aref top p)))
                  (prog1 (val (seq hs))
                    (setf (ind hs) (length top) (seq hs) 0))))
            (prog1 (val (seq hs))
                   (setf (seq hs) (next-node (seq hs))))))))



(defpolymorph iter ((hs hash-set)) (values iter-hashset &optional)
  (iter-hashset :seq (aref (data hs) 0)
                :toplevel (data hs)))


(polymorph.macros::%def (iter-set-intersect (:include iter)) ()
  (fiter iter-hashset (error "Supply first hashset"))
  (sset hash-set (error "Supply second hashset")))


(defpolymorph (next :inline t) ((inter iter-set-intersect)) (values t &optional)
  (let ((it (fiter inter))
        (set (sset inter)))
    (loop (let ((val (next it)))
            (when (contains set val)
              (return val))))))

(defpolymorph (intersection :inline t) ((first hash-set) (second hash-set)) (values iter-set-intersect &optional)
  (iter-set-intersect :fiter (iter first) :sset second))


(polymorph.macros::%def (iter-set-difference (:include iter)) ()
  (fiter iter-hashset (error "Supply first hashset"))
  (sset hash-set (error "Supply second hashset")))


(defpolymorph (next :inline t) ((dif  iter-set-difference)) (values t &optional)
  (let ((it (fiter dif))
        (set (sset dif)))
    (loop (let ((val (next it)))
            (unless (contains set val)
              (return val))))))

(defpolymorph (difference :inline t) ((first hash-set) (second hash-set)) (values iter-set-difference &optional)
  (iter-set-difference :fiter (iter first) :sset second))


(polymorph.macros::%def (iter-set-symmetric-difference (:include iter)) ()
  (difchain chain-it (error "Supply a dif")))

(defpolymorph (next :inline t) ((symdif iter-set-symmetric-difference)) (values t &optional)
  (next (difchain symdif)))

(defpolymorph (symmetric-difference :inline t) ((first hash-set) (second hash-set)) (values iter-set-symmetric-difference &optional)
  (iter-set-symmetric-difference :difchain (chain (difference first second) (difference second first))))


(polymorph.macros::%def (iter-set-union (:include iter)) ()
  (uchain chain-it (error "Supply a dif")))

(defpolymorph (next :inline t) ((union iter-set-union)) (values t &optional)
  (next (uchain union)))

(defpolymorph (union :inline t) ((first hash-set) (second hash-set)) (values iter-set-union &optional)
  (if (> (size first) (size second))
      (iter-set-union :uchain (chain (iter first)
                                     (difference second first)))
      (iter-set-union :uchain (chain (iter second)
                                     (difference first second)))))


(defpolymorph collect ((it iter) (type (eql hashset)) &optional ((combine function) #'identity))
    (values hash-set &optional)
  (declare (ignorable type))
  (let ((res (hashset t)))
    (handler-case (loop (insert res (multiple-value-call combine (next it))))
      (iterator-end (c)
        (declare (ignore c))
        res))))

;; Hash map



(def hash-map-node ()
  (:mut key t)
  (:mut val t)
  (:mut next-node (or null hash-map-node) nil)
  (:mut hash (unsigned-byte 62)))



(def hash-map ()
  (:mut data (simple-array (or (eql 0) hash-map-node) (cl:*)) (make-array 7 :element-type '(or (eql 0) hash-map-node)))
  (:mut size ind)
  (:mut cache (or null hash-map-node) nil))

(defpolymorph capacity ((container hash-map)) (values ind &optional)
  (* 3 (truncate (length (data container)) 4)))


(declaim (inline adjust-hm))
(defun adjust-hm (hm newsize)
  (declare (hash-map hm)
           (ind newsize)
           (optimize speed))
  (let* ((newdata (make-array newsize :element-type '(or (eql 0) hash-map-node)))
         (olddata (data hm)))
    (loop :for ls :across olddata
          :when (hash-map-node-p ls)
            :do (loop :with node :of-type (or null hash-map-node) := ls
                      :while node
                      :do (let ((newplace (mod (hash node) newsize)))
                            (if (hash-map-node-p (aref newdata newplace))
                                (psetf (next-node node) (the hash-map-node (aref newdata newplace))
                                       (aref newdata newplace) node
                                       node (next-node node))
                                (psetf (next-node node) nil
                                       (aref newdata newplace) node
                                       node (next-node node))))))
    (setf (data hm) newdata)))


(defpolymorph hashmap ((key-type symbol) (val-type symbol)) (values hash-map &optional)
  (declare (ignorable key-type val-type))
  (hash-map))


(defpolymorph insert ((container hash-map) (key t) (val t)) (values t boolean &optional)
  (when (> (* 4 (size container)) (* 3 (length (data container))))
    (adjust-hm container (* 2 (length (data container)))))
  (if (and (cache container) (cl:eq key (key (cache container))))
      (multiple-value-prog1
          (values (val (cache container)) t)
        (setf (val (cache container)) val))
      (let* ((hash (cl:sxhash key))
             (place (mod hash (length (data container))))
             (head (aref (data container) place)))
        (if (hash-map-node-p head)
            (loop :while head
                  :do (if (= (key (the hash-map-node head)) key)
                          (return (multiple-value-prog1
                                     (values (val (the hash-map-node head)) t)
                                   (setf (val (the hash-map-node head)) val
                                         (cache container) head)))
                          (if (next-node head)
                              (setf head (next-node head))
                              (progn (setf (next-node head) (hash-map-node :hash hash :key key :val val :next-node nil)
                                           (size container) (the ind (+ 1 (size container)))
                                           (cache container) (next-node head))
                                     (return (values nil nil))))))
            (progn (setf (aref (data container) place) (hash-map-node :hash hash :key key :val val :next-node nil)
                         (size container) (the ind (+ 1 (size container)))
                         (cache container) (aref (data container) place))
                   (values nil nil))))))

;;TODO try-insert
;;

(defpolymorph contains-key ((container hash-map) (key t)) (values boolean &optional)
  (or (and (cache container) (cl:eq key (key (cache container))))
      (let* ((hash (cl:sxhash key))
             (place (mod hash (length (data container))))
             (head (aref (data container) place)))
        (when (hash-map-node-p head)
          (loop :while head
                :do (if (= (key (the hash-map-node head)) key)
                        (progn
                          (setf (cache container) head)
                          (return t))
                        (if (next-node head)
                            (setf head (next-node head))
                            (return nil))))))))


(defpolymorph empty-p ((container hash-map)) (values boolean &optional)
  (= 0 (size container)))

(defpolymorph erase ((container hash-map) (key t)) (values t boolean &optional)
  (when (< (* 4 (size container)) (length (data container)))
    (adjust-hm container (* 2 (size container))))
  (if (and (cache container) (cl:eq key (key (cache container))) (next-node (cache container)))
      (multiple-value-prog1
          (values (val (cache container)) t)
        (let ((c (cache container)))
          (setf (val c) (val (next-node c))
                (key c) (key (next-node c))
                (hash c) (hash (next-node c))
                (next-node c) (next-node (next-node c)))))
      (let* ((hash (cl:sxhash key))
             (place (mod hash (length (data container))))
             (head (aref (data container) place)))
        (declare (type (or (eql 0) hash-map-node) head))
        (when (hash-map-node-p head)
          (if (= key (key (the hash-map-node head)))
              (progn (setf (aref (data container) place) (or (next-node head) 0)
                           (size container) (the ind (- (size container) 1)))
                     (when (and (cache container) (cl:eq key (key (cache container))))
                        (setf (cache container) nil))
                     (values (val (the hash-map-node head)) t))
              (loop :while (next-node head)
                    :do (if (= key (key (next-node head)))
                            (return (multiple-value-prog1
                                        (values (val (next-node head)) t)
                                      (setf (next-node head) (next-node (next-node head))
                                            (size container) (the ind (- (size container) 1)))
                                      (when (and (cache container) (cl:eq key (key (cache container))))
                                        (setf (cache container) (next-node (next-node head))))))
                            (setf head (next-node head)))
                    :finally (return (values nil nil))))))))


(defpolymorph clear ((container hash-map)) (values null &optional)
  (setf (data container) (make-array 7 :element-type '(or (eql 0) hash-map-node))
        (size container) 0
        (cache container) nil))


(defpolymorph at ((container hash-map) (key t)) (values t &optional)
  (if (and (cache container) (cl:eq key (key (cache container))))
      (val (cache container))
      (let* ((hash (cl:sxhash key))
             (place (mod hash (length (data container))))
             (head (aref (data container) place)))
        (if (hash-map-node-p head)
            (loop :while head
                  :do (if (= (key (the hash-map-node head)) key)
                          (progn
                            (setf (cache container) head)
                            (return (val (the hash-map-node head))))
                          (if (next-node head)
                              (setf head (next-node head))
                              (error "Key is not present in the table"))))
            (error "Key is not present in the table")))))

(defpolymorph at-safe ((container hash-map) (key t)) (values t boolean &optional)
  (if (and (cache container) (cl:eq key (key (cache container))))
      (values (val (cache container)) t)
      (let* ((hash (cl:sxhash key))
             (place (mod hash (length (data container))))
             (head (aref (data container) place)))
        (if (hash-map-node-p head)
            (loop :while head
                  :do (if (= (key (the hash-map-node head)) key)
                          (progn
                            (setf (cache container) head)
                            (return (values (val (the hash-map-node head)) t)))
                          (if (next-node head)
                              (setf head (next-node head))
                              (return (values nil nil)))))
            (values nil nil)))))


(defpolymorph (setf at) ((new t) (container hash-map) (key t)) (values t &optional)
  (if (and (cache container) (cl:eq key (key (cache container))))
      (setf (val (cache container)) new)
      (let* ((hash (cl:sxhash key))
             (place (mod hash (length (data container))))
             (head (aref (data container) place)))
        (if (hash-map-node-p head)
            (loop :while head
                  :do (if (= (key (the hash-map-node head)) key)
                          (progn
                            (setf (cache container) head)
                            (return (setf (val (the hash-map-node head)) new)))
                          (if (next-node head)
                              (setf head (next-node head))
                              (error "Key is not present in the table"))))
            (error "Key is not present in the table")))))



(defpolymorph (setf at-safe) ((new t) (container hash-map) (key t)) (values t &optional)
  (insert container key new))

(polymorph.macros::%def (iter-hashmap (:include iter)) (:copy)
  (:mut seq (or (eql 0) hash-map-node) (error "Supply a hashmap"))
  (:mut ind ind)
  (toplevel (simple-array (or (eql 0) hash-map-node) (cl:*)) (error "Supply a hashmap")))


(defpolymorph (next :inline t) ((hm iter-hashmap)) (values t t &optional)
  (let ((top (toplevel hm)))
    (if (eql 0 (seq hm))
        (loop (if (< (ind hm) (length top))
                  (progn
                    (setf (seq hm) (aref top (incf (ind hm))))
                    (unless (eql 0 (seq hm))
                      (return (values (key (seq hm)) (val (seq hm))))))
                  (iter-stop)))
        (if (null (next-node (seq hm)))
            (let ((p (cl:position-if (lambda (x) (not (eql 0 x))) top :start (+ 1 (ind hm)))))
              (if p
                  (multiple-value-prog1 (values (key (seq hm)) (val (seq hm)))
                    (setf (ind hm) p (seq hm) (aref top p)))
                  (multiple-value-prog1 (values (key (seq hm)) (val (seq hm)))
                    (setf (ind hm) (length top) (seq hm) 0))))
            (multiple-value-prog1 (values (key (seq hm)) (val (seq hm)))
                   (setf (seq hm) (next-node (seq hm))))))))



(defpolymorph iter ((hs hash-map)) (values iter-hashmap &optional)
  (iter-hashmap :seq (aref (data hs) 0)
                :toplevel (data hs)))


(defpolymorph collect ((it iter) (type (eql hashmap)) &optional ((combine function) #'values))
    (values hash-map &optional)
  (declare (ignorable type))
  (let ((res (hashmap t t)))
    (handler-case (loop (multiple-value-bind (key val) (multiple-value-call combine (next it))
                          (insert res key val)))
      (iterator-end (c)
        (declare (ignore c))
        res))))

(defun %hmtest ()
  (declare (optimize speed))
  (let ((hm (hashmap 'fixnum 'fixnum)))
    (declare (type hash-map hm))
    (insert hm 1 2)
    (assert (= 2 (at-safe hm 1)))
    (insert hm 1 2)
    (assert (= 2 (at-safe hm 1)))
    (insert hm 3 4)
    (assert (= 4 (at-safe hm 3)))
    (insert hm 5 6)
    (assert (= 6 (at-safe hm 5)))
    (erase hm 5)
    (assert (= nil (at-safe hm 5)))
    (erase hm 1)
    (assert (= nil (at-safe hm 1)))))


(defun foo (array)
  (declare (optimize speed)
           (type (simple-array fixnum (1000000)) array))
  (let ((hm (hashmap 'fixnum 'fixnum)))
    (declare (type hash-map hm))
    (loop :for i :across array
          :do (setf (at-safe hm i) (+ i 1)))
    (loop :for i :across array
          :do (erase hm i))))


(defun bar (array)
  (declare (optimize speed)
           (type (simple-array fixnum (1000000)) array))
  (let ((hm (make-hash-table)))
    (loop :for i :across array
          :do (setf (gethash i hm) (+ i 1)))
    (loop :for i :across array
          :do (remhash i hm))))
