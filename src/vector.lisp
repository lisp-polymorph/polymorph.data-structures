(in-package #:polymorph.data-structures)



(def vec (:copy)
  (:mut data (simple-array t (cl:*)))
  (:mut size ind))

(defpolymorph capacity ((container vec)) (values ind &optional)
  (length (data container)))

(defpolymorph empty-p ((container vec)) (values boolean &optional)
  (= 0 (size container)))

(defpolymorph clear ((container vec)) (values null &optional)
  (unless (= 0 (size container))
    (setf (data container) (make-array 0)
          (size container) 0)))

(defpolymorph at ((container vec) (index ind)) (values t &optional)
  (if (< index (size container))
      (aref (data container) index)
      (error "Index not in vec bounds")))


(defpolymorph at-safe ((container vec) (index ind)) (values t boolean &optional)
  (if (< index (size container))
      (values (aref (data container) index) t)
      (values nil nil)))


(defpolymorph (setf at) ((new t) (container vec) (index ind)) (values t &optional)
  (if (< index (size container))
      (setf (aref (data container) index) new)
      (error "Index not in vec bounds")))


(defpolymorph (setf at-safe) ((new t) (container vec) (index ind)) (values t boolean &optional)
  (if (< index (size container))
      (values (setf (aref (data container) index) new) t)
      (values nil nil)))

(defpolymorph front ((container vec)) (values t &optional)
  (if (= 0 (size container))
      (error "Vec is empty")
      (aref (data container) 0)))

(defpolymorph front-safe ((container vec)) (values t boolean &optional)
  (if (= 0 (size container))
      (values nil nil)
      (values (aref (data container) 0) t)))

(defpolymorph (setf front) ((new t) (container vec)) (values t &optional)
  (if (= 0 (size container))
      (error "Vec is empty")
      (setf (aref (data container) 0) new)))

(defpolymorph (setf front-safe) ((new t) (container vec)) (values t boolean &optional)
  (if (= 0 (size container))
      (values nil nil)
      (values (setf (aref (data container) 0) new) t)))

(defpolymorph back ((container vec)) (values t &optional)
  (if (= 0 (size container))
      (error "Vec is empty")
      (aref (data container) (- (size container) 1))))

(defpolymorph back-safe ((container vec)) (values t boolean &optional)
  (if (= 0 (size container))
      (values nil nil)
      (values (aref (data container) (- (size container) 1)) t)))

(defpolymorph (setf back) ((new t) (container vec)) (values t &optional)
  (if (= 0 (size container))
      (error "Vec is empty")
      (setf (aref (data container) (- (size container) 1)) new)))

(defpolymorph (setf back-safe) ((new t) (container vec)) (values t boolean &optional)
  (if (= 0 (size container))
      (values nil nil)
      (values (setf (aref (data container) (- (size container) 1)) new) t)))




(defpolymorph push-back ((container vec) (new t)) (values ind &optional)
  (let ((s (size container)))
    (if (< s (length (data container)))
        (progn
          (setf (aref (data container) s) new)
          (setf (size container) (the ind (+ s 1))))
        (progn
          (let ((newdata (make-array (+ 1 (* 2 s)))))
            (replace newdata (data container))
            (setf (aref newdata s) new
                  (data container) newdata)
            (setf (size container) (the ind (+ 1 s))))))))


(defpolymorph pop-back ((container vec)) (values t &optional)
  (let ((s (size container)))
    (if (= 0 s)
        (error "Cannot pop from an empty vec")
        (let ((to-pop (aref (data container) (- s 1))))
          (setf (aref (data container) (- s 1)) 0
                (size container) (the ind (- s 1)))
          (when (< (size container) (floor (length (data container)) 4))
            (let ((newdata (make-array (* 2 (size container)))))
              (replace newdata (data container) :end1 (size container))
              (setf (data container) newdata)))
          to-pop))))


(defpolymorph pop-back-safe ((container vec)) (values t boolean &optional)
  (let ((s (size container)))
    (if (= 0 s)
        (values nil nil)
        (let ((to-pop (aref (data container) (- s 1))))
          (setf (aref (data container) (- s 1)) 0
                (size container) (the ind (- s 1)))
          (when (< (size container) (floor (length (data container)) 4))
            (let ((newdata (make-array (* 2 (size container)))))
              (replace newdata (data container) :end1 (size container))
              (setf (data container) newdata)))
          (values to-pop t)))))

(defpolymorph resize ((container vec) (newsize ind)) (values null &optional) ;; TODO check if newsize is proper
   (cond ((> newsize (size container))
          (if (> newsize (length (data container)))
              (let ((newdata (make-array newsize)))
                (replace newdata (data container) :end1 (size container))
                (setf (data container) newdata
                      (size container) newsize))
              (setf (size container) newsize)))
         ((< newsize (size container))
          (let ((newdata (make-array newsize)))
            (replace newdata (data container) :end1 newsize)
            (setf (data container) newdata
                  (size container) newsize))))
  nil)

(defpolymorph push-back-array ((container vec) (other simple-array)) (values ind &optional)
  (let ((s (size container))
        (cur (length (data container)))
        (new (length other)))
    (if (> (- cur s) new)
        (progn
          (replace (data container) other :start1 s)
          (incf (size container) new))
        (let ((newdata (make-array (+ cur new))))
          (replace newdata (data container))
          (replace newdata other :start1 s)
          (setf (data container) newdata)
          (incf (size container) new)))))



;; TODO If you pop back after a huge reserve, it immidiately reallocates
;; which may not be good. Think about what to do.
(defpolymorph reserve ((container vec) (newsize ind)) (values null &optional)
  (when (> newsize (length (data container)))
    (let ((newdata (make-array newsize)))
      (replace newdata (data container) :end1 (size container))
      (setf (data container) newdata))
    nil))


(defpolymorph shrink-to-fit ((container vec)) (values null &optional)
  (unless (= (size container) (length (data container)))
    (let ((newdata (make-array (size container))))
      (replace newdata (data container) :end1 (size container))
      (setf (data container) newdata))
    nil))



(polymorph.macros::%def (iter-vec (:include iter)) (:copy)
  (seq vec (error "Supply a vector"))
  (:mut cur ind))

(defpolymorph (next :inline t) ((v iter-vec)) (values t &optional)
  (if (< (cur v) (size (seq v)))
      (prog1 (at (seq v) (cur v))
        (incf (cur v)))
      (iter-stop)))

(defpolymorph iter ((v vec)) (values iter-vec &optional)
  (iter-vec :seq v))


(defpolymorph (collect :inline t) ((it iter) (type (eql vec)) &optional ((combine function) #'identity))
    (values vec &optional)
  (declare (ignorable type))
  (let ((res (vec)))
    (handler-case (loop (push-back res (multiple-value-call combine (next it))))
      (iterator-end ()
        res))))
