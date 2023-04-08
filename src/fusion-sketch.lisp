(in-package #:polymorph.data-structures)

(defun itertest ()
  (arrows:-> (range 1)
             (filter #'evenp)
             (take-while (lambda (x) (< x 10000000)))
             (filter (lambda (x) (= 0 (mod x 1000000))))
             (skip 5)
             (collect 'vector)))







(COLLECT (RANGE 1 :TO 1000) 'VECTOR)


(collect (filter (enumerate (range 1 :to 1000)) (lambda (i x) (evenp x)))
  'vector
  #'cons)

(let ((res (make-array 0 :adjustable t :fill-pointer 0)))
  (let ((i 1))
    (let ((enum 0))
      (loop
        (progn (if (< i 1000)
                   (if (multiple-value-call (lambda (i x) (evenp x)) i enum)
                       (vector-push-extend (multiple-value-call #'cons i enum) res)
                       nil)
                   (return res))
               (incf i)
               (incf enum))))))


(let ((res (make-array 0 :adjustable t :fill-pointer 0)))
  (let ((i 1))
    (let ((enum 0))
      (loop
        (progn (if (< i 1000)
                   (let ((arg (list i enum)))
                     (if (apply (lambda (i x) (evenp x)) arg)
                         (vector-push-extend (apply #'cons arg) res)
                         nil))
                   (return res))
               (incf i)
               (incf enum))))))

(let ((res (make-array 0 :adjustable t :fill-pointer 0)))
  (let ((i 1))
    (loop
      (progn (if (< i 1000)
                 (vector-push-extend i res)
                 (return res))
             (incf i)))))


(let ((res (make-array 0 :adjustable t :fill-pointer 0))))
(vector-push-extend #result res)
res


(let ((i 1)
      (step 1))
  (loop
    (progn (if (< i 1000)
               body
               (return result))
           (incf i step)
           nil)))

;; ---------------
map
filter
take
take-while
skip
skip-while
enumerate
chunks

zip
chain

;; -------------------
range
iter-struct
repeat

;;---------------
;;

`(let (,@fields)
   (loop
     (progn (if (and ,conditions)
                (let ((args1 (list ,returns1)))
                  (let ((args2 (list ,returns2)))
                    .......
                     ,@body))
                (return ,result))
            (progn ,nexts)))
   (loop ...)
   ...)

returns

`(let (,@fields)
   (tagbody
      start-loop-1
      (progn (if (and ,conditions)
                ,@body
                (go end-loop-1))
            (progn ,nexts)
            (start-next-loop))
      end-loop-1
      start-loop-2
      (progn ....)
      end-loop-2)




  ,result)


(format t "~s ~s" x y)

(let ((v1 #(1 2 3) (a 0))
      (v2 #(1 2 3 4 5 6 7) (b 0))
      (v3 #(1 2 3 4 5) (c 0))
      (v4 #(1 2 3 4 5) (d 0))
      b1 b2 b3 b4)

  (tagbody
     start-loop1
     (format t "~s" (aref v1 a))
     (incf a)
     (if (= a 3)
         (setf b1 t))
     (if b3
         (go start-loop4)
         (go start-loop3))

     end-loop1
     start-loop2
     ()
     end-loop2
     start-loop3
     ()
     end-loop3
     start-loop4
     ()
     end-loop4))



(let ((v1 #(1 2 3)) (a 0)
      (v2 #(1 2 3 4 5 6 7)) (b 0)
      (v3 #(1 2 3 4 5)) (c 0)
      (v4 #(1 2 3 4 5)) (d 0))
  (handler-case
      (loop (let ((x1
                    (handler-case
                        (prog1 (aref v1 a) (incf a))
                      (error ()
                        (prog1 (aref v2 b)
                          (incf b))))))
              (let ((x2
                      (handler-case
                          (prog1 (aref v3 c) (incf c))
                        (error ()
                          (prog1 (aref v4 d)
                            (incf d))))))
                (multiple-value-call (lambda (x y) (format t "~s ~s~%" x y)) x1 x2))))
     (error ()
       (handler-case (loop ............)
         (error ()
           nil)))))




(let (fields)
  (handler-case
      (loop (multiple-value-bind vars1
                (handler-case
                    (multiple-value-prog1 (values returns1)
                      nexts1
                      (if condtions1
                          (iter-stop)))
                  (iterator-end ()
                    ....))
              (m-v-b vars2 ()
                ....
                body)))
    (iterator-end ()
      (handler-case (loop ....)
        ()))))

(let (fields)
  (handler-case
     (multiple-value-bind vars1
        (handler-case
            (multiple-value-prog1 (values returns1)
              nexts1
              (if condtions1
                  (iter-stop)))
          (iterator-end ()
            ....))
      (m-v-b vars2 ()
             ....
             body))))



(defclass fusion-info ()
  ((fields :initarg :fields
           :initform nil)
   (returns :initarg :returns
            :initform nil)
   (stop-cond :initarg :stops
              :initform nil)
   (filters :initarg :filters
            :initform nil)
   (nexts :initarg :nexts
          :initform nil)))


(defmethod nexts ((info fusion-info))
  (slot-value info 'nexts))

(defmethod code-nexts ((info fusion-info))
  `(progn ,@(slot-value info 'nexts)))



(define-polymorphic-function loop-fusion (type &rest args))

(defpolymorph loop-fusion ((type (eql chain)) form1 form2) t
  (declare (ignorable type))
  (multiple-value-bind (fields1 returns1 stop-conditions1 filters1 nexts1) (apply #'loop-fusion form1)
    (multiple-value-bind (fields2 returns2 stop-conditions2 filters2 nexts2) (apply #'loop-fusion form2)
      (values
       `(,@fields1 ,@fields2)
       `(if ,@stop-conditions1)
       `((or ,@stop-conditions1 ,@stop-conditions2))
       `((if ,@stop-conditions1 ,@filters1 ,@filters2))
       `((if ,@stop-conditions1 ,nexts1 ,nexts2))))))




(defpolymorph loop-fusion ((type (eql zip)) form1 form2) t
  (declare (ignorable type))
  (multiple-value-bind (fields1 returns1 stop-conditions1 filters1 nexts1) (apply #'loop-fusion form1)
    (multiple-value-bind (fields2 returns2 stop-conditions2 filters2 nexts2) (apply #'loop-fusion form2)
      (values
       `(,@fields1 ,@fields2)
       `(,@returns1 ,@returns2)
       `((and ,@stop-conditions1 ,@stop-conditions2))
       `((and ,@filters1 ,@filters2))
       `(progn ,nexts1 ,nexts2)))))




(collect (zip (range 1 :to 100)
              (range 100 :to 200))
  'vector)



(defpolymorph loop-fusion ((type (eql range)) (from t) &key (to t) (stp 1)) t
  (declare (ignorable type))
  (let ((from-name (gensym "FROM"))
        (to-name (gensym "TO"))
        (step-name (gensym "STP")))

    (values
     `((,from-name ,from)
       (,to-name ,to)
       (,step-name ,stp))
     `(,from-name)
     `((< ,from-name ,to-name))
     `(t)
     `(progn (incf ,from-name ,step-name)))))




(defpolymorph loop-fusion ((type (eql collect)) (into-type (eql vector)) combine form) t
  (declare (ignorable type into-type))
  (multiple-value-bind (fields stop-conditions filters nexts) (apply #'loop-fusion form)
    (let ((res (gensym "RES")))
      `(let (,@fileds
             (,res (make-array 0 :adjustable t :fill-pointer 0)))
         (loop
               (progn
                 (m-v-b () ..
                   (vector-push-extend () ,res)
                   (return))
                 ,nexts))
         ,res))))

























(defun itertest2 ()
  (declare (optimize speed))
  (COLLECT
     (the polymorph.traversable::skipper
          (SKIP
           (the polymorph.traversable::filter-pred
                (FILTER
                 (the polymorph.traversable::taker-while
                      (TAKE-WHILE
                       (the polymorph.traversable::filter-pred
                            (FILTER
                             (the polymorph.traversable::range-inf
                                  (RANGE 1))
                             #'EVENP))
                       (LAMBDA (X) (declare (real x)) (< X 10000000))))
                 (LAMBDA (X) (declare (real x)) (= 0 (MOD X 1000000)))))
           5))
    'VECTOR))


(defun looptest ()
  (let ((i 1)
        (stp 1)
        (skips 0))
    (let ((res (make-array 0 :adjustable t :fill-pointer 0)))
      (loop (progn
              (if (funcall #'evenp i)
                 (if (funcall (lambda (x) (< x 10000000)) i)
                     (if (funcall (lambda (x) (= 0 (mod x 1000000))) i)
                         (progn (if (> skips 4)
                                   (vector-push-extend i res))
                                (incf skips)))
                     (return-from looptest res)))
              (incf i stp))))))
