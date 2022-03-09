
(in-package #:polymorph.data-structures)


(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defclass c-ring-buffer (ctype::ctype)  ;; ctype for ring-buffer
    ((%simplicity :initarg :simplicity ;; unused as of yet
                  :reader c-rb-simplicity
                  :type (member :simple :complex))
     (%elem-type :initarg :element-type
                 :reader c-rb-element-type)
     (%size :initarg :size  ;; also unused but will be used when we get to fixed size rb
            :reader c-rb-size
            :type ind)))

  (def rb ()
    (:mut begin ind)
    (:mut end ind)
    (:mut size ind)
    (:mut data (simple-array t (cl:*))
          (make-array 0))))

(defmacro define-ring-buffer (type &optional (default (default type))
                                     force-p)
  (unless (and (not force-p)
               (gethash (cons 'ring-buffer (if (listp type) type (list type)))
                        *unparamterize-name*))

    (let* ((buf-type (cons 'ring-buffer (if (listp type) type (list type))))
           (buf-code (gentemp "RING-BUFFER")))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct (,buf-code (:include rb)))

         (setf (gethash ',buf-type *unparamterize-name*) ',buf-code
               (gethash ',buf-code *paramterize-name*) ',buf-type

               (gethash ',buf-code *corresponding-ctype*)      ;; we create here a corresponding
               (make-instance 'c-ring-buffer :element-type ',type))))))  ;; ctype


(deftype ring-buffer (&optional typename)
  (if (eq typename 'cl:*)          
      `rb
      (progn
        (unless (gethash (cons 'ring-buffer (if (listp typename) typename (list typename)))
                        *unparamterize-name*)
         (ensure-ring-buffer typename))
       (gethash (cons 'ring-buffer
                      (if (listp typename) typename (list typename)))
                *unparamterize-name*))))


(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (define-ring-buffer t)

  (defun ensure-ring-buffer (type &optional (default (default type)))
    (eval `(define-ring-buffer ,type ,default))))

(defun ring-buffer (type &optional initial)
  (unless (gethash (cons 'ring-buffer (if (listp type) type (list type)))
                   *unparamterize-name*)
    (ensure-ring-buffer type))
  (let ((l (length initial)))
    (funcall (intern
              (format nil "MAKE-~s"
                      (gethash (cons 'ring-buffer (if (listp type) type (list type)))
                               *unparamterize-name*)))

             :size l
             :data (make-array l :element-type t
                                 :initial-contents initial))))

(define-compiler-macro ring-buffer (type &optional initial)
  (let ((type (eval type))
        (l (gensym "L"))
        (init (gensym "INIT")))
    (unless (gethash (cons 'ring-buffer (if (listp type) type (list type)))
                     *unparamterize-name*)
      (ensure-ring-buffer type))
    `(let* ((,init ,initial)
            (,l (length ,init)))
       (,(intern (format nil "MAKE-~s"
                         (gethash (cons 'ring-buffer (if (listp type) type (list type)))
                                  *unparamterize-name*)))

        :size ,l
        :data (make-array ,l :element-type t
                             :initial-contents ,init)))))
 


(defpolymorph (%resize :inline t) ((buf ring-buffer) (newsize ind)) null
  (let ((newdata (make-array newsize :element-type t))
        (olddata (data buf))
        (front (begin buf))
        (back (end buf)))
    (if (< front back)
        (progn (loop :for i :from front :below back
                     :do (setf (aref newdata (- i front)) (aref olddata i)))
               (setf (begin buf) 0
                     (end buf) (if (= newsize (- back front))
                                   0
                                   (- back front))))
        (let ((j 0))
          (loop :for i :from front :below (length olddata)
                :do (setf (aref newdata j) (aref olddata i))
                    (incf j))
          (loop :for i :from 0 :below back
                :do (setf (aref newdata j) (aref olddata i))
                    (incf j))
          (setf (begin buf) 0
                (end buf) (length olddata))))
    (setf (data buf) newdata)
    nil))


#||
(defpolymorph-compiler-macro %resize (ring-buffer ind) (buf newsize &environment env)
  (let* ((type (%form-type buf env))
         (elem-type (c-rb-element-type (gethash type *corresponding-ctype*)))
         (bufname (gensym "BUF"))
         (sizename (gensym "NEWSIZE")))

    `(let ((,bufname ,buf)
           (,sizename ,newsize))
       (let ((newdata (make-array ,sizename :element-type ',elem-type
                                            :initial-element ,(default elem-type)))
             (olddata (data ,bufname))
             (front (begin ,bufname))
             (back (end ,bufname)))
         (declare (type (simple-array ,elem-type) newdata))
         (if (< front back)
             (progn (loop :for i :from front :below back
                          :do (setf (aref newdata (- i front)) (aref olddata i)))
                    (setf (begin ,bufname) 0
                          (end ,bufname) (if (= ,sizename (- back front))
                                             0
                                             (- back front))))
             (progn (loop :for i :from 0 :below back
                          :do (setf (aref newdata i) (aref olddata i)))
                    (loop :for i :from front :below (length olddata)
                          :do (setf (aref newdata (+ i (- ,sizename (length olddata))))
                                    (aref olddata i)))
                    (setf (begin ,bufname) (the ind
                                                (- (begin ,bufname)
                                                   (- ,sizename (length olddata)))))))
         (setf (data ,bufname) newdata))
      nil)))
||#


(defpolymorph (front :inline t) ((buf ring-buffer)) t
  (if (= 0 (size buf))
      (error "Front requires a non-empty biffer")
      (aref (data buf) (begin buf))))

(defpolymorph-compiler-macro front (ring-buffer) (&whole form buf &environment env)
  (let ((type (%form-type buf env)))
    (if (alexandria::type= type 'rb)
        form
        (let ((elem-type (c-rb-element-type (gethash type *corresponding-ctype*)))
              (bufname (gensym "BUF")))
          `(let ((,bufname ,buf))
             (declare (type ,type ,bufname))
             (if (= 0 (size ,bufname))
                 (error "Front requires a non-empty biffer")
                 (the ,elem-type (aref (data ,bufname) (begin ,bufname)))))))))


(defpolymorph (setf front) ((new t) (buf ring-buffer)) t
  (if (= 0 (size buf))
      (error "Front requires a non-empty biffer")
      (setf (aref (data buf) (begin buf)) new)))

(defpolymorph-compiler-macro (setf front) (t ring-buffer) (&whole form new buf &environment env)
  (let ((type (%form-type buf env)))
    (if (alexandria::type= type 'rb)
        form
        (let ((newtype (%form-type new env))
              (elem-type (c-rb-element-type (gethash type *corresponding-ctype*)))
              (bufname (gensym "BUF"))
              (newname (gensym "NEW")))
          (assert (subtypep newtype elem-type env))
          `(let ((,bufname ,buf)
                 (,newname ,new))
             (declare (type ,type ,bufname)
                      (type ,newtype ,new))
             (if (= 0 (size ,bufname))
                 (error "Front requires a non-empty biffer")
                 (the ,newtype (setf (aref (data ,bufname) (begin ,bufname))))))))))



(defpolymorph push-front ((new t) (buf ring-buffer)) t
  (when (= (length (data buf)) (size buf))
    (%resize buf (the ind (* 2 (+ 1 (length (data buf)))))))
  (setf (begin buf)
        (the ind (if (= 0 (begin buf))
                     (- (length (data buf)) 1)
                     (- (begin buf) 1))))
  (setf (aref (data buf) (begin buf)) new)
  (setf (size buf) (the ind (1+ (size buf))))
  new)


(defpolymorph-compiler-macro push-front (t ring-buffer) (&whole form new buf &environment env)
  (let ((type (%form-type buf env)))
    (if (alexandria::type= type 'rb)
        form
        (let* ((newtype (%form-type new env))
               (elem-type (c-rb-element-type (gethash type *corresponding-ctype*)))
               (bufname (gensym "BUF"))
               (newname (gensym "NEW")))
          (assert (subtypep newtype elem-type env))
          `(let ((,bufname ,buf)
                 (,newname ,new))
             (declare (type ,newtype ,newname))
             (when (= (length (data ,bufname)) (size ,bufname))
               (%resize ,bufname (the ind (* 2 (+ 1 (length (data ,bufname)))))))
             (setf (begin ,bufname)
                   (the ind (if (= 0 (begin ,bufname))
                                (- (length (data ,bufname)) 1)
                                (- (begin ,bufname) 1))))
             (setf (aref (data ,bufname) (begin ,bufname)) ,newname)
             (setf (size ,bufname) (the ind (1+ (size ,bufname))))
             ,newname)))))



(defpolymorph pop-front ((buf ring-buffer)) t
  (if (= 0 (size buf))
      (error "Front requires a non-empty buffer")
      (prog1
          (aref (data buf) (begin buf))
        (when (> (length (data buf)) (* 3 (size buf)))
          (%resize buf (size buf)))
        (setf (begin buf)
              (the ind (if (= (1- (length (data buf))) (begin buf))
                           0
                           (1+ (begin buf)))))
        (setf (size buf) (the ind (1- (size buf)))))))


(defpolymorph-compiler-macro pop-front (ring-buffer) (&whole form buf &environment env)
  (let ((type (%form-type buf env)))
    (if (alexandria::type= type 'rb)
        form
        (let ((elem-type (c-rb-element-type (gethash type *corresponding-ctype*)))
              (bufname (gensym "BUF")))
          `(let ((,bufname ,buf))
             (if (= 0 (size ,bufname))
                 (error "Front requires a non-empty buffer")
                 (the ,elem-type (prog1
                                      (aref (data ,bufname) (begin ,bufname))
                                    (when (> (length (data ,bufname)) (* 3 (size ,bufname)))
                                        (%resize ,bufname (size ,bufname)))
                                    (setf (begin ,bufname)
                                          (the ind (if (= (1- (length (data ,bufname))) (begin ,bufname))
                                                       0
                                                       (1+ (begin ,bufname)))))
                                    (setf (size ,bufname) (the ind (1- (size ,bufname))))))))))))


(defpolymorph (back :inline t) ((buf ring-buffer)) t
  (if (= 0 (size buf))
      (error "Front requires a non-empty biffer")
      (aref (data buf) (end buf))))

(defpolymorph-compiler-macro back (ring-buffer) (&whole form buf &environment env)
  (let ((type (%form-type buf env)))
    (if (alexandria::type= type 'rb)
        form
        (let ((elem-type (c-rb-element-type (gethash type *corresponding-ctype*)))
              (bufname (gensym "BUF")))
          `(let ((,bufname ,buf))
             (declare (type ,type ,bufname))
             (if (= 0 (size ,bufname))
                 (error "Front requires a non-empty biffer")
                 (the ,elem-type (aref (data ,bufname) (end ,bufname)))))))))


(defpolymorph (setf back) ((new t) (buf ring-buffer)) t
  (if (= 0 (size buf))
      (error "Front requires a non-empty biffer")
      (setf (aref (data buf) (end buf)) new)))

(defpolymorph-compiler-macro (setf back) (t ring-buffer) (&whole form new buf &environment env)
  (let ((type (%form-type buf env)))
    (if (alexandria::type= type 'rb)
        form
        (let ((newtype (%form-type new env))
              (elem-type (c-rb-element-type (gethash type *corresponding-ctype*)))
              (bufname (gensym "BUF"))
              (newname (gensym "NEW")))
          (assert (subtypep newtype elem-type env))
          `(let ((,bufname ,buf)
                 (,newname ,new))
             (declare (type ,type ,bufname)
                      (type ,newtype ,new))
             (if (= 0 (size ,bufname))
                 (error "Front requires a non-empty biffer")
                 (the ,newtype (setf (aref (data ,bufname) (end ,bufname))))))))))

(defpolymorph push-back ((new t) (buf ring-buffer)) t
  (when (= (length (data buf)) (size buf))
    (%resize buf (* 2 (+ 1 (length (data buf))))))
  (setf (aref (data buf) (end buf)) new)
  (setf (end buf)
        (the ind (if (= (1- (length (data buf))) (end buf))
                     0
                     (1+ (end buf)))))
  (setf (size buf) (the ind (1+ (size buf))))
  new)


(defpolymorph-compiler-macro push-back (t ring-buffer) (&whole form new buf &environment env)
  (let ((type (%form-type buf env)))
    (if (alexandria::type= type 'rb)
        form
        (let* ((newtype (%form-type new env))
               (elem-type (c-rb-element-type (gethash type *corresponding-ctype*)))
               (bufname (gensym "BUF"))
               (newname (gensym "NEW")))
          (assert (subtypep newtype elem-type env))
          `(let ((,bufname ,buf)
                 (,newname ,new))
             (declare (type ,newtype ,newname))
             (when (= (length (data ,bufname)) (size ,bufname))
               (%resize ,bufname (the ind (* 2 (+ 1 (length (data ,bufname)))))))
             (setf (aref (data ,bufname) (end ,bufname)) ,newname)
             (setf (end ,bufname)
                   (the ind (if (= (1- (length (data ,bufname))) (end ,bufname))
                                0
                                (1+ (end ,bufname)))))
             (setf (size ,bufname) (the ind (1+ (size ,bufname))))
             ,newname)))))


(defpolymorph pop-back ((buf ring-buffer)) t
  (if (= 0 (size buf))
      (error "Back requires a non-empty buffer")
      (progn
        (when (> (length (data buf)) (* 3 (size buf)))
          (%resize buf (size buf)))
        (setf (end buf)
              (the ind (if (= 0 (end buf))
                           (1- (length (data buf)))
                           (1- (end buf)))))
        (setf (size buf) (the ind (1- (size buf))))
        (aref (data buf) (end buf)))))

(defpolymorph-compiler-macro pop-front (ring-buffer) (&whole form buf &environment env)
  (let ((type (%form-type buf env)))
    (if (alexandria::type= type 'rb)
        form
        (let ((elem-type (c-rb-element-type (gethash type *corresponding-ctype*)))
              (bufname (gensym "BUF")))
          `(let ((,bufname ,buf))
             (declare (type ,type ,bufname))
             (if (= 0 (size ,bufname))
                 (error "Front requires a non-empty buffer")
                 (the ,elem-type (progn
                                   (when (> (length (data ,bufname)) (* 3 (size ,bufname)))
                                     (%resize ,bufname (size ,bufname)))
                                   (setf (end ,bufname)
                                         (the ind (if (= 0 (end ,bufname))
                                                      (1- (length (data ,bufname)))
                                                      (1- (end ,bufname)))))
                                   (setf (size ,bufname) (the ind (1- (size ,bufname))))
                                   (aref (data ,bufname) (end ,bufname))))))))))

(defpolymorph empty-p ((buf ring-buffer)) boolean
  (= 0 (size buf)))


(defpolymorph (at :inline t) ((buf ring-buffer) &rest indexes)
    (values (or null t) &optional boolean)
  (let* ((error-policy (member :error indexes))
         (begin (begin buf))
         (size (size buf))
         (data (data buf))
         (indexes (if error-policy
                      (butlast (butlast indexes))
                      indexes))
         (ind (first indexes)))
    (flet ((%at ()
             (let ((maybe-pos (+ begin ind)))
               (if (>= maybe-pos (length data))
                   (aref data (- maybe-pos (length data)))
                   (aref data maybe-pos)))))

      (declare (inline %at))
      (assert (null (cdr indexes)))
      (if error-policy

          (if (second error-policy)
              (progn
                ;(unless (= 1 (length indexes))
                ;  (error 'simple-error :format-control "Only one index is allowed for ring-buffer") ;; FIXME later make this better
                (unless (< ind size)
                  (error 'simple-error
                   :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                   :format-arguments (list ind size size)))
                (%at))
              (if (< ind size)
                  (values (%at) t)
                  (values nil nil)))

          (progn
             ;(unless (= 1 (length indexes))
             ;  (error 'simple-error :format-control "Only one index is allowed for ring-buffer") ;; FIXME later make this better
            (unless (< ind size)
              (error 'simple-error
               :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
               :format-arguments (list (first indexes) size size)))
            (%at))))))


(defpolymorph-compiler-macro at (ring-buffer &rest) (&whole form buf &rest indexes &environment env)
 (let ((type (%form-type buf env)))
    (let ((elem-type
            (if (alexandria::type= type 'rb)
                t
                (c-rb-element-type (gethash type *corresponding-ctype*)))))
      (if (constantp (length indexes) env)
          (let ((error-policy (member :error indexes))
                (indname (gensym "IND"))
                (sizename (gensym "SIZE"))
                (bufname (gensym "BUF"))
                (%at (gensym "AT"))
                (dataname (gensym "DATA"))
                (beginname (gensym "BEGIN")))
            `(let* ((,bufname ,buf)
                    (,sizename (size ,bufname))
                    (,beginname (begin ,bufname))
                    (,dataname (data ,bufname)))
               (declare (type ,type ,bufname))
               (flet ((,%at (ind)
                        (declare (type ind ind))
                        (let ((maybe-pos (+ ,beginname ind)))
                          (if (>= maybe-pos (length ,dataname))
                              (aref ,dataname (- maybe-pos (length ,dataname)))
                              (aref ,dataname maybe-pos)))))
                 (declare (inline ,%at))
                 ,(if error-policy
                      (let ((indexes (butlast (butlast indexes))))
                        `(let ((,indname ,(first indexes)))
                           (if ,(second error-policy)
                               (the (values ,elem-type &optional)
                                    (progn
                                      (unless (< ,indname ,sizename)
                                        (error 'simple-error
                                               :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                                               :format-arguments (list ,indname ,sizename ,sizename)))
                                      (,%at ,indname)))
                               (the (values (or null ,elem-type) boolean &optional)
                                    (if (< ,indname ,sizename)
                                        (values (,%at ,indname) t)
                                        (values nil nil))))))
                      (progn
                        (assert (= 1 (length indexes)))
                        `(the (values ,elem-type &optional)
                              (progn
                                (unless (< ,(first indexes) ,sizename)
                                  (error 'simple-error
                                         :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                                         :format-arguments (list ,(first indexes) ,sizename ,sizename)))
                                (,%at ,(first indexes)))))))))

          `(the (values (or null ,elem-type) &optional boolean) ,form)))))


(defpolymorph (deep-copy :inline t) ((buf ring-buffer)) t
  (make-rb :begin (begin buf)
           :end (end buf)
           :size (size buf)
           :data (deep-copy (data buf))))

(defpolymorph-compiler-macro deep-copy (ring-buffer) (&whole form buf &environment env)
  (let ((type (%form-type buf env)))
    (if (alexandria::type= type 'rb)
        form
        (let ((elem-type (c-rb-element-type (gethash type *corresponding-ctype*)))
              (bufname (gensym "BUF"))
              (dataname (gensym "DATA"))
              (newname (gensym "NEW"))
              (i (gensym "I")))
          `(let* ((,bufname ,buf)
                  (,dataname (data ,bufname))
                  (,newname (make-array (length ,dataname) :element-type t)))
            (declare (type ,type ,bufname))
            (loop :for ,i :from 0 :below (length ,dataname)
                  :do (setf (aref ,newname ,i)
                            (deep-copy (the ,elem-type (aref ,dataname ,i)))))
            (the ,type (,(alexandria:symbolicate 'make- type)
                        :begin (begin ,bufname)
                        :end (end ,bufname)
                        :size (size ,bufname)
                        :data ,newname)))))))

(defpolymorph (shallow-copy :inline t) ((buf ring-buffer)) t
  (make-rb :begin (begin buf)
           :end (end buf)
           :size (size buf)
           :data (shallow-copy (data buf))))

(defpolymorph-compiler-macro shallow-copy (ring-buffer) (&whole form buf &environment env)
  (let ((type (%form-type buf env)))
    (if (alexandria::type= type 'rb)
        form
        (let ((elem-type (c-rb-element-type (gethash type *corresponding-ctype*)))
              (bufname (gensym "BUF"))
              (dataname (gensym "DATA"))
              (newname (gensym "NEW"))
              (i (gensym "I")))
          `(let* ((,bufname ,buf)
                  (,dataname (data ,bufname))
                  (,newname (make-array (length ,dataname) :element-type t)))
            (declare (type ,type ,bufname))
            (loop :for ,i :from 0 :below (length ,dataname)
                  :do (setf (aref ,newname ,i)
                            (the ,elem-type (aref ,dataname ,i))))
            (the ,type (,(alexandria:symbolicate 'make- type)
                        :begin (begin ,bufname)
                        :end (end ,bufname)
                        :size (size ,bufname)
                        :data ,newname)))))))


(defpolymorph (= :inline t) ((first ring-buffer) (second ring-buffer)) boolean
  (and (= (size first) (size second))
       (loop :with fdata := (data first)
             :with sdata := (data second)
             :for i := (begin first) :then (mod (1+ i) (length fdata))
             :for j := (begin second) :then (mod (1+ j) (length sdata))
             :until (= i (end first))
             :always (= (aref fdata i) (aref sdata j)))))



(defun ring-buffer-adhoc-test-fast ()
  (declare (optimize speed))
  (let ((b (ring-buffer 'fixnum)))
    (push-front 1 b)
    (push-front 2 b)
    (push-front 3 b)
    (push-front 4 b)
    (pop-front b)
    (pop-back b)
    (push-back 1 b)
    (push-back 99 b)
    (assert (and (= (at b 0) 3)
                 (= (at b 1) 2)
                 (= (at b 2) 1)
                 (= (at b 3) 99)))))


(defun ring-buffer-adhoc-test ()
  (let ((b (ring-buffer 'fixnum)))
    (push-front 1 b)
    (push-front 2 b)
    (push-front 3 b)
    (push-front 4 b)
    (pop-front b)
    (pop-back b)
    (push-back 1 b)
    (push-back 99 b)
    (assert (and (= (at b 0) 3)
                 (= (at b 1) 2)
                 (= (at b 2) 1)
                 (= (at b 3) 99)))))
