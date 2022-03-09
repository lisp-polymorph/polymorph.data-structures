
(in-package #:polymorph.data-structures)


(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defclass c-dl-list (ctype::ctype)
    ((%elem-type :initarg :element-type
                 :reader c-dl-element-type)))

  (def node ()
    (:mut prev node (error "Recursive allocation is not possible"))
    (:mut data t)
    (:mut next node (error "Recursive allocation is not possible")))

  (def dl ()
    (:mut anchor node (allocate-instance (find-class 'node)))
    (:mut size ind)))


(defmacro define-dl-list (type &optional (default (default type))
                                 force-p)
  (unless (and (not force-p)
               (gethash (cons 'dl-list (if (listp type) type (list type)))
                        *unparamterize-name*))

    (let* ((dl-type (cons 'dl-list (if (listp type) type (list type))))
           (dl-code (gentemp "DL-LIST")))

      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct (,dl-code (:include dl)))

         (setf (gethash ',dl-type *unparamterize-name*) ',dl-code
               (gethash ',dl-code *paramterize-name*) ',dl-type

               (gethash ',dl-code *corresponding-ctype*)      ;; we create here a corresponding
               (make-instance 'c-dl-list :element-type ',type))))))


(deftype dl-list (&optional typename)
  (if (eq typename 'cl:*)
      `dl
      (progn
        (unless (gethash (cons 'dl-list (if (listp typename) typename (list typename)))
                        *unparamterize-name*)
         (ensure-ring-buffer typename))
       (gethash (cons 'dl-list
                      (if (listp typename) typename (list typename)))
                *unparamterize-name*))))



(defmethod print-object ((dl dl) stream)
  (loop :initially (format stream "#D(")
        :with node := (anchor dl)
        :for next := (next node) :then (next next)
        :until (eq next node)
        :do (format stream "~s " (data next))
        :finally (format stream ")")))

(defpolymorph front ((dl-list dl-list)) t
  (if (= 0 (size dl-list))
      (error "Front requires a non-empty list")
      (data (next (anchor dl-list)))))

(defpolymorph-compiler-macro front (dl-list) (&whole form dl-list &environment env)
  (let ((type (%form-type dl-list env)))
    (if (alexandria::type= type 'dl)
        form
        (let ((elem-type (c-dl-element-type (gethash type *corresponding-ctype*))))
           `(the ,elem-type ,form)))))

(defpolymorph (setf front) ((new t) (dl-list dl-list)) t
  (if (= 0 (size dl-list))
      (error "Front requires a non-empty list")
      (setf (data (next (anchor dl-list))) new)))

(defpolymorph-compiler-macro (setf front) (t dl-list) (&whole form new dl-list &environment env)
  (let ((type (%form-type dl-list env)))
    (if (alexandria::type= type 'dl)
        form
        (let ((elem-type (c-dl-element-type (gethash type *corresponding-ctype*)))
              (newtype (%form-type new env)))
          (assert (subtypep newtype elem-type env))
          `(the ,elem-type ,form)))))

(defpolymorph back ((dl-list dl-list)) t
  (data (prev (anchor dl-list))))

(defpolymorph-compiler-macro back (dl-list) (&whole form dl-list &environment env)
  (let ((type (%form-type dl-list env)))
    (if (alexandria::type= type 'dl)
        form
        (let ((elem-type (c-dl-element-type (gethash type *corresponding-ctype*))))
           `(the ,elem-type ,form)))))

(defpolymorph (setf back) ((new t) (dl-list dl-list)) t
  (if (= 0 (size dl-list))
      (error "Back requires a non-empty list")
      (setf (data (prev (anchor dl-list))) new)))

(defpolymorph-compiler-macro (setf back) (t dl-list) (&whole form new dl-list &environment env)
  (let ((type (%form-type dl-list env)))
    (if (alexandria::type= type 'dl)
        form
        (let ((elem-type (c-dl-element-type (gethash type *corresponding-ctype*)))
              (newtype (%form-type new env)))
          (assert (subtypep newtype elem-type env))
          `(the ,elem-type ,form)))))


(defpolymorph empty-p ((dl-list dl-list)) boolean
  (= 0 (size dl-list)))

(defpolymorph push-front ((data t) (dl-list dl-list)) t
  (let* ((anchor (anchor dl-list))
         (current (next anchor))
         (new-node (make-node :data data :next current :prev anchor)))
    (setf (prev current) new-node
          (next anchor) new-node)
    (incf (size dl-list))
    data))


(defpolymorph-compiler-macro push-front (t dl-list) (&whole form data dl-list &environment env)
  (let ((type (%form-type dl-list env)))
    (if (alexandria::type= type 'dl)
        form
        (let ((elem-type (c-dl-element-type (gethash type *corresponding-ctype*)))
              (datatype (%form-type data env)))
          (assert (subtypep datatype elem-type env))
          `(the ,elem-type ,form)))))



(defpolymorph push-back ((data t) (dl-list dl-list)) t
  (let* ((anchor (anchor dl-list))
         (current (prev anchor))
         (new-node (make-node :data data :prev current :next anchor)))
    (setf (next current) new-node
          (prev anchor) new-node)
    (incf (size dl-list))
    data))

(defpolymorph-compiler-macro push-back (t dl-list) (&whole form data dl-list &environment env)
  (let ((type (%form-type dl-list env)))
    (if (alexandria::type= type 'dl)
        form
        (let ((elem-type (c-dl-element-type (gethash type *corresponding-ctype*)))
              (datatype (%form-type data env)))
          (assert (subtypep datatype elem-type env))
          `(the ,elem-type ,form)))))



(defpolymorph pop-front ((dl-list dl-list)) t
  (if (= 0 (size dl-list))
      (error 'simple-error :format-control "Cannot pop from an empty list")
      (let* ((anchor (anchor dl-list))
             (current (next anchor))
             (data (data current)))
        (setf (prev (next current)) anchor
              (next anchor) (next current))
        (decf (size dl-list))
        data)))

(defpolymorph-compiler-macro pop-front (dl-list) (&whole form dl-list &environment env)
  (let ((type (%form-type dl-list env)))
    (if (alexandria::type= type 'dl)
        form
        (let ((elem-type (c-dl-element-type (gethash type *corresponding-ctype*))))
          `(the ,elem-type ,form)))))




(defpolymorph pop-back ((dl-list dl-list)) t
  (if (= 0 (size dl-list))
      (error 'simple-error :format-control "Cannot pop from an empty list")
      (let* ((anchor (anchor dl-list))
             (current (prev anchor))
             (data (data current)))
        (setf (next (prev current)) anchor
              (prev anchor) (prev current))
        (decf (size dl-list))
        data)))

(defpolymorph-compiler-macro pop-back (dl-list) (&whole form dl-list &environment env)
  (let ((type (%form-type dl-list env)))
    (if (alexandria::type= type 'dl)
        form
        (let ((elem-type (c-dl-element-type (gethash type *corresponding-ctype*))))
          `(the ,elem-type ,form)))))


(defpolymorph (at :inline t) ((dl-list dl-list) &rest indexes)
    (values (or null t) &optional boolean)
  (let* ((error-policy (member :error indexes))
         (size (size dl-list))
         (indexes (if error-policy
                      (butlast (butlast indexes))
                      indexes))
         (ind (first indexes)))
    (flet ((%at (ind)
             (declare (type ind ind))
             (loop :with node := (anchor dl-list)
                   :for next := (next node) :then (next next)
                   :until (= 0 ind)
                   :do (decf ind)
                   :finally (return (data next)))))
      (declare (inline %at))
      (assert (null (cdr indexes)))
      (if error-policy
          (if (second error-policy)
              (progn
                (unless (< ind size)
                  (error 'simple-error
                   :format-control "Invalid index ~s for (DL-LIST ~s), should be a non-negative integer below ~s."
                   :format-arguments (list ind size size)))
                (%at ind))
              (if (< ind size)
                  (values (%at ind) t)
                  (values nil nil)))
          (progn
              (unless (< ind size)
                (error 'simple-error
                 :format-control "Invalid index ~s for (DL-LIST ~s), should be a non-negative integer below ~s."
                 :format-arguments (list ind size size)))
              (%at ind))))))


(defpolymorph-compiler-macro at (dl-list &rest) (&whole form dl-list &rest indexes &environment env)
  (let ((type (%form-type dl-list env)))
    (let ((elem-type
            (if (alexandria::type= type 'dl)
                t
                (c-dl-element-type (gethash type *corresponding-ctype*)))))
      (if (constantp (length indexes) env)
          (let* ((error-policy (member :error indexes))
                 (indexes (if error-policy
                              (butlast (butlast indexes))
                              indexes))
                 (dl-listname (gensym "DLLS"))
                 (indname (gensym "IND"))
                 (%at (gensym "AT"))
                 (sizename (gensym "SIZE")))
            (assert (null (cdr indexes)))
            `(let* ((,dl-listname ,dl-list)
                    (,sizename (size ,dl-listname))
                    (,indname ,(first indexes)))
               (flet ((,%at (ind)
                        (declare (type ind ind))
                        (loop :with node := (anchor ,dl-listname)
                              :for next := (next node) :then (next next)
                              :until (= 0 ind)
                              :do (decf ind)
                              :finally (return (data next)))))
                 (declare (inline ,%at))
                 ,(if error-policy
                      `(if ,(second error-policy)
                           (the (values ,elem-type &optional)
                                (progn
                                  (unless (< ,indname ,sizename)
                                    (error 'simple-error
                                     :format-control "Invalid index ~s for (DL-LIST ~s), should be a non-negative integer below ~s."
                                     :format-arguments (list ,indname ,sizename ,sizename)))
                                  (,%at ,indname)))
                           (the (values (or null ,elem-type) boolean &optional)
                                (if (< ,indname ,sizename)
                                    (values (,%at ,indname) t)
                                    (values nil nil))))
                      `(the (values ,elem-type &optional)
                            (progn
                              (unless (< ,indname ,sizename)
                                (error 'simple-error
                                   :format-control "Invalid index ~s for (DL-LIST ~s), should be a non-negative integer below ~s."
                                   :format-arguments (list ,indname ,sizename ,sizename)))
                              (,%at ,indname)))))))
          `(the (values (or null ,elem-type) &optional boolean) ,form)))))


(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defun ensure-dl-list (type &optional (default (default type)))
    (eval `(define-dl-list ,type ,default))))


#||
(defmacro do-nodes (names dl-list &environment env &body body)
  (destructuring-bind (nodename &key (data 'data) (prev 'prev) (next 'next)) names
      (let* ((type (%form-type dl-list env))
             (truetype (gethash type *paramterize-name*))
             (element-type (or (second truetype) t)))
        `(loop :with anchor := (anchor ,dl-list)
               :for ,nodename := (next anchor) :then (next ,nodename)
               :until (eq ,nodename anchor)
               :do (symbol-macrolet ((,data (the ,element-type (data ,nodename)))
                                     (,prev (prev ,nodename))
                                     (,next (next ,nodename)))
                     ,@body)))))
||#

(defun dl-list (type)
  (unless (gethash (cons 'dl-list (if (listp type) type (list type))) *unparamterize-name*)
    (ensure-dl-list type))
  (let* ((res (funcall
               (intern
                (format nil "MAKE-~s" (gethash (cons 'dl-list
                                                     (if (listp type) type (list type)))
                                               *unparamterize-name*)))))
         (anchor (anchor res)))
    (setf (prev anchor) anchor
          (next anchor) anchor)
    res))

(define-compiler-macro dl-list (type)
  (let ((type (eval type))
        (res (gensym "RES"))
        (anchor (gensym "ANCHOR")))
    (unless (gethash (cons 'dl-list (if (listp type) type (list type)))
                     *unparamterize-name*)
      (ensure-dl-list type))
    `(let* ((,res (,(intern
                     (format nil "MAKE-~s"
                             (gethash (cons 'dl-list (if (listp type) type (list type)))
                                      *unparamterize-name*)))))
            (,anchor (anchor ,res)))
       (setf (prev ,anchor) ,anchor
             (next ,anchor) ,anchor)
       ,res)))

(defun dl-list-adhoc-test-fast ()
  (declare (optimize speed))
  (let ((b (dl-list 'fixnum)))
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


(defun dl-list-adhoc-test ()
  (let ((b (dl-list 'fixnum)))
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
