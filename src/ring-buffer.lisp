
(in-package #:polymorph.data-structures)
#||
(defmacro define-ring-buffer (type &optional (default (default type))
                                     force-p)
  (unless (and (not force-p)
               (gethash (cons 'ring-buffer (if (listp type) type (list type)))
                        *unparamterize-name*))
    (let* ((buf-type (cons 'ring-buffer (if (listp type) type (list type))))
           (buf-code (gentemp "RING-BUFFER")))

      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct ,buf-code
           (begin 0 :type ind)
           (end 0 :type ind)
           (size 0 :type ind)
           (data (make-array 0 :element-type ',type :initial-element ,default)
            :type (simple-array ,type (cl:*))))

         (setf (gethash ',buf-type *unparamterize-name*) ',buf-code
               (gethash ',buf-code *paramterize-name*) ',buf-type)

         (defpolymorph begin ((buf ,buf-code)) (values ind &optional)
           (,(intern (format nil "~s-BEGIN" buf-code)) buf))
         (defpolymorph (setf begin) ((new ind) (buf ,buf-code)) ind
           (setf (,(intern (format nil "~s-BEGIN" buf-code)) buf) new))
         (defpolymorph end ((buf ,buf-code)) (values ind &optional)
           (,(intern (format nil "~s-END" buf-code)) buf))
         (defpolymorph (setf end) ((new ind) (buf ,buf-code)) ind
           (setf (,(intern (format nil "~s-END" buf-code)) buf) new))
         (defpolymorph size ((buf ,buf-code)) (values ind &optional)
           (,(intern (format nil "~s-SIZE" buf-code)) buf))
         (defpolymorph (setf size) ((new ind) (buf ,buf-code)) ind
           (setf (,(intern (format nil "~s-SIZE" buf-code)) buf) new))
         (defpolymorph data ((buf ,buf-code)) (values (simple-array ,type (cl:*)) &optional)
           (,(intern (format nil "~s-DATA" buf-code)) buf))
         (defpolymorph (setf data) ((new (simple-array ,type (cl:*)))
                                    (buf ,buf-code))
             (values (simple-array ,type (cl:*)) &optional)
           (setf (,(intern (format nil "~s-DATA" buf-code)) buf) new))


         (defpolymorph (%resize :inline t) ((buf ,buf-code) (newsize ind)) null
           (let ((newdata (make-array newsize :element-type ',type
                                              :initial-element ,default))
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
                 (progn (loop :for i :from 0 :below back
                              :do (setf (aref newdata i) (aref olddata i)))
                        (loop :for i :from front :below (length olddata)
                              :do (setf (aref newdata (+ i (- newsize (length olddata))))
                                        (aref olddata i)))
                        (setf (begin buf) (the ind
                                               (- (begin buf)   ;;FIXME broken
                                                  (- newsize (length olddata)))))))
             (setf (data buf) newdata)
             nil))


         (defpolymorph front ((buf ,buf-code)) ,type
           (if (= 0 (size buf))
               (error "Front requires a non-empty biffer")
               (aref (data buf) (begin buf))))

         (defpolymorph (setf front) ((new ,type) (buf ,buf-code)) ,type
           (if (= 0 (size buf))
               (error "Front requires a non-empty biffer")
               (setf (aref (data buf) (begin buf)) new)))

         (defpolymorph push-front ((new ,type) (buf ,buf-code)) ,type
           (when (= (length (data buf)) (size buf))
             (%resize buf (the ind (* 2 (+ 1 (length (data buf)))))))
           (setf (begin buf)
                 (the ind (if (= 0 (begin buf))
                              (- (length (data buf)) 1)
                              (- (begin buf) 1))))
           (setf (aref (data buf) (begin buf)) new)
           (setf (size buf) (the ind (1+ (size buf))))
           new)

         (defpolymorph pop-front ((buf ,buf-code)) ,type
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


         (defpolymorph back ((buf ,buf-code)) ,type
           (if (= 0 (size buf))
               (error "Back requires a non-empty buffer")
               (aref (data buf)
                     (mod (1- (begin buf)) (size buf)))))

         (defpolymorph (setf back) ((new ,type) (buf ,buf-code)) ,type
           (if (= 0 (size buf))
               (error "Back requires a non-empty buffer")
               (setf (aref (data buf)
                           (mod (1- (begin buf)) (size buf)))
                     new)))

         (defpolymorph push-back ((new ,type) (buf ,buf-code)) ,type
           (when (= (length (data buf)) (size buf))
             (%resize buf (* 2 (+ 1 (length (data buf))))))
           (setf (aref (data buf) (end buf)) new)
           (setf (end buf)
                 (the ind (if (= (1- (length (data buf))) (end buf))
                              0
                              (1+ (end buf)))))
           (setf (size buf) (the ind (1+ (size buf))))
           new)


         (defpolymorph pop-back ((buf ,buf-code)) ,type
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

         (defpolymorph empty-p ((buf ,buf-code)) boolean
           (= 0 (size buf)))

         (defpolymorph (at :inline t) ((buf ,buf-code) &rest indexes)
             (values (or null ,type) &optional boolean)
           (let* ((error-policy (member :error indexes))
                  (begin (begin buf))
                  (size (size buf))
                  (data (data buf))
                  (indexes (if error-policy
                               (nbutlast (nbutlast indexes))
                               indexes))
                  (ind (first indexes)))
             (flet ((%at ()
                      (aref data (if (> ind (+ begin (length data)))
                                     (+ begin ind (- (length data)))
                                     (+ begin ind)))))
               (declare (inline %at))
               (if error-policy

                   (if (second error-policy)
                       (progn
                         (unless (= 1 (length indexes))
                           (error 'simple-error :format-control "Only one index is allowed for ring-buffer")) ;; FIXME later make this better
                         (unless (< (+ begin ind) size)
                           (error 'simple-error
                                   :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                                   :format-arguments (list ind size size)))
                         (%at))
                       (if (and (= 1 (length indexes)) (< (+ begin ind) size))
                           (values (%at) t)
                           (values nil nil)))

                   (progn
                     (unless (= 1 (length indexes))
                       (error 'simple-error :format-control "Only one index is allowed for ring-buffer")) ;; FIXME later make this better
                     (unless (< (+ begin (first indexes)) size)
                       (error 'simple-error
                               :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                               :format-arguments (list (first indexes) size size)))
                     (%at))))))




         (defpolymorph ((setf at) :inline t) ((new ,type) (buf ,buf-code) &rest indexes)
             (values (or null ,type) &optional boolean)
           (let* ((error-policy (member :error indexes))
                  (begin (begin buf))
                  (size (size buf))
                  (data (data buf))
                  (indexes (if error-policy
                               (nbutlast (nbutlast indexes))
                               indexes))
                  (ind (first indexes)))
             (flet ((set-at ()
                      (setf (aref data (if (> ind (+ begin (length data)))
                                           (+ begin ind (- (length data)))
                                           (+ begin ind)))
                            new)))
               (declare (inline set-at))
               (if error-policy

                   (if (second error-policy)
                       (progn
                         (unless (= 1 (length indexes))
                           (error 'simple-error :format-control "Only one index is allowed for ring-buffer")) ;; FIXME later make this better
                         (unless (< (+ begin ind) size)
                           (error 'simple-error
                                   :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                                   :format-arguments (list ind size size)))
                         (set-at))
                       (if (and (= 1 (length indexes)) (< (+ begin ind) size))
                           (values (set-at) t)
                           (values nil nil)))

                   (progn
                     (unless (= 1 (length indexes))
                       (error 'simple-error :format-control "Only one index is allowed for ring-buffer")) ;; FIXME later make this better
                     (unless (< (+ begin (first indexes)) size)
                       (error 'simple-error
                               :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                               :format-arguments (list (first indexes) size size)))
                     (set-at))))))))))

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
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
             :data (make-array l :element-type type
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
        :data (make-array ,l :element-type ',type
                             :initial-contents ,init)))))
||#

(defmacro def (type name inheritance &body slots) ;; This actually comes from polymorph.macros
                                                  ;; and should be there. I put it here just to
  (ecase type                                     ;; illustrate what's happening.
    (:struct
     (let ((typed-slots
             (loop :for slot :in slots
                   :collect (if (listp slot)
                                (destructuring-bind
                                    (sname &optional (stype t) (sform (default stype))) slot
                                  `(,sname ,stype ,sform))
                                `(,slot t t)))))
       `(progn
          ,(if inheritance
               `(defstruct (,name (:include ,@inheritance))
                  ,@(loop :for (sname stype sform) :in typed-slots
                          :collect `(,sname ,sform
                                            :type ,stype)))

               `(defstruct ,name
                  ,@(loop :for (sname stype sform) :in typed-slots
                          :collect `(,sname ,sform
                                            :type ,stype))))
          ,@(loop :for (sname stype _) :in typed-slots
                  :unless (fboundp sname)
                    :collect `(define-polymorphic-function ,sname (object) :overwrite t)
                  :collect `(defpolymorph (,sname :inline t) ((,name ,name)) (values ,stype &optional)
                              (,(intern (format nil "~s-~s" name sname))
                               ,name))
                  :unless (fboundp `(setf ,sname))
                    :collect `(define-polymorphic-function (setf ,sname) (new object) :overwrite t)
                  :collect `(defpolymorph ((setf ,sname) :inline t) ((new ,stype) (,name ,name)) (values ,stype &optional)
                              (setf (,(intern (format nil "~s-~s" name sname))
                                     ,name)
                                    new)))
          ',name)))))

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
            :type ind))))


(defmacro define-ring-buffer (type &optional (default (default type))
                                     force-p)
  (unless (and (not force-p)
               (gethash (cons 'ring-buffer (if (listp type) type (list type)))
                        *unparamterize-name*))

    (let* ((buf-type (cons 'ring-buffer (if (listp type) type (list type))))
           (buf-code (gentemp "RING-BUFFER")))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (def :struct ,buf-code ()
           (begin ind)
           (end ind)
           (size ind)
           (data (simple-array ,type (cl:*))
                 (make-array 0 :element-type ',type :initial-element ,default)))

         (setf (gethash ',buf-type *unparamterize-name*) ',buf-code
               (gethash ',buf-code *paramterize-name*) ',buf-type

               (gethash ',buf-code *corresponding-ctype*)      ;; we create here a corresponding
               (make-instance 'c-ring-buffer :element-type ',type))  ;; ctype

         (deftype ring-buffer (&optional typename)
          (if (eq typename 'cl:*)          ;; by default, a ring-buffer type is just (or ,@all-the-other-rbs)
              `(or ,@',(cons buf-code
                        (loop :for k :being :the :hash-keys :in *unparamterize-name*
                               :using (hash-value v)
                             :when (eql (first k) 'ring-buffer)
                               :collect v)))
              (progn
                (unless (gethash (cons 'ring-buffer (if (listp typename) typename (list typename)))
                                *unparamterize-name*)
                 (ensure-ring-buffer typename))
               (gethash (cons 'ring-buffer
                              (if (listp typename) typename (list typename)))
                        *unparamterize-name*))))))))


(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (define-ring-buffer t)

  (defun ensure-ring-buffer (type &optional (default (default type)))
    (eval `(define-ring-buffer ,type ,default))))




(defpolymorph (front :inline t) ((buf ring-buffer)) t  ;; now we can use ring-buffer as "general"
  (if (= 0 (size buf))                                 ;; type for all ring-buffers
      (error "Front requires a non-empty biffer")
      (aref (data buf) (begin buf))))

(defpolymorph-compiler-macro front (ring-buffer) (buf &environment env) ;; and here we handle the specific
                                                                        ;; types using corresp ctype we saved
  (let* ((type (%form-type buf env))
         (elem-type (c-rb-element-type (gethash type *corresponding-ctype*)))
         (bufname (gensym "BUF")))
    `(let ((,bufname ,buf))
       (declare (type ,type ,bufname))
       (if (= 0 (size ,bufname))
           (error "Front requires a non-empty biffer")
           (the ,elem-type (aref (data ,bufname) (begin ,bufname)))))))


#||
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
||#
