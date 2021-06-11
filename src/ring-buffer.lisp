
(in-package #:polymorph.data-structures)


(defmacro define-ring-buffer (type &optional (default (default type)))
  (unless (gethash (cons 'ring-buffer (if (listp type) type (list type))) *unparamterize-name*)
    (let* ((buf-type  (cons 'ring-buffer (if (listp type) type (list type))))
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


         (defpolymorph %resize ((buf ,buf-code) (newsize ind)) null
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
                        (incf (begin buf) (- newsize (length olddata)))))
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
             (%resize buf (* 2 (+ 1 (length (data buf))))))
           (setf (begin buf)
                 (if (= 0 (begin buf))
                     (- (length (data buf)) 1)
                     (- (begin buf) 1)))
           (setf (aref (data buf) (begin buf)) new)
           (incf (size buf))
           new)

         (defpolymorph pop-front ((buf ,buf-code)) ,type
           (if (= 0 (size buf))
               (error "Front requires a non-empty biffer")
               (prog2
                   (when (> (length (data buf)) (* 3 (size buf)))
                     (%resize buf (size buf)))
                   (aref (data buf) (begin buf))
                 (setf (aref (data buf) (begin buf)) 0)
                 (setf (begin buf)
                       (if (= (1- (length (data buf))) (begin buf))
                           0
                           (1+ (begin buf))))
                 (decf (size buf)))))


         (defpolymorph back ((buf ,buf-code)) ,type
           (if (= 0 (size buf))
               (error "Front requires a non-empty biffer")
               (aref (data buf)
                     (mod (1- (begin buf)) (size buf)))))

         (defpolymorph (setf back) ((new ,type) (buf ,buf-code)) ,type
           (if (= 0 (size buf))
               (error "Front requires a non-empty biffer")
               (setf (aref (data buf)
                           (mod (1- (begin buf)) (size buf)))
                     new)))

         (defpolymorph push-back ((new ,type) (buf ,buf-code)) ,type
           (when (= (length (data buf)) (size buf))
             (%resize buf (* 2 (+ 1 (length (data buf))))))
           (setf (aref (data buf) (end buf)) new)
           (setf (end buf)
               (if (= (1- (length (data buf))) (begin buf))
                   0
                   (1+ (end buf))))
           (incf (size buf))
           new)


         (defpolymorph pop-back ((buf ,buf-code)) ,type
           (if (= 0 (size buf))
               (error "Front requires a non-empty biffer")
               (progn
                 (when (> (length (data buf)) (* 3 (size buf)))
                   (%resize buf (size buf)))
                 (setf (end buf)
                       (if (= 0 (end buf))
                           (1- (length (data buf)))
                           (1- (end buf))))
                 (decf (size buf))
                 (prog1
                     (aref (data buf) (end buf))
                   (setf (aref (data buf) (end buf)) 0)))))

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
             (if error-policy

                 (if (second error-policy)
                     (progn
                       (unless (= 1 (length indexes))
                         (error 'simple-error :format-control "Only one index is allowed for ring-buffer")) ;; FIXME later make this better
                       (unless (< ind size)
                         (error 'simple-error
                                :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                                :format-arguments (list ind size size)))
                       (aref data (if (> ind (length data))
                                      (+ begin ind (- (length data)))
                                      (+ begin ind))))
                     (if (and (= 1 (length indexes)) (< ind size))
                         (values (aref data (if (> ind (length data))
                                                (+ begin ind (- (length data)))
                                                (+ begin ind)))
                                 t)
                         (values nil nil)))

                 (progn
                   (unless (= 1 (length indexes))
                     (error 'simple-error :format-control "Only one index is allowed for ring-buffer")) ;; FIXME later make this better
                   (unless (< (first indexes) size)
                     (error 'simple-error
                            :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                            :format-arguments (list (first indexes) size size)))
                   (aref data (if (> ind (length data))
                                  (+ begin ind (- (length data)))
                                  (+ begin ind)))))))




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
             (if error-policy

                 (if (second error-policy)
                     (progn
                       (unless (= 1 (length indexes))
                         (error 'simple-error :format-control "Only one index is allowed for ring-buffer")) ;; FIXME later make this better
                       (unless (< ind size)
                         (error 'simple-error
                                :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                                :format-arguments (list ind size size)))
                       (setf (aref data (if (> ind (length data))
                                            (+ begin ind (- (length data)))
                                            (+ begin ind)))
                             new))
                     (if (and (= 1 (length indexes)) (< ind size))
                         (values (setf (aref data (if (> ind (length data))
                                                      (+ begin ind (- (length data)))
                                                      (+ begin ind)))
                                       new)
                                 t)
                         (values nil nil)))

                 (progn
                   (unless (= 1 (length indexes))
                     (error 'simple-error :format-control "Only one index is allowed for ring-buffer")) ;; FIXME later make this better
                   (unless (< (first indexes) size)
                     (error 'simple-error
                            :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                            :format-arguments (list (first indexes) size size)))
                   (setf (aref data (if (> ind (length data))
                                       (+ begin ind (- (length data)))
                                       (+ begin ind)))
                         new)))))))))


(defun ensure-ring-buffer (type &optional (default (default type)))
  (eval `(define-ring-buffer ,type ,default)))


 
(defun ring-buffer (type &optional initial)
  (unless (gethash (cons 'ring-buffer (if (listp type) type (list type))) *unparamterize-name*)
    (ensure-ring-buffer type))
  (let ((l (length initial)))
    (funcall (intern (format nil "MAKE-~s"
                             (gethash (cons 'ring-buffer (if (listp type) type (list type))) *unparamterize-name*)))

             :size l
             :data (make-array l :element-type type
                                 :initial-contents initial))))


(define-compiler-macro ring-buffer (type &optional initial)
  (let ((type (eval type))
        (l (gensym "L"))
        (init (gensym "INIT")))
    (unless (gethash (cons 'ring-buffer (if (listp type) type (list type))) *unparamterize-name*)
       (ensure-ring-buffer type))
    `(let* ((,init ,initial)
            (,l (length ,init)))
       ( ,(intern (format nil "MAKE-~s"
                          (gethash (cons 'ring-buffer (if (listp type) type (list type))) *unparamterize-name*)))

         :size ,l
         :data (make-array ,l :element-type ,type
                              :initial-contents ,init)))))
