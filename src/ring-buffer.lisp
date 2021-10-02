
(in-package #:polymorph.data-structures)

(defmacro define-ring-buffer (type &optional (default (default type))
                                     force-p)
  (unless (and (not force-p)
               (gethash (cons 'ring-buffer (if (listp type) type (list type)))
                        *unparamterize-name*))
    (let* ((buf-type (cons 'ring-buffer (if (listp type) type (list type))))
           (buf-code (gentemp "RING-BUFFER")))

      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct ,buf-code
           (begin 0 :type ind) ; index [0, size-1]
           (size 0 :type ind)
           (data (make-array 0 :element-type ',type :initial-element ,default)
            :type (simple-array ,type (cl:*))))

         (setf (gethash ',buf-type *unparamterize-name*) ',buf-code
               (gethash ',buf-code *paramterize-name*) ',buf-type)

         (defpolymorph begin ((buf ,buf-code)) (values ind &optional)
           (,(intern (format nil "~s-BEGIN" buf-code)) buf))
         (defpolymorph (setf begin) ((new ind) (buf ,buf-code)) ind
           (setf (,(intern (format nil "~s-BEGIN" buf-code)) buf) new))
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


         (defpolymorph (%resize :inline nil) ((buf ,buf-code) (newsize ind)) null
           (let ((newdata (make-array newsize :element-type ',type
                                              :initial-element ,default))
                 (olddata (data buf))
                 (begin (begin buf)))
             (if (= (length olddata) 0)
                 (setf (data buf) newdata)
                 (loop :for i :below (size buf)
                       :do (setf (aref newdata i)
                                 (aref olddata (mod (+ begin i) (length olddata))))
                       :finally (setf (begin buf) 0
                                      (data buf) newdata)))))


         (defpolymorph back ((buf ,buf-code)) ,type
           (if (= 0 (size buf))
               (error "BACK requires a non-empty buffer")
               (aref (data buf) (mod (1- (+ (begin buf) (size buf)))
                                     (length (data buf))))))

         (defpolymorph (setf back) ((new ,type) (buf ,buf-code)) ,type
           (if (= 0 (size buf))
               (error "BACK requires a non-empty buffer")
               (setf (aref (data buf) (mod (1- (+ (begin buf) (size buf)))
                                           (length (data buf))))
                     new)))

         (defpolymorph push-back ((new ,type) (buf ,buf-code)) ,type
           (when (= (length (data buf)) (size buf))
             (%resize buf (the ind (* 2 (+ 1 (length (data buf)))))))
           (setf (aref (data buf) (mod (+ (begin buf) (size buf))
                                       (length (data buf))))
                 new)
           (setf (size buf) (the ind (1+ (size buf))))
           new)

         (defpolymorph pop-back ((buf ,buf-code)) ,type
           (if (= 0 (size buf))
               (error "POP-BACK requires a non-empty buffer")
               (prog1
                   (front buf)
                 (when (> (length (data buf)) (* 3 (size buf)))
                   (%resize buf (size buf)))
                 (setf (size buf) (the ind (1- (size buf)))))))


         (defpolymorph front ((buf ,buf-code)) ,type
           (if (= 0 (size buf))
               (error "FRONT requires a non-empty buffer")
               (aref (data buf) (begin buf))))

         (defpolymorph (setf front) ((new ,type) (buf ,buf-code)) ,type
           (if (= 0 (size buf))
               (error "FRONT requires a non-empty buffer")
               (setf (aref (data buf) (begin buf))
                     new)))

         (defpolymorph push-front ((new ,type) (buf ,buf-code)) ,type
           (when (= (length (data buf)) (size buf))
             (%resize buf (* 2 (+ 1 (length (data buf))))))
           (setf (begin buf) (mod (1- (begin buf))
                                  (length (data buf)))
                 (aref (data buf) (begin buf)) new
                 (size buf) (the ind (1+ (size buf))))
           new)


         (defpolymorph pop-front ((buf ,buf-code)) ,type
           (if (= 0 (size buf))
               (error "POP-FRONT requires a non-empty buffer")
               (prog1
                   (back buf)
                 (when (> (length (data buf)) (* 3 (size buf)))
                   (%resize buf (size buf)))
                 (setf (size buf) (the ind (1- (size buf)))
                       (begin buf) (mod (1+ (begin buf))
                                        (length (data buf)))))))

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
                      (aref data (mod (+ begin ind) (length data)))))
               (declare (inline %at))
               (if error-policy

                   (if (second error-policy)
                       (progn
                         (unless (= 1 (length indexes))
                           (error 'simple-error :format-control "Only one index is allowed for ring-buffer")) ;; FIXME later make this better
                         (unless (< ind size)
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
                     (unless (< (first indexes) size)
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
                      (setf (aref data (mod (+ begin ind) (length data)))
                            new)))
               (declare (inline set-at))
               (if error-policy

                   (if (second error-policy)
                       (progn
                         (unless (= 1 (length indexes))
                           (error 'simple-error :format-control "Only one index is allowed for ring-buffer")) ;; FIXME later make this better
                         (unless (< ind size)
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
                     (unless (< (first indexes) size)
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

(defun ring-buffer-adhoc-test ()
  (let ((b (ring-buffer 'fixnum)))
    (push-front 1 b)
    (print b)
    (push-front 2 b)
    (print b)
    (push-front 3 b)
    (print b)
    (push-front 4 b)
    (print b)
    (pop-front b)
    (print b)
    (pop-back b)
    (print b)
    (push-back 5 b)
    (print b)
    (push-back 99 b)
    (print b)
    (assert (and (= (at b 0) 3)
                 (= (at b 1) 2)
                 (= (at b 2) 5)
                 (= (at b 3) 99)))))
