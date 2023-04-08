(in-package #:polymorph.data-structures)


(defmacro def (type name inheritance &body slots)

  (ecase type
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
