

(in-package #:polymorph.data-structures)

(defparameter *paramterize-name* (make-hash-table :test #'equalp))

(defparameter *unparamterize-name* (make-hash-table :test #'equalp))

(define-polymorphic-function front (container))
(define-polymorphic-function back (container))
(define-polymorphic-function push-front (data container))
(define-polymorphic-function push-back (data container))
(define-polymorphic-function pop-front (container))
(define-polymorphic-function pop-back (container))
(define-polymorphic-function size (container))
(define-polymorphic-function empty-p (container))


(define-polymorphic-function prev (node) :overwrite t)
(define-polymorphic-function data (node) :overwrite t)
(define-polymorphic-function next (node) :overwrite t)

(define-polymorphic-function (setf prev) (new node) :overwrite t)
(define-polymorphic-function (setf data) (data node) :overwrite t)
(define-polymorphic-function (setf next) (new node) :overwrite t)

(define-polymorphic-function anchor (container) :overwrite t)
(define-polymorphic-function size (container) :overwrite t)
(define-polymorphic-function (setf size) (new container) :overwrite t)


(defun ensure-dl-list (type &optional (default (default type)))
  (eval `(define-double-linked-list ,type ,default)))

(defmacro define-double-linked-list (type &optional (default (default type)))
  (unless (gethash (cons 'dl-list (if (listp type) type (list type))) *unparamterize-name*)
    (let* ((node-type (cons 'dl-node (if (listp type) type (list type))))
           (dl-type  (cons 'dl-list (if (listp type) type (list type))))
           (node-code (gentemp "NODE"))
           (dl-code (gentemp "DL")))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defstruct ,node-code
             (prev nil :type (or ,node-code null))
             (data ,default :type ,type)
             (next nil :type (or ,node-code null)))


           (defstruct ,dl-code
             (anchor (,(intern (format nil "MAKE-~s" node-code))) :type ,node-code)
             (size 0 :type ind))

           (setf (gethash ',node-type *unparamterize-name*) ',node-code
                 (gethash ',dl-type *unparamterize-name*) ',dl-code
                 (gethash ',dl-code *paramterize-name*) ',dl-type
                 (gethash ',node-code *paramterize-name*) ',node-type)

           (defpolymorph prev ((node ,node-code)) (or ,node-code null)
                         (,(intern (format nil "~s-PREV" node-code)) node))
           (defpolymorph data ((node ,node-code)) ,type
                         (,(intern (format nil "~s-DATA" node-code)) node))
           (defpolymorph next ((node ,node-code)) (or ,node-code null)
                         (,(intern (format nil "~s-NEXT" node-code)) node))
           (defpolymorph (setf prev) ((new ,node-code) (node ,node-code)) ,node-code
                         (setf (,(intern (format nil "~s-PREV" node-code)) node) new))
           (defpolymorph (setf data) ((new ,type) (node ,node-code)) ,type
                         (setf (,(intern (format nil "~s-DATA" node-code)) node) new))
           (defpolymorph (setf next) ((new ,node-code) (node ,node-code)) ,node-code
                         (setf (,(intern (format nil "~s-NEXT" node-code)) node) new))
           (defpolymorph anchor ((container ,dl-code)) ,node-code
                         (,(intern (format nil "~s-ANCHOR" dl-code)) container))
           (defpolymorph size ((container ,dl-code)) ind
                         (,(intern (format nil "~s-SIZE" dl-code)) container))
           (defpolymorph (setf size) ((new ind) (container ,dl-code)) ind
                         (setf (,(intern (format nil "~s-SIZE" dl-code)) container) new))

           (defmethod print-object ((dl ,dl-code) stream)
             (loop :initially (format stream "#D(")
                   :with node := (anchor dl)
                   :for next := (next node) :then (next next)
                   :until (eq next node)
                   :do (format stream "~s " (data next))
                   :finally (format stream ")")))

           (defpolymorph front ((dl-list ,dl-code)) ,type
                         (data (next (anchor dl-list))))

           (defpolymorph back ((dl-list ,dl-code)) ,type
                         (data (prev (anchor dl-list))))

           (defpolymorph empty-p ((dl-list ,dl-code)) boolean
                         (= 0 (size dl-list)))

           (defpolymorph push-front ((data ,type) (dl-list ,dl-code)) ,type
                         (let* ((anchor (anchor dl-list))
                                (current (next anchor))
                                (new-node (,(intern (format nil "MAKE-~s" node-code))
                                            :data data :next current)))
                           (setf (prev current) new-node
                                 (next anchor) new-node)
                           (incf (size dl-list))
                           data))

           (defpolymorph push-back ((data ,type) (dl-list ,dl-code)) ,type
                         (let* ((anchor (anchor dl-list))
                                (current (prev anchor))
                                (new-node (,(intern (format nil "MAKE-~s" node-code))
                                            :data data :prev current)))
                           (setf (next current) new-node
                                 (prev anchor) new-node)
                           (incf (size dl-list))
                           data))

           (defpolymorph pop-front ((dl-list ,dl-code)) ,type
                         (if (= 0 (size dl-list))
                             (error 'simple-error :format-control "Cannot pop from an empty list")
                             (let* ((anchor (anchor dl-list))
                                    (current (next anchor))
                                    (data (data current)))
                               (setf (prev (next current)) nil
                                     (next anchor) (next current))
                               (decf (size dl-list))
                               data)))

           (defpolymorph pop-back ((dl-list ,dl-code)) ,type
                         (if (= 0 (size dl-list))
                             (error 'simple-error :format-control "Cannot pop from an empty list")
                             (let* ((anchor (anchor dl-list))
                                    (current (prev anchor))
                                    (data (data current)))
                               (setf (next (prev current)) nil
                                     (prev anchor) (prev current))
                               (decf (size dl-list))
                               data))))))))



(deftype dl-list (typename)
  (unless (gethash (cons 'dl-list (if (listp type) type (list type))) *unparamterize-name*)
    (ensure-dl-list type))
  (gethash (cons 'dl-list
                 (if (listp typename) typename (list typename))
                 *unparamterize-name*)))


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
    (unless (gethash (cons 'dl-list (if (listp type) type (list type))) *unparamterize-name*)
      (ensure-dl-list type))
    `(let* ((,res (,(intern
                     (format nil "MAKE-~s" (gethash (cons 'dl-list
                                                          (if (listp type) type (list type)))
                                                    *unparamterize-name*)))))
            (,anchor (anchor ,res)))
       (setf (prev ,anchor) ,anchor
             (next ,anchor) ,anchor)
       ,res)))
