;;;;
;;;; red-black tree, implemented with the help and entertainment of MIT's CLRS
;;;;

(in-package #:polymorph.data-structures)

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defclass c-rb-tree (ctype::ctype)
    ((%elem-type :initarg :element-type
                 :reader c-rb-element-type)))

  (def :struct rbt-node ()
    (left rbt-node (error "break"))
    (right rbt-node (error "break"))
    (parent rbt-node (error "break"))
    (color (member :red :black) :red)
    (data t))

  (def :struct rbt ()
    (root rbt-node (allocate-instance (find-class 'node)))
    (sentinel rbt-node (allocate-instance (find-class 'node)))
    (size ind))
  )

(defmacro define-rb-tree (type &optional (default (default type))
                                 force-p)
  (unless (and (not force-p)
               (gethash (cons 'rb-tree (if (listp type) type (list type)))
                        *unparamterize-name*))
    (let ((tree-type (cons 'rb-tree (if (listp type) type (list type))))
          (tree-code (gentemp "RB-TREE")))

      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct (,tree-code (:include rbt)))

         (setf (gethash ',tree-type *unparamterize-name*) ',tree-code
               (gethash ',tree-code *paramterize-name*) ',tree-type

               (gethash ',tree-code *corresponding-ctype*)
               (make-instance 'c-rb-tree :element-type ',type))))))

(deftype rb-tree (&optional typename)
  (if (eq typename 'cl:*)
      `rbt
      (progn
        (unless (gethash (cons 'rb-tree (if (listp typename) typename (list typename)))
                         *unparamterize-name*)
          (ensure-rb-tree typename))
        (gethash (cons 'rb-tree
                       (if (listp typename) typename (list typename)))
                 *unparamterize-name*))))

(defmethod print-object ((node rbt-node) s)
  (format s "#S(~a node :data ~a)"
          (color node)
          (data node)))

(defpolymorph node-null ((node rbt-node) (tree rb-tree)) boolean
  (eq node (sentinel tree)))

(defpolymorph left-child-p ((node rbt-node)) boolean
  (eq node (left (parent node))))

(defpolymorph left-rotate ((tree rb-tree) (x rbt-node)) null
  (let ((y (right x)))
    (setf (right x) (left y))
    (unless (node-null (left y) tree)
      (setf (parent (left y)) x))
    (setf (parent y) (parent x)) ; always valid, (parent root) points to sentinel
    (cond ((node-null (parent x) tree)
           (setf (root tree) y))
          ((left-child-p x)
           (setf (left (parent x)) y))
          (t ; x was right child
           (setf (right (parent x)) y)))
    (setf (left y) x
          (parent x) y)
    (values)))

(defpolymorph right-rotate ((tree rb-tree) (y rbt-node)) null
  (let ((x (left y)))
    (setf (left y) (right x))
    (unless (node-null (right x) tree)
      (setf (parent (right x)) y))
    (setf (parent x) (parent y))
    (cond ((node-null (parent y) tree)
           (setf (root tree) x))
          ((left-child-p y)
           (setf (left (parent y)) x))
          (t ; y was right child
           (setf (right (parent y)) x)))
    (setf (right x) y
          (parent y) x)
    (values)))

;; ()/[] denote red/black nodes
(defpolymorph rb-insert-fixup ((tree rb-tree) (z rbt-node)) null
  (loop :for p = (parent z)
        :while (eq (color (parent z)) :red)
        :do (let ((pp (parent p)))
              (if (left-child-p p)
                  (let ((y (right pp)))
                    (if (eq (color y) :red)
                        ;;      [pp]            (pp)
                        ;;   (p)    (y) ->   [p]    [y]
                        ;; (z)  ?          (z)  ?
                        ;;
                        ;; - double-red invariant is broken by z, p
                        ;; - p is not the root as it's red => pp is not a sentinel
                        ;; this case propagates reds up to pp
                        (setf (color pp) :red
                              (color p) :black (color y) :black
                              z pp)
                        (progn
                          (unless (left-child-p z)
                            ;;     [pp]            [pp]
                            ;;  (p)    [y] ->   (p)    [y]
                            ;;    (z)         (z)
                            ;;
                            ;; double-red invariant is broken by z, p.
                            ;; rotate and switch z,p to change it into the next case
                            (rotatef z p)
                            (left-rotate tree z)) ; (parent p)...
                          ;;       [pp]          (p)              [p]
                          ;;    (p)    [y] -> (z)   [pp]    -> (z)   (pp)
                          ;; (z)                        (y)
                          ;;
                          ;; recolor p, pp, and rebalancing stops as double-reds are fixed
                          (right-rotate tree pp)
                          (setf (color p) :black (color pp) :red))))
                  ;; symmetrical with left/right swapped
                  (let ((y (left pp)))
                    (if (eq (color y) :red)
                        (setf (color pp) :red
                              (color p) :black (color y) :black
                              z pp)
                        (progn
                          (when (left-child-p z)
                            (rotatef z p)
                            (right-rotate tree z))
                          (left-rotate tree pp)
                          (setf (color p) :black (color pp) :red))))))
        :finally (setf (color (root tree)) :black))
  (values))

(defpolymorph next ((node rb-tree)) (or rb-tree null)
  (flet ((sentinelp (node)
           (eq node (left node))))
    ;;(declare (dynamic-extent #'sentinelp))
    (if (sentinelp (right node))
        (loop :with x = node
              :for parent = (parent x)
              :until (or (null parent) (left-child-p x))
              :do (setf x parent)
              :finally (return (parent x))) ; may return NULL
        (loop :with x = (right node)
              :until (sentinelp (left node))
              :do (setf x (left x))
              :finally (return x)))))

(defpolymorph prev ((node rb-tree)) (or rb-tree null)
  (flet ((sentinelp (node)
           (eq node (left node))))
    ;;(declare (dynamic-extent #'sentinelp))
    (if (sentinelp (left node))
        (loop :with x = node
              :for parent = (parent x)
              :until (or (null parent) (not (left-child-p x)))
              :do (setf x parent)
              :finally (return (parent x))) ; may return NULL
        (loop :with x = (left node)
              :until (sentinelp (right node))
              :do (setf x (right x))
              :finally (return x)))))

(defpolymorph find ((tree rb-tree) (item t)) (values rbt-node boolean)
  (loop :with parent = (sentinel tree)
        :with x = (root tree)
        :until (node-null x tree)
        :do (setf parent x)
            (if (polymorph.maths:< item (data x))
                (setf x (left x))
                (setf x (right x)))
        :finally (return (values parent t))))

(defpolymorph insert ((tree rb-tree) (item t)) rbt-node
  (let* ((y (find tree item))
         (z (make-rbt-node :data item
                           :parent y
                           :color :red
                           :left (sentinel tree) :right (sentinel tree))))
    (cond ((node-null y tree)
           (setf (root tree) z))
          ((polymorph.maths:< item (data y))
           (setf (left y) z))
          (t
           (setf (right y) z)))
    (rb-insert-fixup tree z)
    (incf (size tree))
    z))

;; links NEW with OLD's parents
(defpolymorph transplant ((tree rb-tree) (old rbt-node) (new rbt-node)) null
  (cond ((node-null (parent old) tree)
         (setf (root tree) new))
        ((left-child-p old)
         (setf (left (parent old)) new))
        (t
         (setf (right (parent old)) new)))
  (setf (parent new) (parent old))
  (values))

(defpolymorph rb-delete-fixup ((tree rb-tree) (x rbt-node)) null
  (loop :until (or (eq (color x) :red)
                   (eq x (root tree)))
        :do (if (left-child-p x)
                (let* ((parent (parent x))
                       (w (right parent)))
                  (when (eq (color w) :red)
                    (setf (color w) :black (color parent) :red)
                    (left-rotate tree parent)
                    (setf w (right parent))) ; rotation towards x preserves parent
                  (if (and (eq (color (left w)) :black)
                           (eq (color (right w)) :black))
                      (setf (color w) :red
                            x parent)
                      (progn
                        (when (eq (color (right w)) :black)
                          (setf (color (left w)) :black
                                (color w) :red)
                          (right-rotate tree w)
                          (setf w (right parent)))
                        (shiftf (color w) (color parent) :black)
                        (setf (color (right w)) :black)
                        (left-rotate tree parent)
                        (setf x (root tree)))))
                ;; symmetric with left/right, x is right child
                (let* ((parent (parent x))
                       (w (left parent)))
                  (when (eq (color w) :red)
                    (setf (color w) :black (color parent) :red)
                    (right-rotate tree parent)
                    (setf w (left parent)))
                  (if (and (eq (color (right w)) :black)
                           (eq (color (left w)) :black))
                      (setf (color w) :red
                            x parent)
                      (progn
                        (when (eq (color (left w)) :black)
                          (setf (color (right w)) :black
                                (color w) :red)
                          (left-rotate tree w)
                          (setf w (left parent)))
                        (shiftf (color w) (color parent) :black)
                        (setf (color (left w)) :black)
                        (right-rotate tree parent)
                        (setf x (root tree))))))
        :finally (setf (color x) :black))
  (values))

(defpolymorph erase ((tree rb-tree) (z rbt-node)) rbt-node
  (let* ((y z)
         (delete-color (color y))
         sub)
    (cond ((node-null (left z) tree)
           (setf sub (right z))
           (transplant tree z sub))
          ((node-null (right z) tree)
           (setf sub (left z))
           (transplant tree z sub))
          ;; two children, we need to swap y with its successor
          (t
           (loop :for w = (right z) :then (left w)
                 :until (node-null (left w) tree)
                 :finally (setf y w
                                delete-color (color w)))
           (setf sub (right y)) ; right child may be a sentinel
           ;;  (z)
           ;;     (right z)
           ;;   ...
           ;;  (y)
           ;;     (sub)
           ;; note y is not deleted but moved to replace z. Sub replaces y
           (if (eq y (right z))
               (setf (parent sub) y)
               (progn
                 (transplant tree y sub) ; y had no left child
                 (setf (right y) (right z)
                       (parent (right z)) y)))
           (transplant tree z y)
           (setf (left y) (left z)
                 (parent (left z)) y
                 (color y) (color z))))
    ;; removing red nodes does not affect invariants
    (when (eq delete-color :black)
      (rb-delete-fixup tree sub))
    (decf (size tree))
    z))

(defpolymorph check ((tree rb-tree)) null
  (labels ((recur (node)
             (if (node-null node tree)
                 1
                 (let ((l (recur (left node)))
                       (r (recur (right node))))
                   (and l r (= l r) ; black height
                        (cond ((eq (color node) :black)
                               (1+ l))
                              ((and (eq (color (left node)) :black) ; red
                                    (eq (color (right node)) :black))
                               l)
                              (t nil)))))))
    ;;(declare (dynamic-extent #'recur))
    (let ((root (root tree))
          (sentinel (sentinel tree)))
      (assert (and (eq (color root) :black)
                   (node-null (parent root) tree)
                   (eq (color sentinel) :black)
                   (eq sentinel (left sentinel))
                   (recur root)))
      (values))))

(defun tree-to-list (root tree)
  "Returns the contents of the binary tree rooted at ROOT as a list."
  (cond ((node-null root tree) nil)
        (t
         (list root
               (tree-to-list (left root) tree)
               (tree-to-list (right root) tree)))))



(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defun ensure-rb-tree (type &optional (default (default type)))
    (eval `(define-rb-tree ,type ,default))))

(defun rb-tree (type &optional initial)
  (unless (gethash (cons 'rb-tree (if (listp type) type (list type)))
                   *unparamterize-name*)
    (ensure-rb-tree type))
  (let* ((sentinel
           (let ((%node (allocate-instance (find-class 'rbt-node))))
             (setf (color %node) :black
                   ;; these can stay uninitialized
                   ;;(right %node) %node
                   ;;(parent %node) %node
                   ;;(data %node) (default type)
                   (left %node) %node)
             %node))
         (tree
           (funcall (intern
                     (format nil "MAKE-~s"
                             (gethash (cons 'rb-tree (if (listp type) type (list type)))
                                      *unparamterize-name*)))
                    :size 0
                    :sentinel sentinel
                    :root sentinel)))
    (dolist (x initial)
      (insert tree x))
    tree))

(define-compiler-macro rb-tree (type &optional initial)
  (let ((type (eval type))
        (init (gensym "INIT")))
    (unless (gethash (cons 'rb-tree (if (listp type) type (list type)))
                     *unparamterize-name*)
      (ensure-rb-tree type))
    `(let* ((,init ,initial)
            (sentinel
              (let ((%node (allocate-instance (find-class 'rbt-node))))
                (setf (color %node) :black
                      (left %node) %node)
                %node))
            (tree
              (,(intern
                 (format nil "MAKE-~s"
                         (gethash (cons 'rb-tree (if (listp type) type (list type)))
                                  *unparamterize-name*)))
               :size 0
               :sentinel sentinel
               :root sentinel)))
       (dolist (x ,init)
         (insert tree x))
       tree)))

(defun rb-adhoc-test ()
  (declare (optimize debug safety))
  (let ((tree (rb-tree 'fixnum)))
    (loop repeat 10
          for inserted = (list)
          do (print :inserting...)
             (loop for x in (loop repeat 1000 collect (random 1000))
                   do (push x inserted)
                      (insert tree x)
                      (check tree))
             (print :deleting...)
             (loop for item in inserted
                   do (erase tree (find tree item))
                      (check tree))
             (print :mixed...)
             (loop initially (loop for x in (loop repeat 1000 collect (random 1000))
                                   do (push (insert tree x) inserted))
                   while (plusp (size tree))
                   do (if (zerop (random 4))
                          (push (insert tree (random 1000)) inserted)
                          (erase tree (find tree (data (pop inserted)))))
                      (check tree)))))
