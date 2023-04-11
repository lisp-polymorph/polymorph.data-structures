(in-package #:polymorph.data-structures)

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defclass c-rb-tree-map (ctype::ctype)
    ((%key-type :initarg :key-type
                 :reader c-rb-key-type)
     (%val-type :initarg :val-type
                :reader c-rb-value-type)))

  (def rbtm-node ()
    (:mut left rbtm-node (error "break"))
    (:mut right rbtm-node (error "break"))
    (:mut parent rbtm-node (error "break"))
    (:mut color (member :red :black) :red)
    (:mut key t)
    (:mut value t))

  (def rbtm ()
    (:mut root rbtm-node (allocate-instance (find-class 'rbtm-node)))
    (:mut sentinel rbtm-node (allocate-instance (find-class 'rbtm-node)))
    (:mut size ind)))



;;TODO Think about having default in these
(defmacro define-rb-tree-map (keytype valtype &optional
                                     force-p)
  (unless (and (not force-p)
               (gethash (cons 'rb-tree-map (cons
                                            (if (listp keytype) keytype (list keytype))
                                            (if (listp valtype) valtype (list valtype))))
                        *unparamterize-name*))
    (let ((tree-type (cons 'rb-tree-map (cons
                                            (if (listp keytype) keytype (list keytype))
                                            (if (listp valtype) valtype (list valtype)))))
          (tree-code (gentemp "RB-TREE-MAP")))

      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct (,tree-code (:include rbtm)))

         (setf (gethash ',tree-type *unparamterize-name*) ',tree-code
               (gethash ',tree-code *paramterize-name*) ',tree-type

               (gethash ',tree-code *corresponding-ctype*)
               (make-instance 'c-rb-tree-map :key-type ',keytype :val-type ',valtype))))))


(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defun ensure-rb-tree-map (keytype valtype)
    (eval `(define-rb-tree-map ,keytype ,valtype))))



(deftype rb-tree-map (&optional keytype valtype)
  (cond ((and (eq keytype 'cl:*) (eq valtype 'cl:*))
         `rbtm)
        ((or (eq keytype 'cl:*) (eq valtype 'cl:*))
         (error "Generic types like this are not yet supported"))
        (t
         (unless (gethash (cons 'rb-tree-map
                                (cons
                                 (if (listp keytype) keytype (list keytype))
                                 (if (listp valtype) valtype (list valtype))))
                          *unparamterize-name*)
           (ensure-rb-tree-map typename))
         (gethash (cons 'rb-tree-map
                                (cons
                                 (if (listp keytype) keytype (list keytype))
                                 (if (listp valtype) valtype (list valtype))))
                  *unparamterize-name*))))

(defmethod print-object ((node rbtm-node) s)
  (format s "#S(~a node :key ~a :value ~a)"
          (color node)
          (key node)
          (value node)))

(defpolymorph (node-null :inline t) ((node rbtm-node) (tree rb-tree-map)) boolean
  (eq node (sentinel tree)))

(defpolymorph (left-child-p :inline t) ((node rbtm-node)) boolean
  (eq node (left (parent node))))

(defpolymorph (left-rotate :inline :maybe) ((tree rb-tree-map) (x rbtm-node)) null
  (bind (((y :infer) (right x)))
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

(defpolymorph (right-rotate :inline :maybe) ((tree rb-tree-map) (y rbtm-node)) null
  (bind (((x :infer) (left y)))
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
(defpolymorph (rb-insert-fixup :inline t) ((tree rb-tree-map) (z rbtm-node)) null
  (loop :for p :of-type rbtm-node = (parent z)
        :while (eq (color (parent z)) :red)
        :do (bind (((pp :infer) (parent p)))
              (if (left-child-p p)
                  (bind (((y :infer) (right pp)))
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
                  (bind (((y :infer) (left pp)))
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

(defpolymorph next-node ((node rb-tree-map)) (or rb-tree-map null)
  (flet ((sentinelp (node)
           (eq node (left node))))
    (declare (dynamic-extent #'sentinelp))
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

(defpolymorph prev ((node rb-tree-map)) (or rb-tree-map null)
  (flet ((sentinelp (node)
           (eq node (left node))))
    (declare (dynamic-extent #'sentinelp))
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

(defpolymorph (%find :inline t) ((tree rb-tree-map) (key t)) (values rbtm-node &optional)
  (bind (((parent :infer) (sentinel tree))
         ((x :infer) (root tree)))
    (loop :until (node-null x tree)
          :do (setf parent x)
              (if (polymorph.maths:< key (key x))
                  (setf x (left x))
                  (setf x (right x)))
          :finally (return parent))))


(defpolymorph-compiler-macro %find (rb-tree-map t) (&whole form tree item &environment env)
  (let ((type (%form-type tree env)))
    (if (alexandria:type= type 'rbtm)
        form
        (let ((elem-type (c-rb-element-type (gethash type *corresponding-ctype*)))
              (newtype (%form-type item env))
              (treename (gensym "TREE"))
              (itemname (gensym "ITEM"))
              (parent (gensym "PARENT"))
              (x (gensym "X")))
          (assert (subtypep newtype elem-type env)
                  (item)
                  'type-error :context (format nil
                                               "when searching for an element in (RB-TREE-MAP ~s)"
                                               elem-type)
                              :expected-type elem-type :datum 'item)

          (let ((form `(bind (((,treename ,type) ,tree)
                              ((,itemname :infer) ,item))
                         (the (values rbtm-node boolean)
                              (bind (((,parent :infer) (sentinel ,treename))
                                     ((,x :infer) (root ,treename)))
                                (loop :until (node-null ,x ,treename)
                                      :do (setf ,parent ,x)
                                          (if (polymorph.maths:< (the ,newtype ,itemname) (the ,elem-type (key ,x)))
                                              (setf ,x (left ,x))
                                              (setf ,x (right ,x)))
                                      :finally (return (values ,parent t))))))))
            form)))))


(defpolymorph (%find-node :inline t) ((tree rb-tree-map) (item t)) (values rbtm-node &optional)
  (bind (((parent :infer) (sentinel tree))
         ((x :infer) (root tree)))
    (loop :until (node-null x tree)
          :do (setf parent x)
              (cond
                ((= (key x) item) (return parent))
                ((polymorph.maths:< item (key x))
                 (setf x (left x)))
                (t (setf x (right x))))
          :finally (return parent))))

(defpolymorph insert ((tree rb-tree-map) (key t) (value t)) (values rbtm-node &optional)
  (bind* (((y :infer) (%find tree key))
          ((z :infer) (make-rbtm-node :key key
                                      :value value
                                      :parent y
                                      :color :red
                                      :left (sentinel tree) :right (sentinel tree))))
    (cond ((node-null y tree)
           (funcall #'(setf root) z tree))
          ((polymorph.maths:< key (key y))
           (funcall #'(setf left) z y))
          (t
           (funcall #'(setf right) z y)))
    (rb-insert-fixup tree z)
    (incf (size tree))
    z))

(defpolymorph-compiler-macro insert (rb-tree-map t t) (&whole form tree key value &environment env)
  (let ((type (%form-type tree env)))
    (if (alexandria:type= type 'rbtm)
        form
        (let ((key-elem-type (c-rb-key-type (gethash type *corresponding-ctype*)))
              (value-elem-type (c-rb-value-type (gethash type *corresponding-ctype*)))
              (keytype (%form-type key env))
              (valuetype (%form-type value env))
              (treename (gensym "TREE"))
              (keyname (gensym "KEY"))
              (valuename (gensym "VALUE"))
              (y (gensym "Y"))
              (z (gensym "Z")))
          (assert (subtypep keytype key-elem-type env)
                  (key)
                  'type-error :context (format nil
                                               "when inserting an element into (RB-TREE-MAP ~s ~s)"
                                               key-elem-type value-elem-type)
                              :expected-type key-elem-type :datum 'key)
          (assert (subtypep valuetype value-elem-type env)
                  (value)
                  'type-error :context (format nil
                                               "when inserting an element into (RB-TREE-MAP ~s ~s)"
                                               key-elem-type value-elem-type)
                              :expected-type value-elem-type :datum 'value)
          `(bind (((,treename ,type) ,tree)
                  ((,keyname ,keytype) ,key)
                  ((,valuename ,valuetype) ,value))
             (bind* (((,y :infer) (%find ,treename ,itemname))
                     ((,z :infer) (make-rbtm-node :key ,keyname
                                                  :value ,valuename
                                                  :parent ,y
                                                  :color :red
                                                  :left (sentinel ,treename) :right (sentinel ,treename))))
               (cond ((node-null ,y ,treename)
                      (setf (root ,treename) ,z))
                     ((polymorph.maths:< (the ,keytype ,keyname) (the ,key-elem-type (key ,y)))
                      (setf (left ,y) ,z))
                     (t
                      (setf (right ,y) ,z)))
               (rb-insert-fixup ,treename ,z)
               (incf (size ,treename))
               ,z))))))

;; links NEW with OLD's parents
(defpolymorph transplant ((tree rb-tree-map) (old rbtm-node) (new rbtm-node)) null
  (cond ((node-null (parent old) tree)
         (setf (root tree) new))
        ((left-child-p old)
         (setf (left (parent old)) new))
        (t
         (setf (right (parent old)) new)))
  (setf (parent new) (parent old))
  (values))

(defpolymorph rb-delete-fixup ((tree rb-tree-map) (x rbtm-node)) null
  (loop :until (or (eq (color x) :red)
                   (eq x (root tree)))
        :do (if (left-child-p x)
                (bind* (((parent :infer) (parent x))
                        ((w :infer) (right parent)))
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
                (bind* (((parent :infer) (parent x))
                        ((w :infer) (left parent)))
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

(defpolymorph erase ((tree rb-tree-map) (key t)) (values boolean &optional)
  (bind ((z (%find-node tree item)))
    (when (= (key z) key)
      (bind* (((y :infer) z)
              ((delete-color :infer) (color y))
              (sub))
        (cond ((node-null (left z) tree)
               (setf sub (right z))
               (transplant tree z (the rbtm-node sub)))
              ((node-null (the rbtm-node (right z)) tree)
               (setf sub (left z))
               (transplant tree z (the rbtm-node sub)))
              ;; two children, we need to swap y with its successor
              (t
               (loop :for w :of-type rbtm-node = (right z) :then (left w)
                     :until (node-null (the rbtm-node (left w)) tree)
                     :finally (setf y w
                                    delete-color (color w)))
               (setf sub (right y))     ; right child may be a sentinel
               ;;  (z)
               ;;     (right z)
               ;;   ...
               ;;  (y)
               ;;     (sub)
               ;; note y is not deleted but moved to replace z. Sub replaces y
               (if (eq y (right z))
                   (setf (parent sub) y)
                   (progn
                     (transplant tree y (the rbtm-node sub)) ; y had no left child
                     (setf (right y) (right z)
                           (parent (right z)) y)))
               (transplant tree z (the rbtm-node y))
               (setf (left y) (left z)
                     (parent (left z)) y
                     (color y) (color z))))
        ;; removing red nodes does not affect invariants
        (when (eq delete-color :black)
          (rb-delete-fixup tree (the rbtm-node sub)))
        (decf (size tree))
        t))))



(declaim (ftype (function (symbol symbol &optional list) (values rb-tree-map &optional)) rb-tree-map))
(defun rb-tree-map (keytype valtype &optional initial)
  (declare (ignorable initial))
  (unless (gethash (cons 'rb-tree-map (cons
                                       (if (listp keytype) keytype (list keytype))
                                       (if (listp valtype) valtype (list valtype))))
                   *unparamterize-name*)
    (ensure-rb-tree-map keytype valtype))
  (let* ((sentinel
           (let ((%node (allocate-instance (find-class 'rbtm-node))))
             (setf* (color %node) :black
                   ;; these can stay uninitialized
                   ;;(right %node) %node
                   ;;(parent %node) %node
                   ;;(key %node) (default keytype)
                    (left %node) %node
                    (right %node) %node)
             %node))
         (tree
           (funcall (intern
                     (format nil "MAKE-~s"
                             (gethash (cons 'rb-tree-map (cons
                                                          (if (listp keytype) keytype (list keytype))
                                                          (if (listp valtype) valtype (list valtype))))
                                      *unparamterize-name*)))
                    :size 0
                    :sentinel sentinel
                    :root sentinel)))
    ;(dolist (x initial)
    ;  (insert tree x)
    tree))

(define-compiler-macro rb-tree-map (keytype valtype &optional initial)
  (declare (ignorable initial))
  (assert (constantp keytype))
  (assert (constantp valtype))
  (let ((keytype (eval keytype))
        (valtype (eval valtype))
        (init (gensym "INIT")))
    (unless (gethash (cons 'rb-tree-map (cons
                                         (if (listp keytype) keytype (list keytype))
                                         (if (listp valtype) valtype (list valtype))))
                     *unparamterize-name*)
      (ensure-rb-tree-map keytype valtype))
    `(the (rb-tree-map ,keytype ,valtype)
          (let* ((,init ,initial)
                 (sentinel
                   (let ((%node (allocate-instance (find-class 'rbtm-node))))
                     (declare (type rbtm-node %node))
                     (setf* (color %node) :black
                            (left %node) %node
                            (right %node) %node)
                     %node))
                 (tree
                   (,(intern
                      (format nil "MAKE-~s"
                              (gethash (cons 'rb-tree-map (cons
                                                            (if (listp keytype) keytype (list keytype))
                                                            (if (listp valtype) valtype (list valtype))))
                                       *unparamterize-name*)))
                    :size 0
                    :sentinel sentinel
                    :root sentinel)))
            (declare (ignorable ,init))
            ;(dolist (x ,init)
            ;  (insert tree (the ,type x))
            tree))))



;; Iterators and such


(polymorph.macros::%def (iter-rb-tree-map (:include iter)) (:copy)
  (:mut node rbtm-node (error "Supply a red black tree node"))
  (tree rb-tree-map (error "Supply a tree")))


(defpolymorph (next :inline t) ((rbi iter-rb-tree-map)) (values t t &optional)
  (if (node-null (node rbi) (tree rbi))
      (iter-stop)
      (multiple-value-prog1 (values (key (node rbi)) (value (node rbi)))
        (let ((next (node rbi))
              (tree (tree rbi)))
          (if (node-null (right next) tree)
              (progn
                (loop :while (and (not (eql 0 (parent next)))
                                  (eq (right (parent next)) next))
                      :do (setf* next (parent next)))
                (when (not (eql 0 (parent next)))
                  (setf* next (parent next))))
              (progn
                (setf* next (right next))
                (loop :until (node-null (left next) tree)
                      :do (setf* next (left next)))))
          (setf (node rbi) next)))))


(defpolymorph iter ((rbt rb-tree-map)) (values iter-rb-tree-map &optional)
  (let ((first (root rbt)))
    (loop :until (node-null (left first) rbt)
          :do (setf* first (left first)))
    (iter-rb-tree-map :node first
                      :tree rbt)))


(defpolymorph collect ((it iter) (type (eql rb-tree-map)) &optional ((combine function) #'identity))
    (values rb-tree-map &optional)
  (declare (ignorable type))
  (let ((res (rb-tree-map t)))
    (handler-case (loop (multiple-value-bind (key val) (multiple-value-call combine (next it))
                          (insert res key val)))
      (iterator-end ()
        res))))
