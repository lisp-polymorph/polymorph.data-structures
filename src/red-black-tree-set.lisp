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

  (def rbt-node ()
    (:mut left rbt-node (error "break"))
    (:mut right rbt-node (error "break"))
    (:mut parent rbt-node (error "break"))
    (:mut color (member :red :black) :red)
    (:mut data t))

  (def rbt ()
    (:mut root rbt-node (allocate-instance (find-class 'rbt-node)))
    (:mut sentinel rbt-node (allocate-instance (find-class 'rbt-node)))
    (:mut size ind)))
  

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


(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defun ensure-rb-tree (type &optional (default (default type)))
    (eval `(define-rb-tree ,type ,default))))



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

(defpolymorph (node-null :inline t) ((node rbt-node) (tree rb-tree)) boolean
  (eq node (sentinel tree)))

(defpolymorph (left-child-p :inline t) ((node rbt-node)) boolean
  (eq node (left (parent node))))

(defpolymorph (left-rotate :inline :maybe) ((tree rb-tree) (x rbt-node)) null
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

(defpolymorph (right-rotate :inline :maybe) ((tree rb-tree) (y rbt-node)) null
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
(defpolymorph (rb-insert-fixup :inline t) ((tree rb-tree) (z rbt-node)) null
  (loop :for p :of-type rbt-node = (parent z)
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

(defpolymorph next-node ((node rb-tree)) (or rb-tree null)
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

(defpolymorph prev ((node rb-tree)) (or rb-tree null)
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

(defpolymorph (%find :inline t) ((tree rb-tree) (item t)) (values rbt-node &optional)
  (bind (((parent :infer) (sentinel tree))
         ((x :infer) (root tree)))
    (loop :until (node-null x tree)
          :do (setf parent x)
              (if (polymorph.maths:< item (data x))
                  (setf x (left x))
                  (setf x (right x)))
          :finally (return parent))))


(defpolymorph-compiler-macro %find (rb-tree t) (&whole form tree item &environment env)
  (let ((type (%form-type tree env)))
    (if (alexandria:type= type 'rbt)
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
                                               "when searching for an element in (RB-TREE ~s)"
                                               elem-type)
                              :expected-type elem-type :datum 'item)

          (let ((form `(bind (((,treename ,type) ,tree)
                              ((,itemname :infer) ,item))
                         (the (values rbt-node boolean)
                              (bind (((,parent :infer) (sentinel ,treename))
                                     ((,x :infer) (root ,treename)))
                                (loop :until (node-null ,x ,treename)
                                      :do (setf ,parent ,x)
                                          (if (polymorph.maths:< (the ,newtype ,itemname) (the ,elem-type (data ,x)))
                                              (setf ,x (left ,x))
                                              (setf ,x (right ,x)))
                                      :finally (return (values ,parent t))))))))
            form)))))


(defpolymorph (%find-node :inline t) ((tree rb-tree) (item t)) (values rbt-node &optional)
  (bind (((parent :infer) (sentinel tree))
         ((x :infer) (root tree)))
    (loop :until (node-null x tree)
          :do (setf parent x)
              (cond
                ((= (data x) item) (return parent))
                ((polymorph.maths:< item (data x))
                 (setf x (left x)))
                (t (setf x (right x))))
          :finally (return parent))))

(defpolymorph insert ((tree rb-tree) (item t)) (values boolean &optional)
  (bind* (((y :infer) (bind (((parent :infer) (sentinel tree))
                             ((x :infer) (root tree)))
                       (loop :until (node-null x tree)
                             :do (setf parent x)
                                 (cond
                                   ((= (data x) item) (return-from insert nil))
                                   ((polymorph.maths:< item (data x))
                                    (setf x (left x)))
                                   (t (setf x (right x))))
                             :finally (return parent))))
          ((z :infer) (make-rbt-node :data item
                                     :parent y
                                     :color :red
                                     :left (sentinel tree) :right (sentinel tree))))
    (cond ((node-null y tree)
           (funcall #'(setf root) z tree))
          ((polymorph.maths:< item (data y))
           (funcall #'(setf left) z y))
          (t
           (funcall #'(setf right) z y)))
    (rb-insert-fixup tree z)
    (incf (size tree))
    t))

#||
(defpolymorph-compiler-macro insert (rb-tree t) (&whole form tree item &environment env)
  (let ((type (%form-type tree env)))
    (if (alexandria:type= type 'rbt)
        form
        (let ((elem-type (c-rb-element-type (gethash type *corresponding-ctype*)))
              (newtype (%form-type item env))
              (treename (gensym "TREE"))
              (itemname (gensym "ITEM"))
              (y (gensym "Y"))
              (z (gensym "Z")))
          (assert (subtypep newtype elem-type env)
                  (item)
                  'type-error :context (format nil
                                               "when inserting an element into (RB-TREE ~s)"
                                               elem-type)
                              :expected-type elem-type :datum 'item)
          `(bind (((,treename ,type) ,tree)
                  ((,itemname ,newtype) ,item))
             (bind* (((,y :infer) (%find ,treename ,itemname))
                     ((,z :infer) (make-rbt-node :data ,itemname
                                                :parent ,y
                                                :color :red
                                                :left (sentinel ,treename) :right (sentinel ,treename))))
               (cond ((node-null ,y ,treename)
                      (setf (root ,treename) ,z))
                     ((polymorph.maths:< (the ,newtype ,itemname) (the ,elem-type (data ,y)))
                      (setf (left ,y) ,z))
                     (t
                      (setf (right ,y) ,z)))
               (rb-insert-fixup ,treename ,z)
               (incf (size ,treename))
               ,z))))))
||#

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

(defpolymorph erase ((tree rb-tree) (item t)) (values boolean &optional)
  (bind ((z (%find-node tree item)))
    (when (= (data z) item)
      (bind* (((y :infer) z)
              ((delete-color :infer) (color y))
              (sub))
        (cond ((node-null (left z) tree)
               (setf sub (right z))
               (transplant tree z (the rbt-node sub)))
              ((node-null (the rbt-node (right z)) tree)
               (setf sub (left z))
               (transplant tree z (the rbt-node sub)))
              ;; two children, we need to swap y with its successor
              (t
               (loop :for w :of-type rbt-node = (right z) :then (left w)
                     :until (node-null (the rbt-node (left w)) tree)
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
                     (transplant tree y (the rbt-node sub)) ; y had no left child
                     (setf (right y) (right z)
                           (parent (right z)) y)))
               (transplant tree z (the rbt-node y))
               (setf (left y) (left z)
                     (parent (left z)) y
                     (color y) (color z))))
        ;; removing red nodes does not affect invariants
        (when (eq delete-color :black)
          (rb-delete-fixup tree (the rbt-node sub)))
        (decf (size tree))
        t))))



(defpolymorph contains ((container rb-tree) (item t)) (values boolean &optional)
  (bind (((x :infer) (root container)))
    (loop :until (node-null x container)
          :do (cond
                ((= (data x) item) (return t))
                ((polymorph.maths:< item (data x))
                 (setf x (left x)))
                (t (setf x (right x))))
          :finally (return nil))))

(defpolymorph empty-p ((container rb-tree)) (values boolean &optional)
  (node-null (root container) container))


(declaim (ftype (function (symbol &optional list) (values rb-tree &optional)) rb-tree))
(defun rb-tree (type &optional initial)
  (declare (ignorable initial))
  (unless (gethash (cons 'rb-tree (if (listp type) type (list type)))
                   *unparamterize-name*)
    (ensure-rb-tree type))
  (let* ((sentinel
           (let ((%node (allocate-instance (find-class 'rbt-node))))
             (setf* (color %node) :black
                   ;; these can stay uninitialized
                   ;;(right %node) %node
                   ;;(parent %node) %node
                   ;;(data %node) (default type)
                    (left %node) %node
                    (right %node) %node)
             %node))
         (tree
           (funcall (intern
                     (format nil "MAKE-~s"
                             (gethash (cons 'rb-tree (if (listp type) type (list type)))
                                      *unparamterize-name*)))
                    :size 0
                    :sentinel sentinel
                    :root sentinel)))
    ;(dolist (x initial)
    ;  (insert tree x)
    tree))

(define-compiler-macro rb-tree (type &optional initial)
  (declare (ignorable initial))
  (let ((type (eval type))
        (init (gensym "INIT")))
    (unless (gethash (cons 'rb-tree (if (listp type) type (list type)))
                     *unparamterize-name*)
      (ensure-rb-tree type))
    `(the (rb-tree ,type)
          (let* ((,init ,initial)
                 (sentinel
                   (let ((%node (allocate-instance (find-class 'rbt-node))))
                     (declare (type rbt-node %node))
                     (setf* (color %node) :black
                            (left %node) %node
                            (right %node) %node)
                     %node))
                 (tree
                   (,(intern
                      (format nil "MAKE-~s"
                              (gethash (cons 'rb-tree (if (listp type) type (list type)))
                                       *unparamterize-name*)))
                    :size 0
                    :sentinel sentinel
                    :root sentinel)))
            (declare (ignorable ,init))
            ;(dolist (x ,init)
            ;  (insert tree (the ,type x))
            tree))))




(defmacro do-rb-tree-set ((val map) &body body)
  (alexandria:with-gensyms (cur mapname first next)
    `(let* ((,mapname ,map)
            (,cur (let ((,first (root ,mapname)))
                    (loop :until (node-null (left ,first) ,mapname)
                          :do (setf* ,first (left ,first)))
                    ,first)))
       (let ((,val (data ,cur)))
         (loop
           (if (node-null ,cur ,mapname)
               (return)
               (progn
                 (locally ,@body)
                 (let ((,next ,cur))
                   (if (node-null (right ,next) ,mapname)
                       (progn
                         (loop :while (and (not (eql 0 (parent ,next)))
                                           (eq (right (parent ,next)) ,next))
                               :do (setf* ,next (parent ,next)))
                         (when (not (eql 0 (parent ,next)))
                           (setf* ,next (parent ,next))))
                       (progn
                         (setf* ,next (right ,next))
                         (loop :until (node-null (left ,next) ,mapname)
                               :do (setf* ,next (left ,next)))))
                   (setf ,cur ,next))
                 (setf ,val (data ,cur)))))))))



(defpolymorph = ((first rb-tree) (second rb-tree)) (values boolean &optional)
  (and (= (size first) (size second))
       (not (do-rb-tree-set (v first)
             (unless (contains second v)
               (return t))))))




(defpolymorph deep-copy ((obj rb-tree)) (values rb-tree-map &optional)
  (let ((new (rb-tree t)))                ;; FIXME do something about types
    (do-rb-tree-set (v obj)
      (insert new (deep-copy v)))
    new))

(defpolymorph shallow-copy ((obj rb-tree)) (values rb-tree-map &optional)
  (let ((new (rb-tree t)))                ;; FIXME do something about types
    (do-rb-tree-set (v obj)
      (insert new v))
    new))




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
    (declare (dynamic-extent #'recur))
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
                   do (erase tree item)
                      (check tree))
             (print :mixed...)
             (loop initially (loop for x in (loop repeat 1000 collect (random 1000))
                                   do (push (insert tree x) inserted))
                   while (plusp (size tree))
                   do (if (zerop (random 4))
                          (push (insert tree (random 1000)) inserted)
                          (erase tree (data (pop inserted))))
                      (check tree)))))

(defun rb-adhoc-test-fast ()
  (declare (optimize (speed 3) (space 0)))
  (let ((tree (rb-tree 'fixnum)))
    (declare (type (rb-tree fixnum) tree))
    (loop repeat 10
          for inserted = (list)
          do (print :inserting...)
             (loop for x in (loop repeat 1000 collect (random 1000))
                   do (push x inserted)
                      (insert tree (the fixnum x))
                      (check tree))
             (print :deleting...)
             (loop for item in inserted
                   do (erase tree (the fixnum item))
                      (check tree))
             (print :mixed...)
             (loop initially (loop for x in (loop repeat 1000 collect (random 1000))
                                   do (push (insert tree (the fixnum x)) inserted))
                   while (plusp (size tree))
                   do (if (zerop (random 4))
                          (push (insert tree (the fixnum (random 1000))) inserted)
                          (erase tree (the fixnum
                                           (data
                                            (the rbt-node (pop inserted))))))
                      (check tree)))))


(defun %test ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((rbt (rb-tree 'fixnum)))
    (declare (type (rb-tree fixnum) rbt))
    (insert rbt 100)))


(defun time-test ()
  (declare (optimize speed))
  (bind (((rbt :infer) (rb-tree 'fixnum))
         (ar (make-array 1000000 :element-type 'fixnum)))
    (loop :for i :from 0 :below (length ar)
          :do (setf (aref ar i) (random 200)))
    (time (loop :for x :of-type fixnum :across ar
                :do (insert rbt x)))
    rbt))



;; Iterators and such


(polymorph.macros::%def (iter-rb-tree (:include iter)) (:copy)
  (:mut node rbt-node (error "Supply a red black tree node"))
  (tree rb-tree (error "Supply a tree")))


(defpolymorph (next :inline t) ((rbi iter-rb-tree)) (values t &optional)
  (if (node-null (node rbi) (tree rbi))
      (iter-stop)
      (prog1 (data (node rbi))
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


(defpolymorph iter ((rbt rb-tree)) (values iter-rb-tree &optional)
  (let ((first (root rbt)))
    (loop :until (node-null (left first) rbt)
          :do (setf* first (left first)))
    (iter-rb-tree :node first
                  :tree rbt)))



;; TODO The following can be optimized, need to think more

(polymorph.macros::%def (iter-rb-tree-intersect (:include iter)) ()
  (fiter iter-rb-tree (error "Supply first RBTreeSet"))
  (sset rb-tree (error "Supply second RBTreeSet")))


(defpolymorph (next :inline t) ((inter iter-rb-tree-intersect)) (values t &optional)
  (let ((it (fiter inter))
        (set (sset inter)))
    (loop (let ((val (next it)))
            (when (contains set val)
              (return val))))))

(defpolymorph (intersection :inline t) ((first rb-tree) (second rb-tree)) (values iter-rb-tree-intersect &optional)
  (iter-rb-tree-intersect :fiter (iter first) :sset second))


(polymorph.macros::%def (iter-rb-tree-difference (:include iter)) ()
  (fiter iter-rb-tree (error "Supply first RBTreeSet"))
  (sset rb-tree  (error "Supply second RBTreeSet")))


(defpolymorph (next :inline t) ((dif iter-rb-tree-difference)) (values t &optional)
  (let ((it (fiter dif))
        (set (sset dif)))
    (loop (let ((val (next it)))
            (unless (contains set val)
              (return val))))))

(defpolymorph (difference :inline t) ((first rb-tree) (second rb-tree)) (values iter-rb-tree-difference &optional)
  (iter-rb-tree-difference :fiter (iter first) :sset second))


(polymorph.macros::%def (iter-rb-tree-symmetric-difference (:include iter)) ()
  (difchain chain-it (error "Supply a dif")))

(defpolymorph (next :inline t) ((symdif iter-rb-tree-symmetric-difference)) (values t &optional)
  (next (difchain symdif)))

(defpolymorph (symmetric-difference :inline t) ((first rb-tree) (second rb-tree)) (values iter-rb-tree-symmetric-difference &optional)
  (iter-rb-tree-symmetric-difference :difchain (chain (difference first second) (difference second first))))


(polymorph.macros::%def (iter-rb-tree-union (:include iter)) ()
  (uchain chain-it (error "Supply a union")))

(defpolymorph (next :inline t) ((union iter-rb-tree-union)) (values t &optional)
  (next (uchain union)))

(defpolymorph (union :inline t) ((first rb-tree) (second rb-tree)) (values iter-rb-tree-union  &optional)
  (if (> (size first) (size second))
      (iter-rb-tree-union  :uchain (chain (iter first)
                                          (difference second first)))
      (iter-rb-tree-union  :uchain (chain (iter second)
                                          (difference first second)))))



(defpolymorph collect ((it iter) (type (eql rb-tree)) &optional ((combine function) #'identity))
    (values rb-tree &optional)
  (declare (ignorable type))
  (let ((res (rb-tree t)))
    (handler-case (loop (insert res (multiple-value-call combine (next it))))
      (iterator-end ()
        res))))
