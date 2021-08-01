;;;;
;;;; red-black tree, implemented with the help and entertainment of MIT's introduction to algorithms
;;;;

(in-package #:polymorph.data-structures)

(defmacro define-rb-tree (type &optional (default (default type))
                                 force-p)
  (unless (and (not force-p)
               (gethash (cons 'rb-tree (if (listp type) type (list type)))
                        *unparamterize-name*))
    (let* ((tree-type (cons 'rb-tree (if (listp type) type (list type))))
           (node-type (cons 'rb-node (if (listp type) type (list type))))
           (tree-code (gentemp "RB-TREE"))
           (tree-node (gentemp "RB-TREE-NODE")))

      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct ,tree-node
           (left (error "no left node") :type ,tree-node)
           (right (error "no right node") :type ,tree-node)
           (parent (error "no parent node") :type ,tree-node)
           (key ,default :type ,type)
           (color :red :type (member :red :black)))

         (defstruct ,tree-code
           (root (error "no root node") :type ,tree-node)
           (sentinel (error "no sentinel node") :type ,tree-node)
           (size 0 :type ind))

         (setf (gethash ',tree-type *unparamterize-name*) ',tree-code
               (gethash ',tree-code *paramterize-name*) ',tree-type
               (gethash ',node-type *unparamterize-name*) ',tree-node
               (gethash ',tree-node *paramterize-name*) ',node-type)

         (defpolymorph left ((node ,tree-node)) ,tree-node
           (,(intern (format nil "~s-LEFT" tree-node)) node))
         (defpolymorph (setf left) ((new ,tree-node) (node ,tree-node)) ,tree-node
           (setf (,(intern (format nil "~s-LEFT" tree-node)) node) new))

         (defpolymorph right ((node ,tree-node)) ,tree-node
           (,(intern (format nil "~s-RIGHT" tree-node)) node))
         (defpolymorph (setf right) ((new ,tree-node) (node ,tree-node)) ,tree-node
           (setf (,(intern (format nil "~s-RIGHT" tree-node)) node) new))

         (defpolymorph parent ((node ,tree-node)) ,tree-node
           (,(intern (format nil "~s-PARENT" tree-node)) node))
         (defpolymorph (setf parent) ((new ,tree-node) (node ,tree-node)) ,tree-node
           (setf (,(intern (format nil "~s-PARENT" tree-node)) node) new))

         (defpolymorph color ((node ,tree-node)) (member :red :black)
           (,(intern (format nil "~s-COLOR" tree-node)) node))
         (defpolymorph (setf color) ((new (member :red :black)) (node ,tree-node))
             (member :red :black)
           (setf (,(intern (format nil "~s-COLOR" tree-node)) node) new))

         (defpolymorph key ((node ,tree-node)) ,type
           (,(intern (format nil "~s-KEY" tree-node)) node))
         (defpolymorph (setf key) ((new ,type) (node ,tree-node)) ,type
           (setf (,(intern (format nil "~s-KEY" tree-node)) node) new))

         ;; trees
         (defpolymorph size ((tree ,tree-code)) ind
           (,(intern (format nil "~s-SIZE" tree-code)) tree))
         (defpolymorph (setf size) ((new ind) (tree ,tree-code)) ind
           (setf (,(intern (format nil "~s-SIZE" tree-code)) tree) new))

         (defpolymorph root ((tree ,tree-code)) ,tree-node
           (,(intern (format nil "~s-ROOT" tree-code)) tree))
         (defpolymorph (setf root) ((new ,tree-node) (tree ,tree-code)) ,tree-node
           (setf (,(intern (format nil "~s-ROOT" tree-code)) tree) new))

         (defpolymorph sentinel ((tree ,tree-code)) ,tree-node
           (,(intern (format nil "~s-SENTINEL" tree-code)) tree))

         (defmethod print-object ((node ,tree-node) s)
           (format s "#S(~a node :data ~a)"
                   (color node)
                   (key node)))

         (defpolymorph node-null ((node ,tree-node) (tree ,tree-code)) boolean
           (eq node (sentinel tree)))

         (defpolymorph left-child-p ((node ,tree-node)) boolean
           (eq node (left (parent node))))

         (defpolymorph left-rotate ((tree ,tree-code) (x ,tree-node)) null
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
                   (parent x) y)))

         (defpolymorph right-rotate ((tree ,tree-code) (y ,tree-node)) null
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
                   (parent y) x)))

         ;; ()/[] denote red/black nodes
         (defpolymorph rb-insert-fixup ((tree ,tree-code) (z ,tree-node)) null
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
                 :finally (setf (color (root tree)) :black)))

         (defpolymorph find ((tree ,tree-code) (item ,type)) (values ,tree-node boolean)
           (loop :with parent = (sentinel tree)
                 :with x = (root tree)
                 :until (node-null x tree)
                 :do (setf parent x)
                     (if (polymorph.maths:< item (key x))
                         (setf x (left x))
                         (setf x (right x)))
                 :finally (return (values parent t))))

         (defpolymorph insert ((tree ,tree-code) (item ,type)) ,tree-node
           (let* ((y (find tree item))
                  (z (,(intern (format nil "MAKE-~s" tree-node))
                      :key item
                      :color :red :parent y
                      :left (sentinel tree) :right (sentinel tree))))
             (cond ((node-null y tree)
                    (setf (root tree) z))
                   ((polymorph.maths:< item (key y))
                    (setf (left y) z))
                   (t
                    (setf (right y) z)))
             (rb-insert-fixup tree z)
             (incf (size tree))
             z))

         ;; links NEW with OLD's parents
         (defpolymorph transplant ((tree ,tree-code) (old ,tree-node) (new ,tree-node)) null
           (cond ((node-null (parent old) tree)
                  (setf (root tree) new))
                 ((left-child-p old)
                  (setf (left (parent old)) new))
                 (t
                  (setf (right (parent old)) new)))
           (setf (parent new) (parent old)))

         (defpolymorph rb-delete-fixup ((tree ,tree-code) (x ,tree-node)) null
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
                 :finally (setf (color x) :black)))

         (defpolymorph erase ((tree ,tree-code) (z ,tree-node)) ,tree-node
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

         (defpolymorph check ((tree ,tree-code)) null
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
             (let ((root (root tree)))
               (and (eq (color root) :black)
                    (node-null (parent root) tree)
                    (assert (recur root))))))

         (values)))))

(defun tree-to-list (root tree)
  "Returns the contents of the binary tree rooted at ROOT as a list."
  (cond ((node-null root tree) nil)
        (t
         (list root
               (tree-to-list (left root) tree)
               (tree-to-list (right root) tree)))))

(defun ensure-rb-tree (type &optional (default (default type)))
  (eval `(define-rb-tree ,type ,default)))

(defun rb-tree (type &optional initial)
  (unless (gethash (cons 'rb-tree (if (listp type) type (list type)))
                   *unparamterize-name*)
    (ensure-rb-tree type))
  (let* ((sentinel
           (let ((%node
                   (allocate-instance
                    (find-class (gethash (cons 'rb-node (if (listp type) type (list type)))
                                         *unparamterize-name*)))))
             (setf (color %node) :black
                   ;; these can stay uninitialized
                   ;; (left %node) %node
                   ;; (right %node) %node
                   (parent %node) %node
                   (key %node) (default type))
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
              (let ((%node
                      (allocate-instance
                       (find-class
                        ',(gethash (cons 'rb-node (if (listp type) type (list type)))
                                   *unparamterize-name*)))))
                (setf (color %node) :black
                      (parent %node) %node
                      (key %node) (default ',type))
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
                          (erase tree (find tree (pop inserted))))
                      (check tree)))))
