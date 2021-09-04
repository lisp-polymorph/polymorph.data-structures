
(in-package #:polymorph.data-structures)

(defmacro define-priority-queue (type &optional (default (default type))
                                        force-p)
  (unless (and (not force-p)
               (gethash (cons 'priority-queue (if (listp type) type (list type)))
                        *unparamterize-name*))
    (let* ((queue-type (cons 'priority-queue (if (listp type) type (list type))))
           (queue-code (gentemp "PRIORITY-QUEUE")))

      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct ,queue-code
           (data (make-array 256 :element-type ',type :initial-element ,default)
            :type (simple-array ,type (cl:*)))
           (size 1 :type ind))

         (setf (gethash ',queue-type *unparamterize-name*) ',queue-code
               (gethash ',queue-code *paramterize-name*) ',queue-type)

         (defpolymorph size ((queue ,queue-code)) (values ind &optional)
           (,(intern (format nil "~s-SIZE" queue-code)) queue))
         (defpolymorph (setf size) ((new ind) (queue ,queue-code)) ind
           (setf (,(intern (format nil "~s-SIZE" queue-code)) queue) new))

         (defpolymorph empty-p ((queue ,queue-code)) (values ind &optional)
           (= 1 (,(intern (format nil "~s-SIZE" queue-code)) queue)))

         (defpolymorph data ((queue ,queue-code)) (values (simple-array ,type (cl:*)) &optional)
           (,(intern (format nil "~s-DATA" queue-code)) queue))
         (defpolymorph (setf data) ((new (simple-array ,type (cl:*)))
                                    (queue ,queue-code))
             (values (simple-array ,type (cl:*)) &optional)
           (setf (,(intern (format nil "~s-DATA" queue-code)) queue) new))

         ;; children of x exist at 2*x and 2*x+1, parent is at x/2 truncated
         ;;       [1]
         ;;    [2]   [3]
         ;; [4] [5] [6] [7]
         ;;
         ;; note that the initial size is 1 (counting the sentinel), so that with
         ;; capacity 2, we never try to shrink and expanding works simply by x2
         (defpolymorph (sift-up :inline t) ((queue ,queue-code) (last ind)) null
           (loop :with data = (data queue)
                 :for i = last :then parent
                 :for parent = (truncate i 2)
                 :while (and (> parent 0)
                             (polymorph.maths:> (aref data i) (aref data parent)))
                 :do (rotatef (aref data i) (aref data parent))))

         (defpolymorph (sift-down :inline t) ((queue ,queue-code)) null
           "sifts root to bottom"
           (loop :with data = (data queue)
                 :with i = 1
                 :for left-child :of-type ind = (* i 2)
                 :for right-child :of-type ind = (1+ (* i 2))
				 :until (>= right-child (size queue)) ; last elt lies at data[size-1]
                 :do (let ((greater-child (if (polymorph.maths:> (aref data left-child)
                                                                 (aref data right-child))
                                              left-child
                                              right-child)))
                       (if (polymorph.maths:> (aref data greater-child) (aref data i))
                           (progn
                             (rotatef (aref data i) (aref data greater-child))
                             (setf i greater-child))
                           (loop-finish)))))

         (defpolymorph front ((queue ,queue-code)) ,type
           (if (empty-p queue)
               (error "priority queue is empty!")
               (aref (data queue) 1)))

         (defpolymorph back ((queue ,queue-code)) ,type
           (if (empty-p queue)
               (error "priority queue is empty!")
               (aref (data queue) (1- (size queue)))))

         (defpolymorph push-back ((item ,type) (queue ,queue-code)) ,type
           (let ((capacity (array-total-size (data queue)))
                 (size (size queue)))
             (when (= size capacity)
               (setf (data queue)
                     (the (simple-array ,type (cl:*))
                          (adjust-array (data queue) (* capacity 2)))))
             ;; insert after fill pointer, move up
             (setf (aref (data queue) size) item)
             (sift-up queue size)
             (setf (size queue) (the ind (1+ (size queue))))
             item))

         (defpolymorph pop-front ((queue ,queue-code)) ,type
           (let ((capacity (array-total-size (data queue)))
                 (size (size queue)))
             (when (= size 1)
               (error "priority queue is empty!"))
             ;; replace root with last, least with largest fixnum, move down
             (prog1
                 (shiftf (aref (data queue) 1) (aref (data queue) (1- size)))
               (sift-down queue)
               (setf (size queue) (decf size))
               ;; at least n/6 elements are shifted before we shrink and copy O(n) elts
               ;; n/2 would cause catastrophic copying with fluctuating push/pops
               (when (<= size (truncate capacity 3))
                 (setf (data queue)
                       (the (simple-array ,type (cl:*))
                            (adjust-array (data queue) (* size 2))))))))

         (defpolymorph check ((queue ,queue-code)) null
           (labels ((rec (a n i)
                      (let ((elt (aref a i))
                            (left (* i 2))
                            (right (1+ (* i 2))))
                        (if (> right n)
                            0
                            (and (>= elt (rec a n left))
                                 (>= elt (rec a n right))
                                 elt)))))
             (assert (rec (data queue) (size queue) 1))))
         (values)))))

(defun ensure-priority-queue (type &optional (default (default type)))
  (eval `(define-priority-queue ,type ,default)))

(defun priority-queue (type &optional initial)
  (unless (gethash (cons 'priority-queue (if (listp type) type (list type)))
                   *unparamterize-name*)
    (ensure-priority-queue type))
  (let* ((l (length initial))
         (queue
           (funcall (intern
                     (format nil "MAKE-~s"
                             (gethash (cons 'priority-queue
                                            (if (listp type) type (list type)))
                                      *unparamterize-name*)))

                    :size (1+ l)
                    :data (make-array (max 2 l) :element-type type))))
    (loop :for item :in initial
          :do (push-back item queue))
    queue))


(define-compiler-macro priority-queue (type &optional initial)
  (let ((type (eval type))
        (l (gensym "L"))
        (init (gensym "INIT")))
    (unless (gethash (cons 'priority-queue (if (listp type) type (list type)))
                     *unparamterize-name*)
      (ensure-priority-queue type))
    `(let* ((,init ,initial)
            (,l (length ,init))
            (queue
              (,(intern
                 (format nil "MAKE-~s"
                         (gethash (cons 'priority-queue (if (listp type) type (list type)))
                                  *unparamterize-name*)))

               :size (1+ ,l)
               :data (make-array (max 2 ,l) :element-type ',type))))

       (loop :for item :in ,init
             :do (push-back item queue))
       queue)))

(defun priority-queue-adhoc-test ()
  (declare (optimize debug safety))
  (let ((b (priority-queue 'fixnum)))
    (loop repeat 10
          for sorted = (list)
          do (print :inserting...)
             (loop for x in (loop repeat 1000 collect (random 1000))
                   do (push-back x b)
                      (check b))
             (print :deleting...)
             (loop repeat 1000
                   do (push (pop-front b) sorted)
                      (check b))
             (assert (equal (sort sorted #'<) sorted)))
    (loop repeat 10
          for sorted = (list)
          do (print :mixed...)
             (loop with inputs = (loop repeat 1000 collect (random 1000))
                     initially (loop for x in inputs
                                     do (push-back x b)
                                     finally (setf sorted (sort inputs #'>)))
                   while (> (size b) 1)
                   do (if (zerop (random 4))
                          (let ((random (random 1000)))
                            (push-back random b)
                            (push random sorted)
                            (setf sorted (sort sorted #'>)))
                          (assert (= (pop-front b) (pop sorted))))
                      (check b)))))
