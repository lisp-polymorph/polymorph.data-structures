
(in-package #:polymorph.data-structures)


(defstruct ring-buffer
  (begin 0 :type ind)
  (end 0 :type ind)
  (size 0 :type ind)
  (data (make-array 0 :element-type 'fixnum :initial-element 0)))


(define-polymorphic-function %resize (container newsize))

(defpolymorph %resize ((buf ring-buffer) (newsize ind)) null
  (let ((newdata (make-array newsize :element-type 'fixnum))
        (olddata (ring-buffer-data buf))
        (front (ring-buffer-begin buf))
        (back (ring-buffer-end buf)))
    (if (< front back)
        (progn (loop :for i :from front :below back
                     :do (setf (aref newdata (- i front)) (aref olddata i)))
               (setf (ring-buffer-begin buf) 0
                     (ring-buffer-end buf) (if (= newsize (- back front))
                                               0
                                               (- back front))))
        (progn (loop :for i :from 0 :below back
                     :do (setf (aref newdata i) (aref olddata i)))
               (loop :for i :from front :below (length olddata)
                     :do (setf (aref newdata (+ i (- newsize (length olddata)))) (aref olddata i)))
               (incf (ring-buffer-begin buf) (- newsize (length olddata)))))
    (setf (ring-buffer-data buf) newdata)
    nil))


(defpolymorph front ((buf ring-buffer)) fixnum
  (if (= 0 (ring-buffer-size buf))
      (error "Front requires a non-empty biffer")
      (aref (ring-buffer-data buf) (ring-buffer-begin buf))))

(defpolymorph (setf front) ((new fixnum) (buf ring-buffer)) fixnum
  (if (= 0 (ring-buffer-size buf))
      (error "Front requires a non-empty biffer")
      (setf (aref (ring-buffer-data buf) (ring-buffer-begin buf)) new)))

(defpolymorph push-front ((new fixnum) (buf ring-buffer)) fixnum
  (when (= (length (ring-buffer-data buf)) (ring-buffer-size buf))
    (%resize buf (* 2 (+ 1 (length (ring-buffer-data buf))))))
  (setf (ring-buffer-begin buf)
        (if (= 0 (ring-buffer-begin buf))
            (- (length (ring-buffer-data buf)) 1)
            (- (ring-buffer-begin buf) 1)))
  (setf (aref (ring-buffer-data buf) (ring-buffer-begin buf)) new)
  (incf (ring-buffer-size buf))
  new)

(defpolymorph pop-front ((buf ring-buffer)) fixnum
  (if (= 0 (ring-buffer-size buf))
      (error "Front requires a non-empty biffer")
      (prog2
          (when (> (length (ring-buffer-data buf)) (* 3 (ring-buffer-size buf)))
            (%resize buf (ring-buffer-size buf)))
          (aref (ring-buffer-data buf) (ring-buffer-begin buf))
        (setf (aref (ring-buffer-data buf) (ring-buffer-begin buf)) 0)
        (setf (ring-buffer-begin buf)
              (if (= (1- (length (ring-buffer-data buf))) (ring-buffer-begin buf))
                  0
                  (1+ (ring-buffer-begin buf))))
        (decf (ring-buffer-size buf)))))


(defpolymorph back ((buf ring-buffer)) fixnum
  (if (= 0 (ring-buffer-size buf))
      (error "Front requires a non-empty biffer")
      (aref (ring-buffer-data buf)
            (mod (1- (ring-buffer-begin buf)) (ring-buffer-size buf)))))

(defpolymorph (setf back) ((new fixnum) (buf ring-buffer)) fixnum
  (if (= 0 (ring-buffer-size buf))
      (error "Front requires a non-empty biffer")
      (setf (aref (ring-buffer-data buf)
                  (mod (1- (ring-buffer-begin buf)) (ring-buffer-size buf)))
            new)))

(defpolymorph push-back ((new fixnum) (buf ring-buffer)) fixnum
  (when (= (length (ring-buffer-data buf)) (ring-buffer-size buf))
    (%resize buf (* 2 (+ 1 (length (ring-buffer-data buf))))))
  (setf (aref (ring-buffer-data buf) (ring-buffer-end buf)) new)
  (setf (ring-buffer-end buf)
      (if (= (1- (length (ring-buffer-data buf))) (ring-buffer-begin buf))
          0
          (1+ (ring-buffer-end buf))))
  (incf (ring-buffer-size buf))
  new)


(defpolymorph pop-back ((buf ring-buffer)) fixnum
  (if (= 0 (ring-buffer-size buf))
      (error "Front requires a non-empty biffer")
      (progn
        (when (> (length (ring-buffer-data buf)) (* 3 (ring-buffer-size buf)))
          (%resize buf (ring-buffer-size buf)))
        (setf (ring-buffer-end buf)
              (if (= 0 (ring-buffer-end buf))
                  (1- (length (ring-buffer-data buf)))
                  (1- (ring-buffer-end buf))))
        (decf (ring-buffer-size buf))
        (prog1
            (aref (ring-buffer-data buf) (ring-buffer-end buf))
          (setf (aref (ring-buffer-data buf) (ring-buffer-end buf)) 0)))))

(defpolymorph empty-p ((buf ring-buffer)) boolean
  (= 0 (ring-buffer-size buf)))

(defpolymorph size ((buf ring-buffer)) ind
  (ring-buffer-size buf))

(defpolymorph (at :inline t) ((buf ring-buffer) &rest indexes) (values t &optional boolean)
  (let ((error-policy (member :error indexes))
        (begin (ring-buffer-begin buf))
        (size (ring-buffer-size buf))
        (data (ring-buffer-data buf)))
    (if error-policy

        (let ((indexes (nbutlast (nbutlast indexes))))
          (if (second error-policy)
              (progn
                (unless (= 1 (length indexes))
                  (error 'simple-error :format-control "Only one index is allowed for ring-buffer"))  ;; FIXME later make this better
                (unless (< (first indexes) size)
                  (error 'simple-error
                         :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                         :format-arguments (list (first indexes) size size)))
                (aref data (+ begin (first indexes))))
              (if (and (= 1 (length indexes)) (< (first indexes) size))
                  (values (aref data (+ begin (first indexes))) t)
                  (values nil nil))))

        (progn
          (unless (= 1 (length indexes))
            (error 'simple-error :format-control "Only one index is allowed for ring-buffer")) ;; FIXME later make this better
          (unless (< (first indexes) size)
            (error 'simple-error
                   :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                   :format-arguments (list (first indexes) size size)))
          (aref data (+ begin (first indexes)))))))




(defpolymorph ((setf at) :inline t) ((new t) (buf ring-buffer) &rest indexes) (values t &optional boolean)
  (let ((error-policy (member :error indexes))
        (begin (ring-buffer-begin buf))
        (size (ring-buffer-size buf))
        (data (ring-buffer-data buf)))
    (if error-policy

        (let ((indexes (nbutlast (nbutlast indexes))))
          (if (second error-policy)
              (progn
                (unless (= 1 (length indexes))
                  (error 'simple-error :format-control "Only one index is allowed for ring-buffer"))  ;; FIXME later make this better
                (unless (< (first indexes) size)
                  (error 'simple-error
                         :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                         :format-arguments (list (first indexes) size size)))
                (setf (aref data (+ begin (first indexes))) new))
              (if (and (= 1 (length indexes)) (< (first indexes) size))
                  (values (setf (aref data (+ begin (first indexes))) new) t)
                  (values nil nil))))

        (progn
          (unless (= 1 (length indexes))
            (error 'simple-error :format-control "Only one index is allowed for ring-buffer")) ;; FIXME later make this better
          (unless (< (first indexes) size)
            (error 'simple-error
                   :format-control "Invalid index ~s for (RING-BUFFER ~s), should be a non-negative integer below ~s."
                   :format-arguments (list (first indexes) size size)))
          (setf (aref data (+ begin (first indexes))) new)))))

#||
(defpolymorph (= :inline t) ((first ring-buffer)
                             (second ring-buffer))
    (values boolean &optional)
  (let ((size1 (ring-buffer-size first))
        (size2 (ring-buffer-size second)))
    (and (= size1 size2)
         (loop :for i :from 0 :below size1
               :always (= (at first i)
                          (ar second i))))))
||#





(defun ring-buffer (type initial)
  (declare (ignorable type))
  (let ((l (length initial)))
    (make-ring-buffer :size l
                      :data (make-array l :element-type 'fixnum
                                          :initial-contents initial))))
