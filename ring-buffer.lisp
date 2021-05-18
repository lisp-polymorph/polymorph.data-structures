
(in-package #:polymorph.data-structures)


(defstruct ring-buffer
  (begin 0 :type ind)
  (end 0 :type ind)
  (size 0 :type ind)
  (data (make-array 0 :element-type 'fixnum :initial-element 0)))



(defpolymorph front ((buf ring-buffer)) fixnum
  (aref (ring-buffer-data buf) (ring-buffer-begin buf)))


(defpolymorph back ((buf ring-buffer)) fixnum
  (aref (ring-buffer-data buf) (- (ring-buffer-end buf) 1)))

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
      (aref (ring-buffer-data buf) (1- (ring-buffer-begin buf)))))

(defpolymorph (setf back) ((new fixnum) (buf ring-buffer)) fixnum
  (if (= 0 (ring-buffer-size buf))
      (error "Front requires a non-empty biffer")
      (setf (aref (ring-buffer-data buf) (1- (ring-buffer-begin buf))) new)))

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
