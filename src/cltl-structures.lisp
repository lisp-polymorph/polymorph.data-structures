

(in-package #:polymorph.data-structures)


(defpolymorph push-back ((container (and vector (not simple-array))) (new t)) (values ind &optional)
  (vector-push-extend new container))


(defpolymorph pop-back ((container (and vector (not simple-array)))) (values t &optional)
  (vector-pop container))


(defpolymorph pop-back-safe ((container (and vector (not simple-array)))) (values t boolean &optional)
  (let ((s (length container)))
    (if (= 0 s)
        (values nil nil)
        (values (vector-pop container) t))))

(defpolymorph resize ((container (and vector (not simple-array))) (newsize ind)) (values null &optional) ;; TODO check if newsize is proper
  (adjust-array container newsize)
  (setf (fill-pointer container) newsize)
  nil)

(defpolymorph push-back-array ((container (and vector (not simple-array))) (other simple-array)) (values ind &optional)
  (let ((s (size container))
        (cur (array-total-size container))
        (new (length other)))
   (adjust-array container (+ cur new))
   (incf (fill-pointer container) new)
   (replace container other :start1 s)
   (+ s new)))


;; TODO If you pop back after a huge reserve, it immidiately reallocates
;; which may not be good. Think about what to do.
(defpolymorph reserve ((container (and vector (not simple-array))) (newsize ind)) (values null &optional)
  (adjust-array container newsize)
  nil)


(defpolymorph shrink-to-fit ((container (and vector (not simple-array)))) (values null &optional)
  (adjust-array container (length container))
  nil)
