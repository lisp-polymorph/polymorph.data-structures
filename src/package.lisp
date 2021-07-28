;;;; package.lisp

(defpackage #:polymorph.data-structures
  (:use #:cl #:polymorphic-functions)
  (:export #:dl-list
           #:front #:back
           #:push-front #:push-back
           #:pop-front #:pop-back
           #:size #:empty-p))
