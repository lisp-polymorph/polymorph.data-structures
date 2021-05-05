;;;; package.lisp

(defpackage #:polymorph.data-structures
  (:use #:cl #:adhoc-polymorphic-functions #:polymorph.utility)
  (:export #:dl-list
           #:front #:back
           #:push-front #:push-back
           #:pop-front #:pop-back
           #:size #:empty-p))
