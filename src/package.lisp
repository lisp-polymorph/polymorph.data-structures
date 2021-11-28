;;;; package.lisp

(defpackage #:polymorph.data-structures
  (:use #:cl #:polymorphic-functions #:introspect-ctype)
  (:shadow #:find)
  (:import-from #:introspect-ctype #:default #:ind)
  ;; TODO export stuff here
  (:export #:dl-list
           #:front #:back
           #:push-front #:push-back
           #:pop-front #:pop-back
           #:size #:empty-p))
