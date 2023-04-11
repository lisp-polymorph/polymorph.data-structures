;;;; package.lisp

(uiop:define-package #:polymorph.data-structures
  (:use)
  (:mix #:polymorphic-functions #:polymorph.macros #:polymorph.maths
       #:introspect-ctype #:polymorph.copy-cast
       #:polymorph.access #:polymorph.traversable
       #:common-lisp)
  (:reexport #:polymorph.traversable)
  (:import-from #:introspect-ctype #:default #:ind)
  (:import-from #:polymorph.traversable #:next)
  (:shadow #:intersection #:difference #:union #:subsetp #:supersetp)
  ;; TODO export stuff here
  (:export #:dl-list
           #:front #:back
           #:push-front #:push-back
           #:pop-front #:pop-back
           #:size #:empty-p))
