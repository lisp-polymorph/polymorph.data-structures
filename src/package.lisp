;;;; package.lisp

(defpackage #:polymorph.data-structures
  (:use #:cl #:polymorphic-functions #:introspect-ctype
        #:polymorph.copy-cast #:polymorph.macros)
  (:shadow #:find)
  (:shadowing-import-from #:polymorph.maths #:=)
  (:import-from #:introspect-ctype #:default #:ind)
  ;; TODO export stuff here
  (:export #:dl-list
           #:front #:back
           #:push-front #:push-back
           #:pop-front #:pop-back
           #:size #:empty-p))
