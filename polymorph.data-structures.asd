;;;; polymorph.data-strcutures.asd

(asdf:defsystem #:polymorph.data-structures
    :description "Data structures for polymorph.stl"
    :author "Commander Thrashdin"
    :license  "MIT"
    :version "0.5"
    :serial t
    :depends-on (#:polymorph.access
                 #:polymorph.maths
                 #:polymorph.copy-cast
                 #:polymorph.macros
                 #:polymorph.traversable)
    :components ((:module "src"
                  :components
                  ((:file "package")
                   (:file "polymorphs")
                   (:file "vector")
                   (:file "single-linked-list")
                   ;(:file "double-linked-list")
                   ;(:file "ring-buffer")
                   (:file "priority-queue")
                   (:file "red-black-tree")
                   ;(:file "constructors")
                   (:file "hash-map")
                   (:file "iterators-sketch")
                   (:file "slices")))))
