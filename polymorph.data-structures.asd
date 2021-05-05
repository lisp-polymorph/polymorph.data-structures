;;;; polymorph.data-strcutures.asd

(asdf:defsystem #:polymorph.data-structures
    :description "Data structures for polymorph.stl"
    :author "Commander Thrashdin"
    :license  "MIT"
    :version "0.5"
    :serial t
    :depends-on (#:adhoc-polymorphic-functions #:compiler-macro #:polymorph.utility)
    :components ((:file "package")
                 (:file "double-linked-list")))
                 ;(:file "ring-buffer")
                 ;(:file "red-black-tree")
                 ;(:file "avl-tree")))
