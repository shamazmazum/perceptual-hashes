(defsystem :perceptual-hashes
    :name :perceptual-hashes
    :version "0.3"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Perceptual hash algorithms for images"
    :license "2-clause BSD"
    :serial t
    :pathname "src/"
    :components ((:file "package")
                 (:file "resize")
                 (:file "hashes"))
    :in-order-to ((test-op (load-op "perceptual-hashes/tests")))
    :perform (test-op (op system)
                      (declare (ignore op system))
                      (funcall
                       (symbol-function
                        (intern (symbol-name '#:run-tests)
                                (find-package :perceptual-hashes-tests)))))
    :depends-on (:imago/pngload
                 :imago/jpeg-turbo
                 :array-operations
                 :alexandria
                 :serapeum))

(defsystem :perceptual-hashes/tests
    :name :perceptual-hashes/tests
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :pathname "tests/"
    :serial t
    :components ((:file "package")
                 (:file "tests"))
    :depends-on (:perceptual-hashes :fiveam))
