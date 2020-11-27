(defsystem :perceptual-hashes
    :name :perceptual-hashes
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Perceptual hash algorithms for images"
    :license "2-clause BSD"
    :serial t
    :components ((:file "src/package")
                 (:file "src/conditions")
                 (:file "src/load-image")
                 (:file "src/hashes"))
    :in-order-to ((test-op (load-op "perceptual-hashes/tests")))
    :perform (test-op (op system)
                      (declare (ignore op system))
                      (funcall
                       (symbol-function
                        (intern (symbol-name '#:run-tests)
                                (find-package :perceptual-hashes-tests)))))
    :depends-on (:imago/pngload :jpeg-turbo :array-operations))

(defsystem :perceptual-hashes/tests
    :name :perceptual-hashes/tests
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :components ((:file "tests/package")
                 (:file "tests/tests" :depends-on ("tests/package")))
    :depends-on (:perceptual-hashes :fiveam))
