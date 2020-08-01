(defpackage perceptual-hashes
  (:use #:cl
        #:opticl-core
        #:opticl
        #:jpeg-turbo
        #:array-operations)
  (:export #:ahash
           #:dhash
           #:hamming-distance))
