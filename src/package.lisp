(defpackage perceptual-hashes
  (:use #:cl
        #:imago
        #:array-operations)
  (:export #:ahash
           #:dhash
           #:hamming-distance))
