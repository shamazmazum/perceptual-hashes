(defpackage perceptual-hashes
  (:use #:cl
        #:imago
        #:jpeg-turbo
        #:array-operations)
  (:export #:ahash
           #:dhash
           #:hamming-distance))
