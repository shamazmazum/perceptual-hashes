(defpackage perceptual-hashes
  (:use #:cl)
  (:local-nicknames (:sera :serapeum)
                    (:alex :alexandria))
  (:export #:ahash
           #:dhash
           #:hamming-distance))
