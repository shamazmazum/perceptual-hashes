(defpackage perceptual-hashes
  (:use #:cl)
  (:local-nicknames (:sera :serapeum)
                    (:alex :alexandria))
  (:export #:hash
           #:ahash
           #:dhash
           #:hamming-distance))
