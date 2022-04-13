(defpackage perceptual-hashes
  (:use #:cl #:imago)
  (:local-nicknames (:sera :serapeum)
                    (:alex :alexandria))
  (:export #:ahash
           #:dhash
           #:hamming-distance))
