(defpackage perceptual-hashes
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:fft  #:cl-fftw/single))
  (:export #:hash
           #:ahash
           #:dhash
           #:phash
           #:hamming-distance))
