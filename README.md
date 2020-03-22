perceptual-hashes
================
[![Build Status](https://travis-ci.com/shamazmazum/perceptual-hashes.svg?branch=master)](https://travis-ci.com/shamazmazum/perceptual-hashes)

**perceptual-hashes** library computes perceptual hashes for images
(supported formats include formats supported by
[imago](https://github.com/tokenrove/imago) + jpeg). Perceptual hashes
are a measure of similarity between images. Currently only average
hash algorithm (AHash) is supported.

Examples
--------

~~~~~
(defconstant +threshold+ 45
    "The distance between hashes can vary from 0 to (length hash) =
4096. If it is small enough (<= ~45) the images are similar")

(defun images-similar-p (image-name-1 image-name-2)
    (let ((hash1 (perceptual-hashes:ahash image-name-1))
          (hash2 (perceptual-hashes:ahash image-name-2)))
        (< (perceptual-hashes:hamming-distance hash1 hash2)
           +threshold+)))
~~~~~
