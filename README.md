perceptual-hashes
===============
[![Build Status](https://api.cirrus-ci.com/github/shamazmazum/perceptual-hashes.svg)](https://cirrus-ci.com/github/shamazmazum/perceptual-hashes)

**perceptual-hashes** library computes perceptual hashes for images
(supported formats include formats supported by
[imago](https://github.com/tokenrove/imago) + jpeg). Perceptual hashes
are a measure of similarity between images.

Installation
------------
You can install **perceptual-hashes** from [Ultralisp](https://ultralisp.org/)
repository. Add Ultralisp repository to quicklisp (if you haven't already):

~~~~
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
~~~~

and install **perceptual-hashes**:

~~~~
(ql:quickload :perceptual-hashes)
~~~~

Supported algorithms
--------------------
The following algorithms are supported:

* **aHash**: An algorithm based on whenever each pixel of an image is
  brighter or darker than the average luminance of all pixels.
* **dHash**: An algorithm based on whenever each pixel of an image is
  brighter or darker than its neighbours.

You can find more information
[here](http://www.hackerfactor.com/blog/?/archives/529-Kind-of-Like-That.html).

Examples
--------

~~~~~
(defconstant +threshold+ 45
    "The distance between hashes can vary from 0 to (length hash) =
1024. If it is small enough (<= ~45) the images are similar")

(defun images-similar-p (image-name-1 image-name-2)
    (let ((hash1 (perceptual-hashes:ahash image-name-1))
          (hash2 (perceptual-hashes:ahash image-name-2)))
        (< (perceptual-hashes:hamming-distance hash1 hash2)
           +threshold+)))
~~~~~
