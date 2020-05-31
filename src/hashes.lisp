(in-package :perceptual-hashes)

(defconstant +thumb-size+ 32
  "The image will be resized to a small square with a side
  +thumb-size+ before a hash is calculated.")

;; grayscale->grayscale converter
(defmethod convert-to-grayscale ((image grayscale-image))
  image)

(defun ahash (pathname)
  "Return Ahash (average hash) of an image with the path @c(pathname)"
  (declare (type (or string pathname) pathname))
  (let* ((thumbnail (resize
                     (convert-to-grayscale
                      (load-image pathname))
                     +thumb-size+
                     +thumb-size+))
         (pixels (flatten (image-pixels thumbnail)))
         (mean (floor (reduce #'+ pixels)
                      (length pixels))))
    (map 'bit-vector
         (lambda (x) (if (< x mean) 0 1))
         pixels)))

(defun hamming-distance (hash1 hash2)
  "Calculate Hamming distance between two hashes"
  (declare (optimize (speed 3))
           (type simple-bit-vector hash1 hash2))
  (count 1 (bit-xor hash1 hash2)))
