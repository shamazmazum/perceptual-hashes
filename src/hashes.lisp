(in-package :perceptual-hashes)

(defconstant +thumb-size+ 32
  "The image will be resized to a small square with a side
  +thumb-size+ before a hash is calculated.")

;; grayscale->grayscale converter
(defmethod convert-to-grayscale ((image grayscale-image))
  image)

(defun ahash (pathname)
  "Return aHash (average hash) of an image with the path @c(pathname).
This algorithm is based on whenever a pixel is brighter or darker than
the average luminance of all pixels."
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

(defun dhash (pathname)
  "Return dHash of an image with the path @c(pathname).
This algorithm is based on whenever a pixel is brighter or darker than
the neighbour pixels."
  (declare (type (or string pathname) pathname))
  (loop
     with pixels = (image-pixels
                    (resize
                     (convert-to-grayscale
                      (load-image pathname))
                     (+ +thumb-size+ 2)
                     (+ +thumb-size+ 2)))
     with hash = (make-array (* +thumb-size+ +thumb-size+)
                             :element-type 'bit)
     with idx = 0
     for i from 1 to +thumb-size+
     do
       (loop for j from 1 to +thumb-size+ do
            (setf (aref hash idx)
                  (if (< (* (aref pixels i j) 4)
                         (+ (aref pixels (1- i) j)
                            (aref pixels i (1- j))
                            (aref pixels i (1+ j))
                            (aref pixels (1+ i) j)))
                      0 1))
            (incf idx))
     finally (return hash)))

(defun hamming-distance (hash1 hash2)
  "Calculate Hamming distance between two hashes."
  (declare (optimize (speed 3))
           (type simple-bit-vector hash1 hash2))
  (count 1 (bit-xor hash1 hash2)))
