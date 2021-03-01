(in-package :perceptual-hashes)

(defconstant +thumb-size+ 32
  "The image will be resized to a small square with a side
  +thumb-size+ before a hash is calculated.")

(declaim (ftype
          (function ((or string pathname image))
                    (values bit-vector &optional))
          ahash dhash))

(defun get-image (image)
  (declare (type (or string pathname image) image))
  (typecase image
    (image image)
    ((or string pathname)
     (read-image image))))

(defun thumbnail-pixels (image &optional (thumb-size +thumb-size+))
  (declare (type image image)
           (type unsigned-byte thumb-size))
  (image-pixels
   (resize
    (convert-to-grayscale image)
    thumb-size thumb-size)))

(defun ahash (image)
  "Return aHash (average hash) of an @c(image) which can be a string,
a pathname or an @c(imago:image) object. If @c(image) is a string or a
pathname, the image is loaded using this pathname.
This algorithm is based on whenever a pixel is brighter or darker than
the average luminance of all pixels."
  (let* ((pixels
          (flatten (thumbnail-pixels (get-image image))))
         (mean (floor (reduce #'+ pixels)
                      (length pixels))))
    (map 'bit-vector
         (lambda (x) (if (< x mean) 0 1))
         pixels)))

(defun dhash (image)
  "Return dHash (gradient hash) of an @c(image) which can be a string,
a pathname or an @c(imago:image) object. If @c(image) is a string or a
pathname, the image is loaded using this pathname.
This algorithm is based on whenever a pixel is brighter or darker than
the neighbour pixels."
  (let ((hash (make-array (* +thumb-size+ +thumb-size+)
                          :element-type 'bit)))
    (loop
       with pixels = (thumbnail-pixels
                      (get-image image)
                      (+ +thumb-size+ 2))
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
              (incf idx)))
    hash))

(defun hamming-distance (hash1 hash2)
  "Calculate Hamming distance between two hashes."
  (declare (optimize (speed 3))
           (type simple-bit-vector hash1 hash2))
  (count 1 (bit-xor hash1 hash2)))
