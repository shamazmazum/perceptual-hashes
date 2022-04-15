(in-package :perceptual-hashes)

(defconstant +thumb-size+ 32
  "The image will be resized to a small square with a side
  +thumb-size+ before a hash is calculated.")

(declaim (ftype
          (function ((or string pathname imago:image))
                    (values bit-vector &optional))
          ahash dhash))

(defun get-image (image)
  (declare (type (or string pathname imago:image) image))
  (typecase image
    (imago:image image)
    ((or string pathname)
     (imago:read-image image))))

(sera:-> thumbnail-pixels
         (imago:image &optional unsigned-byte)
         (values (simple-array (unsigned-byte 8) (* *)) &optional))
(defun thumbnail-pixels (image &optional (thumb-size +thumb-size+))
  (declare (type imago:image image)
           (type unsigned-byte thumb-size)
           (optimize (speed 3)))
  (let ((pixels (imago:image-pixels
                 (imago:resize
                  (imago:convert-to-grayscale image)
                  thumb-size thumb-size))))
    (declare (type (simple-array imago:grayscale-pixel (* *)) pixels))
    (aops:vectorize* '(unsigned-byte 8)
        (pixels)
      (imago:gray-intensity pixels))))

(defun ahash (image)
  "Return aHash (average hash) of an @c(image) which can be a string,
a pathname or an @c(imago:image) object. If @c(image) is a string or a
pathname, the image is loaded using this pathname.
This algorithm is based on whenever a pixel is brighter or darker than
the average luminance of all pixels."
  (let* ((pixels (aops:flatten (thumbnail-pixels (get-image image))))
         (mean (alex:mean pixels)))
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
                          :element-type 'bit))
        (pixels (thumbnail-pixels
                 (get-image image)
                 (+ +thumb-size+ 1))))
    (declare (optimize (speed 3))
             (type (simple-array (unsigned-byte 8)) pixels))
    (loop
      with idx fixnum = 0
      for i fixnum from 0 below +thumb-size+
      do
         (loop for j fixnum from 0 below +thumb-size+ do
           (setf (aref hash idx)
                 (if (< (* (aref pixels i j) 2)
                        (+ (aref pixels i (1+ j))
                           (aref pixels (1+ i) j)))
                        0 1))
           (incf idx)))
    hash))

(defun hamming-distance (hash1 hash2)
  "Calculate Hamming distance between two hashes."
  (declare (optimize (speed 3))
           (type simple-bit-vector hash1 hash2))
  (count 1 (bit-xor hash1 hash2)))
