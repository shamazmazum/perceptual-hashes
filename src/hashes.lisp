(in-package :perceptual-hashes)

(defconstant +thumb-size+ 32
  "The image will be resized to a small square with a side
+thumb-size+ before a hash is calculated.")
(deftype hash () '(simple-bit-vector #.(* 32 32)))

(sera:-> flatten ((simple-array * (* *)))
         (values (simple-array * (*)) &optional))
(declaim (inline flatten))
(defun flatten (array)
  #+sbcl (sb-ext:array-storage-vector array)
  #-sbcl
  (let ((result (make-array (array-total-size array)
                            :element-type (array-element-type array)))
        (tmp    (make-array (array-total-size array)
                            :element-type (array-element-type array)
                            :displaced-to array)))
    (map-into result #'identity tmp)))

(sera:-> ahash (imago:image) (values hash &optional))
(defun ahash (image)
  "Return aHash (average hash) of an @c(image) of type @c(imago:image)
This algorithm is based on whenever a pixel is brighter or darker than
the average luminance of all pixels."
  (declare (optimize (speed 3)))
  (let* ((pixels (flatten (thumbnail image +thumb-size+)))
         (mean (/ (loop for x across pixels sum x fixnum)
                  (float (length pixels)))))
    (map 'bit-vector
         (lambda (x) (if (< x mean) 0 1))
         pixels)))

(sera:-> dhash (imago:image) (values hash &optional))
(defun dhash (image)
  "Return dHash (gradient hash) of an @c(image) of type
@c(imago:image).  This algorithm is based on whenever a pixel is
brighter or darker than the neighbour pixels."
  (declare (optimize (speed 3)))
  (let ((hash (make-array (* +thumb-size+ +thumb-size+)
                          :element-type 'bit))
        (pixels (thumbnail image (+ +thumb-size+ 1))))
    (loop with idx fixnum = 0
          for i from 0 below +thumb-size+ do
            (loop for j from 0 below +thumb-size+
                  for pixels-idx = (array-row-major-index pixels i j) do
                    (setf (aref hash idx)
                          (if (< (* (row-major-aref pixels pixels-idx) 2)
                                 (+ (row-major-aref pixels (1+ pixels-idx))
                                    (row-major-aref pixels (1+ (+ pixels-idx +thumb-size+)))))
                              0 1))
                    (incf idx)))
    hash))

(defconstant +phash-thumb-size+ 128)

(sera:-> phash (imago:image) (values hash &optional))
(defun phash (image)
  "Return pHash of an @c(image) of type @c(imago:image). This
algorithm is based on Fourier transform."
  (let ((thumbnail (thumbnail image +phash-thumb-size+))
        (scaled (make-array (list +phash-thumb-size+ +phash-thumb-size+)
                            :element-type 'single-float)))
    ;; Convert to float by dividing by 255
    (loop for i below (array-total-size scaled) do
      (setf (row-major-aref scaled i)
            (/ (row-major-aref thumbnail i) 255.0)))
    (let ((fft (fft:%rfft scaled))
          ;; FIXME: Only the block with positive frequencies is
          ;; preserved. Do we need the +/- block?
          (abs (make-array (* +thumb-size+ +thumb-size+)
                           :element-type 'single-float)))
      (loop for i below +thumb-size+
            for idx1 from 0 by +thumb-size+
            for idx2 = (array-row-major-index fft i 0) do
              (loop for j below +thumb-size+ do
                (setf (aref abs (+ idx1 j))
                      (abs (row-major-aref fft (+ idx2 j))))))
      ;; Zero mean value
      (setf (aref abs 0) 0.0)
      (let ((mean (/ (loop for x across abs sum x)
                     (length abs))))
        (map 'bit-vector
             (lambda (x) (if (< x mean) 0 1))
             abs)))))

(sera:-> hamming-distance (hash hash)
         (values (integer 0 #.(* 32 32)) &optional))
(defun hamming-distance (hash1 hash2)
  "Calculate Hamming distance between two hashes."
  (declare (optimize (speed 3)))
  (count 1 (bit-xor hash1 hash2)))
