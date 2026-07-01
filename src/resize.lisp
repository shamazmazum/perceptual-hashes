(in-package :perceptual-hashes)

(sera:-> intensities (imago:image)
         (values (simple-array (unsigned-byte 8) (* *)) &optional))
(defun intensities (image)
  (declare (optimize (speed 3)))
  (let* ((pixels (imago:image-pixels (imago:convert-to-grayscale image)))
         (result (make-array (array-dimensions pixels) :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 16) (* *)) pixels))
    (loop for i below (array-total-size result) do
      (setf (row-major-aref result i)
            (imago:gray-intensity
             (row-major-aref pixels i))))
    result))

;; This will not work properly for images with dimensions less than
;; THUMB-SIZE but since THUMB-SIZE is small, this is probably OK.
(sera:-> %thumbnail ((simple-array (unsigned-byte 8) (* *)) alex:positive-fixnum)
         (values (simple-array (unsigned-byte 8) (* *)) &optional))
(defun %thumbnail (array thumb-size)
  (declare (optimize (speed 3)))
  (let ((sum    (make-array (list thumb-size thumb-size)
                            :element-type 'fixnum
                            :initial-element 0))
        (count  (make-array (list thumb-size thumb-size)
                            :element-type 'fixnum
                            :initial-element 0))
        (result (make-array (list thumb-size thumb-size)
                            :element-type '(unsigned-byte 8)))
        (height (array-dimension array 0))
        (width  (array-dimension array 1)))
    (dotimes (i height)
      (dotimes (j width)
        (let ((target-x (floor (* j thumb-size) width))
              (target-y (floor (* i thumb-size) height)))
          (incf (aref count target-y target-x))
          (incf (aref sum   target-y target-x)
                (aref array i j)))))
    (loop for i below (array-total-size result) do
      (setf (row-major-aref result i)
            (floor (row-major-aref sum i)
                   (max 1 (row-major-aref count i)))))
    result))

(sera:-> thumbnail (imago:image alex:positive-fixnum)
         (values (simple-array (unsigned-byte 8) (* *)) &optional))
(defun thumbnail (image thumb-size)
  "Convert an image into a small thumbnail array of pixel intensity."
  (%thumbnail (intensities image) thumb-size))
