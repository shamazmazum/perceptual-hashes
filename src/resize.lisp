(in-package :perceptual-hashes)

;; This will not work properly for images with dimensions less than
;; THUMB-SIZE but since THUMB-SIZE is small, this is probably OK.

(sera:-> thumbnail
         (imago:image alex:positive-fixnum)
         (values (simple-array (unsigned-byte 8) (* *)) &optional))
(defun thumbnail (image thumb-size)
  "Convert an image into a small thumbnail array of pixel intensity."
  (declare (type imago:image image)
           (type alex:positive-fixnum thumb-size)
           (optimize (speed 3)))
  (let* ((sum (make-array (list thumb-size thumb-size)
                          :element-type 'fixnum
                          :initial-element 0))
         (count (make-array (list thumb-size thumb-size)
                            :element-type 'fixnum
                            :initial-element 0))
         (gray-image (imago:convert-to-grayscale image))
         (width  (imago:image-width  gray-image))
         (height (imago:image-height gray-image))
         (pixels (imago:image-pixels gray-image)))
    (declare (type (simple-array imago:grayscale-pixel) pixels))
    (dotimes (i height)
      (declare (type fixnum i))
      (dotimes (j width)
        (declare (type fixnum j))
        (let ((target-x (floor (* j thumb-size) width))
              (target-y (floor (* i thumb-size) height)))
          (incf (aref count target-y target-x))
          (incf (aref sum   target-y target-x)
                (imago:gray-intensity (aref pixels i j))))))
    (aops:vectorize* '(unsigned-byte 8)
        (sum count)
      (floor sum (max 1 count)))))
