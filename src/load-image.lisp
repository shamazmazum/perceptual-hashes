(in-package :perceptual-hashes)

(defun read-jpeg-grayscale (filename)
  "Read jpeg image into IMAGO:GRAYSCALE-IMAGE object"
  (with-decompressor (handle)
    (multiple-value-bind (array width height)
        (decompress handle filename
                    :pixel-format :gray)
      (let ((image (make-8-bit-gray-image height width)))
        (loop for i below (* width height) do
             (setf (row-major-aref image i)
                   (aref array i)))
        image))))

(progn
  (setf (gethash :jpg  opticl::*image-file-reader-hash-table*) #'read-jpeg-grayscale)
  (setf (gethash :jpeg opticl::*image-file-reader-hash-table*) #'read-jpeg-grayscale))
