(in-package :perceptual-hashes)

(defun read-jpeg-grayscale (filename)
  "Read jpeg image into IMAGO:GRAYSCALE-IMAGE object"
  (with-decompressor (handle)
    (multiple-value-bind (array width height)
        (decompress handle filename
                    :pixel-format :gray)
      ;; We need to recopy because imago works only with simple arrays
      (let ((reshaped (make-array
                       (list height width)
                       :element-type 'grayscale-pixel)))
        (loop for idx below (* width height) do
             (setf (row-major-aref reshaped idx)
                   (aref array idx)))
        (make-instance 'grayscale-image
                       :pixels reshaped)))))

(register-image-io-functions '("jpg" "jpeg")
                             :reader #'read-jpeg-grayscale)
