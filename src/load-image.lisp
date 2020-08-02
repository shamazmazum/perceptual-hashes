(in-package :perceptual-hashes)

(defparameter *image-loaders*
  (make-hash-table :test #'equal)
  "Hash table with image-type->loader-function pairs")

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

(defun load-image (filename)
  "Load image with the name @c(filename)"
  (declare (type (or pathname string) filename))
  (let ((type (pathname-type (pathname filename))))
    (multiple-value-bind (reader reader-found)
        (gethash type *image-loaders*)
      (if reader-found
          (funcall reader filename)
          (error 'hash-unknown-format
                 :format-control "Unknown image type: ~a"
                 :format-arguments (list type))))))

(progn
  (setf (gethash "jpg"  *image-loaders*) #'read-jpeg-grayscale)
  (setf (gethash "jpeg" *image-loaders*) #'read-jpeg-grayscale)
  (setf (gethash "png"  *image-loaders*) #'read-png)
  (setf (gethash "tga"  *image-loaders*) #'read-tga)
  (setf (gethash "pcx"  *image-loaders*) #'read-pcx))
