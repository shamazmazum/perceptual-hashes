(in-package :perceptual-hashes)

;; Imago's read-image can be broken
(defun load-image (filename &key (errorp t))
  (declare (type (or pathname string) filename))
  #+imago-fixed-reader
  (read-image filename :errorp errorp)
  #-imago-fixed-reader
  (let ((reader (gethash
                 (pathname-type (pathname filename))
                 imago::*image-file-readers*)))
    (if (null reader)
        (and errorp (error "Unknown file format."))
        (funcall reader filename))))
