(in-package :perceptual-hashes-tests)

(def-suite hashes :description "Test perceptual hashes")
(defparameter *head*  "tests/head.jpg")
(defparameter *waifu* "tests/waifu.jpg")
(defconstant +threshold+ 30
  "Threshold for similar images")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(hashes))))

(defun find-data (name)
  (asdf:system-relative-pathname
   :perceptual-hashes/tests name))

(defun read-jpeg (pathname)
  (with-decompressor (handle)
    (multiple-value-bind (array width height)
        (decompress handle pathname
                    :pixel-format :rgb)
      (let ((pixels (make-array
                     (list height width)
                     :element-type 'rgb-pixel)))
        (loop
           for i below (* width height)
           for j from 0 by 3
           do
             (setf (row-major-aref pixels i)
                   (make-color
                    (aref array (+ j 0))
                    (aref array (+ j 1))
                    (aref array (+ j 2)))))
        (make-instance 'rgb-image
                       :pixels pixels)))))

(in-suite hashes)

(defun finishes-ok (hash)
  "Test if a hash function computes without an error"
  (declare (type function hash))
  ;; As a filename
  (finishes
    (funcall hash (find-data *head*)))
  ;; As an image
  (finishes
    (funcall
     hash
     (read-jpeg (find-data *head*)))))

(defun amend-image (image)
  "Change an image a bit"
  (let ((result (make-instance
                 (class-of image)
                 :width  (image-width  image)
                 :height (image-height image))))
    (copy result image)
    (do-image-pixels (result color x y)
      (multiple-value-bind (r g b)
          (color-rgb color)
        (setf color (make-color
                     (floor (* r 0.7))
                     g
                     b))))
    result))

(defun test-similar (hash image)
  "Test if hash function recognizes similar images"
  (declare (type function hash)
           (type string image))
  (let* ((original (read-jpeg (find-data image)))
         (blurred (amend-image original)))
    (is-true (< (hamming-distance
                 (funcall hash original)
                 (funcall hash blurred))
                +threshold+))))

(defun test-different (hash)
  "Test if hash function recognizes different images"
  (declare (type function hash))
  (let ((head  (read-jpeg (find-data *head*)))
        (waifu (read-jpeg (find-data *waifu*))))
    (is-true (> (hamming-distance
                 (funcall hash head)
                 (funcall hash waifu))
                +threshold+))))

(defun test-hash (hash)
  "All tests for hash function"
  (finishes-ok    hash)
  (test-different hash)
  (mapc
   (lambda (image)
     (test-similar hash image))
   (list *head* *waifu*)))

(test ahash
  (test-hash #'ahash))

(test dhash
  (test-hash #'dhash))
