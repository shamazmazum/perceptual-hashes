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

(in-suite hashes)

(defun amend-image-1 (image)
  "Change an image a bit (variant 1)"
  (let ((result (make-instance
                 (class-of image)
                 :width  (image-width  image)
                 :height (image-height image))))
    (copy result image)
    (do-image-pixels (result color x y)
      (multiple-value-bind (r g b)
          (color-rgb color)
        (setf color (make-color
                     (floor (* r 0.8))
                     g
                     b))))
    result))

(defun amend-image-2 (image)
  "Change an image a bit (variant 1)"
  (let ((result (make-instance
                 (class-of image)
                 :width  (image-width  image)
                 :height (image-height image))))
    (copy result image)
    (loop repeat 50
          for x = (random (image-width  image))
          for y = (random (image-height image))
          do
          (setf (image-pixel result x y)
                (make-color 0 0 0)))
    result))

(defun test-similar (hash image)
  "Test if hash function recognizes similar images"
  (declare (type function hash)
           (type string image))
  (let* ((original (read-image (find-data image)))
         (changed-1 (amend-image-1 original))
         (changed-2 (amend-image-2 original)))
    (is (< (hamming-distance
            (funcall hash original)
            (funcall hash changed-1))
           +threshold+))
    (is (< (hamming-distance
            (funcall hash original)
            (funcall hash changed-2))
           +threshold+))))

(defun test-different (hash)
  "Test if hash function recognizes different images"
  (declare (type function hash))
  (let ((head  (read-image (find-data *head*)))
        (waifu (read-image (find-data *waifu*))))
    (is (> (hamming-distance
            (funcall hash head)
            (funcall hash waifu))
           +threshold+))))

(defun test-hash (hash)
  "All tests for hash function"
  (test-different hash)
  (mapc
   (lambda (image)
     (test-similar hash image))
   (list *head* *waifu*)))

(test ahash
  (test-hash #'ahash))

(test dhash
  (test-hash #'dhash))
