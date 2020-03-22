(in-package :perceptual-hashes-tests)

(def-suite hashes :description "Test perceptual hashes")
(defparameter *head1* "tests/test-head-1.jpg")
(defparameter *head2* "tests/test-head-2.jpg")
(defparameter *waifu* "tests/test-waifu.png")
(defconstant +threshold+ 45
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

(test ahash
  (let ((hash1 (ahash (find-data *head1*)))
        (hash2 (ahash (find-data *head2*)))
        (hash3 (ahash (find-data *waifu*))))
    (is-true (< (hamming-distance hash1 hash2)
                +threshold+))
    (is-true (> (hamming-distance hash1 hash3)
                +threshold+))))
