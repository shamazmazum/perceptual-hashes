(in-package :perceptual-hashes-tests)

(def-suite hashes :description "Test perceptual hashes")
(defparameter *head1* "test-head-1.jpg")
(defparameter *head2* "test-head-2.jpg")
(defparameter *waifu* "test-waifu.png")
(defconstant +threshold+ 45
  "Threshold for similar images")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(hashes))))

(in-suite hashes)

(test ahash
  (let ((hash1 (ahash *head1*))
        (hash2 (ahash *head2*))
        (hash3 (ahash *waifu*)))
    (is-true (< (hamming-distance hash1 hash2)
                +threshold+))
    (is-true (> (hamming-distance hash1 hash3)
                +threshold+))))
