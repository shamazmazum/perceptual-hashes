(defun do-all()
  (handler-case
      (asdf:load-system :perceptual-hashes/tests)
    (error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "perceptual-hashes-tests:run-tests")
       0 1)))

(do-all)
