(in-package :perceptual-hashes)

(define-condition hash-error (error simple-condition)
  ())

(define-condition hash-unknown-format (hash-error)
  ())
