(in-package :sizimi)

(define-condition sizimi-error (simple-error)
  ())

(define-condition missing-redirection-target (sizimi-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Missing redirection target"))))
