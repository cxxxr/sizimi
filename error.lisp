(in-package :cl-user)
(defpackage :sizimi.error
  (:use :cl)
  (:export
   :sizimi-error
   :missing-redirection-target))
(in-package :sizimi.error)

(define-condition sizimi-error (simple-error)
  ())

(define-condition missing-redirection-target (sizimi-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Missing redirection target"))))
