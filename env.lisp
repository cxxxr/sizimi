(in-package :cl-user)
(defpackage :sizimi.env
  (:use :cl)
  (:export
   :*prompt-function*
   :getenv
   :set-last-status
   :default-prompt))
(in-package :sizimi.env)

(defvar *last-status* 0)
(defvar *prompt-function* 'default-prompt)

(defun getenv (name)
  (cond ((equal name "?")
         (princ-to-string *last-status*))
        (t
         (uiop:getenv name))))

(defun set-last-status (status)
  (setf *last-status* status))

(defun default-prompt (first-line-p)
  (when first-line-p
    "sizimi> "))
