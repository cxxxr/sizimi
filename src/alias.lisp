(in-package :sizimi)

(defvar *aliases* nil)

(defun alias-value (name)
  (cdr (assoc name *aliases* :test #'soft-string=)))

(defun show-alias (name)
  (format t "~&alias ~S ~S~%"
          name
          (alias-value name)))

(defun show-aliases ()
  (dolist (alias *aliases*)
    (show-alias (car alias))))

(defun set-alias (name value)
  (push (cons name value) *aliases*))

(defcommand alias (&rest args)
  (case (length args)
    (0 (show-aliases))
    (1 (show-alias (first args)))
    (otherwise
     (set-alias (first args) (second args))))
  (values))

(defcommand unalias (name)
  (setf *aliases* (remove name *aliases* :test #'soft-string= :key #'car))
  (values))

(defun get-alias (name)
  (loop with input = (list name)
        and *aliases* = *aliases*
        for value = (alias-value (first input))
        if value
          do (unalias (first input))
             (setf input
                   (append (read-input-from-string value)
                           (rest input)))
        else
          return input))
