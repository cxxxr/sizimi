(in-package :sizimi-user)
(defun strip-whitespaces (&rest args)
  (declare (ignorable args))
  (loop for line = (read-line nil nil)
        while line
        do (format t "~A~%" (string-trim '(#\space #\tab) line))))

(defvar *text* "")
(register-virtual-target "/dev/foo"
                         (lambda (string type)
                           (ecase type
                             (:overwrite
                              (setf *text* string))
                             (:append
                              (setf *text*
                                    (concatenate 'string
                                                 *text*
                                                 string))))))
