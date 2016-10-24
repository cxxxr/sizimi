(in-package :cl-user)
(defpackage :sizimi.util
  (:use :cl)
  (:export
   :map-path
   :path-files))
(in-package :sizimi.util)

(defun map-path (fn)
  (dolist (path (uiop:split-string (uiop:getenv "PATH") :separator ":"))
    (dolist (file (directory (merge-pathnames "*.*" (uiop:ensure-directory-pathname path))))
      (funcall fn file))))

(defun path-files ()
  (let ((files))
    (map-path (lambda (file)
                (push (file-namestring file) files)))
    (nreverse files)))
