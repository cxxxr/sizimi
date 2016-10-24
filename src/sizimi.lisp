(in-package :cl-user)
(defpackage :sizimi
  (:use :cl)
  (:import-from
   :sizimi.reader
   :read-input
   :read-input-from-string)
  (:import-from
   :sizimi.run
   :register-virtual-target
   :alias
   :unalias
   :&
   :cd
   :sh
   :run)
  (:import-from
   :sizimi.env
   :*prompt-function*)
  (:export
   :load-rc
   :cd
   :sh
   :alias
   :unalias
   :&
   :toplevel-loop
   :register-virtual-target
   :*prompt-function*))
(in-package :sizimi)

(defun load-rc ()
  (let ((pathname (probe-file
                   (merge-pathnames ".sizimirc"
                                    (user-homedir-pathname)))))
    (when pathname
      (load pathname))))

(defun toplevel-loop ()
  (loop with *package* = (find-package :sizimi-user) do
    (handler-case
        (progn
          (run (read-input))
          (fresh-line))
      (end-of-file ()
        (terpri)
        (return-from toplevel-loop t))
      (error (c)
        (uiop:println c))
      (sb-sys:interactive-interrupt (c)
        (declare (ignore c))
        (terpri)))))
