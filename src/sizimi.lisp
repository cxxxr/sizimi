(in-package :sizimi)

(export '(load-rc
          toplevel-loop))

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

(defcommand & (&rest argv)
  (let ((arg-struct (parse-argv argv)))
    (assert (not (arg-struct-virtual-redirect-spec arg-struct)))
    (run-command arg-struct)))

(defcommand cd (&optional (dir (user-homedir-pathname)))
  (let ((result (uiop:chdir dir)))
    (if (zerop result)
        (let ((newdir (uiop:getcwd)))
          (setf *default-pathname-defaults* newdir)
          newdir)
        nil)))
