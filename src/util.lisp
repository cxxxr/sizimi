(in-package :sizimi)

(defun map-path (fn)
  (dolist (path (uiop:split-string (uiop:getenv "PATH") :separator ":"))
    (dolist (file (directory (merge-pathnames "*.*" (uiop:ensure-directory-pathname path))))
      (funcall fn file))))

(defun path-files ()
  (let ((files))
    (map-path (lambda (file)
                (push (file-namestring file) files)))
    (nreverse files)))

(defun symbol-upcase (sym)
  (intern (string-upcase sym) (symbol-package sym)))

(defun symbol-upcase-tree (tree)
  (cond ((consp tree)
         (cons (symbol-upcase-tree (car tree))
               (symbol-upcase-tree (cdr tree))))
        ((symbolp tree)
         (symbol-upcase tree))
        (t
         tree)))

(defun soft-string= (x y)
  (and (or (stringp x) (symbolp x))
       (or (stringp y) (symbolp y))
       (string= x y)))
