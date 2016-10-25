(in-package :sizimi)

(defun common-prefix (elements)
  (reduce (lambda (str1 str2)
            (subseq str2 0 (mismatch str1 str2)))
          elements))

(defun complete-aux (text elements)
  (let ((elements
          (remove-if-not (alexandria:curry #'alexandria:starts-with-subseq
                                           text)
                         elements)))
    (if (cdr elements)
      (let ((prefix (common-prefix elements)))
        (cons prefix (remove prefix elements :test #'string=)))
      elements)))

(defun complete-command (text start end)
  (declare (ignore start end))
  (complete-aux text (path-files)))

(defun command-position-p (start)
  (eql start
       (position #\space
                 rl:*line-buffer*
                 :test #'char/=)))

(defun complete (text start end)
  (if (command-position-p start)
    (complete-command text start end)
    nil))
