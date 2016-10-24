(in-package :cl-user)
(defpackage :sizimi.reader
  (:use :cl)
  (:import-from
   :sizimi.env
   :getenv
   :*prompt-function*)
  (:import-from
   :sizimi.complete
   :complete)
  (:export
   :read-input
   :read-input-from-string))
(in-package :sizimi.reader)

(defvar *sizimi-readtable* (copy-readtable nil))
(setf (readtable-case *sizimi-readtable*) :preserve)

(defun read-1-or-2 (char)
  (lambda (stream _char)
    (declare (ignore _char))
    (cond ((eql char (peek-char nil stream nil))
           (read-char stream t nil t)
           (format nil "~A~A" char char))
          (t
           (string char)))))

(set-macro-character #\>
                     (read-1-or-2 #\>)
                     nil
                     *sizimi-readtable*)

(set-macro-character #\<
                     (read-1-or-2 #\<)
                     nil
                     *sizimi-readtable*)

(set-macro-character #\|
                     (read-1-or-2 #\|)
                     nil
                     *sizimi-readtable*)

(set-macro-character #\&
                     (lambda (stream char)
                       (declare (ignore char))
                       (cond ((eql #\& (peek-char nil stream nil))
                              (read-char stream t nil t)
                              "&&")
                             (t
                              (intern "&" (find-package :sizimi-user)))))
                     nil
                     *sizimi-readtable*)

(set-macro-character #\$
                     (lambda (stream char)
                       (declare (ignore char))
                       (let ((prev-case
                               (readtable-case *sizimi-readtable*)))
                         (setf (readtable-case *sizimi-readtable*) :preserve)
                         (unwind-protect
                              (let ((x (read stream t nil t)))
                                (or (getenv (princ-to-string x)) ""))
                           (setf (readtable-case *sizimi-readtable*)
                                 prev-case))))
                     nil
                     *sizimi-readtable*)

(defvar *readline-function*
  #+cl-readline
  (lambda (first-line-p)
    (declare (ignore first-line-p))
    (read-line)))

(setf rl::*attempted-completion-function*
      (rl::produce-callback
       (lambda (text start end)
         (let ((result (complete text start end)))
           (if (null result)
             (cffi:null-pointer)
             (prog1 (rl::to-array-of-strings result)
               (setf rl::*attempted-completion-over* t)))))
       :pointer
       (:string :int :int)))

(defun read-input-sexp (line &key (start 0))
  (let ((fstr
          (make-array 0
                      :element-type 'character
                      :fill-pointer 0
                      :adjustable t)))
    (with-output-to-string (stream fstr)
      (loop with eof-value = '#:eof
            for str = line then (funcall *readline-function* nil)
            do (fresh-line stream)
               (write-string str stream :start start)
               (setf start 0)
               (handler-case
                   (multiple-value-bind (v pos)
                       (read-from-string fstr eof-value)
                     (unless (eq v eof-value)
                       (return-from read-input-sexp
                         (values v fstr pos))))
                 (end-of-file ()))))))

(defun read-input-aux ()
  (loop with eof-value = '#:eof
        and tokens = '()
        and pos = 0
        and value
        and line = (funcall *readline-function* t)
        do (multiple-value-bind (start end)
               (ppcre:scan "^\\.\\.?(?:\\s|$)" line :start pos)
             (when start
               (let ((n (- end start)))
                 (push (make-string n :initial-element #\.)
                       tokens)
                 (setf pos end))))
           (handler-case (multiple-value-setq (value pos)
                           (read-from-string line nil eof-value :start pos))
             (end-of-file ()
               (multiple-value-setq (value line pos)
                 (read-input-sexp line :start pos))))
        until (eq eof-value value)
        do (push value tokens)
        finally (return (nreverse tokens))))

(defun read-input-raw ()
  (let ((*readtable* *sizimi-readtable*)
        (*read-eval* nil))
    (loop thereis (read-input-aux))))

(defun read-input-from-string (string)
  (with-input-from-string (stream string)
    (let ((*readline-function*
            (lambda (first-line-p)
              (declare (ignore first-line-p))
              (read-line stream))))
      (read-input-raw))))

(defvar *readline-lines* nil)

(defun rl-readline-function (first-line-p)
  (let ((line
          (rl:readline :prompt
                       (funcall *prompt-function*
                                first-line-p))))
    (cond (line
           (push line *readline-lines*)
           line)
          (t
           (error 'end-of-file)))))

(defun read-input ()
  (let ((*readline-lines* nil)
        (*readline-function* #'rl-readline-function))
    (prog1 (read-input-raw)
      (let ((string
              (format nil "~{~A~^~%~}"
                      (nreverse *readline-lines*))))
        (cffi:foreign-funcall "add_history"
                              :string string
                              :void)))))
