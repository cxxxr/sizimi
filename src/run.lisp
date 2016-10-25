(in-package :cl-user)
(defpackage :sizimi.run
  (:use
   :cl
   :split-sequence
   :sizimi.error)
  (:import-from
   :alexandria
   :destructuring-ecase)
  (:import-from
   :sizimi.fd-streams
   :fd-input-stream
   :fd-output-stream)
  (:import-from
   :sizimi.env
   :set-last-status)
  (:import-from
   :sizimi.util
   :symbol-upcase
   :symbol-upcase-tree
   :soft-string=)
  (:import-from
   :sizimi.reader
   :read-input-from-string)
  (:export
   :register-virtual-target
   :alias
   :unalias
   :&
   :run))
(in-package :sizimi.run)

(defstruct pipe
  read-fd
  write-fd)

(defun pipe ()
  (multiple-value-bind (read-fd write-fd)
      (sb-posix:pipe)
    (make-pipe :read-fd read-fd :write-fd write-fd)))

(defstruct command
  name
  args
  redirect-specs
  virtual-redirect-spec)

(cffi:defcvar *errno* :int)
(cffi:defcfun ("execvp" %execvp) :int (file :pointer) (argv :pointer))
(cffi:defcfun ("strerror" %strerror) :string (errno :int))

(defparameter +stdin+ 0)
(defparameter +stdout+ 1)
(defparameter +stderr+ 2)

(defvar *virtual-targets* nil)

(defun arg-to-string (arg)
  (princ-to-string arg))

(defun register-virtual-target (object function)
  (push (cons object function)
        *virtual-targets*)
  (values))

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

(defun alias (&rest args)
  (case (length args)
    (0 (show-aliases))
    (1 (show-alias (first args)))
    (otherwise
     (set-alias (first args) (second args))))
  (values))

(defun unalias (name)
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

(defun aliasp (name)
  (gethash (arg-to-string name) *aliases*))

(setf (get '& 'quote-args) t)
(defun & (&rest argv)
  (let ((command (parse-argv argv)))
    (assert (not (command-virtual-redirect-spec command)))
    (run-command command)))

(defun cd (&optional (dir (user-homedir-pathname)))
  (let ((result (uiop:chdir dir)))
    (if (zerop result)
      (let ((newdir (uiop:getcwd)))
        (setf *default-pathname-defaults* newdir)
        newdir)
      nil)))

(defun sh (string)
  (run (read-input-from-string string)))

(defun execvp (file args)
  (cffi:with-foreign-string (command file)
    (let ((argc (1+ (length args))))
      (cffi:with-foreign-object (argv :string (1+ argc))
        (loop for i from 1
              for arg in args
              do (setf (cffi:mem-aref argv :string i) arg))
        (setf (cffi:mem-aref argv :string 0) command)
        (setf (cffi:mem-aref argv :string argc) (cffi:null-pointer))
        (when (minusp (%execvp command argv))
          (case *errno*
            (2 (format t "~A: command not found~%" file))
            (otherwise (uiop:println (%strerror *errno*))))
          (uiop:quit *errno*))))))

(defun tried-list (list)
  (loop for rest on list
        for prev = nil then curr
        for curr = (car rest)
        for next = (cadr rest)
        for lastp = (null (cdr list)) then (null (cdr rest))
        collect (list prev curr next lastp)))

(defun redirect-target (x)
  (cond ((or (symbolp x) (numberp x))
         (setf x (arg-to-string x)))
        ((consp x)
         (setf x (symbol-upcase-tree x))))
  (let* ((virtualp nil)
         (target
           (cond ((loop for (test . fn) in *virtual-targets*
                        do (typecase test
                             (function
                                (when (funcall test x)
                                  (setf virtualp t)
                                  (return fn)))
                             (otherwise
                                (when (equal test x)
                                  (setf virtualp t)
                                  (return fn))))))
                 (t x))))
    (values target virtualp)))

(defun file-descriptor-p (x)
  (ppcre:register-groups-bind (n)
      ("^&(\\d+)$" (arg-to-string x))
    (when n
      (parse-integer n))))

(defun expand-files (str)
  (mapcar #'namestring (directory str :resolve-symlinks nil)))

(defun parse-argv (argv)
  (let ((args)
        (redirect-specs)
        (virtual-redirect-spec))
    (loop for rest = (tried-list (rest argv)) then (cdr rest)
          for (prev arg next lastp) = (car rest)
          until (null rest)
          do (cond
               ((equal arg ">")
                (when lastp
                  (error 'missing-redirection-target))
                (let ((left
                        (cond ((and (integerp prev)
                                    (<= 0 prev 3))
                               (pop args)
                               prev)
                              (t
                               +stdout+))))
                  (multiple-value-bind (target virtualp)
                      (or (file-descriptor-p next)
                          (redirect-target next))
                    (cond
                      (virtualp
                       (assert (= left 1))
                       (setf virtual-redirect-spec
                             (list :overwrite target)))
                      (t
                       (push (list :> left target)
                             redirect-specs)))
                    (setf rest (cdr rest)))))
               ((equal arg ">>")
                (when lastp
                  (error 'missing-redirection-target))
                (multiple-value-bind (target virtualp)
                    (redirect-target next)
                  (if virtualp
                    (setf virtual-redirect-spec
                          (list :append target))
                    (push (list :>> +stdout+ target)
                          redirect-specs)))
                (setf rest (cdr rest)))
               ((equal arg "<")
                (when lastp
                  (error 'missing-redirection-target))
                (push (list :< +stdin+ (redirect-target next))
                      redirect-specs)
                (setf rest (cdr rest)))
               (t
                (let ((files
                        (when (or (symbolp arg) (numberp arg))
                          (expand-files (arg-to-string arg)))))
                  (dolist (str (or files (list arg)))
                    (push str args))))))
    (make-command :name (first argv)
                  :args (nreverse args)
                  :redirect-specs (delete-duplicates (nreverse redirect-specs) :key #'second)
                  :virtual-redirect-spec virtual-redirect-spec)))

(defun proceed-redirects-for-stream (redirect-specs virtual-redirect-spec)
  (declare (special cleanup-hooks))
  (flet ((fd-to-stream (fd)
           (cond ((= fd +stdin+) '*standard-input*)
                 ((= fd +stdout+) '*standard-output*)
                 ((= fd +stderr+) '*error-output*)
                 (t (error "invalid fd: ~D" fd))))
         (set-stream (symbol new-value)
           (let ((old-value (symbol-value symbol)))
             (push (lambda ()
                     (setf (symbol-value symbol)
                           old-value))
                   cleanup-hooks))
           (setf (symbol-value symbol)
                 new-value))
         (open* (&rest args)
           (let ((stream (apply 'open args)))
             (push (lambda () (close stream))
                   cleanup-hooks)
             stream)))
    (loop for redirect-spec in redirect-specs
          do (destructuring-ecase redirect-spec
               ((:> left right)
                (set-stream (if left
                                (fd-to-stream left)
                                '*standard-input*)
                            (if (integerp right)
                                (fd-to-stream right)
                                (open* right
                                       :direction :output
                                       :if-exists :supersede))))
               ((:>> newfd file)
                (declare (ignore newfd))
                (set-stream '*standard-output*
                            (open* file
                                   :direction :output
                                   :if-exists :append)))
               ((:< newfd file)
                (declare (ignore newfd))
                (set-stream '*standard-input*
                            (open* file
                                   :direction :input)))))
    (when virtual-redirect-spec
      (destructuring-bind (type target) virtual-redirect-spec
        (let ((stream (make-string-output-stream)))
          (set-stream '*standard-output* stream)
          (push (lambda ()
                  (funcall target (get-output-stream-string stream) type)
                  (close stream))
                cleanup-hooks))))))

(defun lisp-eval (x redirect-specs virtual-redirect-spec
                  &key
                    (stdin *standard-input*)
                    (stdout *standard-output*)
                    (upcase t))
  (let ((*standard-input* stdin)
        (*standard-output* stdout)
        (cleanup-hooks))
    (declare (special cleanup-hooks))
    (proceed-redirects-for-stream redirect-specs virtual-redirect-spec)
    (unwind-protect (handler-case
                        (multiple-value-list
                         (eval (if upcase
                                   (symbol-upcase-tree x)
                                   x)))
                      (error (c)
                        (uiop:println c)
                        -1))
      (mapc 'funcall cleanup-hooks))))

(defun lisp-apply (command
                   &key
                     (stdin *standard-input*)
                     (stdout *standard-output*))
  (lisp-eval (cons (command-name command)
                   (if (get (command-name command) 'quote-args)
                       (mapcar (lambda (arg)
                                 `(quote ,arg))
                               (command-args command))
                       (command-args command)))
             (command-redirect-specs command)
             (command-virtual-redirect-spec command)
             :stdin stdin
             :stdout stdout
             :upcase (not (get (command-name command) 'quote-args))))

(defun proceed-redirects-for-fd (redirect-specs)
  (loop for redirect-spec in redirect-specs
        do (destructuring-ecase redirect-spec
             ((:> left right)
              (let ((fd))
                (sb-posix:dup2 (if (integerp right)
                                 right
                                 (setf fd
                                       (sb-posix:open right
                                                      (logior sb-unix:o_wronly
                                                              sb-unix:o_creat
                                                              sb-unix:o_trunc)
                                                      #o666)))
                               (or left +stdout+))
                (when fd (sb-posix:close fd))))
             ((:>> newfd file)
              (let ((fd (sb-posix:open file
                                       (logior sb-unix:o_append
                                               sb-unix:o_creat
                                               sb-unix:o_wronly)
                                       #o666)))
                (sb-posix:dup2 fd newfd)
                (sb-posix:close fd)))
             ((:< newfd file)
              (let ((fd (sb-posix:open file (logior sb-unix:o_rdonly) #o666)))
                (sb-posix:dup2 fd newfd)
                (sb-posix:close fd))))))

(defun read-from-pipe (fd)
  (let* ((count 100)
         (buf (cffi:foreign-alloc :unsigned-char
                                  :count count
                                  :initial-element 0))
         (octets (make-array count :element-type '(unsigned-byte 8))))
    (apply #'concatenate 'string
           (loop for n = (sb-posix:read fd buf count)
                 until (zerop n)
                 collect (loop for i from 0 below n
                               do (setf (aref octets i)
                                        (cffi:mem-aref buf :unsigned-char i))
                               finally (return (babel:octets-to-string
                                                octets :end n)))))))

(defun run-command (command &optional prev-pipe next-pipe)
  (let ((file (command-name command))
        (args (command-args command))
        (redirect-specs (command-redirect-specs command))
        (virtual-redirect-spec (command-virtual-redirect-spec command)))
    (let ((virtual-pipe (when virtual-redirect-spec (pipe))))
      (let ((pid (sb-posix:fork)))
        (cond
          ((zerop pid)
           (handler-bind ((error (lambda (c)
                                   (warn c)
                                   (uiop:quit -1))))
             (when prev-pipe
               (sb-posix:close (pipe-write-fd prev-pipe))
               (sb-posix:dup2 (pipe-read-fd prev-pipe) +stdin+)
               (sb-posix:close (pipe-read-fd prev-pipe)))
             (when next-pipe
               (sb-posix:close (pipe-read-fd next-pipe))
               (sb-posix:dup2 (pipe-write-fd next-pipe) +stdout+)
               (sb-posix:close (pipe-write-fd next-pipe)))
             (proceed-redirects-for-fd redirect-specs)
             (when virtual-pipe
               (sb-posix:dup2 (pipe-write-fd virtual-pipe) +stdout+)
               (sb-posix:close (pipe-write-fd virtual-pipe))
               (sb-posix:close (pipe-read-fd virtual-pipe)))
             (execvp (arg-to-string file)
                     (mapcar #'arg-to-string args))))
          (t
           (when prev-pipe
             (sb-posix:close (pipe-read-fd prev-pipe))
             (sb-posix:close (pipe-write-fd prev-pipe)))
           (when virtual-redirect-spec
             (sb-posix:close (pipe-write-fd virtual-pipe))
             (let ((text (read-from-pipe (pipe-read-fd virtual-pipe))))
               (sb-posix:close (pipe-read-fd virtual-pipe))
               (destructuring-bind (type target) virtual-redirect-spec
                 (funcall target text type))))
           pid))))))

(defun command-type (command)
  (let ((cmdname (command-name command)))
    (typecase cmdname
      (cons
       :compound-lisp-form)
      (symbol
       (if (fboundp (symbol-upcase cmdname))
           :simple-lisp-form
           :simple-command))
      (string
       :simple-command)
      (otherwise
       :simple-lisp-form))))

(defun eval-dispatch (command prev-pipe next-pipe)
  (let ((stdin
          (when prev-pipe
            (make-instance 'fd-input-stream :fd (pipe-read-fd prev-pipe))))
        (stdout
          (when next-pipe
            (make-instance 'fd-output-stream :fd (pipe-write-fd next-pipe)))))
    (when prev-pipe
      (sb-posix:close (pipe-write-fd prev-pipe)))
    (unwind-protect (ecase (command-type command)
                      (:simple-lisp-form
                       (lisp-apply command
                                   :stdin (or stdin *standard-input*)
                                   :stdout (or stdout *standard-output*)))
                      (:compound-lisp-form
                       (lisp-eval `(progn ,(command-name command)
                                          ,@(command-args command))
                                  (command-redirect-specs command)
                                  (command-virtual-redirect-spec command)
                                  :stdin (or stdin *standard-input*)
                                  :stdout (or stdout *standard-output*))))
      (when prev-pipe (sb-posix:close (pipe-read-fd prev-pipe)))
      (when stdin (close stdin))
      (when stdout (close stdout)))))

(defvar *children* nil)

(defun pipeline-aux (command-list prev-pipe)
  (when command-list
    (let* ((next-pipe (when (rest command-list) (pipe)))
           (command (parse-argv (first command-list)))
           (eval-value)
           (eval-p))
      (ecase (command-type command)
        ((:compound-lisp-form
          :simple-lisp-form)
         (setf eval-p t
               eval-value (eval-dispatch command prev-pipe next-pipe)))
        (:simple-command
         (push (run-command command
                            prev-pipe
                            (when (rest command-list)
                              next-pipe))
               *children*)))
      (cond ((rest command-list)
             (pipeline-aux (rest command-list)
                           next-pipe))
            (eval-p
             (dolist (pid *children*)
               (sb-posix:waitpid pid 0))
             eval-value)
            (t
             (dolist (pid (rest *children*))
               (sb-posix:waitpid pid 0))
             (ash (nth-value 1 (sb-posix:waitpid (first *children*) 0)) -8))))))

(defun pipeline (input)
  (let ((command-list (split-sequence "|" input :test #'equal))
        (*children* '()))
    (pipeline-aux command-list nil)))

(defun true-p (x)
  (or (and (integerp x) (zerop x))
      (not (null x))))

(defun list-&& (input)
  (let ((pos
         (position-if (lambda (x)
                        (or (equal x "&&")
                            (equal x "||")))
                      input)))
    (cond ((null pos)
           (pipeline input))
          ((equal "&&" (elt input pos))
           (let ((status (true-p (pipeline (subseq input 0 pos)))))
             (if (true-p status)
                 (list-&& (subseq input (1+ pos)))
                 status)))
          (t
           (let ((status (true-p (pipeline (subseq input 0 pos)))))
             (if (true-p status)
                 status
                 (list-&& (subseq input (1+ pos)))))))))

(defun replace-alias (input)
  (loop for pos from 0
        for prev = nil then curr
        for curr in input
        if (or (zerop pos) (member prev '("&&" "||" "|") :test #'equal))
          append (get-alias curr)
        else
          collect curr))

(defun run (input)
  (let ((status (list-&& (replace-alias input))))
    (if (listp status)
      (mapc #'pprint status)
      (set-last-status status))
    status))
