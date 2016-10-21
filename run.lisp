(in-package :cl-user)
(defpackage :sizimi.run
  (:use
   :cl
   :split-sequence
   :sizimi.error
   :sizimi.util)
  (:import-from
   :alexandria
   :destructuring-ecase)
  (:import-from
   :sizimi.env
   :set-last-status)
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

(cffi:defcvar *errno* :int)
(cffi:defcfun ("execvp" %execvp) :int (file :pointer) (argv :pointer))
(cffi:defcfun ("strerror" %strerror) :string (errno :int))

(defparameter +stdin+ 0)
(defparameter +stdout+ 1)
(defparameter +stderr+ 2)

(defvar *virtual-targets* nil)
(defvar *aliases* (make-hash-table :test 'equal))

(defun arg-to-string (arg)
  (princ-to-string arg))

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

(defun register-virtual-target (object function)
  (push (cons object function)
        *virtual-targets*)
  (values))

(defun show-alias-1 (name value)
  (format t "alias ~S ~S~%" name value))

(defun show-aliases ()
  (maphash #'show-alias-1
           *aliases*))

(defun show-alias (name)
  (multiple-value-bind (value foundp)
      (gethash name *aliases*)
    (when foundp
      (show-alias-1 name value))))

(defun set-alias (name value)
  (setf (gethash (arg-to-string name) *aliases*)
        (arg-to-string value)))

(defun alias (&rest args)
  (let ((n (length args)))
    (case n
      (0 (show-aliases))
      (1 (show-alias (first args)))
      (otherwise
         (set-alias (first args) (second args)))))
  (values))

(defun unalias (name)
  (remhash name *aliases*)
  (values))

(defun aliasp (name)
  (gethash (arg-to-string name) *aliases*))

(setf (get '& 'quote-args) t)
(defun & (&rest argv)
  (multiple-value-bind (command args redirect-spec virtual-redirect-spec)
      (parse-argv argv)
    (assert (not virtual-redirect-spec))
    (run-command (coerce-command (string command))
                 args
                 redirect-spec
                 nil
                 nil)))

(defun cd (&optional (dir (user-homedir-pathname)))
  (let ((result (uiop:chdir dir)))
    (if (zerop result)
      (let ((newdir (uiop:getcwd)))
        (setf *default-pathname-defaults* newdir)
        newdir)
      nil)))

(defun sh (string)
  (run (read-input-from-string string)))

(defun execvp-1 (file args)
  (cffi:with-foreign-string (command file)
    (let ((argc (1+ (length args))))
      (cffi:with-foreign-object (argv :string (1+ argc))
        (loop for i from 1
              for arg in args
              do (setf (cffi:mem-aref argv :string i) arg))
        (setf (cffi:mem-aref argv :string 0) command)
        (setf (cffi:mem-aref argv :string argc) (cffi:null-pointer))
        (when (minusp (%execvp command argv))
          (cond
            ((null (search-path file))
             (format t "~A: command not found~%" file))
            (t
             (uiop:println (%strerror *errno*))))
          (uiop:quit *errno*))))))

(defun execvp (file args)
  (execvp-1 file
            (mapcar #'arg-to-string
                    args)))

(defun search-path (file)
  (map-path (lambda (file1)
              (when (equal file (pathname-name file1))
                (return-from search-path
                  (princ-to-string file1))))))

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
  (mapcar #'namestring (directory str)))

(defun maybe-expand-alias (argv)
  (let ((string (aliasp (first argv))))
    (if string
      (append (read-input-from-string string)
              (rest argv))
      argv)))

(defun parse-argv (argv)
  (setf argv (maybe-expand-alias argv))
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
    (values (first argv)
            (nreverse args)
            (delete-duplicates (nreverse redirect-specs)
                               :key #'second)
            virtual-redirect-spec)))

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

(defun get-standard-stream-symbol-from-fd (fd)
  (cond ((= fd +stdin+)
         '*standard-input*)
        ((= fd +stdout+)
         '*standard-output*)
        ((= fd +stderr+)
         '*error-output*)
        (t
         (error "invalid fd: ~D" fd))))

(defun lisp-eval (x redirect-specs virtual-redirect-spec
                  &key
                  (stdin *standard-input*)
                  (stdout *standard-output*)
                  (upcase t))
  (let ((*standard-input* stdin)
        (*standard-output* stdout))
    (let ((cleanup-hooks))
      (flet ((set-stream (symbol new-value)
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
                                  (get-standard-stream-symbol-from-fd left)
                                  '*standard-input*)
                                (if (integerp right)
                                  (get-standard-stream-symbol-from-fd right)
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
                    cleanup-hooks)))))
      (unwind-protect (handler-case
                          (multiple-value-list
                           (eval (if upcase
                                   (symbol-upcase-tree x)
                                   x)))
                        (error (c)
                          (uiop:println c)
                          -1))
        (mapc 'funcall cleanup-hooks)))))

(defun lisp-apply (symbol args
                   redirect-specs virtual-redirect-spec
                   &key
                   (stdin *standard-input*)
                   (stdout *standard-output*))
  (lisp-eval (cons symbol
                   (if (get symbol 'quote-args)
                     (mapcar (lambda (arg)
                               `(quote ,arg))
                             args)
                     args))
             redirect-specs virtual-redirect-spec
             :stdin stdin
             :stdout stdout
             :upcase (not (get symbol 'quote-args))))

(defun run-command-internal (file args redirect-specs virtual-redirect-spec
                             child-hook before-parent-hook after-parent-hook)
  (let ((virtual-pipes (when virtual-redirect-spec
                         (multiple-value-list (sb-posix:pipe)))))
    (let ((pid (sb-posix:fork)))
      (cond
        ((zerop pid)
         (handler-bind ((error (lambda (c)
                                 (warn c)
                                 (uiop:quit -1))))
           (when child-hook (funcall child-hook))
           (proceed-redirects-for-fd redirect-specs)
           (when virtual-pipes
             (destructuring-bind (read-fd write-fd) virtual-pipes
               (sb-posix:dup2 write-fd +stdout+)
               (sb-posix:close write-fd)
               (sb-posix:close read-fd)))
           (execvp file args)))
        (t
         (let (output-str)
           (when before-parent-hook (funcall before-parent-hook pid))
           (when virtual-redirect-spec
             (destructuring-bind (read-fd write-fd) virtual-pipes
               (sb-posix:close write-fd)
               (let* ((count 100)
                      (buf (cffi:foreign-alloc :unsigned-char
                                               :count count
                                               :initial-element 0))
                      (octets (make-array count :element-type '(unsigned-byte 8))))
                 (setf output-str
                       (apply #'concatenate 'string
                              (loop for n = (sb-posix:read read-fd buf count)
                                    until (zerop n)
                                    collect (loop for i from 0 below n
                                                  do (setf (aref octets i)
                                                           (cffi:mem-aref buf :unsigned-char i))
                                                  finally (return (babel:octets-to-string
                                                                   octets :end n)))))))
               (sb-posix:close read-fd)))
           (let ((result nil))
             (when after-parent-hook
               (setf result (funcall after-parent-hook pid)))
             (when output-str
               (destructuring-bind (type target) virtual-redirect-spec
                 (funcall target output-str type)))
             result)))))))

(defun run-command (file args redirect-specs virtual-redirect-spec wait-p)
  (run-command-internal file
                        args
                        redirect-specs
                        virtual-redirect-spec
                        nil
                        nil
                        (lambda (pid)
                          (if wait-p
                            (ash (nth-value 1 (sb-posix:waitpid pid 0)) -8)
                            pid))))

(defun coerce-command (cmdname)
  (or (search-path cmdname)
      cmdname))

(defun command-type (command)
  (typecase command
    (cons
       :compound-lisp-form)
    (symbol
       (if (fboundp (symbol-upcase command))
         :simple-lisp-form
         (coerce-command (string command))))
    (string
       (coerce-command command))
    (otherwise
       :simple-lisp-form)))

(defun simple-command (command)
  (multiple-value-bind (first args redirect-specs virtual-redirect-spec)
      (parse-argv command)
    (let ((command (command-type first)))
      (case command
        (:compound-lisp-form
           (lisp-eval `(progn ,first ,@args)
                      redirect-specs
                      virtual-redirect-spec))
        (:simple-lisp-form
           (lisp-apply first args
                       redirect-specs
                       virtual-redirect-spec))
        (otherwise
           (run-command command
                        args
                        redirect-specs
                        virtual-redirect-spec
                        t))))))

(defun call-with-pipeline (prev-read-fd
                           prev-write-fd
                           next-read-fd
                           next-write-fd
                           nextp
                           body-fn)
  (declare (ignore next-read-fd))
  (let ((stdin
          (when prev-read-fd
            (sb-sys:make-fd-stream prev-read-fd :input t)))
        (stdout
          (when nextp
            (sb-sys:make-fd-stream next-write-fd :output t))))
    (when prev-write-fd
      (sb-posix:close prev-write-fd))
    (unwind-protect
         (funcall body-fn stdin stdout)
      (when prev-read-fd (sb-posix:close prev-read-fd))
      (when stdin (close stdin))
      (when stdout (close stdout)))))

(defun pipeline-aux (pids pipeline-pos command-list
                     prev-read-fd prev-write-fd
                     last-eval-status)
  (if (null command-list)
    last-eval-status
    (multiple-value-bind (next-read-fd next-write-fd)
        (sb-posix:pipe)
      (multiple-value-bind (first args redirect-specs virtual-redirect-spec)
          (parse-argv (first command-list))
        (let ((command (command-type first)))
          (case command
            ((:compound-lisp-form
              :simple-lisp-form)
               (setf last-eval-status
                     (call-with-pipeline
                      prev-read-fd prev-write-fd next-read-fd next-write-fd
                      (rest command-list)
                      (if (eq command :simple-lisp-form)
                        (lambda (stdin stdout)
                          (lisp-apply first args
                                      redirect-specs
                                      virtual-redirect-spec
                                      :stdin (or stdin *standard-input*)
                                      :stdout (or stdout *standard-output*)))
                        (lambda (stdin stdout)
                          (lisp-eval `(progn ,first ,@args)
                                     redirect-specs
                                     virtual-redirect-spec
                                     :stdin (or stdin *standard-input*)
                                     :stdout (or stdout *standard-output*)))))))
            (otherwise
               (run-command-internal
                command
                args
                redirect-specs
                virtual-redirect-spec
                (lambda ()
                  (when prev-read-fd
                    (sb-posix:close prev-write-fd)
                    (sb-posix:dup2 prev-read-fd +stdin+)
                    (sb-posix:close prev-read-fd))
                  (when (rest command-list)
                    (sb-posix:close next-read-fd)
                    (sb-posix:dup2 next-write-fd +stdout+)
                    (sb-posix:close next-write-fd)))
                (lambda (pid)
                  (setf (aref pids pipeline-pos) pid)
                  (when prev-read-fd
                    (sb-posix:close prev-read-fd)
                    (sb-posix:close prev-write-fd)))
                #'identity)))))
      (pipeline-aux pids
                    (1+ pipeline-pos)
                    (rest command-list)
                    next-read-fd
                    next-write-fd
                    last-eval-status))))

(defun pipeline (input)
  (let ((command-list (split-sequence "|" input :test #'equal)))
    (if (= 1 (length command-list))
      (simple-command (first command-list))
      (let* ((pids (make-array (length command-list) :initial-element nil))
             (last-eval-status (pipeline-aux pids 0 command-list nil nil 0))
             (status))
        (loop for i from 0
              for pid across pids
              when pid do
                (setf status
                      (ash (nth-value 1 (sb-posix:waitpid pid 0)) -8)))
        (if (aref pids (1- (length pids)))
          status
          last-eval-status)))))

(defun true-p (x)
  (or (and (integerp x) (= x 0))
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

(defun run (input)
  (let ((status (list-&& input)))
    (if (listp status)
      (mapc #'pprint status)
      (set-last-status status))
    status))
