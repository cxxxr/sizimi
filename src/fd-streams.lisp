(in-package :cl-user)
(defpackage :sizimi.fd-streams
  (:use :cl :trivial-gray-streams)
  (:export
   :*default-buffer-size*
   :fd-input-stream
   :fd-output-stream))
(in-package :sizimi.fd-streams)

(cffi:defcfun ("read" %read) :long (fd :int) (buf :pointer) (count :unsigned-long))
(cffi:defcfun ("write" %write) :long (fd :int) (buf :pointer) (count :unsigned-long))

(defun utf8-nbytes (code)
  (cond
   ((<= code #x7f) 1)
   ((<= #xc2 code #xdf) 2)
   ((<= #xe0 code #xef) 3)
   ((<= #xf0 code #xf4) 4)
   (t 1)))

(defparameter *default-buffer-size* 100)

(defclass fd-stream ()
  ((fd
    :initarg :fd
    :reader fd-stream-fd
    :type fixnum)
   (freed-buffer-p
    :initform nil
    :accessor fd-stream-freed-buffer-p
    :type boolean)
   (buffer
    :reader fd-stream-buffer
    :writer fd-stream-set-buffer)
   (buffer-size
    :initarg :buffer-size
    :initform *default-buffer-size*
    :reader fd-stream-buffer-size)))

(defmethod initialize-instance :after ((stream fd-stream) &rest initargs)
  (declare (ignore initargs))
  (fd-stream-set-buffer
   (cffi:foreign-alloc :unsigned-char
                       :count (fd-stream-buffer-size stream)
                       :initial-element 0)
   stream)
  stream)

(defmethod close ((stream fd-stream) &key abort)
  (declare (ignore abort))
  (unless (fd-stream-freed-buffer-p stream)
    (setf (fd-stream-freed-buffer-p stream) t)
    (cffi:foreign-free (fd-stream-buffer stream))))

(defclass fd-input-stream (fd-stream
                           fundamental-character-input-stream
                           fundamental-binary-input-stream)
  ((pos
    :initform 0
    :accessor fd-input-stream-pos
    :type fixnum)
   (len
    :initform 0
    :accessor fd-input-stream-len
    :type (or null fixnum))
   (peek-char
    :initform nil
    :accessor fd-input-stream-peek-char
    :type (or null character))
   (unread-char
    :initform nil
    :accessor fd-input-stream-unread-char
    :type (or null character))))

(defun %read-char-1byte (stream nextp)
  (when (>= (fd-input-stream-pos stream)
            (fd-input-stream-len stream))
    (setf (fd-input-stream-pos stream) 0)
    (let ((v (%read (fd-stream-fd stream)
                    (fd-stream-buffer stream)
                    (fd-stream-buffer-size stream))))
      (setf (fd-input-stream-len stream)
            (if (zerop v) nil v))
      (when (zerop v)
        (return-from %read-char-1byte :eof))))
  (let ((code (cffi:mem-aref (fd-stream-buffer stream)
                             :unsigned-char
                             (fd-input-stream-pos stream))))
    (when nextp
      (incf (fd-input-stream-pos stream)))
    code))

(defun stream-read-char-internal (stream)
  (let ((code (%read-char-1byte stream nil)))
    (if (eq :eof code)
      :eof
      (let ((nbytes (utf8-nbytes code)))
        (if (= nbytes 1)
          (progn
            (incf (fd-input-stream-pos stream))
            (code-char code))
          (let ((octets (make-array nbytes :element-type '(unsigned-byte 8))))
            (setf (aref octets 0) code)
            (loop repeat nbytes
                  for i from 0
                  for o = (%read-char-1byte stream t)
                  do (if (eq :eof o)
                       (return)
                       (setf (aref octets i) o)))
            (schar (babel:octets-to-string octets) 0)))))))

(defmethod stream-read-char ((stream fd-input-stream))
  (cond
    ((fd-input-stream-unread-char stream)
     (prog1 (fd-input-stream-unread-char stream)
       (setf (fd-input-stream-unread-char stream) nil)))
    (t
     (setf (fd-input-stream-peek-char stream) nil)
     (stream-read-char-internal stream))))

(defmethod stream-unread-char ((stream fd-input-stream) char)
  (setf (fd-input-stream-unread-char stream) char)
  nil)

(defmethod stream-read-char-no-hang ((stream fd-input-stream))
  (and (stream-listen stream)
       (stream-read-char stream)))

(defmethod stream-peek-char ((stream fd-input-stream))
  (or (fd-input-stream-unread-char stream)
      (fd-input-stream-peek-char stream)
      (setf (fd-input-stream-peek-char stream)
            (stream-read-char-internal stream))))

(defmethod stream-listen ((stream fd-input-stream))
  (not (eq :eof (stream-peek-char stream))))

#+nil (defmethod stream-read-line ((stream fd-input-stream)))
#+nil (defmethod stream-clear-input ((stream fd-input-stream)))

(defmethod stream-read-byte ((stream fd-input-stream))
  (%read-char-1byte stream t))

(defclass fd-output-stream (fd-stream
                            fundamental-character-output-stream
                            fundamental-binary-output-stream)
  ((column
    :initform 0
    :accessor fd-output-stream-column
    :type fixnum)
   (start-line-p
    :initform t
    :accessor fd-output-stream-start-line-p
    :type boolean)))

(defmethod stream-write-char ((stream fd-output-stream) char)
  (setf (fd-output-stream-start-line-p stream)
        (if (find char '(#\newline #\return)) t nil))
  (let ((octets (babel:string-to-octets (string char)))
        (buffer (fd-stream-buffer stream)))
    (loop for o across octets
          for i from 0
          do (setf (cffi:mem-aref buffer :unsigned-char i) o))
    (%write (fd-stream-fd stream) buffer (length octets)))
  char)

(defmethod stream-start-line-p ((stream fd-output-stream))
  (fd-output-stream-start-line-p stream))

(defmethod stream-write-string ((stream fd-output-stream) string &optional start end)
  (let* ((octets (babel:string-to-octets string :start start :end end))
         (octets-length (length octets))
         (buffer (fd-stream-buffer stream))
         (buffer-size (fd-stream-buffer-size stream))
         (fd (fd-stream-fd stream)))
    (loop with octets-index = 0
          while (< octets-index octets-length)
          do (loop with count = 0
                   while (< octets-index octets-length)
                   for i from 0 below buffer-size
                   for o = (aref octets octets-index)
                   do (setf (fd-output-stream-start-line-p stream)
                            (if (find o '#.(mapcar #'char-code '(#\newline #\return)))
                              t
                              nil))
                      (incf count)
                      (setf (cffi:mem-aref buffer :unsigned-char i) o)
                      (incf octets-index)
                   finally (%write fd buffer count))))
  string)

#+nil(defmethod stream-fresh-line ((stream fd-output-stream)))
#+nil(defmethod stream-finish-output ((stream fd-output-stream)))
#+nil(defmethod stream-force-output ((stream fd-output-stream)))
#+nil(defmethod stream-clear-output ((stream fd-output-stream)))
#+nil(defmethod stream-advance-to-column ((stream fd-output-stream) column))

(defmethod stream-write-byte ((stream fd-output-stream) integer)
  (let ((buffer (fd-stream-buffer stream)))
    (setf (cffi:mem-aref buffer :unsigned-char 0) integer)
    (%write (fd-stream-fd stream) buffer 1))
  integer)
