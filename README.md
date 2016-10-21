# sizimi
shell

## install
    $ ros install cxxxr/sizimi

## usage
    $ sizimi

## examples

   内部コマンドはlispの関数

    (in-package :sizimi-user)
    (defun strip-whitespaces (&rest args)
      (declare (ignorable args))
      (loop for line = (read-line nil nil)
        while line
        do (format t "~A~%" (string-trim '(#\space #\tab) line))))

    $ cat text.txt | strip-whitespaces > text.txt.2 && mv text.txt.2 text.txt

リダイレクト先を関数に指定

    (in-package :sizimi-user)
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

    $ echo "hello world" > /dev/foo
    $ echo "append" >> /dev/foo
    $ princ *text*
    hello world
    append

非同時実行は内部コマンド

    $ & emacs

## Licence
    MIT
