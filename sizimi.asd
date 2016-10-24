(in-package :cl-user)
(defpackage :sizimi-asd
  (:use :cl :asdf))
(in-package :sizimi-asd)

(defsystem :sizimi
  :depends-on (:cffi
               :cl-readline
               :cl-ppcre
               :alexandria
               :split-sequence)
  :components ((:module "src"
                :serial t
                :components ((:file "error")
                             (:file "util")
                             (:file "env")
                             (:file "complete")
                             (:file "reader")
                             (:file "run")
                             (:file "sizimi")
                             (:file "sizimi-user")))))
