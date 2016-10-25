(in-package :cl-user)
(defpackage :sizimi-asd
  (:use :cl :asdf))
(in-package :sizimi-asd)

(defsystem :sizimi
  :depends-on (:cffi
               :cl-readline
               :cl-ppcre
               :alexandria
               :split-sequence
               :trivial-gray-streams)
  :components ((:module "src"
                :serial t
                :components ((:file "fd-streams")
                             (:file "error")
                             (:file "util")
                             (:file "env")
                             (:file "complete")
                             (:file "reader")
                             (:file "run")
                             (:file "sizimi")
                             (:file "sizimi-user")))))
