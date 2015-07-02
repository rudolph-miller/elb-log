#|
  This file is a part of elb-log project.
  Copyright (c) 2015 Rudolph Miller
|#

#|
  ELB log manager for Common Lisp

  Author: Rudolph Miller
|#

(in-package :cl-user)
(defpackage elb-log-asd
  (:use :cl :asdf))
(in-package :elb-log-asd)

(defsystem elb-log
  :version "0.1"
  :author "Rudolph Miller"
  :license "MIT"
  :homepage "https://github.com/Rudolph-Miller/elb-log"
  :depends-on (:cl-syntax
               :cl-syntax-interpol
               :cl-syntax-annot
               :cl-annot-prove
               :cl-ppcre
               :zs3
               :local-time)
  :components ((:module "src"
                :serial t
                :components
                ((:file "util")
                 (:file "struct")
                 (:file "elb-log"))))
  :description "ELB log manager for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :perform (test-op (op c)
                    (uiop:symbol-call :cl-annot-prove :run-system-tests c)))
