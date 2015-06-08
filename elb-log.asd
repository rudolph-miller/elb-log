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
  :license "LLGPL"
  :depends-on (:cl-ppcre
               :zs3)
  :components ((:module "src"
                :components
                ((:file "elb-log"))))
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
  :in-order-to ((test-op (test-op elb-log-test))))
