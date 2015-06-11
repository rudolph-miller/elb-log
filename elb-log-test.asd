#|
  This file is a part of elb-log project.
  Copyright (c) 2015 Rudolph Miller
|#

(in-package :cl-user)
(defpackage elb-log-test-asd
  (:use :cl :asdf))
(in-package :elb-log-test-asd)

(defsystem elb-log-test
  :author "Rudolph Miller"
  :license "MIT"
  :depends-on (:elb-log
               :prove)
  :components ((:module "t"
                :components
                ((:file "init")
                 (:test-file "util")
                 (:test-file "struct")
                 (:test-file "elb-log"))))
  :description "Test system for elb-log"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
