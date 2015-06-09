(in-package :cl-user)
(defpackage elb-log-test.struct
  (:use :cl
        :prove
        :elb-log.util
        :elb-log.struct))
(in-package :elb-log-test.struct)

(plan nil)

(subtest "elb-log")

(subtest "log-bucket")

(subtest "log-key")

(subtest "log-line")

(finalize)
