(in-package :cl-user)
(defpackage elb-log-test
  (:use :cl
        :elb-log.util
        :elb-log.struct
        :elb-log
        :prove)
  (:import-from :elb-log
                :set-accout-id-and-region
                :format-bucket-prefix
                :make-log-bucket))
(in-package :elb-log-test)

(plan nil)

(subtest "set-accout-id-and-region")

(subtest "format-bucket-prefix")

(subtest "make-log-bucket")

(subtest "with-elb-log")

(subtest "with-specified-date-elb-log")

(subtest "with-this-elb-log")

(subtest "log-keys")

(subtest "log-lines")

(finalize)
