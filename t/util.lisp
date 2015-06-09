(in-package :cl-user)
(defpackage elb-log-test.util
  (:use :cl
        :prove
        :ppcre
        :local-time
        :elb-log.util))
(in-package :elb-log-test.util)

(plan nil)

(subtest "*key-scanner*"
  )

(subtest "*timestamp-scanner*"
  )

(subtest "*log-line-scanner*"
  )

(subtest "parse-date")

(subtest "parse-timestamp")

(subtest "format-date")

(finalize)
