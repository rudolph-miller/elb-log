(in-package :cl-user)
(defpackage elb-log-test.util
  (:use :cl
        :prove
        :ppcre
        :local-time
        :elb-log.util
        :elb-log-test.init))
(in-package :elb-log-test.util)

(plan nil)

(subtest "*key-scanner*"
  (is (scan-to-strings *key-scanner* *sample-key*)
      *sample-key*
      "can scan the whole key."))

(subtest "*log-line-scanner*"
  (is (scan-to-strings *log-line-scanner* *sample-log*)
      *sample-log*
      "can scan the whole line.")

  (is (scan-to-strings *log-line-scanner* *sample-log2*)
      *sample-log2*
      "can scan the whole line with empty user-agent."))

(subtest "parse-date"
  (is (parse-date "2014/12/31")
      (encode-timestamp 0 0 0 0 31 12 2014 :timezone +utc-zone+)
      "can parse YYYY/MM/DD."
      :test #'timestamp=))

(subtest "parse-timestamp"
  (is (parse-timestamp "20140215T2339Z")
      (encode-timestamp 0 0 39 23 15 02 2014 :timezone +utc-zone+)
      "can parse YYYYMMDDTHHmmZ."
      :test #'timestamp=))

(subtest "format-date"
  (is (format-date (encode-timestamp 0 0 0 0 31 12 2014 :timezone +utc-zone+))
      "2014/12/31"
      "can format date."))

(finalize)
