(in-package :cl-user)
(defpackage elb-log-test
  (:use :cl
        :prove
        :local-time
        :elb-log.util
        :elb-log.struct
        :elb-log
        :elb-log-test.init)
  (:import-from :elb-log
                :set-accout-id-and-region
                :format-bucket-prefix
                :make-log-bucket))
(in-package :elb-log-test)

(plan nil)

(subtest "set-accout-id-and-region"
  (skip 1 "have to test via S3 or stub connection."))

(subtest "format-bucket-prefix"
  (let ((date (encode-timestamp 0 0 0 0 31 12 2014 :timezone +utc-zone+))
        (elb-log (make-elb-log (cons "ACCESS_KEY" "SECRET_KEY") "elb-log")))
    (setf (elb-log-account-id elb-log) "ACCOUNT_ID")
    (setf (elb-log-region elb-log) "ap-northeast-1")
    (is (format-bucket-prefix date elb-log)
        "AWSLogs/ACCOUNT_ID/elasticloadbalancing/ap-northeast-1/2014/12/31"
        "can format bucket prefix with the date.")))

(subtest "make-log-bucket"
  (skip 1 "have to test via S3."))

(subtest "with-elb-log"
  (with-stub-make-log-bucket
    (let ((*default-test-function* #'equalp)
          (credentials (cons "ACCESS_KEY" "SECRET_KEY"))
          (bucket-name "elb-log"))
      (with-elb-log (credentials bucket-name)
        (is *elb-log*
            (make-elb-log credentials bucket-name)
            "can bind *elb-log*.")

        (is *log-bucket*
            (%make-log-bucket :bucket nil
                              :elb-log *elb-log*)
            "can bind *log-bucket*.")))))

(subtest "with-specified-date-elb-log"
  (with-stub-make-log-bucket
    (let ((*default-test-function* #'equalp)
          (date (encode-timestamp 0 0 0 0 31 12 2014))
          (credentials (cons "ACCESS_KEY" "SECRET_KEY"))
          (bucket-name "elb-log"))
      (with-specified-date-elb-log date (credentials bucket-name)
        (is *log-date*
            date
            "can bind *log-date*.")

        (is *elb-log*
            (make-elb-log credentials bucket-name)
            "can bind *elb-log*.")

        (is *log-bucket*
            (%make-log-bucket :bucket nil
                              :elb-log *elb-log*)
            "can bind *log-bucket*.")))))

(subtest "with-this-elb-log"
  (with-stub-make-log-bucket
    (let ((*default-test-function* #'equalp)
          (credentials (cons "ACCESS_KEY" "SECRET_KEY"))
          (bucket-name "elb-log"))
      (with-this-elb-log (credentials bucket-name)
        (is *log-date*
            (today)
            "can bind *log-date*."
            :test #'timestamp=)

        (is *elb-log*
            (make-elb-log credentials bucket-name)
            "can bind *elb-log*.")

        (is *log-bucket*
            (%make-log-bucket :bucket nil
                              :elb-log *elb-log*)
            "can bind *log-bucket*.")))))

(subtest "log-keys"
  (skip 1 "have to test via S3."))

(subtest "log-lines"
  (skip 1 "have to test via S3."))

(finalize)
