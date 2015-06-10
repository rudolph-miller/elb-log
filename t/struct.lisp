(in-package :cl-user)
(defpackage elb-log-test.struct
  (:use :cl
        :prove
        :local-time
        :elb-log.util
        :elb-log.struct
        :elb-log-test.init))
(in-package :elb-log-test.struct)

(plan nil)

(subtest "elb-log"
  (let ((obj (make-elb-log (cons "ACCESS_KEY" "SERCET_KEY") "elb-log")))
    (is-type obj
             'elb-log
             "can make-elb-log.")

    (is (elb-log-credentials obj)
        (cons "ACCESS_KEY" "SERCET_KEY")
        "can set credentials.")

    (is (elb-log-bucket-name obj)
        "elb-log"
        "can set bucket-name.")))

(subtest "log-bucket"
  (skip 1 "have to test via S3 or stub connection."))

(subtest "log-key"
  (skip 1 "have to test via S3 or stub connection."))

(subtest "log-line"
  (let ((obj (make-log-line *sample-log*)))
    (is-type obj
             'log-line
             "can make-log-line.")

    (is (log-line-time obj)
        (encode-timestamp 945958000 43 39 23 15 2 2014 :timezone +utc-zone+)
        "can set time."
        :test #'timestamp=)

    (is (log-line-elb-name obj)
        "my-loadbalancer"
        "can set elb-name.")

    (is (log-line-client obj)
        "192.168.131.39"
        "can set client.")

    (is (log-line-client-port obj)
        2817
        "can set client-port.")

    (is (log-line-backend obj)
        "10.0.0.1"
        "can set backend.")

    (is (log-line-backend-port obj)
        80
        "can set backend-port.")

    (is (log-line-request-processing-time obj)
        0.000073
        "can set request-processing-time.")

    (is (log-line-backend-processing-time obj)
        0.001048
        "can set backend-processing-time.")

    (is (log-line-response-processing-time obj)
        0.000057
        "can set response-processing-time.")

    (is (log-line-elb-status-code obj)
        200
        "can set elb-status-code.")

    (is (log-line-backend-status-code obj)
        200
        "can set backend-status-code.")

    (is (log-line-received-bytes obj)
        0
        "can set received-bytes.")

    (is (log-line-sent-bytes obj)
        29
        "can set sent-bytes.")

    (is (log-line-request-method obj)
        "GET"
        "can set request-method.")

    (is (log-line-request-uri obj)
        "http://www.example.com:80/"
        "can set request-uri.")

    (is (log-line-request-protocol obj)
        "HTTP/1.1"
        "can set request-protocol.")))

(finalize)
