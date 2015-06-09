(in-package :cl-user)
(defpackage elb-log-test.init
  (:use :cl)
  (:export :*sample-key*
           :*sample-log*))
(in-package :elb-log-test.init)

(defvar *sample-key* "AWSLogs/123456789012/elasticloadbalancing/us-west-2/2014/02/15/123456789012_elasticloadbalancing_us-west-2_my-loadbalancer_20140215T2340Z_172.160.001.192_20sg8hgm.log")

(defvar *sample-log* "2014-02-15T23:39:43.945958Z my-loadbalancer 192.168.131.39:2817 10.0.0.1:80 0.000073 0.001048 0.000057 200 200 0 29 \"GET http://www.example.com:80/ HTTP/1.1\"")
