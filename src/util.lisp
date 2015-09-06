(in-package :cl-user)
(defpackage elb-log.util
  (:use :cl
        :annot.prove)
  (:import-from :ppcre
                :create-scanner
                :register-groups-bind
                :scan-to-strings)
  (:import-from :local-time
                :+utc-zone+
                :parse-timestring
                :encode-timestamp
                :format-timestring
                :timestamp=))
(in-package :elb-log.util)

(syntax:use-syntax :cl-interpol)
(syntax:use-syntax :annot)

@export
(defvar *sample-key* "AWSLogs/123456789012/elasticloadbalancing/us-west-2/2014/02/15/123456789012_elasticloadbalancing_us-west-2_my-loadbalancer_20140215T2340Z_172.160.001.192_20sg8hgm.log")

@export
(defvar *sample-log* "2014-02-15T23:39:43.945958Z my-loadbalancer 192.168.131.39:2817 10.0.0.1:80 0.000073 0.001048 0.000057 200 200 0 29 \"GET http://www.example.com:80/ HTTP/1.1\" \"curl/7.38.0\" - -")

@export
@tests
((is (scan-to-strings *key-scanner* *sample-key*)
     *sample-key*
     "can scan the whole key."))
(defvar *key-scanner* (create-scanner #?/^AWSLogs\/([0-9]{12})\/elasticloadbalancing\/(.+?)\/([0-9]{4}\/[0-9]{2}\/[0-9]{2})\/[0-9]{12}_elasticloadbalancing_.+?_([^_]+)_([0-9]{8}T[0-9]{4}Z)_(.+?)_(.+).log/))

(defvar *timestamp-scanner* (create-scanner #?/^([1-9][0-9]{3})(0[1-9]|11|12)(0[1-9]|[1-3][0-9])T([0-2][0-9])([0-5][0-9])Z$/))

@export
@tests
((is (scan-to-strings *log-line-scanner* *sample-log*)
     *sample-log*
     "can scan the whole line."))
(defvar *log-line-scanner* (create-scanner #?/^(\d{4}-\d{2}-\d{2}T\d{2}\:\d{2}\:\d{2}\.\d{6}Z) (.+?) ([\d.]+)\:(\d+) ([\d.]+)\:(\d+) (.+?) (.+?) (.+?) (.+?) (.+?) (.+?) (.+?) \"(.+?) (.+?) (.+?)\" \"(.+?)\" (.+?) (.+?)$/))

@export
@tests
((is (parse-date "2014/12/31")
     (encode-timestamp 0 0 0 0 31 12 2014 :timezone +utc-zone+)
     "can parse YYYY/MM/DD."
     :test #'timestamp=))
(defun parse-date (date)
  (parse-timestring date :date-separator #\/))

@export
@tests
((is (parse-timestamp "20140215T2339Z")
     (encode-timestamp 0 0 39 23 15 02 2014 :timezone +utc-zone+)
     "can parse YYYYMMDDTHHmmZ."
     :test #'timestamp=))
(defun parse-timestamp (timestamp)
  (register-groups-bind ((#'parse-integer year month day hour minute)) (*timestamp-scanner* timestamp)
    (encode-timestamp 0 0 minute hour day month year :timezone +utc-zone+)))

@export
@tests
((is (format-date (encode-timestamp 0 0 0 0 31 12 2014 :timezone +utc-zone+))
     "2014/12/31"
     "can format date."))
(defun format-date (date)
  (format-timestring nil date :format '(:year "/" (:month 2 #\0) "/" (:day 2 #\0)) :timezone +utc-zone+))

@export
(defmacro with-stubs ((&rest fn-name-body-alist) &body body)
  (let ((stub-table (gensym "stub")))
    `(let ((,stub-table (make-hash-table)))
       ,@(mapcar #'(lambda (item)
                     `(setf (gethash ',(car item) ,stub-table)
                            (symbol-function ',(car item))))
                 fn-name-body-alist)
       (unwind-protect
            (progn
              ,@(mapcar #'(lambda (item)
                            `(setf (symbol-function ',(car item)) ,(cdr item)))
                        fn-name-body-alist)
                   ,@body)
         ,@(mapcar #'(lambda (item)
                       `(setf (symbol-function ',(car item))
                              (gethash ',(car item) ,stub-table)))
                   fn-name-body-alist)))))

