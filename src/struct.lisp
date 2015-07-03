(in-package :cl-user)
(defpackage elb-log.struct
  (:use :cl
        :annot.doc
        :annot.prove
        :elb-log.util)
  (:import-from :zs3
                :key
                :name
                :access-key
                :secret-key)
  (:import-from :ppcre
                :register-groups-bind)
  (:import-from :local-time
                :timestamp
                :parse-timestring)
  (:export ;; elb-log
           :elb-log
           :elb-log-credentials
           :elb-log-bucket-name
           :elb-log-account-id
           :elb-log-region
           :make-elb-log

           ;; log-bucket
           :log-bucket
           :log-bucket-buckets
           :log-bucket-elb-log
           :%make-log-bucket

           ;; log-key
           :log-key
           :log-key-account-id
           :log-key-region
           :log-key-date
           :log-key-elb-name
           :log-key-timestamp
           :log-key-elb-ip
           :log-key-hash
           :log-key-key
           :make-log-key

           ;; log-line
           :log-line
           :log-line-time
           :log-line-elb-name
           :log-line-client
           :log-line-client-port
           :log-line-backend
           :log-line-backend-port
           :log-line-request-processing-time
           :log-line-backend-processing-time
           :log-line-response-processing-time
           :log-line-elb-status-code
           :log-line-backend-status-code
           :log-line-received-bytes
           :log-line-sent-bytes
           :log-line-request-method
           :log-line-request-uri
           :log-line-request-protocol
           :make-log-line))
(in-package :elb-log.struct)

(syntax:use-syntax :cl-annot)

@tests.around
(let ((obj (make-elb-log (cons "ACCESS_KEY" "SECRET_KEY") "elb-log")))
  (call-tests))
@tests
((is-type obj
          'elb-log
          "can make-elb-log.")

 (is (elb-log-credentials obj)
     (cons "ACCESS_KEY" "SECRET_KEY")
     "can set credentials.")

 (is (elb-log-bucket-name obj)
     "elb-log"
     "can set bucket-name."))
@doc
"Struct of account information."
(defstruct (elb-log (:constructor %make-elb-log))
  (credentials nil :type cons)
  (bucket-name nil :type string)
  (account-id nil :type (or null string))
  (region nil :type (or null string)))

@doc
"Return #S(elb-log credentials bucket-name).
CREDENTIALS should be (cons \"AWS_ACCESS_KEY\" \"AWS_SECRET_KEY\").
BUCKET-NAME should be bucket name of ELB log."
(defun make-elb-log (credentials bucket-name)
  (%make-elb-log :credentials credentials :bucket-name bucket-name))

(defmethod access-key ((obj elb-log))
  (car (elb-log-credentials obj)))

(defmethod secret-key ((obj elb-log))
  (cdr (elb-log-credentials obj)))

@doc
"Struct of ELB log bucket."
(defstruct (log-bucket (:constructor %make-log-bucket))
  (buckets nil :type list)
  (elb-log nil :type (or null elb-log)))

@doc
"Struct of ELB log object key."
(defstruct (log-key (:constructor %make-log-key))
  (account-id nil :type (or null string))
  (region nil :type (or null string))
  (date nil :type (or null timestamp))
  (elb-name nil :type (or null string))
  (timestamp nil :type (or null timestamp))
  (elb-ip nil :type (or null string))
  (hash nil :type (or null string))
  (key nil :type (or null key)))

(defun make-log-key (key)
  (register-groups-bind (account-id region date elb-name timestamp elb-ip hash) (*key-scanner* (name key))
    (%make-log-key :account-id account-id
                  :region region
                  :date (parse-date date)
                  :elb-name elb-name
                  :timestamp (parse-timestamp timestamp)
                  :elb-ip elb-ip
                  :hash hash
                  :key key)))

@tests.around
(let ((obj (make-log-line elb-log.util::*sample-log*)))
  (call-tests))
@tests
((is-type obj
          'log-line
          "can make-log-line.")

 (is (log-line-time obj)
     (local-time:encode-timestamp 945958000 43 39 23 15 2 2014 :timezone local-time:+utc-zone+)
     "can set time."
     :test #'local-time:timestamp=)

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
     "can set request-protocol."))
@doc
"Struct of ELB log line."
(defstruct (log-line (:constructor %make-log-line))
  (time nil :type (or null timestamp))
  (elb-name nil :type (or null string))
  (client nil :type (or null string))
  (client-port nil :type (or null integer))
  (backend nil :type (or null string))
  (backend-port nil :type (or null integer))
  (request-processing-time nil :type (or null float))
  (backend-processing-time nil :type (or null float))
  (response-processing-time nil :type (or null float))
  (elb-status-code nil :type (or null integer))
  (backend-status-code nil :type (or null integer))
  (received-bytes nil :type (or null integer))
  (sent-bytes nil :type (or null integer))
  (request-method nil :type (or null string))
  (request-uri nil :type (or null string))
  (request-protocol nil :type (or null string)))

(defun make-log-line (string)
  (register-groups-bind ((#'parse-timestring time) elb-name client (#'parse-integer client-port) backend
                         (#'parse-integer backend-port)
                         (#'read-from-string request-processing-time backend-processing-time response-processing-time)
                         (#'parse-integer elb-status-code backend-status-code received-bytes sent-bytes)
                         request-method request-uri request-protocol)
      (*log-line-scanner* string)
    (%make-log-line :time time
                    :elb-name elb-name
                    :client client
                    :client-port client-port
                    :backend backend
                    :backend-port backend-port
                    :request-processing-time request-processing-time
                    :backend-processing-time backend-processing-time
                    :response-processing-time response-processing-time
                    :elb-status-code elb-status-code
                    :backend-status-code backend-status-code
                    :received-bytes received-bytes
                    :sent-bytes sent-bytes
                    :request-method request-method
                    :request-uri request-uri
                    :request-protocol request-protocol)))

