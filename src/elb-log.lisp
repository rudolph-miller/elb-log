(in-package :cl-user)
(defpackage elb-log
  (:use :cl
        :elb-log.util)
  (:import-from :zs3
                :bucket-listing
                :bucket-name
                :key
                :name
                :access-key
                :secret-key
                :query-bucket
                :keys
                :get-string)
  (:import-from :ppcre
                :register-groups-bind)
  (:import-from :local-time
                :timestamp)
  (:export :*elb-log*
           :*log-bucket*
           :elb-log
           :elb-log-credentials
           :elb-log-bucket-name
           :make-elb-log
           :log-bucket
           :log-bucket-bucket
           :log-bucket-elb-log
           :make-log-bucket
           :with-elb-log
           :log-key
           :log-key-account-id
           :log-key-region
           :log-key-date
           :log-key-elb-name
           :log-key-timestamp
           :log-key-elb-ip
           :log-key-hash
           :log-key-key
           :log-keys
           :log-line
           :log-line-time
           :log-line-elb-name
           :log-line-client
           :log-line-client-port
           :log-line-backend
           :log-line-backend-port
           :log-line-request-processing-time
           :log-line-backend-processing-time
           :log-line-request-processing-time
           :log-line-elb-status-code
           :log-line-backend-status-code
           :log-line-received-bytes
           :log-line-sent-bytes
           :log-line-request-method
           :log-line-request-uri
           :log-line-request-protocol
           :log-lines))
(in-package :elb-log)

(defvar *elb-log* nil)

(defvar *log-bucket* nil)

(defstruct (elb-log (:constructor %make-elb-log))
  (credentials nil :type cons)
  (bucket-name nil :type string))

(defun make-elb-log (credentials bucket-name)
  (%make-elb-log :credentials credentials :bucket-name bucket-name))

(defmethod access-key ((obj elb-log))
  (car (elb-log-credentials obj)))

(defmethod secret-key ((obj elb-log))
  (cdr (elb-log-credentials obj)))

(defstruct (log-bucket (:constructor %make-log-bucket))
  (bucket nil :type (or null bucket-listing))
  (elb-log nil :type (or null elb-log)))

(defun make-log-bucket (&optional (obj *elb-log*))
  (let ((bucket (query-bucket (elb-log-bucket-name obj) :credentials obj)))
    (%make-log-bucket :bucket bucket
                      :elb-log obj)))

(defmacro with-elb-log ((credentials bucket-name) &body body)
  `(let* ((*elb-log* (make-elb-log ,credentials ,bucket-name))
          (*log-bucket* (make-log-bucket)))
     ,@body))

(defstruct log-key
  (account-id nil :type (or null string))
  (region nil :type (or null string))
  (date nil :type (or null timestamp))
  (elb-name nil :type (or null string))
  (timestamp nil :type (or null timestamp))
  (elb-ip nil :type (or null string))
  (hash nil :type (or null string))
  (key nil :type (or null key)))

(defun log-keys (&optional (bucket *log-bucket*))
  (loop for key across (keys (log-bucket-bucket bucket))
        for log-key = (register-groups-bind (account-id region date elb-name timestamp elb-ip hash) (*key-scanner* (name key))
                        (make-log-key :account-id account-id
                                      :region region
                                      :date (parse-date date)
                                      :elb-name elb-name
                                      :timestamp (parse-timestamp timestamp)
                                      :elb-ip elb-ip
                                      :hash hash
                                      :key key))
        when log-key
          collecting log-key))

(defstruct log-line
  (time nil :type (or null string))
  (elb-name nil :type (or null string))
  (client nil :type (or null string))
  (client-port nil :type (or null string))
  (backend nil :type (or null string))
  (backend-port nil :type (or null string))
  (request-processing-time nil :type (or null string))
  (backend-processing-time nil :type (or null string))
  (response-processing-time nil :type (or null string))
  (elb-status-code nil :type (or null string))
  (backend-status-code nil :type (or null string))
  (received-bytes nil :type (or null string))
  (sent-bytes nil :type (or null string))
  (request-method nil :type (or null string))
  (request-uri nil :type (or null string))
  (request-protocol nil :type (or null string)))

(defun make-log-line-from-string (string)
  (register-groups-bind (time elb-name client client-port backend backend-port request-processing-time
                         backend-processing-time response-processing-time elb-status-code backend-status-code
                         received-bytes sent-bytes request-method request-uri request-protocol)
      (*log-line-scanner* string)
    (make-log-line :time time
                   :elb-name elb-name
                   :client client
                   :client-port client-port
                   :backend backend
                   :backend-port backend-port
                   :request-processing-time request-processing-time
                   :backend-processing-time backend-processing-time
                   :request-processing-time response-processing-time
                   :elb-status-code elb-status-code
                   :backend-status-code backend-status-code
                   :received-bytes received-bytes
                   :sent-bytes sent-bytes
                   :request-method request-method
                   :request-uri request-uri
                   :request-protocol request-protocol)))

(defun log-lines (log-key &key (bucket *log-bucket*))
  (let ((stream (make-string-input-stream (get-string (bucket-name (log-bucket-bucket bucket))
                                                      (log-key-key log-key)
                                                      :credentials (log-bucket-elb-log bucket)))))
    (loop for line = (read-line stream nil)
          while line
          collecting (make-log-line-from-string line))))
