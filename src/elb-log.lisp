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
           :log-key-content))
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

(defun log-key-content (log-key &key (bucket *log-bucket*))
  (get-string (bucket-name (log-bucket-bucket bucket)) (log-key-key log-key)
              :credentials (log-bucket-elb-log bucket)))
