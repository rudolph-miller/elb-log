(in-package :cl-user)
(defpackage elb-log
  (:use :cl
        :elb-log.util
        :elb-log.struct)
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
  (:import-from :local-time
                :timestamp
                :today
                :parse-timestring)
  (:export ;; globals
           :*elb-log*
           :*log-bucket*
           :*log-date*

           ;; struct

           ;; elb-log
           :elb-log
           :elb-log-credentials
           :elb-log-bucket-name
           :elb-log-accout-id
           :elb-log-region

           ;; log-bucket
           :log-bucket
           :log-bucket-bucket
           :log-bucket-elb-log

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
           :log-line-request-processing-time
           :log-line-elb-status-code
           :log-line-backend-status-code
           :log-line-received-bytes
           :log-line-sent-bytes
           :log-line-request-method
           :log-line-request-uri
           :log-line-request-protocol

           ;; macros
           :with-elb-log
           :with-specified-date-elb-log
           :with-this-elb-log

           ;; util
           :log-keys
           :log-lines))
(in-package :elb-log)

(defvar *elb-log* nil)

(defvar *log-bucket* nil)

(defvar *log-date* nil)

(defun set-accout-id-and-region (obj)
  (let ((bucket (query-bucket (elb-log-bucket-name obj) :max-keys 5 :credentials obj)))
    (loop for key across (keys bucket)
          for log-key = (make-log-key key)
          when log-key
            do (setf (elb-log-account-id obj) (log-key-account-id log-key)
                     (elb-log-region obj) (log-key-region log-key))
               (return-from set-accout-id-and-region t)
          finally (error "Could not set-accout-id and region."))))

(defun format-bucket-prefix (date &optional (obj *elb-log*))
  (unless (and (elb-log-account-id obj)
               (elb-log-region obj))
    (set-accout-id-and-region obj))
  (format nil "AWSLogs/~a/elasticloadbalancing/~a/~a" (elb-log-account-id obj) (elb-log-region obj) (format-date date)))

(defun make-log-bucket (&optional (obj *elb-log*) (date *log-date*))
  (let ((bucket (query-bucket (elb-log-bucket-name obj)
                              :credentials obj
                              :prefix (when date (format-bucket-prefix date)))))
    (%make-log-bucket :bucket bucket
                      :elb-log obj)))

(defmacro with-elb-log ((credentials bucket-name) &body body)
  `(let* ((*elb-log* (make-elb-log ,credentials ,bucket-name))
          (*log-bucket* (make-log-bucket)))
     ,@body))

(defmacro with-specified-date-elb-log (date (credentials bucket-name) &body body)
  `(let ((*log-date* ,date))
     (with-elb-log (,credentials ,bucket-name) ,@body)))

(defmacro with-this-elb-log ((credentials bucket-name) &body body)
  `(with-specified-date-elb-log (today) (,credentials ,bucket-name) ,@body))

(defun log-keys (&optional (bucket *log-bucket*))
  (loop for key across (keys (log-bucket-bucket bucket))
        for log-key = (make-log-key key)
        when log-key
          collecting log-key))

(defun log-lines (log-key &key (bucket *log-bucket*))
  (let ((stream (make-string-input-stream (get-string (bucket-name (log-bucket-bucket bucket))
                                                      (log-key-key log-key)
                                                      :credentials (log-bucket-elb-log bucket)))))
    (loop for line = (read-line stream nil)
          while line
          collecting (make-log-line line))))
