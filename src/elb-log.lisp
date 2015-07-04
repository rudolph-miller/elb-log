(in-package :cl-user)
(defpackage elb-log
  (:use :cl
        :annot.doc
        :annot.prove
        :elb-log.util)
  (:import-from :zs3
                :bucket-name
                :name
                :query-bucket
                :continue-bucket-query
                :keys
                :truncatedp
                :get-string)
  (:import-from :local-time
                :+utc-zone+
                :encode-timestamp
                :timestamp=
                :today)
  (:import-from :elb-log.struct
                :elb-log
                :elb-log-credentials
                :elb-log-bucket-name
                :elb-log-account-id
                :elb-log-region
                :make-elb-log

                :log-bucket
                :log-bucket-buckets
                :log-bucket-elb-log
                :%make-log-bucket

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
                :make-log-line)
  (:export ;; globals
           :*elb-log*
           :*log-bucket*
           :*log-date*

           ;; struct

           ;; elb-log
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
           :make-log-bucket

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

(syntax:use-syntax :cl-annot)

(defmacro with-stub-make-log-bucket (&body body)
  `(with-stubs ((make-log-bucket . #'(lambda (&optional (elb-log *elb-log*))
                                       (%make-log-bucket :buckets nil
                                                         :elb-log elb-log))))
     ,@body))

@doc
"Default value of #S(elb-log)."
(defvar *elb-log* nil)

@doc
"Default value of #S(log-bucket)."
(defvar *log-bucket* nil)

@doc
"Default value of date used by #'make-log-bucket."
(defvar *log-date* nil)

(defun set-accout-id-and-region (elb-log)
  (let ((bucket (query-bucket (elb-log-bucket-name elb-log) :max-keys 5 :credentials elb-log)))
    (loop for key across (keys bucket)
          for log-key = (make-log-key key)
          when log-key
            do (setf (elb-log-account-id elb-log) (log-key-account-id log-key)
                     (elb-log-region elb-log) (log-key-region log-key))
               (return-from set-accout-id-and-region t)
          finally (error "Could not set-accout-id and region."))))

@tests.around
(let ((date (encode-timestamp 0 0 0 0 31 12 2014 :timezone +utc-zone+))
      (elb-log (make-elb-log (cons "ACCESS_KEY" "SECRET_KEY") "elb-log")))
  (setf (elb-log-account-id elb-log) "ACCOUNT_ID")
  (setf (elb-log-region elb-log) "ap-northeast-1")
  (call-tests))
@tests
((is (format-bucket-prefix date elb-log)
        "AWSLogs/ACCOUNT_ID/elasticloadbalancing/ap-northeast-1/2014/12/31"
        "can format bucket prefix with the date."))
(defun format-bucket-prefix (date &optional (elb-log *elb-log*))
  (unless (and (elb-log-account-id elb-log)
               (elb-log-region elb-log))
    (set-accout-id-and-region elb-log))
  (format nil "AWSLogs/~a/elasticloadbalancing/~a/~a" (elb-log-account-id elb-log) (elb-log-region elb-log) (format-date date)))

@doc
"Return #S(log-bucket).
ELB-LOG should be #S(elb-log).
DATE should be an instance of loca-time:timestamp."
(defun make-log-bucket (&optional (elb-log *elb-log*) (date *log-date*) max-keys)
  (when (> max-keys 1000)
    (warn "Note that Amazon currently limits responses to at most 1000 key objects even if max-keys is greater than 1000
"))
  (let* ((zs3:*credentials* elb-log)
         (buckets (loop for bucket = (query-bucket (elb-log-bucket-name elb-log)
                                                   :prefix (when date (format-bucket-prefix date))
                                                   :max-keys max-keys)
                          then (continue-bucket-query bucket)
                        collecting bucket
                        while (and (or (null max-keys)
                                       (> (decf max-keys (length (keys bucket))) 0))
                                   (truncatedp bucket)))))
    (%make-log-bucket :buckets buckets
                      :elb-log elb-log)))

@tests.around
(with-stub-make-log-bucket
  (let ((*default-test-function* #'equalp)
        (credentials (cons "ACCESS_KEY" "SECRET_KEY"))
        (bucket-name "elb-log"))
    (with-elb-log (credentials bucket-name)
      (call-tests))))
@tests
((is *elb-log*
     (make-elb-log credentials bucket-name)
     "can bind *elb-log*.")

 (is *log-bucket*
     (%make-log-bucket :buckets nil
                       :elb-log *elb-log*)
     "can bind *log-bucket*."))
@doc
"Bind *elb-log* to #S(elb-log credentials bucket-name),
*log-bucket* to #S(log-bucket bucket *elb-log*)."
(defmacro with-elb-log ((credentials bucket-name) &body body)
  `(let* ((*elb-log* (make-elb-log ,credentials ,bucket-name))
          (*log-bucket* (make-log-bucket)))
     ,@body))

@tests.around
(with-stub-make-log-bucket
  (let ((*default-test-function* #'equalp)
        (date (encode-timestamp 0 0 0 0 31 12 2014))
        (credentials (cons "ACCESS_KEY" "SECRET_KEY"))
        (bucket-name "elb-log"))
    (with-specified-date-elb-log date (credentials bucket-name)
      (call-tests))))
@tests
((is *log-date*
     date
     "can bind *log-date*.")

 (is *elb-log*
     (make-elb-log credentials bucket-name)
     "can bind *elb-log*.")

 (is *log-bucket*
     (%make-log-bucket :buckets nil
                       :elb-log *elb-log*)
     "can bind *log-bucket*."))
@doc
"Bind *elb-log* to #S(elb-log credentials bucket-name),
*log-bucket* to #S(log-bucket bucket *elb-log*),
*log-date* to date."
(defmacro with-specified-date-elb-log (date (credentials bucket-name) &body body)
  `(let ((*log-date* ,date))
     (with-elb-log (,credentials ,bucket-name) ,@body)))

@tests.around
(with-stub-make-log-bucket
  (let ((*default-test-function* #'equalp)
        (credentials (cons "ACCESS_KEY" "SECRET_KEY"))
        (bucket-name "elb-log"))
    (with-this-elb-log (credentials bucket-name)
      (call-tests))))
@tests
((is *log-date*
     (today)
     "can bind *log-date*."
     :test #'timestamp=)

 (is *elb-log*
     (make-elb-log credentials bucket-name)
     "can bind *elb-log*.")

 (is *log-bucket*
     (%make-log-bucket :buckets nil
                       :elb-log *elb-log*)
     "can bind *log-bucket*."))
@doc
"Bind *elb-log* to #S(elb-log credentials bucket-name),
*log-bucket* to #S(log-bucket bucket *elb-log*),
*log-date* to (local-time:today)."
(defmacro with-this-elb-log ((credentials bucket-name) &body body)
  `(with-specified-date-elb-log (today) (,credentials ,bucket-name) ,@body))

@doc
"Return a list of #S(log-key)."
(defun log-keys (&optional (log-bucket *log-bucket*))
  (loop for bucket in (log-bucket-buckets log-bucket)
        nconc (loop for key across (keys bucket)
                    for log-key = (make-log-key key)
                    when log-key
                      collecting log-key)))

@doc
"Return a list of #S(log-line)."
(defun log-lines (log-key &key (bucket *log-bucket*))
  (let ((stream (make-string-input-stream (get-string (bucket-name (car (log-bucket-buckets bucket)))
                                                      (log-key-key log-key)
                                                      :credentials (log-bucket-elb-log bucket)))))
    (loop for line = (read-line stream nil)
          while line
          collecting (make-log-line line))))
