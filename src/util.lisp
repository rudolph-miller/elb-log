(in-package :cl-user)
(defpackage elb-log.util
  (:use :cl)
  (:import-from :ppcre
                :create-scanner
                :register-groups-bind)
  (:import-from :local-time
                :+utc-zone+
                :parse-timestring
                :encode-timestamp
                :format-timestring)
  (:export :*key-scanner*
           :*log-line-scanner*
           :parse-date
           :parse-timestamp
           :format-date))
(in-package :elb-log.util)

(syntax:use-syntax :cl-interpol)

(defvar *key-scanner* (create-scanner #?/^AWSLogs\/([0-9]{12})\/elasticloadbalancing\/(.+?)\/([0-9]{4}\/[0-9]{2}\/[0-9]{2})\/[0-9]{12}_elasticloadbalancing_.+?_([^_]+)_([0-9]{8}T[0-9]{4}Z)_(.+?)_(.+).log/))

(defvar *timestamp-scanner* (create-scanner #?/^([1-9][0-9]{3})(0[1-9]|11|12)(0[1-9]|[1-3][0-9])T([0-2][0-9])([0-5][0-9])Z$/))

(defvar *log-line-scanner* (create-scanner #?/^(\d{4}-\d{2}-\d{2}T\d{2}\:\d{2}\:\d{2}\.\d{6}Z) (.+?) (.+)\:(.+) (.+)\:(.+) (.+?) (.+?) (.+?) (.+?) (.+?) (.+?) (.+?) \"(.+?) (.+?) (.+?)\"$/))

(defun parse-date (date)
  (parse-timestring date :date-separator #\/))

(defun parse-timestamp (timestamp)
  (register-groups-bind ((#'parse-integer year month day hour minute)) (*timestamp-scanner* timestamp)
    (encode-timestamp 0 0 minute hour day month year :timezone +utc-zone+)))

(defun format-date (date)
  (format-timestring nil date :format '(:year "/" (:month 2 #\0) "/" (:day 2 #\0)) :timezone +utc-zone+))
