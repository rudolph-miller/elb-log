(with-elb-log  ((cons (asdf::getenv "AWS_ACCESS_KEY") (asdf::getenv "AWS_SECRET_KEY")) "elb-log")
  (dolist (key (log-keys))
    (do-something (log-lines key))))

;;; Same as above

(let* ((elb-log (make-elb-log (cons (asdf::getenv "AWS_ACCESS_KEY") (asdf::getenv "AWS_SECRET_KEY")) "elb-log"))
       (log-bucket (make-log-bucket elb-log)))
  (dolist (key (log-keys log-bucket))
    (do-something (log-lines key))))
