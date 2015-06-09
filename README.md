# Elb Log - ELB log manager for Common Lisp
[![Build Status](https://circleci.com/gh/Rudolph-Miller/elb-log.svg?style=shield)](https://circleci.com/gh/Rudolph-Miller/elb-log)

## Usage

```Lisp
(with-elb-log  ((cons (asdf::getenv "AWS_ACCESS_KEY") (asdf::getenv "AWS_SECRET_KEY")) "elb-log")
  (dolist (key (log-keys))
    (do-something (log-lines key))))

;;; Same as above

(let* ((elb-log (make-elb-log (cons (asdf::getenv "AWS_ACCESS_KEY") (asdf::getenv "AWS_SECRET_KEY")) "elb-log"))
       (log-bucket (make-log-bucket elb-log)))
  (dolist (key (log-keys log-bucket))
    (do-something (log-lines key))))
```

## API

## Author

* Rudolph Miller

## Copyright

Copyright (c) 2015 Rudolph Miller

## License

Licensed under the LLGPL License.
