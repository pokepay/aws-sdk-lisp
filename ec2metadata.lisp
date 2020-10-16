(defpackage #:aws-sdk/ec2metadata
  (:use #:cl)
  (:import-from #:aws-sdk/connection-cache
                #:*use-connection-cache*)
  (:import-from #:dexador)
  (:import-from #:trivial-timeout
                #:with-timeout
                #:timeout-error)
  (:export #:ec2metadata
           #:ec2-region))
(in-package #:aws-sdk/ec2metadata)

(defun ec2metadata (path)
  (with-timeout (5)
    (dex:get (format nil "http://169.254.169.254/latest/meta-data~A"
                     (or path "/"))
             :keep-alive *use-connection-cache*)))

(defun ec2-region ()
  (handler-case
      (let ((res (ec2metadata "/placement/availability-zone")))
        (subseq res 0 (1- (length res))))
    (timeout-error () nil)))
