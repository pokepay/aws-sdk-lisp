(defpackage #:aws-sdk/api
  (:use #:cl)
  (:import-from #:aws-sdk/credentials
                #:aws-credentials
                #:credential-keys
                #:credential-headers)
  (:import-from #:aws-sdk/ec2metadata
                #:ec2-region)
  (:import-from #:aws-sdk/utils
                #:getenv)
  (:import-from #:aws-sdk/utils/config
                #:read-from-file)
  (:import-from #:aws-sign4)
  (:import-from #:dexador)
  (:import-from #:quri)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:aws-request))
(in-package #:aws-sdk/api)

(defun aws-region ()
  (or (getenv "AWS_REGION")
      (and (probe-file #P"~/.aws/config")
           (aget (read-from-file #P"~/.aws/config"
                                 :profile (or (getenv "AWS_PROFILE")
                                              "default"))
                 "region"))
      (ec2-region)
      (error "AWS region is not configured.")))

(defun aws-host (service)
  (format nil "~(~A~).~(~A~).amazonaws.com" service (aws-region)))

(defun aws-request (&key (path "/") service method params headers payload
                      credentials)
  (let* ((credentials (or credentials (aws-credentials)))
         (host (aws-host service))
         (aws-sign4:*aws-credentials* (or aws-sign4:*aws-credentials*
                                          (lambda () (credential-keys credentials)))))
    (multiple-value-bind (authorization x-amz-date)
        (aws-sign4:aws-sign4 :region (aws-region)
                             :service service
                             :method method
                             :host host
                             :path path
                             :params params
                             :headers headers
                             :payload (or payload ""))
      (dex:request (format nil "http://~A~A?~A" host path
                           (quri:url-encode-params params))
                   :method method
                   :headers `(("Authorization" . ,authorization)
                              ("X-Amz-Date" . ,x-amz-date)
                              ,@(credential-headers credentials)
                              ("Content-Type" . "application/x-amz-json-1.0")
                              ,@headers)
                   :content payload))))
