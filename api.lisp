(defpackage #:aws-sdk/api
  (:use #:cl)
  (:import-from #:aws-sdk/credentials
                #:aws-credentials)
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
  (let ((environment-region (uiop:getenv "AWS_REGION")))
    (or (if (and environment-region
                 (string/= environment-region ""))
            environment-region
            (aget (read-from-file #P"~/.aws/config") "region"))
        (error "AWS region is not configured."))))

(defun aws-host (service)
  (format nil "~(~A~).~(~A~).amazonaws.com" service (aws-region)))

(defun aws-request (&key (path "/") service method params headers payload)
  (let ((host (aws-host service))
        (aws-sign4:*aws-credentials* (or aws-sign4:*aws-credentials*
                                         #'aws-credentials)))
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
                              ("Content-Type" . "application/x-amz-json-1.0")
                              ,@headers)
                   :content payload))))
