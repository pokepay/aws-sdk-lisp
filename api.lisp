(defpackage #:aws-sdk/api
  (:use #:cl)
  (:import-from #:aws-sdk/session
                #:*session*
                #:session-credentials
                #:session-region)
  (:import-from #:aws-sdk/credentials
                #:credentials-keys
                #:credentials-headers
                #:default-aws-credentials)
  (:import-from #:aws-sdk/request
                #:request
                #:request-session
                #:request-service
                #:request-headers
                #:request-payload
                #:request-method
                #:request-path
                #:request-params
                #:request-host
                #:request-endpoint)
  (:import-from #:aws-sign4)
  (:import-from #:dexador)
  (:import-from #:quri)
  (:export #:*session*
           #:aws-request))
(in-package #:aws-sdk/api)

(defun aws-request (req)
  (check-type req request)
  (let* ((session (request-session req))
         (credentials (or (session-credentials session)
                          (default-aws-credentials)))
         (region (session-region session))
         (service (request-service req)))
    (unless credentials
      (error "No credentials are found"))
    (unless region
      (error "AWS region is not configured"))
    (let ((aws-sign4:*aws-credentials* (lambda () (credentials-keys credentials)))
          (headers (append (credentials-headers credentials)
                           (request-headers req)))
          (payload (request-payload req)))
      (multiple-value-bind (authorization x-amz-date)
          (aws-sign4:aws-sign4 :region region
                               :service service
                               :method (request-method req)
                               :host (request-host req region)
                               :path (request-path req)
                               :params (mapcar (lambda (kv)
                                                 (if (null (cdr kv))
                                                     (cons (car kv) "")
                                                     kv))
                                               (request-params req))
                               :headers headers
                               :payload (or payload ""))
        (multiple-value-list
          (handler-bind ((dex:http-request-failed #'dex:ignore-and-continue))
            (dex:request (request-endpoint req region)
                         :method (request-method req)
                         :headers `(("Authorization" . ,authorization)
                                    ("X-Amz-Date" . ,x-amz-date)
                                    ("X-Amz-Content-Sha256" . ,(aws-sdk/utils::sha-256 (or payload "")))
                                    ("Content-Type" . "application/x-amz-json-1.0")
                                    ,@headers)
                         :content payload)))))))
