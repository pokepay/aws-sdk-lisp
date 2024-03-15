(defpackage #:aws-sdk/request
  (:use #:cl)
  (:import-from #:aws-sdk/session
                #:*session*)
  (:import-from #:quri
                #:url-encode-params
                #:make-uri
                #:render-uri)
  (:import-from #:assoc-utils
                #:alistp)
  (:export #:request
           #:request-service
           #:request-method
           #:request-path
           #:request-params
           #:request-headers
           #:request-payload
           #:request-session
           #:request-host
           #:request-endpoint
           #:request-signing-name))
(in-package #:aws-sdk/request)

(defparameter *global-service-endpoints* '("iam"
                                           "globalaccelerator"
                                           "cloudfront"
                                           "networkmanager"
                                           "organizations"
                                           "route53"
                                           "shield"
                                           "waf"))

(defclass request ()
  ((service :initarg :service
            :initform (error ":service is required")
            :reader request-service)
   (host-prefix :initarg :host-prefix
                :initform nil
                :reader request-host-prefix)
   (global-host :initarg :global-host
                :initform nil
                :reader request-global-host)
   (api-version :initarg :api-version
                :initform (error ":api-version is required")
                :reader request-api-version)
   (operation :initarg :operation
              :initform (error ":operation is required")
              :reader request-operation)
   (signing-name :initarg :signing-name
                 :initform nil
                 :reader request-signing-name)
   (method :initarg :method
           :initform (error ":method is required")
           :reader request-method)
   (path :initarg :path
         :initform "/"
         :accessor request-path)
   (params :initarg :params
           :initform nil
           :accessor request-params)
   (headers :initarg :headers
            :initform nil
            :accessor request-headers)
   (payload :initarg :payload
            :initform nil
            :accessor request-payload)
   (session :initarg :session
            :initform *session*
            :reader request-session)))

(defgeneric request-host (request region)
  (:method ((req request) region)
    (if (member (request-service req) *global-service-endpoints* :test #'string=)
        (or (request-global-host req)
            (format nil "~(~A~).amazonaws.com" (request-service req)))
        (format nil "~(~A~).~(~A~).amazonaws.com" (request-host-prefix req) region))))

(defgeneric request-endpoint (request region)
  (:method ((req request) region)
    (with-slots (path params) req
      (quri:render-uri
       (quri:make-uri  :scheme "https"
                       :host (request-host req region)
                       :path path)))))
