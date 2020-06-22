(defpackage #:aws-sdk/credentials
  (:use #:cl)
  (:import-from #:aws-sdk/credentials/base
                #:make-credentials
                #:credentials
                #:retrieve
                #:credentials-keys
                #:credentials-headers)
  (:import-from #:aws-sdk/credentials/env
                #:env-provider)
  (:import-from #:aws-sdk/credentials/shared
                #:shared-provider)
  (:import-from #:aws-sdk/credentials/ec2role
                #:ec2role-provider)
  (:export #:credentials
           #:make-credentials
           #:default-aws-credentials
           #:credentials-keys
           #:credentials-headers))
(in-package #:aws-sdk/credentials)

(defun chained-providers ()
  (list (make-instance 'env-provider)
        (make-instance 'shared-provider)
        (make-instance 'ec2role-provider)))

(defun default-aws-credentials (&optional (providers 'chained-providers))
  (loop for provider in (funcall providers)
        for credentials = (retrieve provider)
        when credentials
          do (return credentials)))
