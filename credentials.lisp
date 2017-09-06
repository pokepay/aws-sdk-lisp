(defpackage #:aws-sdk/credentials
  (:use #:cl)
  (:import-from #:aws-sdk/credentials/base
                #:make-credentials
                #:credentials
                #:retrieve
                #:credential-keys
                #:credential-headers)
  (:import-from #:aws-sdk/credentials/env
                #:env-provider)
  (:import-from #:aws-sdk/credentials/shared
                #:shared-provider)
  (:import-from #:aws-sdk/credentials/ec2role
                #:ec2role-provider)
  (:export #:credentials
           #:make-credentials
           #:aws-credentials
           #:credential-keys
           #:credential-headers))
(in-package #:aws-sdk/credentials)

(defvar *chained-providers*
  (list (make-instance 'env-provider)
        (make-instance 'shared-provider)
        (make-instance 'ec2role-provider)))

(defun aws-credentials ()
  (loop for provider in *chained-providers*
        for credentials = (retrieve provider)
        when credentials
          do (return credentials)))
