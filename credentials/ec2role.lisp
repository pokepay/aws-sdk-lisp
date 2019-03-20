(defpackage #:aws-sdk/credentials/ec2role
  (:use #:cl)
  (:import-from #:aws-sdk/credentials/base
                #:make-credentials
                #:provider
                #:retrieve
                #:provider-expiration)
  (:import-from #:aws-sdk/ec2metadata
                #:ec2metadata)
  (:import-from #:trivial-timeout
                #:timeout-error)
  (:import-from #:cl-ppcre)
  (:import-from #:yason)
  (:import-from #:local-time)
  (:export #:ec2role-provider))
(in-package #:aws-sdk/credentials/ec2role)

(defclass ec2role-provider (provider)
  ())

(defmethod retrieve ((provider ec2role-provider))
  (handler-case
      (let ((role (ppcre:scan-to-strings "^.+?(?=(?:[\\r\\n]|$))"
                                         (ec2metadata "/iam/security-credentials/"))))
        (when role
          (let ((res (yason:parse
                      (ec2metadata (format nil "/iam/security-credentials/~A" role)))))
            (setf (provider-expiration provider)
                  (local-time:parse-timestring (gethash "Expiration" res)))
            (make-credentials
             :access-key-id (gethash "AccessKeyId" res)
             :secret-access-key (gethash "SecretAccessKey" res)
             :session-token (gethash "Token" res)
             :provider-name "ec2role-provider"))))
    (timeout-error () nil)))
