(defpackage #:aws-sdk/credentials/shared
  (:use #:cl)
  (:import-from #:aws-sdk/credentials/base
                #:make-credentials
                #:provider
                #:retrieve
                #:expiredp)
  (:import-from #:aws-sdk/utils/config
                #:read-from-file)
  (:import-from #:aws-sdk/utils
                #:getenv)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:shared-provider))
(in-package #:aws-sdk/credentials/shared)

(defclass shared-provider (provider)
  ((file :initarg :file
         :initform #P"~/.aws/credentials")
   (profile :initarg :profile)

   (retrievedp :initform nil)))

(defun provider-profile (provider)
  (or (getenv "AWS_PROFILE")
      (and (slot-boundp provider 'profile)
           (slot-value provider 'profile))
      "default"))

(defun read-config (provider)
  (read-from-file (slot-value provider 'file)
                  :profile (provider-profile provider)))

(defmethod retrieve ((provider shared-provider))
  (with-slots (file retrievedp) provider
    (setf retrievedp nil)
    (let ((creds (read-config provider)))
      (when creds
        (setf retrievedp t)
        (make-credentials
         :access-key-id (aget creds "aws_access_key_id")
         :secret-access-key (aget creds "aws_secret_access_key")
         :session-token (aget creds "aws_session_token")
         :provider-name "shared-provider")))))

(defmethod expiredp ((provider shared-provider))
  (not (slot-value provider 'retrievedp)))
