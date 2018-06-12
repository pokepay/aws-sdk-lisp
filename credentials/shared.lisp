(defpackage #:aws-sdk/credentials/shared
  (:use #:cl)
  (:import-from #:aws-sdk/credentials/base
                #:make-credentials
                #:provider
                #:retrieve
                #:expiredp)
  (:import-from #:aws-sdk/utils/config
                #:read-from-file
                #:*aws-profile*)
  (:import-from #:aws-sdk/utils
                #:getenv)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:shared-provider))
(in-package #:aws-sdk/credentials/shared)

(defclass shared-provider (provider)
  ((file :initarg :file
         :initform #P"~/.aws/credentials")
   (profile :initarg :profile
            :initform *aws-profile*)

   (retrievedp :initform nil)))

(defun provider-profile (provider)
  (or (getenv "AWS_PROFILE")
      (slot-value provider 'profile)))

(defun read-config (provider)
  (with-slots (file) provider
    (when (probe-file file)
      (read-from-file file
                      :profile (provider-profile provider)))))

(defmethod retrieve ((provider shared-provider))
  (with-slots (retrievedp) provider
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
