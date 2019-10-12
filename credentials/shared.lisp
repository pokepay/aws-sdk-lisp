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
  (:import-from #:assoc-utils
                #:aget)
  (:export #:shared-provider))
(in-package #:aws-sdk/credentials/shared)

(defclass shared-provider (provider)
  ((file :initarg :file
         :initform (merge-pathnames ".aws/credentials"
                                    (user-homedir-pathname)))
   (profile :initarg :profile
            :initform *aws-profile*
            :accessor provider-profile)

   (retrievedp :initform nil)))

(defun read-credentials (provider)
  (with-slots (file) provider
    (when (probe-file file)
      (read-from-file file
                      :profile (provider-profile provider)))))

(defmethod retrieve ((provider shared-provider))
  (with-slots (retrievedp) provider
    (setf retrievedp nil)
    (let ((section (read-credentials provider)))
      (when section
        (setf retrievedp t)
        (make-credentials
         :access-key-id (aget section "aws_access_key_id")
         :secret-access-key (aget section "aws_secret_access_key")
         :session-token (aget section "aws_session_token")
         :provider-name "shared-provider")))))

(defmethod expiredp ((provider shared-provider))
  (not (slot-value provider 'retrievedp)))
