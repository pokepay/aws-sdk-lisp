(defpackage #:aws-sdk/credentials/env
  (:use #:cl)
  (:import-from #:aws-sdk/credentials/base
                #:make-credentials
                #:provider
                #:retrieve
                #:expiredp)
  (:import-from #:aws-sdk/utils
                #:getenv)
  (:export #:env-provider))
(in-package #:aws-sdk/credentials/env)

(defclass env-provider (provider)
  ((retrievedp :initform nil)))

(defmethod retrieve ((provider env-provider))
  (setf (slot-value provider 'retrievedp) nil)
  (let ((access-key-id (or (getenv "AWS_ACCESS_KEY_ID")
                           (getenv "AWS_ACCESS_KEY")))
        (secret-access-key (or (getenv "AWS_SECRET_ACCESS_KEY")
                               (getenv "AWS_SECRET_KEY"))))
    (when (and access-key-id
               secret-access-key)
      (setf (slot-value provider 'retrievedp) t)
      (make-credentials
       :access-key-id access-key-id
       :secret-access-key secret-access-key
       :session-token (getenv "AWS_SESSION_TOKEN")
       :provider-name "env-provider"))))

(defmethod expiredp ((provider env-provider))
  (not (slot-value provider 'retrievedp)))
