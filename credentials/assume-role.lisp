(defpackage #:aws-sdk/credentials/assume-role
  (:use #:cl)
  (:import-from #:aws-sdk/credentials/base
                #:provider
                #:retrieve
                #:make-credentials
                #:credentials-keys
                #:provider-expiration)
  (:import-from #:aws-sdk/session
                #:*session*
                #:session-region
                #:%make-session)
  (:import-from #:aws-sdk/services/sts
                #:assume-role)
  (:import-from #:local-time)
  (:import-from #:assoc-utils
                #:aget))
(in-package #:aws-sdk/credentials/assume-role)

(defclass assume-role-provider (provider)
  ((role-arn :initarg :role-arn)
   (role-session-name :initarg :role-session-name)
   (duration-seconds :initarg :duration-seconds
                     :initform 900)
   (external-id :initarg :external-id
                :initform nil)
   (policy :initarg :policy
           :initform nil)
   (serial-number :initarg :serial-number
                  :initform nil)
   (token-code :initarg :token-code
               :initform nil)

   (shared-credentials :initarg :shared-credentials)))

(defmethod retrieve ((provider assume-role-provider))
  (let* ((res
           (with-slots (role-arn
                        role-session-name
                        duration-seconds
                        external-id
                        policy
                        serial-number
                        token-code) provider
             (unless role-session-name
               (setf role-session-name
                     (local-time:format-timestring nil (local-time:now)
                                                   :format '((:year) #\- (:month 2) #\- (:day 2) #\T (:hour 2) #\- (:min 2) #\- (:sec 2) #\. (:usec 6)) :timezone local-time:+gmt-zone+)))
             (let ((*session* (%make-session :credentials
                                             (slot-value provider 'shared-credentials)
                                             :region (session-region *session*))))
               (aws/sts:assume-role :role-arn role-arn
                                    :role-session-name role-session-name
                                    :duration-seconds duration-seconds
                                    :external-id external-id
                                    :policy policy
                                    :serial-number serial-number
                                    :token-code token-code))))
         (credentials (aget res "Credentials")))
    (setf (provider-expiration provider)
          (local-time:parse-timestring (first (aget credentials "Expiration"))))
    (make-credentials
     :access-key-id (first (aget credentials "AccessKeyId"))
     :secret-access-key (first (aget credentials "SecretAccessKey"))
     :session-token (first (aget credentials "SessionToken"))
     :provider-name "assume-role-provider")))
