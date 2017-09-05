(defpackage #:aws-sdk/credentials/base
  (:use #:cl)
  (:import-from #:local-time)
  (:export #:credentials
           #:credential-keys
           #:credential-headers
           #:make-credentials
           #:credentials-access-key-id
           #:credentials-secret-access-key
           #:credentials-session-token
           #:credentials-provider-name

           #:provider
           #:provider-expiration
           #:provider-expiry-window
           #:retrieve
           #:expiredp))
(in-package #:aws-sdk/credentials/base)

(defstruct credentials
  access-key-id
  secret-access-key
  session-token
  provider-name)

(defun credential-keys (credentials)
  (values
   (credentials-access-key-id credentials)
   (credentials-secret-access-key credentials)))

(defun credential-headers (credentials)
  (when (credentials-session-token credentials)
    `(("X-Amz-Security-Token" . ,(credentials-session-token credentials)))))

(defclass provider ()
  ((credentials :initform nil)
   (expiration :initform nil)
   (expiry-window :initarg :expiry-window
                  :initform 0
                  :accessor provider-expiry-window)))

(defgeneric provider-expiration (provider)
  (:method ((provider provider))
    (slot-value provider 'expiration)))

(defgeneric (setf provider-expiration) (value provider)
  (:method (value (provider provider))
    (let ((window (provider-expiry-window provider)))
      (setf (slot-value provider 'expiration)
            (local-time:timestamp- value window :sec)))))

(defgeneric retrieve (provider)
  (:method :around (provider)
    (if (expiredp provider)
        (setf (slot-value provider 'credentials)
              (call-next-method))
        (slot-value provider 'credentials))))

(defgeneric expiredp (provider)
  (:method (provider)
    (let ((expiration (provider-expiration provider)))
      (or (null expiration)
          (local-time:timestamp< expiration (local-time:now))))))
