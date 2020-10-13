(defpackage #:aws-sdk/credentials/base
  (:use #:cl)
  (:import-from #:local-time)
  (:export #:credentials
           #:credentials-keys
           #:credentials-headers
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

(defclass credentials () ())

(defclass basic-credentials (credentials)
  ((access-key-id :initarg :access-key-id)
   (secret-access-key :initarg :secret-access-key)
   (session-token :initarg :session-token)
   (provider-name :initarg :provider-name
                  :accessor credentials-provider-name)))

(defclass provider-credentials (credentials)
  ((provider :initarg :provider)))

(defun make-credentials (&rest args &key access-key-id secret-access-key session-token provider-name provider)
  (if provider
      (progn
        (when (or access-key-id secret-access-key session-token provider-name)
          (error "Extra arguments are not allowed with :provider"))
        (make-instance 'provider-credentials :provider provider))
      (apply #'make-instance 'basic-credentials
             :allow-other-keys t
             args)))

(defun provider-credentials-credentials (credentials)
  (check-type credentials provider-credentials)
  (retrieve (slot-value credentials 'provider)))

(defmacro define-wrapped-method (name slot-name)
  `(defgeneric ,name (credentials)
     (:method ((credentials credentials))
       (slot-value credentials ',slot-name))
     (:method ((credentials provider-credentials))
       (,name (provider-credentials-credentials credentials)))))

(define-wrapped-method credentials-access-key-id access-key-id)
(define-wrapped-method credentials-secret-access-key secret-access-key)
(define-wrapped-method credentials-session-token session-token)

(defgeneric credentials-keys (credentials)
  (:method ((credentials credentials))
    (if (slot-boundp credentials 'session-token)
        (values (slot-value credentials 'access-key-id)
                (slot-value credentials 'secret-access-key)
                (slot-value credentials 'session-token))
        (values (slot-value credentials 'access-key-id)
                (slot-value credentials 'secret-access-key))))
  (:method ((credentials provider-credentials))
    (credentials-keys (provider-credentials-credentials credentials))))

(defun credentials-headers (credentials)
  (when (credentials-session-token credentials)
    `(("X-Amz-Security-Token" . ,(credentials-session-token credentials)))))

(defclass provider ()
  ((credentials :initarg :credentials
                :initform nil)
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
