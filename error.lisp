(defpackage #:aws-sdk/error
  (:use #:cl)
  (:export #:aws-error
           #:aws-error-code
           #:aws-error-message
           #:aws-error-status
           #:aws-error-body))
(in-package #:aws-sdk/error)

(define-condition aws-error (error)
  ((code :initarg :code
         :reader aws-error-code)
   (message :initarg :message
            :reader aws-error-message)
   (status :initarg :status
           :initform nil
           :reader aws-error-status)
   (body :initarg :body
         :initform nil
         :reader aws-error-body)))

(defmethod print-object ((error aws-error) stream)
  (print-unreadable-object (error stream :type t :identity t)
    (with-slots (code message) error
      (format stream "~A (Code=~A)" message code))))
