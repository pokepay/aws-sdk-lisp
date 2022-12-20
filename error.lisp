(defpackage #:aws-sdk/error
  (:use #:cl)
  (:export #:aws-error
           #:aws-error-message
           #:aws-error-status
           #:aws-error-body))
(in-package #:aws-sdk/error)

(define-condition aws-error (error)
  ((message :initarg :message
            :initform nil
            :reader aws-error-message)
   (status :initarg :status
           :initform nil
           :reader aws-error-status)
   (body :initarg :body
         :initform nil
         :reader aws-error-body)))

(defmethod print-object ((error aws-error) stream)
  (print-unreadable-object (error stream :identity t)
    (princ (class-name (class-of error)) stream)
    (with-slots (message) error
      (when message
        (write-string ": " stream)
        (write-string message stream)))))
