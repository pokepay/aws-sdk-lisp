(defpackage #:aws-sdk/request
  (:use #:cl)
  (:import-from #:aws-sdk/session
                #:*session*)
  (:import-from #:quri
                #:url-encode-params
                #:make-uri
                #:render-uri)
  (:import-from #:assoc-utils
                #:alistp)
  (:export #:request
           #:request-service
           #:request-method
           #:request-path
           #:request-params
           #:request-headers
           #:request-payload
           #:request-session
           #:request-host
           #:request-endpoint))
(in-package #:aws-sdk/request)

(defclass request ()
  ((service :initarg :service
            :initform (error ":service is required")
            :reader request-service)
   (method :initarg :method
           :initform ":method is required"
           :reader request-method)
   (path :initarg :path
         :initform "/"
         :reader request-path)
   (params :initarg :params
           :initform nil
           :reader request-params)
   (headers :initarg :headers
            :initform nil
            :reader request-headers)
   (payload :initarg :payload
            :initform nil
            :reader request-payload)
   (session :initarg :session
            :initform *session*
            :reader request-session)))

(defmethod initialize-instance :after ((req request) &rest args &key path params &allow-other-keys)
  (declare (ignore args))
  (let ((uri (quri:uri path)))
    (setf (slot-value req 'path) (quri:uri-path uri))
    (setf (slot-value req 'params)
          (append
            (quri:uri-query-params uri)
            (loop for (k . v) in params
                  append (to-query-params k v))))))

(defun to-query-params (key value)
  (typecase value
    (null)
    (cons
     (if (alistp value)
         (mapcar (lambda (kv)
                   (cons
                    (format nil "~A.~A" key (car kv))
                    (cdr kv)))
                 (loop for (k . v) in value
                       append (to-query-params k v)))
         (loop for i from 1
               for v in value
               collect (cons (format nil "~A.member.~A" key i) v))))
    (boolean
     (list (cons key
                 (if value
                     "true"
                     "false"))))
    (otherwise (list (cons key value)))))

(defgeneric request-host (request region)
  (:method ((req request) region)
    (format nil "~(~A~).~(~A~).amazonaws.com" (request-service req) region)))

(defgeneric request-endpoint (request region)
  (:method ((req request) region)
    (with-slots (path params) req
      (quri:render-uri
        (quri:make-uri :scheme "https"
                       :host (request-host req region)
                       :path path
                       :query (and params
                                   (quri:url-encode-params params)))))))
