(defpackage #:aws-sdk/query-request
  (:use #:cl)
  (:import-from #:aws-sdk/request
                #:request
                #:request-payload
                #:request-headers)
  (:import-from #:aws-sdk/session
                #:*session*)
  (:export #:query-request))
(in-package #:aws-sdk/query-request)

(defclass query-request (request)
  ())

(defmethod initialize-instance :after ((req query-request) &rest args &key params operation api-version &allow-other-keys)
  (declare (ignore args))
  (alexandria:appendf (request-headers req)
                      '(("Content-Type" . "application/x-www-form-urlencoded")))
  (setf (request-payload req)
        (quri:url-encode-params (append `(("Action" . ,operation)
                                          ("Version" . ,api-version))
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
