(defpackage #:aws-sdk/rest-json-request
  (:use #:cl)
  (:import-from #:aws-sdk/request
                #:request
                #:request-path)
  (:import-from #:aws-sdk/session
                #:*session*)
  (:export #:rest-json-request))
(in-package #:aws-sdk/rest-json-request)

(defclass rest-json-request (request)
  ())


(defmethod initialize-instance :after ((req rest-json-request) &rest args &key params path &allow-other-keys)
  (declare (ignore args))
  (let ((uri (quri:uri path)))
    (setf (request-path req)
          (quri:render-uri
           (quri:make-uri :path (quri:uri-path uri)
                          :query (quri:url-encode-params (append (quri:uri-query-params uri)
                                                                 params)))))))
