(defpackage #:aws-sdk/rest-xml-request
  (:use #:cl)
  (:import-from #:aws-sdk/request
                #:request
                #:request-headers
                #:request-path)
  (:import-from #:aws-sdk/session
                #:*session*)
  (:export #:rest-xml-request))
(in-package #:aws-sdk/rest-xml-request)

(defclass rest-xml-request (request)
  ())


(defmethod initialize-instance :after ((req rest-xml-request) &rest args &key params path &allow-other-keys)
  (declare (ignore args))
  (let ((uri (quri:uri path)))
    (setf (request-path req)
          (quri:render-uri
           (quri:make-uri :path (quri:uri-path uri)
                          :query (quri:url-encode-params (append (quri:uri-query-params uri)
                                                                 params)))))))
