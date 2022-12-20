(defpackage #:aws-sdk/services/s3/customization
  (:use #:cl)
  (:import-from #:aws-sdk/services/s3/api
                #:s3-request)
  (:import-from #:aws-sdk/request
                #:request-host
                #:request-params))
(in-package #:aws-sdk/services/s3/customization)

(defmethod request-host ((request s3-request) region)
  (let ((bucket-name
          (cdr (assoc "Bucket" (request-params request) :test 'equal))))
    (unless bucket-name
      (return-from request-host (call-next-method)))

    (format nil "~(~A~).s3.~(~A~).amazonaws.com" bucket-name region)))
