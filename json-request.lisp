(defpackage #:aws-sdk/json-request
  (:use #:cl)
  (:import-from #:aws-sdk/request
                #:request
                #:request-payload
                #:request-headers
                #:request-operation)
  (:import-from #:aws-sdk/session
                #:*session*)
  (:import-from #:trivial-types
                #:property-list-p
                #:association-list-p)
  (:import-from #:yason)
  (:import-from #:cl-base64)
  (:export #:json-request))
(in-package #:aws-sdk/json-request)

(defclass json-request (request)
  ((json-version :initarg :json-version
                 :reader json-request-json-version)
   (target-prefix :initarg :target-prefix
                  :reader json-request-target-prefix)))


(defmethod initialize-instance :after ((req json-request) &rest args &key params json-version target-prefix operation &allow-other-keys)
  (declare (ignore args))
  (setf (request-payload req) (to-json params))
  (alexandria:appendf (request-headers req)
                      (list (cons "Content-Type"
                                  (uiop:strcat "application/x-amz-json-" json-version)))
                      (when target-prefix
                        (list (cons "X-Amz-Target"
                                    (format nil "~A.~A" target-prefix operation))))))


(defun %to-json (object)
  (typecase object
    (null (yason:encode :null))
    ((satisfies association-list-p)
     (yason:with-object ()
       (loop for (key . val) in object
             do (yason:with-object-element (key)
                  (%to-json val)))))
    (list
     (yason:with-array ()
       (dolist (i object)
         (yason:encode-array-element i))))
    ((and vector (not string))
     (yason:encode (base64:usb8-array-to-base64-string object)))
    (keyword (let ((*print-case* :downcase))
               (yason:encode (princ-to-string object))))
    (t (yason:encode object))))

(defun to-json (params)
  (yason:with-output-to-string* () (%to-json params)))
