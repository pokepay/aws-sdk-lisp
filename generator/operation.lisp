(defpackage #:aws-sdk/generator/operation
  (:use #:cl
        #:aws-sdk/utils)
  (:import-from #:aws-sdk/generator/shape
                #:make-request-with-input)
  (:import-from #:aws-sdk/api
                #:aws-request)
  (:import-from #:aws-sdk/request
                #:request)
  (:import-from #:aws-sdk/error
                #:aws-error)
  (:import-from #:quri)
  (:import-from #:cl-ppcre
                #:regex-replace-all
                #:do-matches-as-strings)
  (:import-from #:assoc-utils
                #:aget)
  (:import-from #:alexandria
                #:when-let
                #:when-let*
                #:ensure-car
                #:length=
                #:emptyp)
  (:import-from #:xmls)
  (:export #:compile-operation))
(in-package #:aws-sdk/generator/operation)

(defun %xmls-to-alist (xmls)
  (unless (consp xmls)
    (return-from %xmls-to-alist xmls))

  (destructuring-bind (name-and-ns attrs &rest contents) xmls
    (declare (ignore attrs))
    (cons (ensure-car name-and-ns)
          (mapcar #'%xmls-to-alist contents))))
(defun xmls-to-alist (xmls)
  (list (%xmls-to-alist xmls)))

(defun parse-response-error (body status content-type error-map)
  (let ((body-str (ensure-string (or body "")))
        err-class
        err-message
        err-code)
    (cond
      ((emptyp body-str)
       (error "Unexpected error raised with status=~A" status))
      ((member content-type '("application/xml" "text/xml")
               :test #'string=)
       (let* ((xml-data (xmls-to-alist (xmls:parse-to-list body-str))))
         (when-let* ((err-response (aget xml-data "ErrorResponse"))
                     (err (aget err-response "Error")))
           (setf err-class (aget error-map (aget err "Code"))
                 err-message (aget err "Message")))
         (when-let ((err-alist (aget xml-data "Error")))
           (setf err-class (aget error-map (first (aget err-alist "Code")))
                 err-code (aget err-alist "Code")
                 err-message (aget err-alist "Message")))))
      ((uiop:string-prefix-p "application/x-amz-json-1." content-type)
       (let* ((json-data (yason:parse body-str))
              (_type (gethash "__type" json-data)))
         (setf err-message (gethash "message" json-data)
               err-class (or (aget error-map
                                   (if (string= content-type "application/x-amz-json-1.1")
                                       _type
                                       (subseq _type (1+ (position #\# _type)))))
                             'aws-error))))
      ((string= content-type "application/vnd.error+json")
       (setf err-message (gethash "message" (yason:parse body-str)))))
    (error (or err-class 'aws-error)
           :code err-code
           :message (or err-message "Unknown")
           :status status
           :body body-str)))

(defun parse-response (response body-type wrapper-name error-map)
  (destructuring-bind (body status headers &rest ignore-args)
      response
    (declare (ignore ignore-args))
    (let ((content-type (gethash "content-type" headers)))
      (if (<= 400 status 599)
          (parse-response-error body status content-type error-map)
          (if (equal body-type "blob")
              body
              (let ((body-str (ensure-string (or body ""))))
                (cond
                  ((string= content-type "application/xml")
                   (let* ((output (xmls-to-alist (xmls:parse-to-list body-str)))
                          (output ;; Unwrap the root element
                            (cdr (first output))))
                     (if wrapper-name
                         (values (aget output wrapper-name)
                                 (aget output "ResponseMetadata"))
                         output)))
                  ((member content-type '("application/json" "application/x-amz-json-1.1" "application/x-amz-json-1.0")
                           :test #'string=)
                   (yason:parse body-str :object-as :alist))
                  (t
                   body-str))))))))

(defun compile-path-pattern (path-pattern)
  (when path-pattern
    (let ((slots
            (let (slots)
              (ppcre:do-matches-as-strings (match "(?<={)[^}\\+]+\\+?(?=})" path-pattern (nreverse slots))
                (let* ((plus-ends (char= #\+ (aref match (1- (length match)))))
                       (slot-symbol (lispify (if plus-ends
                                                 (subseq match 0 (1- (length match)))
                                                 match))))
                  (push
                    (if plus-ends
                        `(slot-value input ',slot-symbol)
                        `(quri:url-encode (slot-value input ',slot-symbol)))
                    slots))))))
      (if slots
          `(lambda (input)
             (format nil ,(ppcre:regex-replace-all "{[^}]+}" path-pattern "~A")
                     ,@slots))
          path-pattern))))

(defun compile-operation (service name options params body-type error-map)
  (let* ((output (gethash "output" options))
         (method (gethash "method" (gethash "http" options)))
         (request-uri (gethash "requestUri" (gethash "http" options))))
    (if params
        (let ((input-shape-name (lispify (gethash "shape" (gethash "input" options)))))
          `(progn
             (defun ,(lispify name) (&rest args &key ,@params)
               (declare (ignorable ,@params))
               (let ((input (apply ',(intern (format nil "~:@(~A-~A~)" :make input-shape-name)) args)))
                 (parse-response
                  (aws-request
                    (make-request-with-input
                      ',(intern (format nil "~:@(~A-REQUEST~)" service))
                      input ,method ,(compile-path-pattern request-uri) ,name)
                    ,@(when (equal body-type "blob")
                        '(:want-stream t)))
                  ,body-type
                  ,(and output
                        (gethash "resultWrapper" output))
                  ,error-map)))
             (export ',(lispify name))))
        `(progn
           (defun ,(lispify name) ()
             (parse-response
               (aws-request
                 (make-instance ',(intern (format nil "~:@(~A-REQUEST~)" service))
                                :method ,method
                                :path ,request-uri
                                :operation ,name)
                 ,@(when (equal body-type "blob")
                     '(:want-stream t)))
              ,body-type
              ,(and output
                    (gethash "resultWrapper" output))
              ,error-map))
           (export ',(lispify name))))))
