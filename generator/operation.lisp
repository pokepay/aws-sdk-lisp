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
  (:import-from #:babel
                #:octets-to-string)
  (:import-from #:assoc-utils
                #:aget)
  (:import-from #:alexandria
                #:when-let
                #:ensure-car)
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

(defun ensure-string (value)
  (if (stringp value)
      value
      (babel:octets-to-string value)))

(defun parse-response (response body-type wrapper-name error-map)
  (destructuring-bind (body status headers &rest ignore-args)
      response
    (declare (ignore ignore-args headers))
    (if (<= 400 status 599)
        (when (and body (/= 0 (length body)))
          (let* ((output (xmls-to-alist (xmls:parse-to-list (ensure-string body))))
                 (error-alist (aget output "Error"))
                 (error-class (or (aget error-map (first (aget error-alist "Code")))
                                  'aws-error)))
            (error error-class
                   :code (first (aget error-alist "Code"))
                   :message (first (aget error-alist "Message"))
                   :status status
                   :body body)))
        (if (equal body-type "blob")
            body
            (when (and body (/= 0 (length body)))
              (let* ((output (xmls-to-alist (xmls:parse-to-list (ensure-string body))))
                     (output ;; Unwrap the root element
                       (cdr (first output))))
                (if wrapper-name
                    (values (aget output wrapper-name)
                            (aget output "ResponseMetadata"))
                    output)))))))

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

(defun compile-operation (service name version options params body-type error-map)
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
                      input ,method ,(compile-path-pattern request-uri) ,name ,version))
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
                                :params `(("Action" . ,,name) ("Version" . ,,version))))
              ,body-type
              ,(and output
                    (gethash "resultWrapper" output))
              ,error-map))
           (export ',(lispify name))))))
