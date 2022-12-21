(defpackage #:aws-sdk/utils
  (:use #:cl)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-sequence
                #:ascii-string-to-byte-array)
  (:import-from #:kebab
                #:to-lisp-case)
  (:import-from #:babel
                #:octets-to-string)
  (:export #:lispify
           #:gethash+
           #:ensure-string))
(in-package #:aws-sdk/utils)

(defun lispify (value &optional (package *package*))
  (intern (string-upcase (kebab:to-lisp-case value)) package))

(defun getenv (var)
  (let ((value (uiop:getenv var)))
    (when (and (stringp value)
               (string/= value ""))
      value)))

(defun sha-256 (value)
  (ironclad:byte-array-to-hex-string
    (etypecase value
      (string
       (ironclad:digest-sequence :sha256
                                 (ironclad:ascii-string-to-byte-array value)))
      ((simple-array (unsigned-byte 8) (*))
       (ironclad:digest-sequence :sha256 value))
      (pathname
       (ironclad:digest-file :sha256 value)))))

(defun gethash+ (keys hash)
  (reduce (lambda (hash key)
            (when hash
              (gethash key hash)))
          keys
          :initial-value hash))

(defun slurp-string-stream (stream)
  (with-output-to-string (out)
    (loop with buffer = (make-string 1024)
          for read-bytes = (read-sequence buffer stream)
          collect (write-string buffer out :end read-bytes)
          while (= read-bytes 1024))))

(defun ensure-string (value)
  (etypecase value
    (string value)
    (vector (babel:octets-to-string value))
    (stream (slurp-string-stream value))))
