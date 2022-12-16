(defpackage #:aws-sdk/utils
  (:use #:cl)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-sequence
                #:ascii-string-to-byte-array)
  (:import-from #:kebab
                #:to-lisp-case)
  (:export #:lispify))
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
