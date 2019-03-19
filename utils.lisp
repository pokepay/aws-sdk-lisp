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

(defun sha-256 (str)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256
                             (ironclad:ascii-string-to-byte-array str))))
