(defpackage #:aws-sdk/utils
  (:use #:cl)
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
