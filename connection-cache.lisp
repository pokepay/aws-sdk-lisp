(defpackage #:aws-sdk/connection-cache
  (:use #:cl)
  (:export #:*use-connection-cache*))
(in-package #:aws-sdk/connection-cache)

(defvar *use-connection-cache* nil)
