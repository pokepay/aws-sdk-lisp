(defpackage #:aws-sdk
  (:nicknames #:aws-sdk/main #:aws)
  (:use #:cl)
  (:import-from #:aws-sdk/utils/config
                #:*aws-profile*)
  (:export #:*aws-profile*))
(in-package #:aws-sdk)
