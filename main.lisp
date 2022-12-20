(uiop:define-package #:aws-sdk
  (:nicknames #:aws-sdk/main #:aws)
  (:use #:cl)
  (:use-reexport #:aws-sdk/session
                 #:aws-sdk/make-session
                 #:aws-sdk/credentials
                 #:aws-sdk/api
                 #:aws-sdk/error)
  (:import-from #:aws-sdk/utils/config
                #:*aws-profile*)
  (:export #:*aws-profile*))
(in-package #:aws-sdk)
