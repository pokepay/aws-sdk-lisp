(defpackage #:aws-sdk
  (:nicknames #:aws-sdk/main #:aws)
  (:use #:cl)
  (:import-from #:aws-sdk/utils/config
                #:*aws-profile*)
  (:import-from #:aws-sdk/session
                #:session
                #:make-session)
  (:import-from #:aws-sdk/credentials
                #:make-credentials
                #:aws-credentials)
  (:import-from #:aws-sdk/api
                #:*session*
                #:aws-request)
  (:export #:*aws-profile*
           #:*session*
           #:session
           #:make-session
           #:make-credentials
           #:aws-credentials
           #:aws-request))
(in-package #:aws-sdk)
