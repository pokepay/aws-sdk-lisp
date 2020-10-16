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
  (:import-from #:aws-sdk/connection-cache
                #:*use-connection-cache*)
  (:export #:*aws-profile*
           #:*session*
           #:*use-connection-cache*
           #:session
           #:make-session
           #:make-credentials
           #:aws-credentials
           #:aws-request))
(in-package #:aws-sdk)
