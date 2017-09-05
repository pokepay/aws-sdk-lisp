(defpackage #:aws-sdk
  (:nicknames #:aws-sdk/main #:aws)
  (:use #:cl)
  (:import-from #:aws-sdk/utils/config
                #:*aws-profile*)
  (:import-from #:aws-sdk/credentials
                #:aws-credentials)
  (:import-from #:aws-sdk/api
                #:aws-request)
  (:export #:*aws-profile*
           #:aws-credentials
           #:aws-request))
(in-package #:aws-sdk)
