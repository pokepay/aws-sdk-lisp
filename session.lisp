(defpackage #:aws-sdk/session
  (:use #:cl)
  (:import-from #:aws-sdk/credentials
                #:credentials)
  (:import-from #:aws-sdk/utils
                #:getenv)
  (:import-from #:aws-sdk/utils/config
                #:read-from-file
                #:*aws-profile*)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:session
           #:make-session
           #:session-credentials
           #:session-region))
(in-package #:aws-sdk/session)

(defun aws-region ()
  (or (getenv "AWS_REGION")
      (and (probe-file #P"~/.aws/config")
           (aget (read-from-file #P"~/.aws/config"
                                 :profile (or (getenv "AWS_PROFILE")
                                              *aws-profile*))
                 "region"))))

(defstruct session
  (credentials nil :type (or credentials null))
  (region (aws-region) :type (or string null)))
