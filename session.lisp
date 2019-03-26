(defpackage #:aws-sdk/session
  (:use #:cl)
  (:import-from #:aws-sdk/credentials
                #:credentials)
  (:export #:session
           #:session-credentials
           #:session-region))
(in-package #:aws-sdk/session)

(defvar *session* nil)

(defstruct (session (:constructor %make-session))
  (credentials nil :type (or credentials null))
  (region nil :type (or string null)))
