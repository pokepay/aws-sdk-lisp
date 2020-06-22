(defpackage #:aws-sdk/make-session
  (:use #:cl)
  (:import-from #:aws-sdk/session
                #:%make-session)
  (:import-from #:aws-sdk/credentials
                #:default-aws-credentials
                #:credentials
                #:make-credentials)
  (:import-from #:aws-sdk/shared-config
                #:make-shared-config
                #:shared-config-credentials
                #:shared-config-assume-role
                #:shared-config-region
                #:assume-role-role-arn
                #:assume-role-source-profile
                #:assume-role-credential-source
                #:assume-role-external-id
                #:assume-role-serial-number
                #:assume-role-role-session-name)
  (:import-from #:aws-sdk/credentials/assume-role
                #:assume-role-provider)
  (:import-from #:aws-sdk/utils/config
                #:*aws-profile*)
  (:import-from #:aws-sdk/utils
                #:getenv)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:make-session
           #:make-session-default
           #:call-with-session
           #:init-session
           #:with-session))
(in-package #:aws-sdk/make-session)

(defun make-session (&key credentials region (profile *aws-profile*))
  (let ((shared-config (make-shared-config :profile profile)))
    (%make-session
     :credentials
     (or credentials
         (cond
           ((shared-config-assume-role shared-config)
            (let* ((assume-role (shared-config-assume-role shared-config))
                   (assume-role-shared-config
                     (make-shared-config :profile
                                         (assume-role-source-profile assume-role))))
              (make-credentials
               :provider
               (make-instance 'assume-role-provider
                              :shared-credentials (shared-config-credentials assume-role-shared-config)
                              :role-arn (assume-role-role-arn assume-role)
                              :external-id (assume-role-external-id assume-role)
                              :serial-number (assume-role-serial-number assume-role)
                              :role-session-name (assume-role-role-session-name assume-role)))))
           ((shared-config-credentials shared-config))
           (t nil)))
     :region (or region
                 (shared-config-region shared-config)
                 (getenv "AWS_REGION")))))

(defun make-session-default (&rest args &key region profile)
  (declare (ignore region profile))
  (let ((new-args (copy-list args))
        (*aws-profile* (or profile *aws-profile*)))
    (remf new-args :credentials)
    (apply #'make-session :credentials (default-aws-credentials) new-args)))

(defun call-with-session (cb &rest args &key region profile)
  (declare (ignore region profile))
  (funcall cb (apply #'make-session-default args)))

(defmacro with-session ((&rest session-args &key region profile) &body body)
  (declare (ignore region profile))
  `(call-with-session (lambda (aws-sdk/session:*session*)
                        ,@body)
                      ,@session-args))

(defun init-session (&rest args &key region profile)
  (declare (ignore region profile))
  (setf aws-sdk/session:*session* (apply #'make-session-default args)))
