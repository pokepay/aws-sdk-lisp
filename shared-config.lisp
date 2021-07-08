(defpackage #:aws-sdk/shared-config
  (:use #:cl)
  (:import-from #:aws-sdk/credentials
                #:make-credentials)
  (:import-from #:aws-sdk/utils/config
                #:read-from-file)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:shared-config
           #:make-shared-config
           #:shared-config-credentials
           #:shared-config-region
           #:shared-config-assume-role

           #:assume-role-config
           #:assume-role-role-arn
           #:assume-role-source-profile
           #:assume-role-credential-source
           #:assume-role-external-id
           #:assume-role-serial-number
           #:assume-role-role-session-name
           #:assume-role-credentials))
(in-package #:aws-sdk/shared-config)

(defstruct (assume-role-config (:conc-name assume-role-))
  (role-arn nil :type string)
  (source-profile nil :type (or string null))
  (credential-source nil :type (or string null))
  (external-id nil :type (or string null))
  (serial-number nil :type (or string null))
  (role-session-name nil :type (or string null))
  credentials)

(defstruct (shared-config (:constructor %make-shared-config))
  (credentials-path (merge-pathnames ".aws/credentials"
                                     (user-homedir-pathname)))
  (config-path (merge-pathnames ".aws/config"
                                (user-homedir-pathname)))
  profile

  region
  credentials
  (assume-role nil :type (or assume-role-config null)))

(defun make-shared-config (&rest args &key credentials-path config-path profile)
  (declare (ignore credentials-path config-path))
  (let ((shared-config (apply #'%make-shared-config args)))
    (let ((section (with-slots (credentials-path profile) shared-config
                     (when (and credentials-path
                                (probe-file credentials-path))
                       (read-from-file credentials-path
                                       :profile profile
                                       :allow-no-profile t)))))
      (let ((access-key-id (aget section "aws_access_key_id"))
            (secret-access-key (aget section "aws_secret_access_key")))
        (when (and access-key-id secret-access-key)
          (setf (shared-config-credentials shared-config)
                (make-credentials
                 :access-key-id access-key-id
                 :secret-access-key secret-access-key
                 :session-token (aget section "aws_session_token")
                 :provider-name (format nil "shared-config: ~A"
                                        (shared-config-config-path shared-config))))))

      (let ((role-arn (aget section "role_arn"))
            (source-profile (aget section "source_profile"))
            (credential-source (aget section "credential_source")))
        (when (and role-arn
                   (or source-profile credential-source))
          (when (and source-profile
                     credential-source)
            (error "Profile ~S has both of source_profile and credential_source"
                   profile))
          (when credential-source
            (error "credential_source is not supported yet"))
          (setf (shared-config-assume-role shared-config)
                (make-assume-role-config :role-arn role-arn
                                         :source-profile source-profile
                                         :credential-source credential-source
                                         :external-id (aget section "external_id")
                                         :serial-number (aget section "mfa_serial")
                                         :role-session-name (aget section "role_session_name"))))))

    (let ((section (with-slots (config-path profile) shared-config
                     (when (and config-path
                                (probe-file config-path))
                       (read-from-file config-path
                                       :profile profile
                                       :allow-no-profile t)))))
      (let ((region (aget section "region")))
        (when region
          (setf (shared-config-region shared-config) region))))

    shared-config))
