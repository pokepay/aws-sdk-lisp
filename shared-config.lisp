(defpackage #:aws-sdk/shared-config
  (:use #:cl)
  (:import-from #:aws-sdk/credentials
                #:make-credentials)
  (:import-from #:aws-sdk/utils/config
                #:*aws-profile*
                #:read-from-file)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:shared-config
           #:make-shared-config
           #:shared-config-credentials
           #:shared-config-region
           #:shared-config-assume-role
           #:shared-config-profile

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
  (profile *aws-profile*)

  region
  credentials
  (assume-role nil :type (or assume-role-config null)))

(defun make-shared-config (&rest args &key credentials-path config-path profile)
  (declare (ignore credentials-path config-path profile))
  (let* ((shared-config (apply #'%make-shared-config args))
         (base-config-section (with-slots (config-path profile) shared-config
                                (when (and config-path
                                           (probe-file config-path))
                                  (read-from-file config-path
                                                  :profile profile
                                                  :allow-no-profile t))))
         (base-creds-section (with-slots (credentials-path profile) shared-config
                               (when (and credentials-path
                                          (probe-file credentials-path))
                                 (read-from-file credentials-path
                                                 :profile profile
                                                 :allow-no-profile t))))
         (source-profile-name (aget base-config-section "source_profile"))
         (source-config-section (when source-profile-name
                                  (with-slots (config-path) shared-config
                                    (when (and config-path
                                               (probe-file config-path))
                                      (read-from-file config-path
                                                      :profile source-profile-name)))))
         (source-creds-section (when source-profile-name
                                 (with-slots (credentials-path) shared-config
                                   (when (and credentials-path
                                              (probe-file credentials-path))
                                     (read-from-file credentials-path
                                                     :profile source-profile-name)))))
         (role-arn (aget base-config-section "role_arn"))

         ;; You cannot specify both source_profile and credential_source in the same profile.
         (credential-source (if (and (aget base-config-section "credential_source")  source-profile-name)
                                (error "You cannot specify both source_profile and credential_source in the same profile")
                                (aget base-config-section "credential_source")))
         (access-key-id (or (aget base-creds-section "aws_access_key_id")
                            (aget source-creds-section "aws_access_key_id")))
         (secret-access-key (or (aget base-creds-section "aws_secret_access_key")
                                (aget source-creds-section "aws_secret_access_key")))
         ;; I'm not sure if this is in the actual standard
         (region  (or (aget base-config-section "region")
                      (aget source-config-section "region"))))

    (when (and credential-source (not (member credential-source '("Environment" "Ec2InstanceMetadata" "EcsContainer"))))
      (error "Invalid credential_source"))

    (when (and role-arn
               (or source-profile-name credential-source))
      (setf (shared-config-assume-role shared-config)
            (make-assume-role-config :role-arn role-arn
                                     :source-profile source-profile-name
                                     :credential-source credential-source
                                     :external-id (aget base-config-section "external_id")
                                     :serial-number (aget base-config-section "mfa_serial")
                                     :role-session-name (aget base-config-section "role_session_name"))))
    (when (and access-key-id secret-access-key)
      (setf (shared-config-credentials shared-config)
            (make-credentials
             :access-key-id access-key-id
             :secret-access-key secret-access-key
             :session-token (or (aget base-creds-section "aws_session_token")
                                (aget source-creds-section "aws_session_token"))
             :provider-name (format nil "shared-config: ~A"
                                    (shared-config-config-path shared-config)))))

    (when region
      (setf (shared-config-region shared-config) region))

    shared-config))
