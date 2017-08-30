(defpackage #:aws-sdk/credentials
  (:use #:cl)
  (:import-from #:aws-sdk/utils/config
                #:read-from-file)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:aws-credentials))
(in-package #:aws-sdk/credentials)

(defun file-credentials (&key (file #P"~/.aws/credentials") (profile "default"))
  (read-from-file file :profile profile))

(defun environment-credentials ()
  (let ((access-key (uiop:getenv "AWS_ACCESS_KEY_ID"))
        (secret-key (uiop:getenv "AWS_SECRET_ACCESS_KEY")))
    (when (and access-key
               secret-key
               (string/= access-key "")
               (string/= secret-key ""))
      `(("aws_access_key_id" . ,access-key)
        ("aws_secret_access_key" . ,secret-key)))))

(defun aws-credentials (&key (profile "default"))
  (let ((credentials (or (environment-credentials)
                         (file-credentials :profile profile))))
    (when credentials
      (values
       (aget credentials "aws_access_key_id")
       (aget credentials "aws_secret_access_key")))))
