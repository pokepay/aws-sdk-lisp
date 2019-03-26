# AWS-SDK for Common Lisp

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Usage

AWS-SDK provides interfaces for each AWS services as individual systems under "aws-sdk/services/*".

Here's an example to send an SMS via Amazon Simple Notification Service:

```common-lisp
(ql:quickload '(:aws-sdk :aws-sdk/services/sns))

(setf aws:*session* (aws:make-session))

;; Sending 
(aws/sns:publish :phone-number "+8190xxxxxxxx" :message "Hi, there")
```

## Configuring the SDK

### Configuring Credentials

Before using the SDK, you'll need to set AWS credentials for AWS services. AWS-SDK supports multiple methods to configure AWS credentials.

* Environment Credentials: Set `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY`
* Shared Credentials file (~/.aws/credentials).
* EC2 Instance Role Credentials

It's also can be configured via `aws:*session*`:

```common-lisp
(setf aws:*session*
      (aws:make-session :credentials
                        (aws:make-credentials
                         :access-key-id "XXXXXXXXXXXXXXXXXXXX"
                         :secret-access-key "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                         :session-token "xxxxxxxxxxx")))
```

### Configuring AWS Region

For making AWS API request, AWS region is also required. There're multiple methods to configure it.

* Environment variable: `AWS_REGION`
* Shared configuration file (~/.aws/config).

It's also can be configured via `aws:*session*`:

```common-lisp
(setf aws:*session* (aws:make-session :region "us-west-2"))
```

## Development note

### Generating all services

```
$ lake
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

This product is developed with the generous support of [Pocket Change, K.K.](https://www.pocket-change.jp/)

## Copyright

Copyright (c) 2017 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
