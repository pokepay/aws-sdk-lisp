# AWS-SDK for Common Lisp

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Quick Start

AWS-SDK provides interfaces for each AWS services as individual systems under "aws-sdk/services/*".

Here's an example to send an SMS via Amazon Simple Notification Service:

```common-lisp
;; You must load aws-sdk first, then you can load services
(ql:quickload :aws-sdk)

;; Services can be loaded by name with quicklisp
(ql:quickload :aws-sdk/services/sns)

;; ...or via aws-load
(aws-load sns)

;; "Log in" to AWS by setting *session* via make-session
(setf aws:*session* (aws:make-session))

;; Sending 
(aws/sns:publish :phone-number "+8190xxxxxxxx" :message "Hi, there")
```

You can also configure AWS-SDK to use a configured profile for authentication:
```
(setf aws:*session* (aws:make-session :profile "profile_name_here"))
```

You can _also_ also use the `LOG-IN` function as shorthand:

``` common-lisp
(log-in :profile "profile_name_here")
```


## Configuring the SDK

### Configuring Credentials

Before using the SDK, you'll need to set AWS credentials for AWS services. AWS-SDK supports multiple methods to configure AWS credentials.

* Environment Credentials: Set `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY`
* Shared Credentials file (~/.aws/credentials).
* EC2 Instance Role Credentials

To "log in" to AWS, you must set the `*session*` variable with a session containing your credentials, and any configuration.  This can be done via the `make-session` function.
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
From a Common Lisp REPL, with the current working directory set to the root of aws-sdk-lisp:
```
(ql:quickload :lake)
(lake:lake)
```

Or, from a command-line with
```
$ lake
```

See: [github.com/takagi/lake](https://github.com/takagi/lake)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

This product is developed with the generous support of [Pocket Change, K.K.](https://www.pocket-change.jp/)

## Copyright

Copyright (c) 2017 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
