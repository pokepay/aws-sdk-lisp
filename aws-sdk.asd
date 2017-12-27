(require 'asdf)

(asdf:defsystem "aws-sdk"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "AWS SDK for Common Lisp"
  :depends-on ("aws-sdk/main"))
