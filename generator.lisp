(uiop:define-package #:aws-sdk/generator
    (:use #:cl)
  (:use-reexport #:aws-sdk/generator/shape)
  (:use-reexport #:aws-sdk/generator/operation)
  (:use-reexport #:aws-sdk/generator/service))
