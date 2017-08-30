(defpackage #:aws-sdk/utils/config
  (:use #:cl)
  (:import-from #:parser.ini
                #:parse)
  (:export #:read-from-file
           #:*aws-profile*))
(in-package #:aws-sdk/utils/config)

(defvar *aws-profile* "default")

(defun read-from-file (file &key (profile *aws-profile*))
  (let* ((data (parser.ini:parse file 'list))
         (section (getf (find-if (lambda (section)
                                   (string= (first (getf section :name)) profile))
                                 data)
                        :section)))
    (unless section
      (error "Profile '~A' doesn't exist in '~A'." profile file))

    (mapcar (lambda (option)
              (cons
               (first (getf (first option) :name))
               (getf (first option) :value)))
            (getf section :section-option))))
