(defpackage #:aws-sdk/utils/config
  (:use #:cl)
  (:import-from #:parser.ini
                #:*include-empty-sections?*
                #:parse)
  (:export #:read-from-file
           #:*aws-profile*))
(in-package #:aws-sdk/utils/config)

(defvar *aws-profile* "default")

(defun read-from-file (file &key (profile *aws-profile*))
  (let* ((parser.ini:*include-empty-sections?* t)
         (data (parser.ini:parse file 'list))
         (section (or (find-if (lambda (section)
                                 (string= (first (getf section :name)) profile))
                               data)
                      ;; Fallback to 'profile <name>'
                      (find-if (lambda (section)
                                 (string= (first (getf section :name)) (format nil "profile ~A" profile)))
                               data))))
    (unless section
      (error "Profile '~A' doesn't exist in '~A'." profile file))

    (mapcar (lambda (option)
              (cons
               (first (getf (first option) :name))
               (getf (first option) :value)))
            (getf (getf section :section) :section-option))))
