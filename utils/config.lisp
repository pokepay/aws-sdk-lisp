(defpackage #:aws-sdk/utils/config
  (:use #:cl)
  (:import-from #:aws-sdk/utils
                #:getenv)
  (:import-from #:parser.ini
                #:*include-empty-sections?*
                #:parse)
  (:export #:parse-file
           #:read-from-file
           #:*aws-profile*))
(in-package #:aws-sdk/utils/config)

(defvar *aws-profile* (or (getenv "AWS_PROFILE") "default"))

(defvar *cached-ini* (make-hash-table :test 'equal))

(defun %parse-file (file)
  (let* ((parser.ini:*include-empty-sections?* t)
         (data (parser.ini:parse file 'list)))
    (mapcar (lambda (section)
              (cons (first (getf section :name))
                    (mapcar (lambda (option)
                              (cons (first (getf (first option) :name))
                                    (getf (first option) :value)))
                            (getf (getf section :section) :section-option))))
            data)))

(defun parse-file (file)
  (let ((filename (namestring file)))
    (when (and (gethash filename *cached-ini*)
               (<= (file-write-date filename)
                   (car (gethash filename *cached-ini*))))
      (return-from parse-file (cdr (gethash filename *cached-ini*))))
    (let ((data (%parse-file file)))
      (setf (gethash filename *cached-ini*)
            (cons (file-write-date file) data))
      data)))

(defun read-from-file (file &key (profile *aws-profile*))
  (let* ((data (parse-file file))
         (section
           (or (assoc profile data :test 'equal)
               ;; Fallback to 'profile <name>'
               (assoc (format nil "profile ~A" profile) data :test 'equal))))
    (unless section
      (error "Profile '~A' doesn't exist in '~A'." profile file))
    (cdr section)))
