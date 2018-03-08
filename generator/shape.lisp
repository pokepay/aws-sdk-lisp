(defpackage #:aws-sdk/generator/shape
  (:use #:cl
        #:trivial-types
        #:aws-sdk/utils)
  (:import-from #:alexandria
                #:ensure-car)
  (:import-from #:assoc-utils
                #:alistp)
  (:export #:compile-shape
           #:shape-to-params))
(in-package #:aws-sdk/generator/shape)

(defun composite-type-p (type-name)
  (<= (char-code #\A) (char-code (aref type-name 0)) (char-code #\Z)))

(defun lisp-native-type (type-name)
  (cond
    ((string= type-name "boolean") 'cl:boolean)
    ((string= type-name "string") 'cl:string)
    ((string= type-name "character") 'cl:character)
    ((string= type-name "blob") '(cl:simple-array (unsigned-byte 8) (*)))
    ((or (string= type-name "integer")
         (string= type-name "long")) 'cl:integer)
    ((string= type-name "float") 'cl:single-float)
    ((string= type-name "double") 'cl:double-float)
    ((string= type-name "timestamp") 'cl:string)
    (t (intern type-name))))

(defun lispify* (value &optional (package *package*))
  (check-type value string)
  (if (composite-type-p value)
      (lispify value package)
      (let ((*package* package))
        (lisp-native-type value))))

(defgeneric shape-to-params (shape)
  (:method (shape) shape))

(defun to-query-params (key value)
  (typecase value
    (null)
    (cons
     (if (alistp value)
         (mapcar (lambda (kv)
                   (cons
                    (format nil "~A.~A" key (car kv))
                    (cdr kv)))
                 (loop for (k . v) in value
                       append (to-query-params k v)))
         (loop for i from 1
               for v in value
               collect (cons (format nil "~A.member.~A" key i) v))))
    (otherwise (list (cons key value)))))

(defun compile-structure-shape (name &key required members)
  `(progn
     (defstruct (,(lispify* name) (:copier nil))
       ,@(loop for key being each hash-key of members
                 using (hash-value value)
               collect `(,(lispify key)
                         ,(if (find key required :test #'string=)
                              `(error ,(format nil ":~A is required" (lispify* key)))
                              nil)
                         :type (or ,(lispify* (gethash "shape" value)) null))))
     (export (list ',(lispify* name)
                   ',(intern (format nil "~:@(~A-~A~)" '#:make (lispify* name)))))
     (defmethod shape-to-params ((shape ,(lispify* name)))
       (append
        ,@(loop for key being each hash-key of members
                collect `(to-query-params ,key
                                          (shape-to-params (slot-value shape ',(lispify key)))))))))

(defun compile-list-shape (name member)
  `(progn
     (deftype ,(lispify* name) () '(proper-list ,(lispify* member)))
     (defun ,(intern (format nil "~A-~A" '#:make (lispify* name))) (&rest members)
       (check-type members (proper-list ,(lispify* member)))
       members)))

(defun compile-map-shape (name)
  `(defstruct (,(lispify* name) (:constructor
                                    ,(intern (format nil "~A-~A" '#:make (lispify* name)))
                                    (key value)))
     key
     value))

(defun compile-otherwise (name type)
  (when (or (composite-type-p name)
            (not (eq (symbol-package (ensure-car (lisp-native-type name))) (find-package :cl))))
    `(deftype ,(if (composite-type-p name)
                   (lispify name)
                   (lisp-native-type name)) ()
       ',(if (composite-type-p type)
             (lispify type)
             (lisp-native-type type)))))

(defun compile-shape (name options)
  (let ((type (gethash "type" options)))
    (cond
      ((string= type "map")
       (compile-map-shape name))
      ((string= type "list")
       (let ((member-type (gethash "shape" (gethash "member" options))))
         (assert member-type)
         (compile-list-shape name member-type)))
      ((string= type "structure")
       (compile-structure-shape name
                                :required (gethash "required" options)
                                :members (gethash "members" options)))
      (t
       (compile-otherwise name type)))))
