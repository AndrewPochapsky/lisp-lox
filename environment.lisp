(defpackage :environment
  (:use :common-lisp)
  (:export #:define #:get-value #:assign #:environment #:create-env #:create-env-with-enclosing #:define-with-name #:environment-enclosing))

(in-package #:environment)

(defstruct environment table enclosing)

(defun create-env()
    (make-environment :table (make-hash-table :test #'equal) :enclosing nil))

(defun create-env-with-enclosing (enclosing)
    (make-environment :table (make-hash-table :test #'equal) :enclosing enclosing))

(defun define (env name-token value)
  (make-environment
    :table (add-to-hash-table (environment-table env) (lexer:token-lexeme name-token) value)
    :enclosing (environment-enclosing env)))

(defun define-with-name (env name value)
  (make-environment
    :table (add-to-hash-table (environment-table env) name value)
    :enclosing (environment-enclosing env)))

(defun get-value (env name-token)
  (let* ((name (lexer:token-lexeme name-token))
         (value (gethash name (environment-table env) :not-found)))
    (if (eq value :not-found)
        (if (not (null (environment-enclosing env)))
            (get-value (environment-enclosing env) name-token)
            (error "Undefined variable '~a'." name))
        value)))

(defun assign-new (env name-token value)
  (let* ((name (lexer:token-lexeme name-token))
         (existing-value (gethash name (environment-table env) :not-found)))
    (if (eq existing-value :not-found)
        (if (not (null (environment-enclosing env)))
            (make-environment :table (environment-table env) :enclosing (assign-new (environment-enclosing env) name-token value))
            (error "Undefined variable '~a'." name))
        (define-with-name env name value))))

#|(defparameter env (create-env))

(setf env (define-with-name env "key" 1))

(setf env (assign-new env (lexer:create-token 'identifier "key" nil 0) 2))

(get-value env (lexer:create-token 'identifer "key" nil 0))

(defparameter env-2 (create-env-with-enclosing env))

(print env-2)

(get-value env-2 (lexer:create-token 'identifer "key" nil 0))

(setf env-2 (assign-new env-2 (lexer:create-token 'identifier "key" nil 0) 6))|#

(defun assign (env name-token value)
  (let* ((name (lexer:token-lexeme name-token))
         (existing-value (gethash name (environment-table env) :not-found)))
    (if (eq existing-value :not-found)
        (if (not (null (environment-enclosing env)))
            (assign (environment-enclosing env) name-token value)
            (error "Undefined variable '~a'." name))
        (setf (gethash name (environment-table env)) value))))

(defun add-to-hash-table (hash-table key value)
  "Returns a new hash table with the key-value pair added to it."
  (let ((new-hash-table (make-hash-table :test #'equal)))
    (maphash (lambda (k v) (setf (gethash k new-hash-table) v)) hash-table)
    (setf (gethash key new-hash-table) value)
    new-hash-table))

(defun get-from-hash-table (hash-table key)
  "Returns the value associated with the key in the hash table."
  (gethash key hash-table))

(defun test ()
  (let ((hash-table (make-hash-table :test #'equal)))
    (setf (gethash "a" hash-table) 1)
    (setf (gethash "b" hash-table) 2)
    (setf (gethash "c" hash-table) 3)
    (format t "Original hash table:~%~A~%" hash-table)
    (let ((new-hash-table (add-to-hash-table hash-table "d" 4)))
      (format t "New hash table:~%~A~%" new-hash-table)
      (format t "Value of 'b' in original hash table: ~A~%" (get-from-hash-table hash-table "b"))
      (format t "Value of 'b' in new hash table: ~A~%" (get-from-hash-table new-hash-table "b"))
      (format t "Value of 'd' in new hash table: ~A~%" (get-from-hash-table new-hash-table "d")))))
