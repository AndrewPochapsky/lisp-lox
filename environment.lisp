(defpackage :environment
  (:use :common-lisp)
  (:export #:define #:get-value #:assign #:environment #:create-env #:create-env-with-enclosing #:define-with-name))

(in-package #:environment)

(defstruct environment table enclosing)

(defun create-env()
    (make-environment :table (make-hash-table :test #'equal) :enclosing nil))

(defun create-env-with-enclosing (enclosing)
    (make-environment :table (make-hash-table :test #'equal) :enclosing enclosing))

(defun define (env name-token value)
  (setf (gethash (lexer:token-lexeme name-token) (environment-table env)) value))

(defun define-with-name (env name value)
  (setf (gethash name (environment-table env)) value))

(defun get-value (env name-token)
  (let* ((name (lexer:token-lexeme name-token))
         (value (gethash name (environment-table env) :not-found)))
    (if (eq value :not-found)
        (if (not (null (environment-enclosing env)))
            (get-value (environment-enclosing env) name-token)
            (error "Undefined variable '~a'." name))
        value)))

(defun assign (env name-token value)
  (let* ((name (lexer:token-lexeme name-token))
         (existing-value (gethash name (environment-table env) :not-found)))
    (if (eq existing-value :not-found)
        (if (not (null (environment-enclosing env)))
            (assign (environment-enclosing env) name-token value)
            (error "Undefined variable '~a'." name))
        (setf (gethash name (environment-table env)) value))))
