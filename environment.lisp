(defpackage :environment
  (:use :common-lisp)
  (:export #:define #:get-value #:assign #:environment #:create-env))

(in-package #:environment)

(defstruct environment table)

(defun create-env()
    (make-environment :table (make-hash-table :test #'equal)))

(defun define (env name-token value)
  (setf (gethash (lexer:token-lexeme name-token) (environment-table env)) value))

(defun get-value (env name-token)
  (let* ((name (lexer:token-lexeme name-token))
         (value (gethash name (environment-table env) :not-found)))
    (if (not (eq value :not-found))
        value
        (error "Undefined variable ~a." name))))

(defun assign (env name-token value)
  (let* ((name (lexer:token-lexeme name-token))
         (existing-value (gethash name (environment-table env) :not-found)))
    (if (eq existing-value :not-found)
        (error "Undefined variable ~a." name)
        (setf (gethash name (environment-table env)) value))))
