(defpackage :environment
  (:use :common-lisp)
  (:export #:define #:get-value #:assign))

(in-package #:environment)

(defun define (table name-token value)
  (setf (gethash (lexer:token-lexeme name-token) table) value))

(defun get-value (table name-token)
  (let* ((name (lexer:token-lexeme name-token))
         (value (gethash name table :not-found)))
    (if (not (eq value :not-found))
        value
        (error "Undefined variable ~a." name))))

(defun assign (table name-token value)
  (let* ((name (lexer:token-lexeme name-token))
         (existing-value (gethash name table :not-found)))
    (if (eq existing-value :not-found)
        (error "Undefined variable ~a." name)
        (setf (gethash name table) value))))
