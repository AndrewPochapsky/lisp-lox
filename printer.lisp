(defpackage :printer
  (:use :common-lisp))

(in-package #:printer)

(ast:defvisit literal (value) value)
(ast:defvisit unary (operator right) (format nil "(~a ~a)" (lexer:token-lexeme operator) (accept right)))
(ast:defvisit binary (left operator right) (format nil "(~a ~a ~a)" (lexer:token-lexeme operator) (accept left) (accept right)))
(ast:defvisit grouping (expression) (format nil "(group ~a)" (accept expression)))

(defun visit-printer (object)
  "Implements the operation for OBJECT using the visitor pattern."
  (case (type-of object)
    ((ast:binary) (visit-binary object))
    ((ast:unary) (visit-unary object))
    ((ast:grouping) (visit-grouping object))
    ((ast:literal) (visit-literal object))))

(defun accept (obj)
    (ast:accept obj #'visit-printer))

(let ((pack (find-package :printer)))
  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
