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

(defvar bin (ast:make-binary :left "left" :operator "op" :right "right"))
(defvar literal (ast:make-literal :value "Literal"))

(defvar expr (ast:make-binary
               :left (ast:make-unary
                       :operator (lexer:create-token 'minus  "-" nil 0)
                       :right (ast:make-literal :value 123))
               :operator (lexer:create-token 'star "*" nil 0)
               :right (ast:make-grouping :expression (ast:make-literal :value 45.67))))

(defun accept (obj)
    (ast:accept obj #'visit-printer))

(accept expr)
