(defpackage :printer
  (:use :common-lisp))

(in-package #:printer)

(defmacro defvisit (struct-name value &rest body)
  (let ((function-name (intern (concatenate
                                 'string
                                 (symbol-name :visit-)
                                 (write-to-string struct-name)
                                 )))
        (arg-name (intern (concatenate 'string (write-to-string struct-name) "-" (write-to-string value)) (find-package :ast))))
  `(defun ,function-name (obj &key (,value (funcall ',arg-name obj)))
     ,@body)))

(defvisit literal value value)
(defvisit binary operator operator)

(visit-binary (ast:make-binary :left nil :operator "The Op" :right nil))

(visit-literal (ast:make-literal :value "asdf"))

;(defun visit-binary (binary &key (left (ast:binary-left binary)) (op (ast:binary-operator binary)) (right (ast:binary-right binary)))
;  "Implements the operation for OBJECT of binary."
;  (print "Visiting binary"))

(defun visit-unary (object)
  "Implements the operation for OBJECT of unary."
  (print "Visiting unary"))

;(defun visit-literal (literal &key (value (ast:literal-value literal)))
;  "Implements the operation for OBJECT of literal."
;  value)

(defun visit-grouping (grouping &key (expr (ast:grouping-expression grouping)))
  "Implements the operation for OBJECT of grouping."
  (print "Visiting grouping"))

(defun visit-printer (object)
  "Implements the operation for OBJECT using the visitor pattern."
  (case (type-of object)
    ((ast:binary) (visit-binary object))
    ((ast:unary) (visit-unary object))
    ((ast:grouping) (visit-grouping object))
    ((ast:literal) (visit-literal object))))

(defvar bin (ast:make-binary :left "left" :operator "op" :right "right"))
(defvar literal (ast:make-literal :value "Literal"))

(ast:binary-left (ast:make-binary :left "left" :operator "op" :right "right"))
(ast:binary-left bin)

(ast:literal-value literal)

;(ast:accept bin #'visit-printer)

;(ast:accept literal #'visit-printer)

