(defpackage :ast
  (:use :common-lisp))
  ;(:export :binary :make-binary :literal :make-literal :accept))

(in-package #:ast)

(defstruct binary left operator right)
(defstruct grouping expression)
(defstruct literal value)
(defstruct unary operator)

(defun accept (object visitor)
  "Accepts an OBJECT and a VISITOR function and calls the VISITOR function with the OBJECT as an argument."
  (funcall visitor object))

(let ((pack (find-package :ast)))
  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
