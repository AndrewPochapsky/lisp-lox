(defpackage :ast
  (:use :common-lisp))

(in-package #:ast)

(defstruct variable-decl name initializer)
(defstruct function-decl name params body)

;;; Statements
(defstruct print-stmt expression)
(defstruct expression-stmt expression)
(defstruct block-stmt statements)
(defstruct if-stmt condition then-branch else-branch)
(defstruct while-stmt condition body)
(defstruct function

;;; Expressions
(defstruct binary left operator right)
(defstruct logical left operator right)
(defstruct grouping expression)
(defstruct literal value)
(defstruct unary operator right)
(defstruct variable-ref name)
(defstruct assign name expression)
(defstruct call callee paren arguments)

(defun accept (object visitor)
  "Accepts an OBJECT and a VISITOR function and calls the VISITOR function with the OBJECT as an argument."
  (funcall visitor object))

(defmacro defvisit (struct-name values &rest body)
  (let ((function-name (intern (concatenate
                                 'string
                                 (symbol-name :visit-)
                                 (write-to-string struct-name)
                                 )))
        (param-list (loop for value in values
                          for arg-name = (intern (concatenate 'string (write-to-string struct-name) "-" (write-to-string value)) (find-package :ast))
                          collect `(,value (funcall ',arg-name obj)))))
  `(defun ,function-name (obj &key ,@param-list)
     ,@body)))


(let ((pack (find-package :ast)))
  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
