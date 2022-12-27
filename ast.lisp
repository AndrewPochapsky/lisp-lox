(defpackage :ast
  (:use :common-lisp))

(in-package #:ast)

(defstruct binary left operator right)
(defstruct grouping expression)
(defstruct literal value)
(defstruct unary operator right)

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