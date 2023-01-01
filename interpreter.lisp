(defpackage :interpreter
  (:use :common-lisp)
  (:export #:interpret))

(in-package #:interpreter)

(defparameter *environment* (environment:create-env))

(ast:defvisit literal (value) value)
(ast:defvisit grouping (expression) (accept expression))
(ast:defvisit unary (operator right)
  (let ((right (accept right))
        (type (symbol-name (lexer:token-type operator))))
    (cond
      ((string= "MINUS" type)
       (progn
         (check-number right)
         (- right)))
      ((string= "BANG" type) (not (is-truthy right)))
      (t nil))))

(ast:defvisit binary (left operator right)
  (let ((left (accept left))
        (right (accept right))
        (type (symbol-name (lexer:token-type operator))))
    (cond
      ((string= "GREATER" type)
       (progn
         (check-numbers left right)
         (> left right)))
      ((string= "GREATER-EQUAL" type)
       (progn
         (check-numbers left right)
         (>= left right)))
      ((string= "LESS" type)
       (progn
         (check-numbers left right)
         (< left right)))
      ((string= "LESS-EQUAL" type)
       (progn
         (check-numbers left right)
         (<= left right)))
      ((string= "BANG-EQUAL" type) (not (is-equal left right)))
      ((string= "EQUAL-EQUAL" type) (is-equal left right))
      ((string= "MINUS" type)
       (progn
         (check-numbers left right)
         (- left right)))
      ((string= "PLUS" type)
       (cond
         ((and (stringp left) (stringp right)) (concatenate 'string left right))
         ((and (numberp left) (numberp right))
          (progn
            (check-numbers left right)
            (+ left right)))
         (t (error "Operands must be two numbers or two strings."))))
      ((string= "SLASH" type)
       (progn
         (check-numbers left right)
         (/ left right)))
      ((string= "STAR" type)
       (progn
         (check-numbers left right)
         (* left right)))
      (t nil))))

(ast:defvisit expression-stmt (expression)
  (progn
    (accept expression)
    nil))

(ast:defvisit print-stmt (expression)
  (print
    (accept expression)))

(ast:defvisit variable-decl (name initializer)
  (if (null initializer)
      (environment:define *environment* name initializer)
      (environment:define *environment* name (accept initializer))))

(ast:defvisit variable-ref (name)
  (environment:get-value *environment* name))

(ast:defvisit assign (name expression)
  (let ((value (accept expression)))
    (progn
      (environment:assign *environment* name value)
      value)))

(defun check-number (operand)
  (if (numberp operand)
      T
      ; TODO: Make this more informative, I just can't be bothered right now.
      (error "Operand must be a number")))

(defun check-numbers (left right)
  (if (and (numberp left) (numberp right))
      T
      ; TODO: Make this more informative, I just can't be bothered right now.
      (error "Operands must be a number")))

(defun is-equal (a b)
  (cond
    ((and (null a) (null b)) T)
    ((or (null a) (null b)) nil)
    ((and (stringp a) (stringp b)) (string= a b))
    (t (eq a b))))

(defun is-truthy (obj)
  (let ((value (ast:literal-value obj)))
    (if (null value)
        nil
        (let ((name (symbol-name value)))
          (cond
            ((string= name "TRUE") T)
            ((string= name "FALSE") nil)
            (t T))))))

(defun visit-interpreter (object)
  "Implements the operation for OBJECT using the visitor pattern."
  (case (type-of object)
    ((ast:expression-stmt) (visit-expression-stmt object))
    ((ast:print-stmt) (visit-print-stmt object))
    ((ast:variable-decl) (visit-variable-decl object))
    ((ast:variable-ref) (visit-variable-ref object))
    ((ast:assign) (visit-assign object))
    ((ast:binary) (visit-binary object))
    ((ast:unary) (visit-unary object))
    ((ast:grouping) (visit-grouping object))
    ((ast:literal) (visit-literal object))))

(defun accept (obj)
    (ast:accept obj #'visit-interpreter))

(defun interpret (expressions)
  (if (null expressions)
      nil
      (progn
        (accept (car expressions))
        (interpret (cdr expressions)))))
