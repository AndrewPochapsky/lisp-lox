(defpackage :interpreter
  (:use :common-lisp)
  (:export #:interpret))

(in-package #:interpreter)

(defparameter *globals* (environment:create-env))
(defstruct clock arity call)

(environment:define-with-name *globals* "clock"
  (make-clock
    :arity (lambda () 0)
    :call (lambda (arguments) (/ (get-internal-real-time) 1000))))

(define-condition lox-return (error)
  ((value :initarg :value :reader value)))

(defparameter *environment* *globals*)

(defstruct lox-function arity call)
(defun create-lox-function (params body)
    (let ((arity (lambda () (length params)))
          (call (lambda (arguments)
                  (let ((env (environment:create-env-with-enclosing *globals*)))
                    (labels ((helper (params arguments)
                               (if (not (null params))
                                   (progn
                                     (environment:define-with-name env (lexer:token-lexeme (car params)) (car arguments))
                                     (helper (cdr params) (cdr arguments)))
                                   nil)))
                      (progn
                        (helper params arguments)
                        (let ((prev-env *environment*))
                          (handler-case
                              (execute-block body env)
                            (lox-return (c)
                              (progn
                                (setf *environment* prev-env)
                                (value c)))))))))))
      (make-lox-function :arity arity :call call)))

(defun call (func arguments)
  (case (type-of func)
    ((clock) (funcall (clock-call func) arguments))
    ((lox-function) (funcall (lox-function-call func) arguments))
    (t (error "Invalid function type"))))

(defun arity (func)
  (case (type-of func)
    ((clock) (funcall (clock-arity func)))
    ((lox-function) (funcall (lox-function-arity func)))
    (t (error "Invalid function"))))

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

(ast:defvisit block-stmt (statements)
  (execute-block statements (environment:create-env-with-enclosing *environment*)))

(ast:defvisit if-stmt (condition then-branch else-branch)
  (if (is-truthy (accept condition))
      (accept then-branch)
      (if (not (null else-branch))
          (accept else-branch)
          nil)))

(ast:defvisit while-stmt (condition body)
  (if (is-truthy (accept condition))
      (progn
        (accept body)
        (visit-while-stmt ast:obj))
      nil))

(ast:defvisit logical (left operator right)
  (let ((left (accept left)))
    (if (string= (lexer:token-type operator) 'or)
        (if (is-truthy left)
            left
            (accept right))
        (if (not (is-truthy left))
            left
            (accept right)))))

(ast:defvisit call (callee arguments)
  (let ((callee (accept callee))
        (arguments (mapcar #'accept arguments)))
    (if (eq (length arguments) (arity callee))
        (call callee arguments)
        (error "Expected ~a arguments but got ~a." (arity callee) (length arguments)))))

(ast:defvisit function-decl (name params body)
  (let ((func (create-lox-function params body)))
    (if (eq *environment* *globals*)
        (progn
          (environment:define *environment* name func)
          (environment:define *globals* name func))
        (environment:define *environment* name func))))

(ast:defvisit return-stmt (expression)
  (let ((value
          (if (null expression)
              nil
              (accept expression))))
    (error 'lox-return :value value)))

(defun execute-block (statements new-env)
  (let ((previous-env *environment*))
    (progn
      (setf *environment* new-env)
      (dolist (statement statements)
        (accept statement))
      (setf *environment* previous-env)
      nil)))

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

(defun is-truthy (value)
  (if (null value)
      nil
      (if (symbolp value)
          (let ((name (symbol-name value)))
            (cond
              ((string= name "TRUE") T)
              ((string= name "FALSE") nil)
              (t T)))
          T)))

(defun visit-interpreter (object)
  "Implements the operation for OBJECT using the visitor pattern."
  (case (type-of object)
    ((ast:expression-stmt) (visit-expression-stmt object))
    ((ast:print-stmt) (visit-print-stmt object))
    ((ast:while-stmt) (visit-while-stmt object))
    ((ast:variable-decl) (visit-variable-decl object))
    ((ast:function-decl) (visit-function-decl object))
    ((ast:variable-ref) (visit-variable-ref object))
    ((ast:block-stmt) (visit-block-stmt object))
    ((ast:return-stmt) (visit-return-stmt object))
    ((ast:if-stmt) (visit-if-stmt object))
    ((ast:call) (visit-call object))
    ((ast:logical) (visit-logical object))
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
