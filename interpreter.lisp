(defpackage :interpreter
  (:use :common-lisp)
  (:export #:interpret))

(in-package #:interpreter)

(defstruct lox-class name superclass methods)
(defun create-lox-class (name superclass methods)
  (make-lox-class :name name :superclass superclass :methods methods))

(defun find-class-method (class name)
  (let ((method (gethash name (lox-class-methods class) :not-found))
        (superclass (lox-class-superclass class)))
    (if (eq :not-found method)
        (if (null superclass)
            :not-found
            (find-class-method superclass name))
        method)))

(defstruct lox-instance class fields)
(defun create-lox-instance (class)
  (make-lox-instance :class class :fields (make-hash-table :test #'equal)))

(defun get-instance-value (instance name-token)
  (let* ((name (lexer:token-lexeme name-token))
         (fields (lox-instance-fields instance))
         (existing-value (gethash name fields :not-found))
         (existing-method (find-class-method (lox-instance-class instance) name)))
    (if (eq existing-value :not-found)
        (if (eq existing-method :not-found)
            (error "Undefined property '~a'." name)
            (let* ((env (environment:create-env-with-enclosing (lox-function-closure existing-method)))
                   (env (environment:define-with-name env "this" instance)))
              (create-lox-function (lox-function-params existing-method) (lox-function-body existing-method)
                                   env (lox-function-is-initializer existing-method))))
        existing-value)))

(defun set-instance-value (instance name-token value)
  (let ((name (lexer:token-lexeme name-token))
        (fields (lox-instance-fields instance)))
    (setf (gethash name fields) value)))

(define-condition lox-return (error)
  ((value :initarg :value :reader value)))

(defstruct lox-function arity call closure params body is-initializer)
(defun create-lox-function (params body closure is-initializer)
    (let ((arity (lambda () (length params)))
          (call (lambda (arguments)
                  (let ((env (environment:create-env-with-enclosing closure)))
                    (labels ((helper (params arguments env)
                               (if (not (null params))
                                     (helper
                                       (cdr params)
                                       (cdr arguments)
                                       (environment:define-with-name env (lexer:token-lexeme (car params)) (car arguments)))
                                   env)))
                        (let ((env (helper params arguments env)))
                          (if is-initializer
                              (progn
                                (execute-block body env)
                                ; return this in init
                                (environment:get-value env (lexer:create-token 'this "this" nil 0)))
                              (handler-case
                                  (execute-block body env)
                                ; Code doesn't currently gracefully prevent returning in an initializer
                                (lox-return (c)
                                  (value c))))))))))
      (make-lox-function :arity arity :call call :closure closure :params params :body body :is-initializer is-initializer)))

(defun call (callable arguments)
  (case (type-of callable)
    ((lox-function) (funcall (lox-function-call callable) arguments))
    ((lox-class)
     (let* ((instance (create-lox-instance callable))
            (initializer (find-class-method callable "init")))
       (if (eq initializer :not-found)
           instance
           (let* ((env (environment:create-env-with-enclosing (lox-function-closure initializer)))
                  (env (environment:define-with-name env "this" instance)))
             (progn
               (call (create-lox-function (lox-function-params initializer) (lox-function-body initializer) env T) arguments)
               instance)))))
    (t (error "Invalid callable type for call"))))

(defun arity (callable)
  (case (type-of callable)
    ((lox-function) (funcall (lox-function-arity callable)))
    ((lox-class)
     (let ((initializer (find-class-method callable "init")))
       (if (eq initializer :not-found)
           0
           (arity initializer))))
    (t (error "Invalid callable type for arity ~a" callable))))

(ast:defvisit literal (value) (list value ast:env))

(ast:defvisit grouping (expression)
  (let* ((result (accept expression ast:env))
         (value (first result))
         (env (second result)))
    (list value env)))

(ast:defvisit unary (operator right)
  (let* ((result (accept right ast:env))
         (right (first result))
         (env (second result))
         (type (symbol-name (lexer:token-type operator))))
    (cond
      ((string= "MINUS" type)
       (progn
         (check-number right)
         (list (- right) env)))
      ((string= "BANG" type)
       (list (not (is-truthy right)) env))
      (t nil))))

(ast:defvisit binary (left operator right)
  (let* ((left-result (accept left ast:env))
         (right-result (accept right (second left-result)))
         (left (first left-result))
         (right (first right-result))
         (env (second right-result))
         (type (symbol-name (lexer:token-type operator))))
    (cond
      ((string= "GREATER" type)
       (progn
         (check-numbers left right)
         (list (> left right) env)))
      ((string= "GREATER-EQUAL" type)
       (progn
         (check-numbers left right)
         (list (>= left right) env)))
      ((string= "LESS" type)
       (progn
         (check-numbers left right)
         (list (< left right) env)))
      ((string= "LESS-EQUAL" type)
       (progn
         (check-numbers left right)
         (list (<= left right) env)))
      ((string= "BANG-EQUAL" type) (not (is-equal left right)))
      ((string= "EQUAL-EQUAL" type) (is-equal left right))
      ((string= "MINUS" type)
       (progn
         (check-numbers left right)
         (list (- left right) env)))
      ((string= "PLUS" type)
       (cond
         ((and (stringp left) (stringp right)) (list (concatenate 'string left right) env))
         ((and (numberp left) (numberp right))
          (progn
            (check-numbers left right)
            (list (+ left right) env)))
         (t (error "Operands must be two numbers or two strings. Actual: ~a ~a" left right))))
      ((string= "SLASH" type)
       (progn
         (check-numbers left right)
         (list (/ left right) env)))
      ((string= "STAR" type)
       (progn
         (check-numbers left right)
         (list (* left right) env)))
      (t nil))))

(ast:defvisit expression-stmt (expression)
  (let* ((result (accept expression ast:env))
         (env (second result)))
    (list nil env)))

(ast:defvisit print-stmt (expression)
  (let* ((result (accept expression ast:env))
         (value (first result))
         (env (second result)))
    (list (print value) env)))

(ast:defvisit variable-decl (name initializer)
  (if (null initializer)
      (list nil (environment:define ast:env name nil))
      (let* ((result (accept initializer ast:env))
            (value (first result))
            (env (second result)))
        (list nil (environment:define env name value)))))

(ast:defvisit variable-ref (name)
  (list (environment:get-value ast:env name) ast:env))

(ast:defvisit this (keyword)
  (list (environment:get-value ast:env keyword) ast:env))

(ast:defvisit assign (name expression)
  (let* ((result (accept expression ast:env))
         (value (first result))
         (env (second result)))
    (progn
      ; assign is modifying the current env inplace
      (environment:assign env name value)
      (list value env))))

(ast:defvisit block-stmt (statements)
  (execute-block statements (environment:create-env-with-enclosing ast:env)))

(ast:defvisit if-stmt (condition then-branch else-branch)
  (let* ((result (accept condition ast:env))
         (condition (first result))
         (env (second result)))
    (if (is-truthy condition)
        (accept then-branch env)
        (if (not (null else-branch))
            (accept else-branch env)
            (list nil env)))))

(ast:defvisit while-stmt (condition body)
  (let* ((result (accept condition ast:env))
         (condition (first result))
         (env (second result)))
    (if (is-truthy condition)
        (let* ((result (accept body env))
               (env (second result)))
          (visit-while-stmt ast:obj env)))
    (list nil env)))

(ast:defvisit logical (left operator right)
  (let* ((result (accept left ast:env))
         (left (first result))
         (env (second result)))
    (if (string= (lexer:token-type operator) 'or)
        (if (is-truthy left)
            (list left env)
            (accept right env))
        (if (not (is-truthy left))
            (list left env)
            (accept right env)))))

(ast:defvisit call (callee arguments)
  (let* ((result (accept callee ast:env))
         (callee (first result))
         (env (second result))
         (func (lambda (argument) (accept argument env)))
         (arguments (mapcar  #'first (mapcar func arguments))))
    (if (eq (length arguments) (arity callee))
        (list (call callee arguments) env)
        (error "Expected ~a arguments but got ~a." (arity callee) (length arguments)))))

(ast:defvisit class-decl (name superclass methods)
  (let* ((env (environment:define ast:env name "placeholder"))
         (methods-table (make-hash-table :test #'equal))
         (evaled-superclass
           (if (null superclass)
               nil
               (let ((superclass (first (accept superclass env))))
                 (if (not (lox-class-p superclass))
                     (error "Superclass must be a class")
                     superclass))))
         (env (if (null evaled-superclass)
                  env
                  (environment:define-with-name (environment:create-env-with-enclosing env) "super" evaled-superclass))))
    (progn
      (dolist (method methods)
        (let ((func (create-lox-function (ast:function-decl-params method) (ast:function-decl-body method) env
                                         (string= (lexer:token-lexeme (ast:function-decl-name method)) "init"))))
          (setf (gethash (lexer:token-lexeme (ast:function-decl-name method)) methods-table) func)))
      (let ((env (if (not (null superclass))
                     (environment:environment-enclosing env)
                     env)))
        (environment:assign env name (create-lox-class name evaled-superclass methods-table))
        (list nil env)))))

(ast:defvisit super (keyword method)
  (let* ((superclass (environment:get-value ast:env keyword))
         ; Might need to get from enclosing
         (instance (environment:get-value ast:env (lexer:create-token 'this "this" nil 0)))
         (evaled-method (find-class-method superclass (lexer:token-lexeme method))))
    (if (eq evaled-method :not-found)
        (error "Undefined property '~a'" (lexer:token-lexeme method))
        (let* ((env (environment:create-env-with-enclosing (lox-function-closure evaled-method)))
               (env (environment:define-with-name env "this" instance)))
          (list (create-lox-function (lox-function-params evaled-method) (lox-function-body evaled-method) env (lox-function-is-initializer evaled-method)) ast:env)))))

(ast:defvisit function-decl (name params body)
  (let* ((env (environment:define ast:env name "placeholder"))
         (func (create-lox-function params body env nil)))
    (progn
      (environment:assign env name func)
      (list nil env))))

(ast:defvisit get-expr (object name)
  (let* ((result (accept object ast:env))
         (object (first result))
         (env (second result)))
    (if (lox-instance-p object)
        (list (get-instance-value object name) env)
        (error "Only instances have properties"))))

(ast:defvisit set-expr (object name value)
  (let* ((result (accept object ast:env))
         (object (first result))
         (env (second result)))
    (if (not (lox-instance-p object))
        (error "Only instances have fields")
        (let* ((result (accept value env))
               (value (first result))
               (env (second result)))
          (progn
            (set-instance-value object name value)
            (list value env))))))

(ast:defvisit return-stmt (expression)
  (let ((value
          (if (null expression)
              nil
              (accept expression ast:env))))
    ; Do we need to pass env in here? I don't think so...
    (error 'lox-return :value (car value))))

(defun execute-block (statements new-env)
  (labels ((helper (statements env)
             (if (null statements)
                 env
                 (let* ((result (accept (car statements) env))
                        (env (second result)))
                   (helper (cdr statements) env)))))
    (let* ((env (helper statements new-env)))
      (list nil (environment:environment-enclosing env)))))

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

(defun visit-interpreter (object env)
  "Implements the operation for OBJECT using the visitor pattern."
  (case (type-of object)
    ((ast:expression-stmt) (visit-expression-stmt object env))
    ((ast:print-stmt) (visit-print-stmt object env))
    ((ast:while-stmt) (visit-while-stmt object env))
    ((ast:variable-decl) (visit-variable-decl object env))
    ((ast:function-decl) (visit-function-decl object env))
    ((ast:class-decl) (visit-class-decl object env))
    ((ast:get-expr) (visit-get-expr object env))
    ((ast:set-expr) (visit-set-expr object env))
    ((ast:variable-ref) (visit-variable-ref object env))
    ((ast:this) (visit-this object env))
    ((ast:super) (visit-super object env))
    ((ast:block-stmt) (visit-block-stmt object env))
    ((ast:return-stmt) (visit-return-stmt object env))
    ((ast:if-stmt) (visit-if-stmt object env))
    ((ast:call) (visit-call object env))
    ((ast:logical) (visit-logical object env))
    ((ast:assign) (visit-assign object env))
    ((ast:binary) (visit-binary object env))
    ((ast:unary) (visit-unary object env))
    ((ast:grouping) (visit-grouping object env))
    ((ast:literal) (visit-literal object env))))

(defun accept (obj env)
    (ast:accept obj env #'visit-interpreter))

(defun interpret (expressions)
  (let* ((globals
          (environment:define-with-name (environment:create-env) "clock"
            (make-lox-function
              :arity (lambda () 0)
              :call (lambda (arguments) (/ (get-internal-real-time) 1000))
              :closure nil)))
         (env (environment:create-env-with-enclosing globals)))
    (labels ((helper (expressions env)
               (if (null expressions)
                   nil
                   (let* ((result (accept (car expressions) env))
                          (env (second result)))
                     (helper (cdr expressions) env)))))
      (helper expressions env))))
