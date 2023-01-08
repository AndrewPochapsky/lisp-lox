(defpackage :parser
  (:use :common-lisp))

(in-package #:parser)

(defun parse (input)
  (labels ((helper (stmts input)
             (if (string= (symbol-name (lexer:token-type (car input))) "EOF")
                 (reverse stmts)
                 (let* ((result (decl input))
                        (stmt (first result))
                        (rest (second result)))
                   (helper (cons stmt stmts) rest)))))
    (helper '() input)))

(defun decl (input)
  (let ((token-type (lexer:token-type (car input))))
    (cond
      ((string= token-type 'var) (var-declaration (cdr input)))
      ((string= token-type 'class) (class-declaration (cdr input)))
      ((string= token-type 'fun) (function-declaration (cdr input) "function"))
      (t (statement input)))))

(defun class-declaration (input)
  (let ((name (car input)))
    (if (not (string= (lexer:token-type name) 'identifier))
        (error "Expected class name")
        (let* ((superclass
                 (if (string= (lexer:token-type (cadr input)) 'less)
                     (if (not (string= (lexer:token-type (caddr input)) 'identifier))
                         (error "Expected superclass name.")
                         (ast:make-variable-ref :name (caddr input)))
                     nil))
               (rest (if (null superclass)
                         (cdr input)
                         (cdddr input))))
          ; cadr input
          (if (not (string= (lexer:token-type (car rest)) 'left-brace))
              (error "Expected '{' before class body")
              (labels ((helper (methods input)
                         (let ((next-type (lexer:token-type (car input))))
                           (if (or (string= next-type 'right-brace) (string= next-type 'eof))
                               ; Note that this returns the methods in reverse order.
                               (list methods input)
                               (let* ((result (function-declaration input "method"))
                                      (method (first result))
                                      (rest (second result)))
                                 (helper (push method methods) rest))))))
                (let* ((result (helper '() (cdr rest)))
                       (methods (first result))
                       (rest (second result)))
                  (if (not (string= (lexer:token-type (car rest)) 'right-brace))
                      (error "Expected '}' after class body but was ~a" (car rest))
                      (list (ast:make-class-decl :name name :superclass superclass :methods methods) (cdr rest))))))))))

(defun function-declaration (input kind)
  (let ((name (car input)))
    (if (not (string= (lexer:token-type name) 'identifier))
        (error "Expected ~a name" kind)
        (if (not (string= (lexer:token-type (cadr input)) 'left-paren))
            (error "Expected '(' after ~a name." kind)
            (if (string= (lexer:token-type (caddr input)) 'right-paren)
                (if (not (string= (lexer:token-type (cadddr input)) 'left-brace))
                    (error "Expected '{' before ~a body." kind)
                    (let* ((result (block-statement (cddddr input)))
                           (body (first result))
                           (rest (second result)))
                      (list (ast:make-function-decl :name name :params '() :body body) rest)))
                (labels ((helper (parameters input)
                           (if (not (string= (lexer:token-type (car input)) 'identifier))
                               (error "Expected parameter name")
                               (if (not (string= (lexer:token-type (cadr input)) 'comma))
                                   (list (reverse (push (car input) parameters)) (cdr input))
                                   (helper (push (car input) parameters) (cddr input))))))
                  (let* ((result (helper '() (cddr input)))
                         (parameters (first result))
                         (rest (second result)))
                    (if (not (string= (lexer:token-type (car rest)) 'right-paren))
                        (error "Expect 'right-paren' after paramters")
                        (if (not (string= (lexer:token-type (cadr rest)) 'left-brace))
                            (error "Expected '{' before ~a body" kind)
                            (let* ((result (block-statement (cddr rest)))
                                   (body (first result))
                                   (rest (second result)))
                              (list (ast:make-function-decl :name name :params parameters :body body) rest)))))))))))

(defun var-declaration (input)
  (let ((name (car input)))
    (if (string= (symbol-name (lexer:token-type name)) "IDENTIFIER")
        (if (string= (symbol-name (lexer:token-type (cadr input))) "EQUAL")
            (let* ((result (expression (cddr input)))
                   (initializer (first result))
                   (rest (second result)))
              (if (string= (symbol-name (lexer:token-type (car rest))) "SEMI-COLON")
                  (list (ast:make-variable-decl :name name :initializer initializer) (cdr rest))
                  (error "Expected ';' after variable declaration")))
            (if (string= (symbol-name (lexer:token-type (cadr input))) "SEMI-COLON")
              (list (ast:make-variable-decl :name name :initializer nil) (cddr input))
              (error "Expected ';' after variable declaration")))
        (error "Expected variable name"))))

(defun statement (input)
  (let ((token-type (lexer:token-type (car input))))
    (cond
      ((string= token-type 'if) (if-statement (cdr input)))
      ((string= token-type 'for) (for-statement (cdr input)))
      ((string= token-type 'return) (return-statement (cdr input)))
      ((string= token-type 'while) (while-statement (cdr input)))
      ((string= token-type 'print) (print-statement (cdr input)))
      ((string= token-type 'left-brace)
       (let* ((result (block-statement (cdr input)))
              (statements (first result))
              (rest (second result)))
         (list (ast:make-block-stmt :statements statements) rest)))
      (t (expression-statement input)))))


(defun return-statement (input)
  (if (string= (lexer:token-type (car input)) 'semi-colon)
      (list (ast:make-return-stmt :expression nil) (cdr input))
      (let* ((result (expression input))
             (expr (first result))
             (rest (second result)))
        (if (string= (lexer:token-type (car rest)) 'semi-colon)
            (list (ast:make-return-stmt :expression expr) (cdr rest))
            (error "Expected ';' after return value.")))))

(defun while-statement (input)
  (if (string= (lexer:token-type (car input)) 'left-paren)
      (let* ((result (expression (cdr input)))
             (condition (first result))
             (rest (second result)))
        (if (string= (lexer:token-type (car rest)) 'right-paren)
            (let* ((result (statement (cdr rest)))
                   (body (first result))
                   (rest (second result)))
              (list (ast:make-while-stmt :condition condition :body body) rest))
            (error "Expected ')' after condition.")))
      (error "Expected '(' after 'while'.")))

(defun for-statement (input)
  (if (string= (lexer:token-type (car input)) 'left-paren)
      (let* ((token-type (lexer:token-type (cadr input)))
             (init-result
               (cond
                 ((string= token-type 'semi-colon) (list nil (cddr input)))
                 ((string= token-type 'var) (var-declaration (cddr input)))
                 (t (expression-statement (cdr input)))))
             (condition-result
               (if (string= (lexer:token-type (car (second init-result))) 'semi-colon)
                   (list nil (cdr (second init-result)))
                   (let ((result (expression (second init-result))))
                     (if (string= (lexer:token-type (car (second result))) 'semi-colon)
                         (list (first result) (cdr (second result)))
                         (error "Expected ';' after loop condition")))))
             (increment-result
               (if (not (string= (lexer:token-type (car (second condition-result))) 'right-paren))
                   (let ((result (expression (second condition-result))))
                     (if (string= (lexer:token-type (car (second result))) 'right-paren)
                         (list (first result) (cdr (second result)))
                         (error "Expected ')' after for clauses")))
                   (list nil (cdr (second condition-result)))))
             (body-result (statement (second increment-result)))
             (body
               (if (not (null (first increment-result)))
                   (ast:make-block-stmt :statements (list (first body-result) (first increment-result)))
                   (first body-result)))
             (body
               (if (null (first condition-result))
                   (ast:make-while-stmt :condition (ast:make-literal :value 'true) :body body)
                   (ast:make-while-stmt :condition (first condition-result) :body body)))
             (body
               (if (not (null (first init-result)))
                   (ast:make-block-stmt :statements (list (first init-result) body))
                   body)))
        (list body (second body-result)))
    (error "Expected '(' after 'for'.")))

(defun if-statement (input)
  (if (string= (lexer:token-type (car input)) 'left-paren)
      (let* ((result (expression (cdr input)))
             (condition (first result))
             (rest (second result)))
        (if (string= (lexer:token-type (car rest)) 'right-paren)
            (let* ((result (statement (cdr rest)))
                   (then-branch (first result))
                   (rest (second result)))
              (if (string= (lexer:token-type (car rest)) 'else)
                  (let* ((result (statement (cdr rest)))
                         (else-branch (first result))
                         (rest (second result)))
                    (list (ast:make-if-stmt :condition condition :then-branch then-branch :else-branch else-branch) rest))
                  (list (ast:make-if-stmt :condition condition :then-branch then-branch :else-branch nil) rest)))
            (error "Expected ')' after if condition but was ~a" (car rest))))
      (error "Expected '(' after 'if' but was ~a" (car input))))

(defun print-statement (input)
  (let* ((result (expression input))
         (expr (first result))
         (rest (second result)))
    (if (string= (symbol-name (lexer:token-type (car rest))) "SEMI-COLON")
        (list (ast:make-print-stmt :expression expr) (cdr rest))
        (error "Expected ';' after value but was ~a" (car rest)))))

(defun expression-statement (input)
  (let* ((result (expression input))
         (expr (first result))
         (rest (second result)))
    (if (string= (symbol-name (lexer:token-type (car rest))) "SEMI-COLON")
        (list (ast:make-expression-stmt :expression expr) (cdr rest))
        (error "Expected ';' after expression but was ~a" (car rest)))))

(defun block-statement (input)
  (labels ((helper (statements input)
             (let ((token-type (lexer:token-type (car input))))
               (cond
                 ((string= token-type 'right-brace)
                  (list (reverse statements) (cdr input)))
                 ((string= token-type 'eof)
                  (error "Expected '}' after block"))
                 (t (let* ((result (decl input))
                           (value (first result))
                           (rest (second result)))
                      (helper (push value statements) rest)))))))
    (helper '() input)))


(defun expression (input)
  (assignment input))

(defun assignment (input)
  (let* ((result (or-expression input))
         (expr (first result))
         (rest (second result)))
    (if (string= (lexer:token-type (car rest)) 'equal)
        (let* ((result (assignment (cdr rest)))
               (value (first result))
               (rest (second result)))
          (cond
            ((ast:variable-ref-p expr)
             (list (ast:make-assign :name (ast:variable-ref-name expr) :expression value) rest))
            ((ast:get-expr-p expr)
             (list (ast:make-set-expr :object (ast:get-expr-object expr) :name (ast:get-expr-name expr) :value value) rest))
            (t (error "Invalid assignment target"))))
        (list expr rest))))

(defun or-expression (input)
  (let* ((result (and-expression input))
         (expr (first result))
         (rest (second result)))
    (labels ((helper (expr input)
               (let ((operator (car input)))
                 (if (string= (lexer:token-type operator) 'or)
                     (let* ((result (and-expression (cdr input)))
                            (right (first result))
                            (rest (second result)))
                       (helper (ast:make-logical :left expr :operator operator :right right) rest))
                     (list expr input)))))
      (helper expr rest))))


(defun and-expression (input)
  (let* ((result (equality input))
         (expr (first result))
         (rest (second result)))
    (labels ((helper (expr input)
               (let ((operator (car input)))
                 (if (string= (lexer:token-type operator) 'and)
                     (let* ((result (equality (cdr input)))
                            (right (first result))
                            (rest (second result)))
                       (helper (ast:make-logical :left expr :operator operator :right right) rest))
                     (list expr input)))))
      (helper expr rest))))

(defun equality (input)
  (let* ((result (comparison input))
         (expr (first result))
         (rest (second result)))
    (labels ((helper (expr input)
               (if (member (symbol-name (lexer:token-type (car input))) '("BANG-EQUAL" "EQUAL-EQUAL") :test #'string=)
                   (let* ((result (comparison (cdr input)))
                          (right (first result))
                          (rest (second result)))
                     (helper (ast:make-binary :left expr :operator (car input) :right right) rest))
                   (list expr input))))
      (helper expr rest))))

(defun comparison (input)
  (let* ((result (term input))
         (expr (first result))
         (rest (second result)))
    (labels ((helper (expr input)
               (let ((operator (car input)))
                 (if (member (symbol-name (lexer:token-type operator)) '("GREATER" "GREATER-EQUAL" "LESS" "LESS-EQUAL") :test #'string=)
                     (let* ((result (term (cdr input)))
                            (right (first result))
                            (rest (second result)))
                       (helper (ast:make-binary :left expr :operator operator :right right) rest))
                     (list expr input)))))
      (helper expr rest))))


(defun term (input)
    (let* ((result (factor input))
           (expr (first result))
           (rest (second result)))
      (labels ((helper (expr input)
                 (let ((operator (car input)))
                   (if (member (symbol-name (lexer:token-type operator)) '("MINUS" "PLUS") :test #'string=)
                       (let* ((result (factor (cdr input)))
                              (right (first result))
                              (rest (second result)))
                         (helper (ast:make-binary :left expr :operator operator :right right) rest))
                       (list expr input)))))
        (helper expr rest))))

(defun factor (input)
    (let* ((result (unary input))
           (expr (first result))
           (rest (second result)))
      (labels ((helper (expr input)
                 (let ((operator (car input)))
                   (if (member (symbol-name (lexer:token-type operator)) '("SLASH" "STAR") :test #'string=)
                       (let* ((result (unary (cdr input)))
                              (right (first result))
                              (rest (second result)))
                         (helper (ast:make-binary :left expr :operator operator :right right) rest))
                       (list expr input)))))
        (helper expr rest))))


(defun unary (input)
  (let ((operator (car input)))
    (if (member (symbol-name (lexer:token-type operator)) '("BANG" "MINUS") :test #'string=)
        (let* ((result (unary (cdr input)))
               (right (first result))
               (rest (second result)))
          (list (ast:make-unary :operator operator :right right) rest))
        (call input))))

(defun call (input)
  (let* ((result (primary input))
         (expr (first result))
         (rest (second result)))
    (labels ((helper (expr input)
               (let ((token-type (lexer:token-type (car input))))
                 (cond
                   ((string= token-type 'dot)
                    (let ((name (cadr input)))
                      (if (not (string= (lexer:token-type name) 'identifier))
                          (error "Expected property name after '.'.")
                          (helper (ast:make-get-expr :object expr :name name) (cddr input)))))
                   ((string= token-type 'left-paren)
                    (let* ((result (finish-call expr (cdr input)))
                           (expr (first result))
                           (rest (second result)))
                      (helper expr rest)))
                   (t (list expr input))))))
      (helper expr rest))))

(defun finish-call (callee input)
  (if (not (string= (lexer:token-type (car input)) 'right-paren))
      (labels ((helper (arguments input)
                 (if (string= (lexer:token-type (car input)) 'comma)
                     (let* ((result (expression (cdr input)))
                            (argument (first result))
                            (rest (second result)))
                       (helper (push argument arguments) rest))
                     (list (reverse arguments) input))))
        (let* ((result (expression input))
               (argument (first result))
               (rest (second result)))
          (let* ((result (helper (list argument) rest))
                 (arguments (first result))
                 (rest (second result)))
            (if (string= (lexer:token-type (car rest)) 'right-paren)
                (list (ast:make-call :callee callee :paren nil :arguments arguments) (cdr rest))
                (error "Expected ')' after arguments.")))))
    (list (ast:make-call :callee callee :paren nil :arguments '()) (cdr input))))

(defun primary (input)
  (let ((token-type (lexer:token-type (car input))))
    (cond
      ((string= token-type 'identifier) (list (ast:make-variable-ref :name (car input)) (cdr input)))
      ((string= token-type 'this) (list (ast:make-this :keyword (car input)) (cdr input)))
      ((string= token-type 'false) (list (ast:make-literal :value 'false) (cdr input)))
      ((string= token-type 'true) (list (ast:make-literal :value 'true) (cdr input)))
      ((string= token-type 'null) (list (ast:make-literal :value nil) (cdr input)))
      ((string= token-type 'number) (list (ast:make-literal :value (lexer:token-lexeme (car input))) (cdr input)))
      ((string= token-type 'string) (list (ast:make-literal :value (lexer:token-lexeme (car input))) (cdr input)))
      ((string= token-type 'left-paren) (let* ((result (expression (cdr input)))
                                            (expr (first result))
                                            (rest (second result)))
                                       (if (string= (symbol-name (lexer:token-type (car rest))) "RIGHT-PAREN")
                                           (list (ast:make-grouping :expression expr) (cdr rest))
                                           (error "Expected ')' after expression."))))
      (t (error "Expected expression but got unexpected token ~a" (car input))))))

(let ((pack (find-package :parser)))
  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
