(defpackage :lexer
  (:use :common-lisp))

(in-package #:lexer)

(defstruct token
  type
  lexeme
  literal
  line)

(defun create-token (type lexeme literal line)
  (make-token :type type :lexeme lexeme :literal literal :line line))

(defun create-eof ()
    (create-token 'eof "" nil 0))

(defmacro create-helpers (symbols)
  `(progn
     ,@(loop for sym in symbols
         collect (let ((func-name (intern (concatenate 'string (symbol-name :create-) (symbol-name sym)))))
           `(defun ,func-name (lexeme)
              (create-token ',sym lexeme nil 0))))))

(create-helpers (and class else false for if nil or print return super this true
                   var while number string left-paren right-paren left-brace right-brace
                   comma dot minus plus semi-colon star bang-equal bang equal-equal equal
                   less-equal less greater-equal greater slash identifier fun))

(defun parse-number (str)
  (with-input-from-string (stream str) (read stream)))

(defun valid-identifier (c)
  (or (alphanumericp c) (eq c #\_)))

(defparameter *keywords* (make-hash-table :test #'equal))
(setf (gethash "and" *keywords*) 'and)
(setf (gethash "class" *keywords*) 'class)
(setf (gethash "else" *keywords*) 'else)
(setf (gethash "false" *keywords*) 'false)
(setf (gethash "for" *keywords*) 'for)
(setf (gethash "fun" *keywords*) 'fun)
(setf (gethash "if" *keywords*) 'if)
(setf (gethash "nil" *keywords*) 'nil)
(setf (gethash "or" *keywords*) 'or)
(setf (gethash "print" *keywords*) 'print)
(setf (gethash "return" *keywords*) 'return)
(setf (gethash "super" *keywords*) 'super)
(setf (gethash "this" *keywords*) 'this)
(setf (gethash "true" *keywords*) 'true)
(setf (gethash "var" *keywords*) 'var)
(setf (gethash "while" *keywords*) 'while)

(defun scan-tokens (chars &optional tokens)
  (if (null chars)
      (if (or (null tokens) (not (eq (token-type (car tokens)) 'eof)))
          (reverse (push (create-eof) tokens))
          (reverse tokens))
        (multiple-value-bind (new-chars token) (scan-token chars)
          (scan-tokens new-chars (push token tokens)))))

(defun scan-token (chars &optional current-lexeme lexeme-type)
  (let ((c (car chars)))
   (cond
     ((and (null c) (eq lexeme-type 'string)) (error "Unterminated string."))
     ((null c) (values nil (create-eof)))
     ((and (eq c #\NewLine ) (eq lexeme-type 'comment)) (scan-token (cdr chars) nil nil))
     ((eq lexeme-type 'comment) (scan-token (cdr chars) nil 'comment))

     ((and (eq lexeme-type 'string) (eq c #\")) (values (cdr chars) (create-token 'string current-lexeme nil 0)))
     ((eq lexeme-type 'string) (scan-token (cdr chars) (concatenate 'string current-lexeme (string c)) 'string))

     ((or (alpha-char-p c) (and (valid-identifier c) (eq lexeme-type 'identifier)))
      (let ((updated-lexeme (concatenate 'string current-lexeme (string c))))
        (if (valid-identifier (cadr chars))
            (scan-token (cdr chars) updated-lexeme 'identifier)
            (let ((keyword (gethash updated-lexeme *keywords*)))
              (values (cdr chars) (create-token (if keyword keyword 'identifier) updated-lexeme nil 0))))))

     ((digit-char-p c)
      (let ((updated-lexeme (concatenate 'string current-lexeme (string c))))
        (if (or (digit-char-p (cadr chars)) (and (eq (cadr chars) #\.) (eq lexeme-type 'integer)))
            ; This if statement insures that if we are in 'float mode we won't overwrite it with 'integer
            (scan-token (cdr chars) updated-lexeme (if (not lexeme-type) 'integer lexeme-type))
            ; There might be a problem with text like 123abc. How should this be handled?
            (values (cdr chars) (create-number (parse-number updated-lexeme))))))
     ((and (eq c #\.) (eq lexeme-type 'integer)) (scan-token (cdr chars) (concatenate 'string current-lexeme (string c)) 'float))

     ((or (eq c #\NewLine ) (eq c #\Space)) (scan-token (cdr chars)))

     ((eq c #\" ) (scan-token (cdr chars) "" 'string))
     ((eq c #\( ) (values (cdr chars) (create-left-paren (string c))))
     ((eq c #\) ) (values (cdr chars) (create-right-paren (string c))))
     ((eq c #\{ ) (values (cdr chars) (create-left-brace (string c))))
     ((eq c #\} ) (values (cdr chars) (create-right-brace (string c))))
     ((eq c #\, ) (values (cdr chars) (create-comma (string c))))
     ((eq c #\. ) (values (cdr chars) (create-dot (string c))))
     ((eq c #\- ) (values (cdr chars) (create-minus (string c))))
     ((eq c #\+ ) (values (cdr chars) (create-plus (string c))))
     ((eq c #\; ) (values (cdr chars) (create-semi-colon (string c))))
     ((eq c #\* ) (values (cdr chars) (create-star (string c))))
     ((eq c #\! ) (if (eq (cadr chars) #\=)
                      (values (cddr chars) (create-bang-equal "!="))
                      (values (cdr chars) (create-bang (string c)))))
     ((eq c #\= ) (if (eq (cadr chars) #\=)
                      (values (cddr chars) (create-equal-equal "=="))
                      (values (cdr chars) (create-equal (string c)))))
     ((eq c #\< ) (if (eq (cadr chars) #\=)
                      (values (cddr chars) (create-less-equal "<="))
                      (values (cdr chars) (create-less (string c)))))
     ((eq c #\> ) (if (eq (cadr chars) #\=)
                      (values (cddr chars) (create-greater-equal ">="))
                      (values (cdr chars) (create-greater (string c)))))
     ((eq c #\/ ) (if (eq (cadr chars) #\/)
                      (scan-token (cddr chars) nil 'comment)
                      (values (cdr chars) (create-slash (string c)))))
     (t (error "Unexpected character ~C" c)))))

(let ((pack (find-package :lexer)))
  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
