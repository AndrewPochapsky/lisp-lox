;;;; lisp-lox.lisp

(in-package #:lisp-lox)

(defvar *had-error* nil)

(defstruct token
  type
  lexeme
  literal
  line)

(defun create-token (type lexeme literal line)
  (make-token :type type :lexeme lexeme :literal literal :line line))

(defun main(filename)
    (run-file filename))

(defun run-file (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      (run contents))))

(defun run (source)
  (scan-tokens (coerce source 'list)))

(defun scan-tokens (chars &optional tokens current)
  (if (null chars)
      (reverse (push (create-token 'eof "" nil 0) tokens))
      (multiple-value-bind (new-chars token) (scan-token chars)
          (scan-tokens new-chars (push token tokens)))))

(defun scan-token (chars &optional current-lexeme lexeme-type)
  (let ((c (car chars)))
   (cond
     ((and (null c) (eq lexeme-type 'string)) (error "Unterminated string."))
     ((and (eq c #\NewLine ) (eq lexeme-type 'comment)) (scan-token (cdr chars) nil nil))
     ((eq lexeme-type 'comment) (scan-token (cdr chars) nil 'comment))

     ((and (eq lexeme-type 'string) (eq c #\")) (values (cdr chars) (create-token 'string current-lexeme nil 0)))
     ((eq lexeme-type 'string) (scan-token (cdr chars) (concatenate 'string current-lexeme (string c)) 'string))

     ((or (eq c #\NewLine ) (eq c #\Space)) (scan-token (cdr chars)))

     ((eq c #\" ) (scan-token (cdr chars) "" 'string))
     ((eq c #\( ) (values (cdr chars) (create-token 'left-paren (string c) nil 0)))
     ((eq c #\) ) (values (cdr chars) (create-token 'right-paren (string c) nil 0)))
     ((eq c #\{ ) (values (cdr chars) (create-token 'left-brace (string c) nil 0)))
     ((eq c #\} ) (values (cdr chars) (create-token 'right-brace (string c) nil 0)))
     ((eq c #\, ) (values (cdr chars) (create-token 'comma (string c) nil 0)))
     ((eq c #\. ) (values (cdr chars) (create-token 'dot (string c) nil 0)))
     ((eq c #\- ) (values (cdr chars) (create-token 'minus (string c) nil 0)))
     ((eq c #\+ ) (values (cdr chars) (create-token 'plus (string c) nil 0)))
     ((eq c #\; ) (values (cdr chars) (create-token 'semi-colon (string c) nil 0)))
     ((eq c #\* ) (values (cdr chars) (create-token 'star (string c) nil 0)))
     ((eq c #\! ) (if (eq (cadr chars) #\=)
                      (values (cddr chars) (create-token 'bang-equal "!=" nil 0))
                      (values (cdr chars) (create-token 'bang (string c) nil 0))))
     ((eq c #\= ) (if (eq (cadr chars) #\=)
                      (values (cddr chars) (create-token 'equal-equal "==" nil 0))
                      (values (cdr chars) (create-token 'equal (string c)nil 0))))
     ((eq c #\< ) (if (eq (cadr chars) #\=)
                      (values (cddr chars) (create-token 'less-equal "<=" nil 0))
                      (values (cdr chars) (create-token 'less (string c)  nil 0))))
     ((eq c #\> ) (if (eq (cadr chars) #\=)
                      (values (cddr chars) (create-token 'greater-equal ">=" nil 0))
                      (values (cdr chars) (create-token 'greater (string c) nil 0))))
     ((eq c #\/ ) (if (eq (cadr chars) #\/)
                      (scan-token (cddr chars) nil 'comment)
                      (values (cdr chars) (create-token 'slash (string c) nil 0))))
     ((null c) (values nil nil))
     (t (error "Unexpected character ~C" c)))))

(main "test.lox")
