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

(defun scan-token (chars &optional current-lexeme is-comment)
  (let ((c (car chars)))
   (cond
     ((and (eq c #\NewLine ) is-comment) (scan-token (cdr chars) nil nil))
     (is-comment (scan-token (cdr chars) nil T))
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
     ((eq c #\NewLine ) (values (cdr chars) (create-token 'newline "" nil 0)))
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
                      (scan-token (cddr chars) nil T)
                      (values (cdr chars) (create-token 'slash (string c) nil 0))))
     (t (error "Unexpected character ~C" c)))))

(main "test.lox")
