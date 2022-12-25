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
      (reverse tokens)
      (scan-tokens (cdr chars) (push (scan-token chars) tokens))))

(defun scan-token (chars)
  (let ((c (car chars)))
   (cond
     ((eq c #\( ) (create-token 'left-paren (string c) nil 0))
     ((eq c #\) ) (create-token 'right-paren (string c) nil 0))
     ((eq c #\{ ) (create-token 'left-brace (string c) nil 0))
     ((eq c #\} ) (create-token 'right-brace (string c) nil 0))
     ((eq c #\, ) (create-token 'comma (string c) nil 0))
     ((eq c #\. ) (create-token 'dot (string c) nil 0))
     ((eq c #\- ) (create-token 'minus (string c) nil 0))
     ((eq c #\+ ) (create-token 'plus (string c) nil 0))
     ((eq c #\; ) (create-token 'semi-colon (string c) nil 0))
     ((eq c #\* ) (create-token 'star (string c) nil 0))
     (t (create-token 'eof "" nil 0)))))

(main "test.lox")

#|
(defun scan-tokens
    (let ((tokens (list)))
      (loop
        while (not (is-at-end))
          do ()
          finally (setf (car tokens) (create-token 'eof "" 0)))))
|#

;(defvar test-token (create-token 'type "lexeme" 'literal 32))

;(format t "~a, ~a, ~a, ~a" (token-type test-token) (token-lexeme test-token) (token-literal test-token) (token-line test-token))

