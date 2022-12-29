(defpackage tests
  (:use :cl))

(in-package tests)

(defun tokens-eq (a b)
  (and
    (eq (lexer:token-type a) (lexer:token-type b))
    (and
      (string= (lexer:token-lexeme a) (lexer:token-lexeme b))
      (and
        (eq (lexer:token-literal a) (lexer:token-literal b))
        (eq (lexer:token-line a) (lexer:token-line b))))))

(defun lists-eq (a b func)
  (if (and (null a) (null b))
      T
      (and
        (eq (length a) (length b))
        (and
          (funcall func (car a) (car b))
          (lists-eq (cdr a) (cdr b) func)))))


#|(defun test-lexer-works()
      (let ((source (format nil "
            // this is a comment\n
            (( )){} // grouping stuff
            // \"This should be ignored\"
            !*+-/=<> <= == // operators
            !! 456 \"This is a asdf string\"
            \"Numbers in a string are all good 123.4556\"
            123
            12.56\"asdf\"...
            12.1.\"\"

            var example = 123;

            fun example() {
                return x;
            }
      ")))
        (lexer:scan-tokens (coerce source 'list))))|#

(defun test-lexer-works-empty()
      (let ((source ""))
        (assert (lists-eq (list (lexer:create-eof)) (lexer:scan-tokens (coerce source 'list)) #'tokens-eq ))))

(test-lexer-works-empty)

;(tokens-eq (lexer:create-eof) (lexer:create-eof))

;(token-lists-eq (list (lexer:create-eof)) (list (lexer:create-eof)))

;(eq "" "")

