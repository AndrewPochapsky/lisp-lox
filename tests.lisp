(defpackage tests
  (:use :cl))

(in-package tests)

(defun tokens-eq (a b)
  (and
    (eq (lexer:token-type a) (lexer:token-type b))
    (and
      (if (stringp b)
          (string= (lexer:token-lexeme a) (lexer:token-lexeme b))
          (equal (lexer:token-lexeme a) (lexer:token-lexeme b)))
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


(defun test-lexer-works()
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
        (assert (lists-eq (list
                            (lexer:create-left-paren "(")
                            (lexer:create-left-paren "(")
                            (lexer:create-right-paren ")")
                            (lexer:create-right-paren ")")
                            (lexer:create-left-brace "{")
                            (lexer:create-right-brace "}")
                            (lexer:create-bang "!")
                            (lexer:create-star "*")
                            (lexer:create-plus "+")
                            (lexer:create-minus "-")
                            (lexer:create-slash "/")
                            (lexer:create-equal "=")
                            (lexer:create-less "<")
                            (lexer:create-greater ">")
                            (lexer:create-less-equal "<=")
                            (lexer:create-equal-equal "==")
                            (lexer:create-bang "!")
                            (lexer:create-bang "!")
                            (lexer:create-number 456)
                            (lexer:create-string "This is a asdf string")
                            (lexer:create-string "Numbers in a string are all good 123.4556")
                            (lexer:create-number 123)
                            (lexer:create-number 12.56)
                            (lexer:create-string "asdf")
                            (lexer:create-dot ".")
                            (lexer:create-dot ".")
                            (lexer:create-dot ".")
                            (lexer:create-number 12.1)
                            (lexer:create-dot ".")
                            (lexer:create-string "")
                            (lexer:create-var "var")
                            (lexer:create-identifier "example")
                            (lexer:create-equal "=")
                            (lexer:create-number 123)
                            (lexer:create-semi-colon ";")
                            (lexer:create-fun "fun")
                            (lexer:create-identifier "example")
                            (lexer:create-left-paren "(")
                            (lexer:create-right-paren ")")
                            (lexer:create-left-brace "{")
                            (lexer:create-return "return")
                            (lexer:create-identifier "x")
                            (lexer:create-semi-colon ";")
                            (lexer:create-right-brace "}")
                            (lexer:create-eof)) (lexer:scan-tokens (coerce source 'list)) #'tokens-eq))))

(defun test-lexer-works-no-trailing-space()
      (let ((source "(1 + 1)"))
        (assert (lists-eq (list
                            (lexer:create-left-paren "(")
                            (lexer:create-number 1)
                            (lexer:create-plus "+")
                            (lexer:create-number 1)
                            (lexer:create-right-paren ")")
                            (lexer:create-eof))
                          (lexer:scan-tokens (coerce source 'list)) #'tokens-eq))))

(lexer:scan-tokens (coerce "(1+1)" 'list))
(test-lexer-works-no-trailing-space)

(defun test-lexer-works-simple()
      (let ((source (format nil "
          (123) + 12.6

          fun test() {

          }
      ")))
        (assert (lists-eq (list
                            (lexer:create-left-paren "(")
                            (lexer:create-number 123)
                            (lexer:create-right-paren ")")
                            (lexer:create-plus "+")
                            (lexer:create-number 12.6)
                            (lexer:create-fun "fun")
                            (lexer:create-identifier "test")
                            (lexer:create-left-paren "(")
                            (lexer:create-right-paren ")")
                            (lexer:create-left-brace "{")
                            (lexer:create-right-brace "}")
                            (lexer:create-eof))
                          (lexer:scan-tokens (coerce source 'list)) #'tokens-eq))))

(defun test-lexer-works-empty()
      (let ((source ""))
        (assert (lists-eq (list (lexer:create-eof)) (lexer:scan-tokens (coerce source 'list)) #'tokens-eq ))))

(defun test-parser-works()
    (let ((source "(5 + 2) * (5 - 2) / 3 "))
      (let ((tree (parser:parse (lexer:scan-tokens (coerce source 'list)))))
        (progn
          (assert (string= "(/ (* (group (+ 5 2)) (group (- 5 2))) 3)" (printer:accept tree)))))))

(defun run-lexer-tests()
    (progn
      (test-lexer-works-simple)
      (test-lexer-works)
      (test-lexer-works-no-trailing-space)
      (test-lexer-works-empty)))

(defun run-parser-tests()
    (progn
      (test-parser-works)))

(run-lexer-tests)
(run-parser-tests)
