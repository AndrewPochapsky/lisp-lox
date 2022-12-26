;;;; lisp-lox.lisp

(in-package #:lisp-lox)

(defvar *had-error* nil)

(defun main(filename)
    (run-file filename))

(defun run-file (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      (run contents))))

(defun run (source)
  (scan-tokens (coerce source 'list)))

(main "test.lox")
