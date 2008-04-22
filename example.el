;;; example to use test

(require 'test)

(defun my-t () nil)

(defcase my-example-test-1 (example-suite) nil
  (test-assert-ok t)
  (test-assert-ok (my-t)))

(defun add2 (n) (+ 3 n))
(defun say () "hello, world")
(defun elem () 'where)

(defcase my-example-test-2 (example-suite) nil
  (test-assert-eq (add2 2) 4)
  (test-assert-string-equal (say) "hello, test")
  (test-assert-memq (elem) '(which what how)))