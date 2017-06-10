;;; -*- lexical-binding: t -*-

(ert-deftest-async
 test/js99/debounce
 (done)
 (let ((f (js99-debounce (lambda () (funcall done)) 0.1)))
   (funcall f)
   (funcall f)
   (funcall f)))

(js99-define-ast-type 'A)
(js99-define-ast-type 'B '(A))
(js99-define-ast-type 'C '(A))
(js99-define-ast-type 'D '(B C))
(js99-define-ast-type 'E '(D))

(ert-deftest test/js99/ast-subtypes ()
  (should (js99-ast-subtypeof 'D 'B))
  (should (js99-ast-subtypeof 'D 'C))
  (should (js99-ast-subtypeof 'D 'A))
  (should (not (js99-ast-subtypeof 'D 'E)))
  (should (js99-ast-subtypeof 'E 'D))
  (should (js99-ast-subtypeof 'B 'A))
  (should (js99-ast-subtypeof 'A 'A)))

(ert-deftest test/js99/ast-parents ()
  (should (equal (js99-ast-parent-types 'D) '(B C)))
  (should (equal (js99-ast-parent-types 'A) nil)))

(ert-deftest test/js99/ast-ancestors ()
  (should (equal (js99-ast-ancestor-types 'D) '(B C A)))
  (should (equal (js99-ast-ancestor-types 'A) nil)))
