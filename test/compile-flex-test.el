;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of buffer-manager-test.el
;;
;;; Code:

(require 'ert)
(require 'dash)
(require 'compile-flex)
(require 'compile-flex-make)

(ert-deftest test-manager-instance ()
  "Test successful evaluation of compile-flex"
  (let ((this the-flex-compile-manager))
    (should (eq t (eieio-object-p this)))
    (should (eq 'flex-compile-manager (eieio-object-class this)))))

(ert-deftest test-compiler-registration ()
  "Test registration of compilers."
  (should (equal '("disable" "make")
		 (-> the-flex-compile-manager
		     flex-compile-manager-compiler-names
		     (sort 'equal)))))
